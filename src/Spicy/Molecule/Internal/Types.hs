{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- |
-- Module      : Spicy.Molecule.Internal.Types
-- Description : Definitions of a molecule and its context
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides definitions of molecules and ONIOM setups. This includes the
-- context/input/output of calculations.
--
-- Molecules are defined with a lot of context and 'Molecule' this is the single most important type
-- in Spicy. The layout of the 'Molecule' data type keeps layered calculations, complex input
-- formats and fragmentation of molecules in mind and tries to apply to them all.
--
-- Regarding the hierarhy of a 'Molecule' in the context of ONIOM, the following conditions apply:
--
-- - The top layer of the recursion is the "real" system in ONIOM terminology.
-- - Stepping down the recursion more "model"-like ONIOM layers are reached.
module Spicy.Molecule.Internal.Types
  ( -- * Molecular Structure
    -- $moleculeStructure
    Element (..),
    Atom (..),
    Molecule (..),
    HasMolecule (..),
    LinkInfo (..),
    _NotLink,
    _IsLink,
    FFType (..),
    _FFMol2,
    _FFTXYZ,
    _FFPDB,
    _FFXYZ,
    _FFBq,
    Fragment (..),
    Trajectory,
    MolID,

    -- * Calculations
    -- $calculations
    CalcID (..),
    CalcK (..),
    _ONIOMKey,
    ONIOMHierarchy (..),

    -- ** Multipole Properties
    Multipoles (..),
    Monopole (..),
    MultipoleR0,
    Dipole (..),
    MultipoleR1,
    Quadrupole (..),
    MultipoleR2,
    Octopole (..),
    MultipoleR3,
    Hexadecapole (..),
    MultipoleR4,

    -- ** ONIOM Embedding
    Embedding (..),
    _Mechanical,
    _Electronic,

    -- ** Energy Derivatives
    EnergyDerivatives (..),

    -- ** Wrapper Context
    NumericalEfficiency (..),
    WrapperTask (..),
    QMContext (..),
    MMContext (..),
    QMMMSpec (..),
    _QM,
    _MM,
    CalcContext (..),
    CalcInput (..),
    CalcOutput (..),

    -- ** Programs
    Program (..),

    -- * Local Helper Types
    FragmentAtomInfo (..),
    TXYZAtomInfo (..),
  )
where

import Data.Aeson
import Data.List.Split (chunksOf)
import Data.Massiv.Array as Massiv hiding
  ( toList,
  )
import Formatting
import Optics hiding (element)
import RIO hiding (Lens', lens, view, (^.))
import qualified RIO.Text as Text
import Spicy.Aeson
import Spicy.Common

{-
####################################################################################################
-}

-- $moleculeStructure
-- These types define the molecule and its topology. This includes atom positions, bonds, atom types
-- and so on.

-- |
-- All chemical elements. Have them very clear because force fields and pdb names may interfer and are
-- just arbitrary strings.
data Element
  = H
  | He
  | Li
  | Be
  | B
  | C
  | N
  | O
  | F
  | Ne
  | Na
  | Mg
  | Al
  | Si
  | P
  | S
  | Cl
  | Ar
  | K
  | Ca
  | Sc
  | Ti
  | V
  | Cr
  | Mn
  | Fe
  | Co
  | Ni
  | Cu
  | Zn
  | Ga
  | Ge
  | As
  | Se
  | Br
  | Kr
  | Rb
  | Sr
  | Y
  | Zr
  | Nb
  | Mo
  | Tc
  | Ru
  | Rh
  | Pd
  | Ag
  | Cd
  | In
  | Sn
  | Sb
  | Te
  | I
  | Xe
  | Cs
  | Ba
  | La
  | Ce
  | Pr
  | Nd
  | Pm
  | Sm
  | Eu
  | Gd
  | Tb
  | Dy
  | Ho
  | Er
  | Tm
  | Yb
  | Lu
  | Hf
  | Ta
  | W
  | Re
  | Os
  | Ir
  | Pt
  | Au
  | Hg
  | Tl
  | Pb
  | Bi
  | Po
  | At
  | Rn
  | Fr
  | Ra
  | Ac
  | Th
  | Pa
  | U
  | Np
  | Pu
  | Am
  | Cm
  | Bk
  | Cf
  | Es
  | Fm
  | Md
  | No
  | Lr
  | Rf
  | Db
  | Sg
  | Bh
  | Hs
  | Mt
  | Ds
  | Rg
  | Cn
  | Nh
  | Fl
  | Mc
  | Lv
  | Ts
  | Og
  deriving (Show, Eq, Read, Ord, Enum, Generic, NFData)

instance ToJSON Element where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Element

instance PrettyPrint Element where
  prettyP = displayShow

----------------------------------------------------------------------------------------------------

-- |
-- An Atom in a 'Molecule'. Atoms are compared by their indices only and they must therefore be unique.
-- The coordinates of the 'Atom' are defined as 'Seq', as this is extremely easy to concatenate when
-- building a coordinate vector.
data Atom = Atom
  { -- | Chemical 'Element' of the atom.
    element :: !Element,
    -- | Label, e.g. from a pdb, just for identification,
    --   can be empty.
    label :: !Text,
    -- | Boolean, telling if this is a Link atom,
    --   introduced because a bond was broken. Also known as
    --   link atom in ONIOM.
    isLink :: !LinkInfo,
    -- | Whether the atom is a dummy atom, only providing
    --   multipole information.
    isDummy :: !Bool,
    -- | Label depending on the MM software used,
    --   identifying topological atom.
    ffType :: !FFType,
    -- | Coordinates of the atom, cartesian in R³. Relies on
    --   the parser to fill with exactly 3 values.
    coordinates :: !(VectorS Double),
    -- | Atom-centred multipole information after a
    --   calculation.
    multipoles :: !Multipoles
  }
  deriving (Show, Eq, Generic)

instance ToJSON Atom where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Atom

-- Lenses
instance (k ~ A_Lens, a ~ Element, b ~ a) => LabelOptic "element" k Atom Atom a b where
  labelOptic = lens (\s -> element s) $ \s b -> s {element = b}

instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "label" k Atom Atom a b where
  labelOptic = lens (\s -> (label :: Atom -> Text) s) $ \s b -> (s {label = b} :: Atom)

instance (k ~ A_Lens, a ~ LinkInfo, b ~ a) => LabelOptic "isLink" k Atom Atom a b where
  labelOptic = lens (\s -> isLink s) $ \s b -> s {isLink = b}

instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "isDummy" k Atom Atom a b where
  labelOptic = lens (\s -> isDummy s) $ \s b -> s {isDummy = b}

instance (k ~ A_Lens, a ~ FFType, b ~ a) => LabelOptic "ffType" k Atom Atom a b where
  labelOptic = lens (\s -> ffType s) $ \s b -> s {ffType = b}

instance (k ~ A_Lens, a ~ VectorS Double, b ~ a) => LabelOptic "coordinates" k Atom Atom a b where
  labelOptic = lens (\s -> coordinates s) $ \s b -> s {coordinates = b}

instance (k ~ A_Lens, a ~ Multipoles, b ~ a) => LabelOptic "multipoles" k Atom Atom a b where
  labelOptic = lens (\s -> (multipoles :: Atom -> Multipoles) s) $ \s b -> (s {multipoles = b} :: Atom)

----------------------------------------------------------------------------------------------------

-- |
-- A molecule, which might be the whole system, an ONIOM layer or a fragment of the system, each
-- containing possibly even higher layers for ONIOM or fragments. Stores all associated informations of
-- a layer.
--
-- For reference layers will be labeled \([0, \dots, n]\) by their recursion depth, with \(0\) being
-- the "real" system in ONIOM style calculations. Multiple fragments would be on the same recursion
-- depth but a different index in the '_molecule_SubMol' 'SparseArray'. Therefore, if \(0\) would be
-- the whole/real system, its fragments would be at recursion depth \(1\) and fragment \(f\) at
-- recursion depth \(1\) would be labeled \(1.f\).
--
-- Starting from a top level molecule, all atoms and bonds of the system are expected to be in the in
-- this top layer (except linkatoms of deeper layers). Therefore if atoms are in a deeper layers of
-- the recursion, their information is not used to describe a higher lying layer (which is lower with
-- respect to computational cost). Instead, all atoms of deeper layers (except linkatoms) must be
-- also replicated in a higher layer. While the atoms of deeper layers keep the same indices as in the
-- higher layers, it is acceptable, that an index, that was used for an atom in layer \(n\) is used for
-- a link atom in layer \(n + m, m > 0\).
--
-- Link atoms are specific to layers and may be passed down the recursion to deeper layers but not
-- passed up to higher layers. Therefore, while the counting of non-link-atoms must be consistent
-- through all layers, the link atoms must be only consitent the recursion downwards but not
-- necessarily between fragments of the same layer. If a layer \(n\) contains a link atom with index
-- \(p\), for example, and layer \(n + 1\) still contains this link atom, it must still be given index
-- \(p\). If layer \(n + 1\) on the other hand side would be stripped of this link atom, the index
-- \(p\) could be used for another link atom.
--
-- The data structure makes it necessary to construct the 'Molecule' top down, not bottom up.
data Molecule = Molecule
  { -- | Comment or description of a
    --   molecule. This can be empty and is
    --   not to be confused with the unique
    --   identifier used in the 'Map Text'
    --   structures.
    comment :: !Text,
    -- | An 'IntMap' of 'Atom's, 'Atom's
    --   identified by their 'Int' index.
    atoms :: !(IntMap Atom),
    -- | An sparse representation of the
    --   bond matrix. Is expected to be
    --   defined bidirectorial. Only the
    --   existing bonds would need to be
    --   defined as 'True'. The rest will
    --   be assumed to be 'False'.
    bonds :: !BondMatrix,
    -- | A Molecule might contain deeper
    --   ONIOM layers.
    subMol :: !(IntMap Molecule),
    -- | Fragments definition. They are
    --   meant to be either empty, or
    --   contain the whole system, usually
    --   without bond cuts. The keys of
    --   the 'IntMap' assign numbers to
    --   the fragments, while the 'IntSet'
    --   contains selections of atom
    --   numbers (and indices of the bond
    --   matrix, which belong to a
    --   fragment). Fragments should not
    --   contain deeper layers.
    fragment :: !(IntMap Fragment),
    -- | The potential energy and its
    --   derivatives.
    energyDerivatives :: !EnergyDerivatives,
    -- | Calculations to perform on
    --    __this__ layer of the molecule.
    --   The 'Text' values of the 'Map'
    --   serve as unique identifiers for a
    --   calculation on this molecule.
    calcContext :: !(Map CalcK CalcContext),
    -- | The Jacobian matrix for energy
    --   derivative transformation from
    --   this system to its parent system.
    jacobian :: !(Maybe (MatrixS Double))
  }
  deriving (Show, Eq, Generic)

instance ToJSON Molecule where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Molecule

-- Lenses
instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "comment" k Molecule Molecule a b where
  labelOptic = lens (\s -> comment s) $ \s b -> s {comment = b}

instance (k ~ A_Lens, a ~ IntMap Atom, b ~ a) => LabelOptic "atoms" k Molecule Molecule a b where
  labelOptic = lens (\s -> (atoms :: Molecule -> IntMap Atom) s) $ \s b -> (s {atoms = b} :: Molecule)

instance (k ~ A_Lens, a ~ BondMatrix, b ~ a) => LabelOptic "bonds" k Molecule Molecule a b where
  labelOptic = lens (\s -> bonds s) $ \s b -> s {bonds = b}

instance (k ~ A_Lens, a ~ IntMap Molecule, b ~ a) => LabelOptic "subMol" k Molecule Molecule a b where
  labelOptic = lens (\s -> subMol s) $ \s b -> s {subMol = b}

instance (k ~ A_Lens, a ~ IntMap Fragment, b ~ a) => LabelOptic "fragment" k Molecule Molecule a b where
  labelOptic = lens (\s -> fragment s) $ \s b -> s {fragment = b}

instance (k ~ A_Lens, a ~ EnergyDerivatives, b ~ a) => LabelOptic "energyDerivatives" k Molecule Molecule a b where
  labelOptic = lens (\s -> (energyDerivatives :: Molecule -> EnergyDerivatives) s) $ \s b -> (s {energyDerivatives = b} :: Molecule)

instance (k ~ A_Lens, a ~ Map CalcK CalcContext, b ~ a) => LabelOptic "calcContext" k Molecule Molecule a b where
  labelOptic = lens (\s -> calcContext s) $ \s b -> s {calcContext = b}

instance (k ~ A_Lens, a ~ Maybe (MatrixS Double), b ~ a) => LabelOptic "jacobian" k Molecule Molecule a b where
  labelOptic = lens (\s -> jacobian s) $ \s b -> s {jacobian = b}

-- Reader Class
class HasMolecule env where
  moleculeL :: Lens' env Molecule

instance HasMolecule Molecule where
  moleculeL = castOptic simple

----------------------------------------------------------------------------------------------------

-- |
-- Flag if an atom is a link atom. If an atom is a link atom (set 2 atom), then it will contain
-- information about the linking. See "[A new ONIOM implementation in Gaussian98. Part I. The
-- calculation of energies, gradients, vibrational frequencies and electric field
-- derivatives](https://doi.org/10.1016/S0166-1280(98)00475-8)"
data LinkInfo
  = -- | The atom is not a link atom.
    NotLink
  | -- | The atom is a link atom.
    IsLink
      { -- | The key of the atom in the model system (set 1) to which this
        --   atom binds.
        linkModelPartner :: Int,
        -- | The key of the atom in the real system (set 3), which this
        --   atom replaces for the model system.
        linkRealPartner :: Int,
        -- | The scaling factor \(g\).
        linkGFactor :: Double
      }
  deriving (Show, Eq, Generic)

instance ToJSON LinkInfo where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON LinkInfo

-- Prisms
_NotLink :: Prism' LinkInfo ()
_NotLink = prism' (const NotLink) $ \s -> case s of
  NotLink -> Just ()
  _ -> Nothing

_IsLink :: Prism' LinkInfo (Int, Int, Double)
_IsLink = prism' (\(a, b, c) -> IsLink a b c) $ \s -> case s of
  IsLink mP rP g -> Just (mP, rP, g)
  _ -> Nothing

-- Lenses
instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "linkModelPartner" k LinkInfo LinkInfo a b where
  labelOptic = lens (\s -> linkModelPartner s) $ \s b -> s {linkModelPartner = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "linkRealPartner" k LinkInfo LinkInfo a b where
  labelOptic = lens (\s -> linkRealPartner s) $ \s b -> s {linkRealPartner = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "linkGFactor" k LinkInfo LinkInfo a b where
  labelOptic = lens (\s -> linkGFactor s) $ \s b -> s {linkGFactor = b}

----------------------------------------------------------------------------------------------------

-- |
-- These are labels for molecular mechanics software. The strings are basically arbitrary and depending
-- on the MM software used.
data FFType
  = FFMol2 !Text
  | FFTXYZ !Int
  | FFPDB !Text
  | FFXYZ
  | FFBq
  deriving (Show, Generic)

instance Eq FFType where
  FFMol2 _ == FFMol2 _ = True
  FFMol2 _ == _ = False
  FFTXYZ _ == FFTXYZ _ = True
  FFTXYZ _ == _ = False
  FFPDB _ == FFPDB _ = True
  FFPDB _ == _ = False
  FFXYZ == FFXYZ = True
  FFXYZ == _ = False
  FFBq == FFBq = True
  FFBq == _ = False

instance ToJSON FFType where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON FFType

-- Prisms
_FFMol2 :: Prism' FFType Text
_FFMol2 = prism' (\b -> FFMol2 b) $ \s -> case s of
  FFMol2 b -> Just b
  _ -> Nothing

_FFTXYZ :: Prism' FFType Int
_FFTXYZ = prism' (\b -> FFTXYZ b) $ \s -> case s of
  FFTXYZ b -> Just b
  _ -> Nothing

_FFPDB :: Prism' FFType Text
_FFPDB = prism' (\b -> FFPDB b) $ \s -> case s of
  FFPDB b -> Just b
  _ -> Nothing

_FFXYZ :: Prism' FFType ()
_FFXYZ = prism' (const FFXYZ) $ \s -> case s of
  FFXYZ -> Just ()
  _ -> Nothing

_FFBq :: Prism' FFType ()
_FFBq = prism' (const FFBq) $ \s -> case s of
  FFBq -> Just ()
  _ -> Nothing

----------------------------------------------------------------------------------------------------

-- |
-- Definition of a fragment. The fragments are strictly a subset of a given layer.
data Fragment = Fragment
  { -- | The name of a fragment. Doesn't need to be unique.
    label :: !Text,
    -- | Meant for PDB and similiar molecules, where protein chains
    --   need to be distinguished.
    chain :: !(Maybe Char),
    -- | The atoms and bonds of the fragment. Relative to the
    --   molecule layer which contains the fragment.
    atoms :: !IntSet
  }
  deriving (Show, Eq, Generic)

instance ToJSON Fragment where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Fragment

-- Lenses
instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "label" k Fragment Fragment a b where
  labelOptic = lens (\s -> (label :: Fragment -> Text) s) $ \s b -> (s {label = b} :: Fragment)

instance (k ~ A_Lens, a ~ Maybe Char, b ~ a) => LabelOptic "chain" k Fragment Fragment a b where
  labelOptic = lens (\s -> chain s) $ \s b -> s {chain = b}

instance (k ~ A_Lens, a ~ IntSet, b ~ a) => LabelOptic "atoms" k Fragment Fragment a b where
  labelOptic = lens (\s -> (atoms :: Fragment -> IntSet) s) $ \s b -> (s {atoms = b} :: Fragment)

----------------------------------------------------------------------------------------------------

-- |
-- Trajectories are simply Sequences of 'Molecule's.
type Trajectory = Seq Molecule

----------------------------------------------------------------------------------------------------

-- |
-- A data structure to get a layer in the 'Molecule' contructor. Enables to find a specific layer.
-- It works by giving number of lookups stepping down the recursion of the 'Molecule' structure along
-- the 'subMol' structure. An empty sequency of indices is the top layer. Then each element
-- will be used as a key for the 'IntMap' structure in 'subMol' structure, to step down to
-- the next layer.
type MolID = Seq Int

{-
====================================================================================================
-}

-- $calculations
-- These types define the necessary context for calculations on a current layer of the molecule. They
-- are meant to be updated frequently by the driver routines, which implement the multilayer methods.
--
-- They are defined in the context of a wrapper call, which means that they expose a lot of computer
-- system specific information regarding execution of the program and not only molecular/hamiltonian
-- information.
--
-- Furthermore the definition of outputs obtainable from the wrappers are defined here.

-- |
-- Find a specific calculation for a specific layer of the molecule. The calculation is defined by the
-- 'MolID' and the 'Text' key of the '_molecule_CalcContext' 'Map'.
data CalcID = CalcID
  { -- | Molecule layer of interest.
    molID :: MolID,
    -- | The actual calculation.
    calcKey :: CalcK
  }
  deriving (Eq, Show)

-- Lenses
instance (k ~ A_Lens, a ~ MolID, b ~ a) => LabelOptic "molID" k CalcID CalcID a b where
  labelOptic = lens (\s -> molID s) $ \s b -> s {molID = b}

instance (k ~ A_Lens, a ~ CalcK, b ~ a) => LabelOptic "calcKey" k CalcID CalcID a b where
  labelOptic = lens (\s -> calcKey s) $ \s b -> s {calcKey = b}

----------------------------------------------------------------------------------------------------

-- |
-- Identifier for a calculation context supposed to be used as key in the '_molecule_CalcContext'
-- field.
data CalcK
  = -- | An ONIOM calculation. The 'ONIOMHierarchy' defines if this
    --   calculation has been inherited or not.
    ONIOMKey ONIOMHierarchy
  deriving (Eq, Show, Ord, Generic, FromJSONKey, ToJSONKey)

instance ToJSON CalcK where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON CalcK

-- Prisms
_ONIOMKey :: Prism' CalcK ONIOMHierarchy
_ONIOMKey = prism' (\b -> ONIOMKey b) $ \s -> case s of
  ONIOMKey b -> Just b

----------------------------------------------------------------------------------------------------

-- |
-- For all except the real system layer in an ONIOM calculation, two calculations on the molecular
-- structure can be defined. The actual one of interest for this system and the one inherited from the
-- layer above, which is required for subtraction.
data ONIOMHierarchy
  = -- | The calculation of actual interest.
    Original
  | -- | The calculation required to subtract this part from the layer above.
    Inherited
  deriving (Eq, Show, Ord, Generic, FromJSONKey, ToJSONKey)

instance ToJSON ONIOMHierarchy where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON ONIOMHierarchy

{-
====================================================================================================
-}

-- |
-- Representation of multipoles for expansion of the electrostatic potential.
data Multipoles = Multipoles
  { monopole :: !(Maybe Monopole),
    dipole :: !(Maybe Dipole),
    quadrupole :: !(Maybe Quadrupole),
    octopole :: !(Maybe Octopole),
    hexadecapole :: !(Maybe Hexadecapole)
  }
  deriving (Eq, Show, Generic)

instance ToJSON Multipoles where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Multipoles

instance Default Multipoles where
  def =
    Multipoles
      { monopole = Nothing,
        dipole = Nothing,
        quadrupole = Nothing,
        octopole = Nothing,
        hexadecapole = Nothing
      }

instance PrettyPrint Multipoles where
  prettyP = ppMultipoles

-- Lenses
instance (k ~ A_Lens, a ~ Maybe Monopole, b ~ a) => LabelOptic "monopole" k Multipoles Multipoles a b where
  labelOptic = lens (\s -> monopole s) $ \s b -> s {monopole = b}

instance (k ~ A_Lens, a ~ Maybe Dipole, b ~ a) => LabelOptic "dipole" k Multipoles Multipoles a b where
  labelOptic = lens (\s -> dipole s) $ \s b -> s {dipole = b}

instance (k ~ A_Lens, a ~ Maybe Quadrupole, b ~ a) => LabelOptic "quadrupole" k Multipoles Multipoles a b where
  labelOptic = lens (\s -> quadrupole s) $ \s b -> s {quadrupole = b}

instance (k ~ A_Lens, a ~ Maybe Octopole, b ~ a) => LabelOptic "octopole" k Multipoles Multipoles a b where
  labelOptic = lens (\s -> octopole s) $ \s b -> s {octopole = b}

instance (k ~ A_Lens, a ~ Maybe Hexadecapole, b ~ a) => LabelOptic "hexadecapole" k Multipoles Multipoles a b where
  labelOptic = lens (\s -> hexadecapole s) $ \s b -> s {hexadecapole = b}

----------------------------------------------------------------------------------------------------

-- |
-- A monopole moment.
data Monopole = Monopole
  { q00 :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Monopole where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Monopole

instance PrettyPrint Monopole where
  prettyP = ppMonopole

type MultipoleR0 = Monopole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q00" k Monopole Monopole a b where
  labelOptic = lens (\s -> q00 s) $ \s b -> s {q00 = b}

----------------------------------------------------------------------------------------------------

-- |
-- A spherical dipole moment.
data Dipole = Dipole
  { q10 :: Double,
    q11c :: Double,
    q11s :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Dipole where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Dipole

instance PrettyPrint Dipole where
  prettyP = ppDipole

type MultipoleR1 = Dipole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q10" k Dipole Dipole a b where
  labelOptic = lens (\s -> q11c s) $ \s b -> s {q10 = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q11c" k Dipole Dipole a b where
  labelOptic = lens (\s -> q11c s) $ \s b -> s {q11c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q11s" k Dipole Dipole a b where
  labelOptic = lens (\s -> q11s s) $ \s b -> s {q11s = b}

----------------------------------------------------------------------------------------------------

-- |
-- A spherical quadrupole moment.
data Quadrupole = Quadrupole
  { q20 :: Double,
    q21c :: Double,
    q21s :: Double,
    q22c :: Double,
    q22s :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Quadrupole where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Quadrupole

instance PrettyPrint Quadrupole where
  prettyP = ppQuadrupole

type MultipoleR2 = Quadrupole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q20" k Quadrupole Quadrupole a b where
  labelOptic = lens (\s -> q20 s) $ \s b -> s {q20 = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q21c" k Quadrupole Quadrupole a b where
  labelOptic = lens (\s -> q21c s) $ \s b -> s {q21c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q21s" k Quadrupole Quadrupole a b where
  labelOptic = lens (\s -> q21s s) $ \s b -> s {q21s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q22c" k Quadrupole Quadrupole a b where
  labelOptic = lens (\s -> q22c s) $ \s b -> s {q22c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q22s" k Quadrupole Quadrupole a b where
  labelOptic = lens (\s -> q22s s) $ \s b -> s {q22s = b}

----------------------------------------------------------------------------------------------------

-- | A spherical octopole moment.
data Octopole = Octopole
  { q30 :: Double,
    q31c :: Double,
    q31s :: Double,
    q32c :: Double,
    q32s :: Double,
    q33c :: Double,
    q33s :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Octopole where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Octopole

instance PrettyPrint Octopole where
  prettyP = ppOctopole

type MultipoleR3 = Octopole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q30" k Octopole Octopole a b where
  labelOptic = lens (\s -> q30 s) $ \s b -> s {q30 = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q31c" k Octopole Octopole a b where
  labelOptic = lens (\s -> q31c s) $ \s b -> s {q31c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q31s" k Octopole Octopole a b where
  labelOptic = lens (\s -> q31s s) $ \s b -> s {q31s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q32c" k Octopole Octopole a b where
  labelOptic = lens (\s -> q32c s) $ \s b -> s {q32c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q32s" k Octopole Octopole a b where
  labelOptic = lens (\s -> q32s s) $ \s b -> s {q32s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q33c" k Octopole Octopole a b where
  labelOptic = lens (\s -> q33c s) $ \s b -> s {q33c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q33s" k Octopole Octopole a b where
  labelOptic = lens (\s -> q33s s) $ \s b -> s {q33s = b}

----------------------------------------------------------------------------------------------------

-- |
-- A spherical octopole moment.
data Hexadecapole = Hexadecapole
  { q40 :: Double,
    q41c :: Double,
    q41s :: Double,
    q42c :: Double,
    q42s :: Double,
    q43c :: Double,
    q43s :: Double,
    q44c :: Double,
    q44s :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Hexadecapole where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Hexadecapole

instance PrettyPrint Hexadecapole where
  prettyP = ppHexadecapole

type MultipoleR4 = Hexadecapole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q40" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q40 s) $ \s b -> s {q40 = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q41c" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q41c s) $ \s b -> s {q41c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q41s" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q41s s) $ \s b -> s {q41s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q42c" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q42c s) $ \s b -> s {q42c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q42s" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q42s s) $ \s b -> s {q42s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q43c" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q43c s) $ \s b -> s {q43c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q43s" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q43s s) $ \s b -> s {q43s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q44c" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q44c s) $ \s b -> s {q44c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q44s" k Hexadecapole Hexadecapole a b where
  labelOptic = lens (\s -> q44s s) $ \s b -> s {q44s = b}

{-
====================================================================================================
-}

-- |
-- Available embedding methods for layer interaction in ONIOM.
data Embedding
  = -- | Mechanical embedding.
    Mechanical
  | -- | Electrostatic embedding. Scaling factors of charges in a
    --   given distance to a capped atom can be given.
    Electronic (Maybe (Seq Double))
  deriving (Eq, Show, Generic)

instance ToJSON Embedding where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Embedding

-- Prisms
_Mechanical :: Prism' Embedding ()
_Mechanical = prism' (const Mechanical) $ \s -> case s of
  Mechanical -> Just ()
  _ -> Nothing

_Electronic :: Prism' Embedding (Maybe (Seq Double))
_Electronic = prism' (\b -> Electronic b) $ \s -> case s of
  Electronic b -> Just b
  _ -> Nothing

{-
====================================================================================================
-}

-- | Derivatives of the potential energy, that can be calculated for a 'Molecule' or molecular layer.
data EnergyDerivatives = EnergyDerivatives
  { -- | The potential energy.
    energy :: !(Maybe Double),
    -- | The nuclear gradient.
    gradient :: !(Maybe (VectorS Double)),
    -- | The nuclear hessian.
    hessian :: !(Maybe (MatrixS Double))
  }
  deriving (Eq, Show, Generic)

instance ToJSON EnergyDerivatives where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON EnergyDerivatives

instance Default EnergyDerivatives where
  def =
    EnergyDerivatives
      { energy = Nothing,
        gradient = Nothing,
        hessian = Nothing
      }

-- Lenses
instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "energy" k EnergyDerivatives EnergyDerivatives a b where
  labelOptic = lens (\s -> energy s) $ \s b -> s {energy = b}

instance (k ~ A_Lens, a ~ Maybe (VectorS Double), b ~ a) => LabelOptic "gradient" k EnergyDerivatives EnergyDerivatives a b where
  labelOptic = lens (\s -> gradient s) $ \s b -> s {gradient = b}

instance (k ~ A_Lens, a ~ Maybe (MatrixS Double), b ~ a) => LabelOptic "hessian" k EnergyDerivatives EnergyDerivatives a b where
  labelOptic = lens (\s -> hessian s) $ \s b -> s {hessian = b}

{-
====================================================================================================
-}

-- |
-- How efficient a task can be performed. Used for gradient calculations mainly.
data NumericalEfficiency
  = -- | Analytical derivatives are available and will be used.
    Analytical
  | -- | Finite displacements will be used for derivatives.
    Numerical
  deriving (Eq, Show, Generic)

instance ToJSON NumericalEfficiency where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON NumericalEfficiency

----------------------------------------------------------------------------------------------------

-- |
-- A task the wrapper needs to perform.
data WrapperTask
  = -- | Single point energy calculation.
    WTEnergy
  | -- | Gradient calculation.
    WTGradient
  | -- | Hessian calculation.
    WTHessian
  deriving (Eq, Show, Generic)

instance ToJSON WrapperTask where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON WrapperTask

----------------------------------------------------------------------------------------------------

-- |
-- Context specific to a QM calculation.
data QMContext = QMContext
  { -- | Charge of the (sub)system as transparent to the QM software.
    charge :: !Int,
    -- | Multiplicity of the (sub)system as transparent to the QM software.
    mult :: !Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON QMContext where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON QMContext

-- Lenses
instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "charge" k QMContext QMContext a b where
  labelOptic = lens (\s -> charge s) $ \s b -> s {charge = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "mult" k QMContext QMContext a b where
  labelOptic = lens (\s -> mult s) $ \s b -> s {mult = b}

----------------------------------------------------------------------------------------------------

-- |
-- Context specific to a MM calculation.
data MMContext = MMContext
  deriving (Show, Eq, Generic)

instance ToJSON MMContext where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON MMContext

----------------------------------------------------------------------------------------------------

-- |
-- Information needed either by a QM or MM calculation.
data QMMMSpec
  = -- | Information only needed for QM calculations.
    QM !QMContext
  | -- | Information only needed for MM calculations.
    MM !MMContext
  deriving (Show, Eq, Generic)

instance ToJSON QMMMSpec where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON QMMMSpec

-- Prisms
_QM :: Prism' QMMMSpec QMContext
_QM = prism' (\b -> QM b) $ \s -> case s of
  QM b -> Just b
  _ -> Nothing

_MM :: Prism' QMMMSpec MMContext
_MM = prism' (\b -> MM b) $ \s -> case s of
  MM b -> Just b
  _ -> Nothing

----------------------------------------------------------------------------------------------------

-- |
-- The context of a calculation is the combination of its input and output values.
data CalcContext = CalcContext
  { -- | Necessary information to define a calculation.
    input :: !CalcInput,
    -- | Information produced by the calculation as
    --   defined by the 'CalcInput'.
    output :: !(Maybe CalcOutput)
  }
  deriving (Eq, Show, Generic)

instance ToJSON CalcContext where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON CalcContext

instance (k ~ A_Lens, a ~ CalcInput, b ~ a) => LabelOptic "input" k CalcContext CalcContext a b where
  labelOptic = lens (\s -> input s) $ \s b -> s {input = b}

instance (k ~ A_Lens, a ~ Maybe CalcOutput, b ~ a) => LabelOptic "output" k CalcContext CalcContext a b where
  labelOptic = lens (\s -> output s) $ \s b -> s {output = b}

----------------------------------------------------------------------------------------------------

-- |
-- The context for a wrapper calculation. These are information used by the system call to the wrapper
-- and the Mustache template engine for the wrapper input file as well as some Spicy internals on
data CalcInput = CalcInput
  { -- | A 'Task' the wrapper needs to perform.
    task :: !WrapperTask,
    -- | Potentially a restart file (wavefunction),
    --   which might me present.
    restartFile :: !(Maybe JFilePathAbs),
    -- | The 'Program', which is being used as a
    --   wrapped calculator.
    software :: !Program,
    -- | Some prefix, which can be used to name
    --   output files. Consider this a project name.
    prefixName :: !String,
    -- | Directory, where permanent output of the
    --   calculation shall be stored. This is the
    --   final path for a calculation and needs to
    --   be updated between iterations.
    permaDir :: !JDirPathAbs,
    -- | Directory, with fast access for scratch
    --   files of the software.
    scratchDir :: !JDirPathAbs,
    -- | Number of (MPI) processes to run in
    --   parallel for a software.
    nProcs :: !Int,
    -- | Number of threads per MPI process or OpenMP
    --   threads.
    nThreads :: !Int,
    -- | Memory per process in MiB for the program.
    memory :: !Int,
    -- | Information specific to either a QM or MM
    --   calculation.
    qMMMSpec :: !QMMMSpec,
    -- | A Mustache template for the program.
    template :: !Text,
    -- | The embedding type for this calculation
    --   part. Might be ignored for Inherited
    --   calculations in ONIOM (low calculation
    --   level on model system).
    embedding :: !Embedding
  }
  deriving (Eq, Show, Generic)

instance ToJSON CalcInput where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON CalcInput

-- Lenses
instance (k ~ A_Lens, a ~ WrapperTask, b ~ a) => LabelOptic "task" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> task s) $ \s b -> s {task = b}

instance (k ~ A_Lens, a ~ (Maybe JFilePathAbs), b ~ a) => LabelOptic "restartFile" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> restartFile s) $ \s b -> s {restartFile = b}

instance (k ~ A_Lens, a ~ Program, b ~ a) => LabelOptic "software" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> software s) $ \s b -> s {software = b}

instance (k ~ A_Lens, a ~ String, b ~ a) => LabelOptic "prefixName" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> prefixName s) $ \s b -> s {prefixName = b}

instance (k ~ A_Lens, a ~ JDirPathAbs, b ~ a) => LabelOptic "permaDir" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> permaDir s) $ \s b -> s {permaDir = b}

instance (k ~ A_Lens, a ~ JDirPathAbs, b ~ a) => LabelOptic "scratchDir" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> scratchDir s) $ \s b -> s {scratchDir = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "nProcs" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> nProcs s) $ \s b -> s {nProcs = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "nThreads" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> nThreads s) $ \s b -> s {nThreads = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "memory" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> memory s) $ \s b -> s {memory = b}

instance (k ~ A_Lens, a ~ QMMMSpec, b ~ a) => LabelOptic "qMMMSpec" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> qMMMSpec s) $ \s b -> s {qMMMSpec = b}

instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "template" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> template s) $ \s b -> s {template = b}

instance (k ~ A_Lens, a ~ Embedding, b ~ a) => LabelOptic "embedding" k CalcInput CalcInput a b where
  labelOptic = lens (\s -> embedding s) $ \s b -> s {embedding = b}

----------------------------------------------------------------------------------------------------

-- |
-- Results from a wrapper calculation on the given niveau. If for example multiple computations are
-- performed on the same 'Molecule' layer as in ONIOM (high and low level calculation on the model
-- system), this type allows to store the intermediate information before combining them and putting
-- them back to the '_molecule_EnergyDerivatives' field.
data CalcOutput = CalcOutput
  { -- | All information from the calculation,
    --   that will be stored
    energyDerivatives :: !EnergyDerivatives,
    -- | Mapping 'Atom' indices to the
    --   'Multipoles', that were calculated
    --   by a wrapper calculation.
    multipoles :: !(IntMap Multipoles)
  }
  deriving (Eq, Show, Generic)

instance ToJSON CalcOutput where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON CalcOutput

instance (k ~ A_Lens, a ~ EnergyDerivatives, b ~ a) => LabelOptic "energyDerivatives" k CalcOutput CalcOutput a b where
  labelOptic = lens (\s -> (energyDerivatives :: CalcOutput -> EnergyDerivatives) s) $
    \s b -> (s {energyDerivatives = b} :: CalcOutput)

instance (k ~ A_Lens, a ~ IntMap Multipoles, b ~ a) => LabelOptic "multipoles" k CalcOutput CalcOutput a b where
  labelOptic = lens (\s -> (multipoles :: CalcOutput -> IntMap Multipoles) s) $
    \s b -> (s {multipoles = b} :: CalcOutput)

{-
====================================================================================================
-}

-- |
-- A known computational chemistry program to use.
data Program
  = Psi4
  | Nwchem
  deriving (Eq, Show, Generic)

instance ToJSON Program where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Program

{-
####################################################################################################
-}

-- $prettyPrinters
-- Helper functions to implement the 'PrettyPrint' class for various elements.

-- | Formatter for short doubles.
fShortDouble :: Double -> Text
fShortDouble = sformat (left 8 ' ' %. fixed 6)

-- | Formatter for long doubles.
fLongDouble :: Double -> Text
fLongDouble = sformat (left 8 ' ' %. fixed 6)

----------------------------------------------------------------------------------------------------

-- | Formatter for the multipole labels.
fMPLabel :: Text -> Text
fMPLabel = sformat ((fitRight 4 %. right 4 ' ' %. stext) Formatting.% " = ")

-- | Formatter for a labeled Multipole component.
fMPComp :: (Text, Double) -> Text
fMPComp (mLabel, value) = (fMPLabel mLabel) <> (fShortDouble value)

-- | Formatter for components of the multipole.
fMP :: [(Text, Double)] -> Utf8Builder
fMP components =
  display
    . Text.unlines
    . fmap (foldl' (<>) mempty)
    . chunksOf 5
    . fmap ((<> "  ") . fMPComp)
    $ components

-- | PrettyPrinter for Monopoles.
ppMonopole :: Monopole -> Utf8Builder
ppMonopole pole = fMP [("Q00", pole ^. #q00)]

-- | PrettyPrinter for Dipoles.
ppDipole :: Dipole -> Utf8Builder
ppDipole pole =
  let magnitude = sqrt $ (pole ^. #q11c) ** 2 + (pole ^. #q11s) ** 2
   in fMP
        [ ("|Q1|", magnitude),
          ("Q11c", pole ^. #q11c),
          ("Q11s", pole ^. #q11s)
        ]

-- | PrettyPrinter for Quadrupoles.
ppQuadrupole :: Quadrupole -> Utf8Builder
ppQuadrupole pole =
  let magnitude =
        sqrt $
          (pole ^. #q20) ** 2
            + (pole ^. #q22c) ** 2
            + (pole ^. #q22s) ** 2
   in fMP
        [ ("|Q2|", magnitude),
          ("Q20", pole ^. #q20),
          ("Q22c", pole ^. #q22c),
          ("Q22s", pole ^. #q22s)
        ]

-- | PrettyPrinter for Quadrupoles.
ppOctopole :: Octopole -> Utf8Builder
ppOctopole pole =
  let magnitude =
        sqrt $
          (pole ^. #q31c) ** 2
            + (pole ^. #q31s) ** 2
            + (pole ^. #q33c) ** 2
            + (pole ^. #q33s) ** 2
   in fMP
        [ ("|Q3|", magnitude),
          ("Q31c", pole ^. #q31c),
          ("Q31s", pole ^. #q31s),
          ("Q33c", pole ^. #q33c),
          ("Q33s", pole ^. #q33s)
        ]

-- | PrettyPrinter for Quadrupoles.
ppHexadecapole :: Hexadecapole -> Utf8Builder
ppHexadecapole pole =
  let magnitude =
        sqrt $
          (pole ^. #q40) ** 2
            + (pole ^. #q42c) ** 2
            + (pole ^. #q42s) ** 2
            + (pole ^. #q44c) ** 2
            + (pole ^. #q44s) ** 2
   in fMP
        [ ("|Q4|", magnitude),
          ("Q40", pole ^. #q40),
          ("Q42c", pole ^. #q42c),
          ("Q42s", pole ^. #q42s),
          ("Q44c", pole ^. #q44c),
          ("Q44s", pole ^. #q44s)
        ]

-- | PrettyPrinter for the Multipoles.
ppMultipoles :: Multipoles -> Utf8Builder
ppMultipoles mp =
  let mono = prettyP <$> mp ^. #monopole
      di = prettyP <$> mp ^. #dipole
      quadru = prettyP <$> mp ^. #quadrupole
      octo = prettyP <$> mp ^. #octopole
      hexadeca = prettyP <$> mp ^. #hexadecapole
   in "Multipole moments / atomic units (ea_0^k for rank k):\n"
        <> (if mp == def then "x" else mempty)
        <> (fromMaybe mempty mono <> "\n")
        <> (fromMaybe mempty di <> "\n")
        <> (fromMaybe mempty quadru <> "\n")
        <> (fromMaybe mempty octo <> "\n")
        <> (fromMaybe mempty hexadeca <> "\n")

{-
####################################################################################################
-}

-- $localHelperTypes
-- These types are not meant to be returned or consumed by any exported function. Instead they are used
-- only locally to pass information around, which would otherwise need to be stored in unannotated
-- tuples or something.

-- |
-- Type to pass atomwise information from parsers with atom-fragment associations around.
data FragmentAtomInfo = FragmentAtomInfo
  { -- | The index of the atom in the whole structure.
    faiAtomIndex :: Int,
    -- | The index of the fragment in the whole structure.
    faiFragmentIndex :: Int,
    -- | The atom of this line, associated to an fragment.
    faiAtom :: Atom,
    -- | The fragment associated to this atom. Possibly the selection
    --   of atom indices in the fragment contains only one atom and
    --   matching fragments must then be joined.
    faiFragment :: Fragment
  }

----------------------------------------------------------------------------------------------------

-- |
-- Type just to handle the information obtained from parsing a single atom line of a TXYZ file.
data TXYZAtomInfo = TXYZAtomInfo
  { -- | Index of this atom in the TXYZ file. First number in a row.
    txyzIndex :: Int,
    -- | The atom of this line.
    txyzAtom :: Atom,
    -- | The indices of the atoms, to which this atom has a bond.
    txyzBondTargets :: [Int]
  }
