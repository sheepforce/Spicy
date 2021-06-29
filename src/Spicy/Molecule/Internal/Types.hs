{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- |
-- Module      : Spicy.Molecule.Internal.Types
-- Description : Definitions of a molecule and its context
-- Copyright   : Phillip Seeber, 2021
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
    HasDirectMolecule (..),
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

    -- ** Optimiser Settings
    Optimisation (..),
    CoordType (..),
    HessianUpdate (..),
    MicroStep (..),
    OptType (..),
    _Minimum,
    _SaddlePoint,
    TSOptAlg (..),
    MinOptAlg (..),
    GeomConv (..),

    -- ** Energy Derivatives
    EnergyDerivatives (..),

    -- ** Wrapper Context
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
    _XTB,
    _Psi4,
    _Turbomole,
    _ProgramHamiltonian,
    isPsi4,
    isXTB,
    isTurbomole,
    GFN (..),
    renderGFN,

    -- *** Generic information for all QC programs
    QCHamiltonian (..),
    BasisSet (..),
    SCF (..),
    Damp (..),
    RI (..),
    _OtherRI,
    RefWfn (..),
    DFT (..),
    _RKS,
    _UKS,
    Correlation (..),
    Excitations (..),

    -- * Local Helper Types
    FragmentAtomInfo (..),
    TXYZAtomInfo (..),
  )
where

import Data.Aeson
import Data.Default
import Optics hiding (element)
import RIO hiding (Lens', lens, view, (^.))
import Spicy.Aeson
import Spicy.Common
import Spicy.Wrapper.IPI.Types hiding (hessian, input, output)

{-
####################################################################################################
-}

-- $moleculeStructure
-- These types define the molecule and its topology. This includes atom positions, bonds, atom types
-- and so on.

-- | All chemical elements. Have them very clear because force fields and pdb names may interfer and
-- are just arbitrary strings.
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

instance FromJSON Element where
  parseJSON = genericParseJSON spicyJOption

----------------------------------------------------------------------------------------------------

-- | An Atom in a 'Molecule'. Atoms are compared by their indices only and they must therefore be
-- unique. The coordinates of the 'Atom' are defined as 'Seq', as this is extremely easy to
-- concatenate when building a coordinate vector.
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

instance FromJSON Atom where
  parseJSON = genericParseJSON spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ Element, b ~ a) => LabelOptic "element" k Atom Atom a b where
  labelOptic = lens element $ \s b -> s {element = b}

instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "label" k Atom Atom a b where
  labelOptic = lens (label :: Atom -> Text) $ \s b -> (s {label = b} :: Atom)

instance (k ~ A_Lens, a ~ LinkInfo, b ~ a) => LabelOptic "isLink" k Atom Atom a b where
  labelOptic = lens isLink $ \s b -> s {isLink = b}

instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "isDummy" k Atom Atom a b where
  labelOptic = lens isDummy $ \s b -> s {isDummy = b}

instance (k ~ A_Lens, a ~ FFType, b ~ a) => LabelOptic "ffType" k Atom Atom a b where
  labelOptic = lens ffType $ \s b -> s {ffType = b}

instance (k ~ A_Lens, a ~ VectorS Double, b ~ a) => LabelOptic "coordinates" k Atom Atom a b where
  labelOptic = lens coordinates $ \s b -> s {coordinates = b}

instance (k ~ A_Lens, a ~ Multipoles, b ~ a) => LabelOptic "multipoles" k Atom Atom a b where
  labelOptic = lens (multipoles :: Atom -> Multipoles) $ \s b -> (s {multipoles = b} :: Atom)

----------------------------------------------------------------------------------------------------

-- | A molecule, which might be the whole system, an ONIOM layer or a fragment of the system, each
-- containing possibly even higher layers for ONIOM or fragments. Stores all associated informations
-- of a layer.
--
-- For reference layers will be labeled \([0, \dots, n]\) by their recursion depth, with \(0\) being
-- the "real" system in ONIOM style calculations. Multiple fragments would be on the same recursion
-- depth but a different index in the '_molecule_SubMol' 'SparseArray'. Therefore, if \(0\) would be
-- the whole/real system, its fragments would be at recursion depth \(1\) and fragment \(f\) at
-- recursion depth \(1\) would be labeled \(1.f\).
--
-- Starting from a top level molecule, all atoms and bonds of the system are expected to be in the
-- in this top layer (except linkatoms of deeper layers). Therefore if atoms are in a deeper layers
-- of the recursion, their information is not used to describe a higher lying layer (which is lower
-- with respect to computational cost). Instead, all atoms of deeper layers (except linkatoms) must
-- be also replicated in a higher layer. While the atoms of deeper layers keep the same indices as
-- in the higher layers, it is acceptable, that an index, that was used for an atom in layer \(n\)
-- is used for a link atom in layer \(n + m, m > 0\).
--
-- Link atoms are specific to layers and may be passed down the recursion to deeper layers but not
-- passed up to higher layers. Therefore, while the counting of non-link-atoms must be consistent
-- through all layers, the link atoms must be only consitent the recursion downwards but not
-- necessarily between fragments of the same layer. If a layer \(n\) contains a link atom with index
-- \(p\), for example, and layer \(n + 1\) still contains this link atom, it must still be given
-- index \(p\). If layer \(n + 1\) on the other hand side would be stripped of this link atom, the
-- index \(p\) could be used for another link atom.
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
    -- | Fragments definition. Every atom must
    -- be assigned to exactly one fragment. This
    -- fragment might be the whole system or a
    -- fragment in the sense of a PDB residue
    -- for example. Bond cuts through fragments
    -- are okay.
    fragment :: !(IntMap Fragment),
    -- | The potential energy and its
    --   derivatives.
    energyDerivatives :: !EnergyDerivatives,
    -- | Calculations to perform on
    -- __this__ layer of the molecule.
    -- The 'Text' values of the 'Map'
    -- serve as unique identifiers for a
    -- calculation on this molecule.
    calcContext :: !(Map CalcK CalcContext),
    -- | The Jacobian matrix for energy
    -- derivative transformation from
    -- this system to its parent system.
    jacobian :: !(Maybe (MatrixS Double)),
    -- | Neighbourlists that have possibly been
    -- calculated together with a given search distance.
    -- Maps from the search distance to a sparse neighbourlist
    -- representation. Also see 'Spicy.Molecule.Internal.Util.neighbourList'.
    neighbourlist :: !(Map Double NeighbourList)
  }
  deriving (Generic)

instance ToJSON Molecule where
  toEncoding = genericToEncoding spicyJOption

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

instance (k ~ A_Lens, a ~ Map Double NeighbourList, b ~ a) => LabelOptic "neighbourlist" k Molecule Molecule a b where
  labelOptic = lens (\s -> neighbourlist s) $ \s b -> s {neighbourlist = b}

-- Reader Class

-- | Access to the shared state of the 'Molecule' in a shared variable. This is a stateful
-- representation of the molecule.
class HasMolecule env where
  moleculeL :: Lens' env (TVar Molecule)

-- | RIO style reader constraint class, where there is direct (pure) access to the 'Molecule' in the
-- environemt. While 'HasMolecule' gives access to the current state of the 'Molecule' in the
-- runtime environemt and therefore requires 'IO', this is the direct access to an immutable
-- 'Molecule' in the environment. While the runtime environemt does not hold this type, it is a
-- useful accessor for pure 'MonadReader' stacks.
class HasDirectMolecule env where
  moleculeDirectL :: Lens' env Molecule

instance HasDirectMolecule Molecule where
  moleculeDirectL = castOptic simple

----------------------------------------------------------------------------------------------------

-- | Flag if an atom is a link atom. If an atom is a link atom (set 2 atom), then it will contain
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

instance FromJSON LinkInfo where
  parseJSON = genericParseJSON spicyJOption

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

-- | These are labels for molecular mechanics software. The strings are basically arbitrary and
-- depend on the MM software used.
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

instance FromJSON FFType where
  parseJSON = genericParseJSON spicyJOption

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

-- | Definition of a fragment. The fragments are strictly a subset of a given layer.
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

instance FromJSON Fragment where
  parseJSON = genericParseJSON spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "label" k Fragment Fragment a b where
  labelOptic = lens (label :: Fragment -> Text) $ \s b -> (s {label = b} :: Fragment)

instance (k ~ A_Lens, a ~ Maybe Char, b ~ a) => LabelOptic "chain" k Fragment Fragment a b where
  labelOptic = lens chain $ \s b -> s {chain = b}

instance (k ~ A_Lens, a ~ IntSet, b ~ a) => LabelOptic "atoms" k Fragment Fragment a b where
  labelOptic = lens (atoms :: Fragment -> IntSet) $ \s b -> (s {atoms = b} :: Fragment)

----------------------------------------------------------------------------------------------------

-- | Trajectories are simply Sequences of 'Molecule's.
type Trajectory = Seq Molecule

----------------------------------------------------------------------------------------------------

-- | A data structure to get a layer in the 'Molecule' contructor. Enables to find a specific layer.
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

-- | Find a specific calculation for a specific layer of the molecule. The calculation is defined by the
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
  labelOptic = lens molID $ \s b -> s {molID = b}

instance (k ~ A_Lens, a ~ CalcK, b ~ a) => LabelOptic "calcKey" k CalcID CalcID a b where
  labelOptic = lens calcKey $ \s b -> s {calcKey = b}

----------------------------------------------------------------------------------------------------

-- | Identifier for a calculation context supposed to be used as key in the '_molecule_CalcContext'
-- field.
data CalcK
  = -- | An ONIOM calculation. The 'ONIOMHierarchy' defines if this
    --   calculation has been inherited or not.
    ONIOMKey ONIOMHierarchy
  deriving (Eq, Show, Ord, Generic, FromJSONKey, ToJSONKey)

instance ToJSON CalcK where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON CalcK where
  parseJSON = genericParseJSON spicyJOption

-- Prisms
_ONIOMKey :: Prism' CalcK ONIOMHierarchy
_ONIOMKey = prism' ONIOMKey $ \s -> case s of
  ONIOMKey b -> Just b

----------------------------------------------------------------------------------------------------

-- | For all except the real system layer in an ONIOM calculation, two calculations on the molecular
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

instance FromJSON ONIOMHierarchy where
  parseJSON = genericParseJSON spicyJOption

{-
====================================================================================================
-}

-- | Representation of multipoles for expansion of the electrostatic potential.
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

instance FromJSON Multipoles where
  parseJSON = genericParseJSON spicyJOption

instance Default Multipoles where
  def =
    Multipoles
      { monopole = Nothing,
        dipole = Nothing,
        quadrupole = Nothing,
        octopole = Nothing,
        hexadecapole = Nothing
      }

-- Lenses
instance (k ~ A_Lens, a ~ Maybe Monopole, b ~ a) => LabelOptic "monopole" k Multipoles Multipoles a b where
  labelOptic = lens monopole $ \s b -> s {monopole = b}

instance (k ~ A_Lens, a ~ Maybe Dipole, b ~ a) => LabelOptic "dipole" k Multipoles Multipoles a b where
  labelOptic = lens dipole $ \s b -> s {dipole = b}

instance (k ~ A_Lens, a ~ Maybe Quadrupole, b ~ a) => LabelOptic "quadrupole" k Multipoles Multipoles a b where
  labelOptic = lens quadrupole $ \s b -> s {quadrupole = b}

instance (k ~ A_Lens, a ~ Maybe Octopole, b ~ a) => LabelOptic "octopole" k Multipoles Multipoles a b where
  labelOptic = lens octopole $ \s b -> s {octopole = b}

instance (k ~ A_Lens, a ~ Maybe Hexadecapole, b ~ a) => LabelOptic "hexadecapole" k Multipoles Multipoles a b where
  labelOptic = lens hexadecapole $ \s b -> s {hexadecapole = b}

----------------------------------------------------------------------------------------------------

-- | A monopole moment.
data Monopole = Monopole
  { q00 :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Monopole where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Monopole where
  parseJSON = genericParseJSON spicyJOption

type MultipoleR0 = Monopole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q00" k Monopole Monopole a b where
  labelOptic = lens q00 $ \s b -> s {q00 = b}

----------------------------------------------------------------------------------------------------

-- | A spherical dipole moment.
data Dipole = Dipole
  { q10 :: Double,
    q11c :: Double,
    q11s :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Dipole where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Dipole where
  parseJSON = genericParseJSON spicyJOption

type MultipoleR1 = Dipole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q10" k Dipole Dipole a b where
  labelOptic = lens q11c $ \s b -> s {q10 = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q11c" k Dipole Dipole a b where
  labelOptic = lens q11c $ \s b -> s {q11c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q11s" k Dipole Dipole a b where
  labelOptic = lens q11s $ \s b -> s {q11s = b}

----------------------------------------------------------------------------------------------------

-- | A spherical quadrupole moment.
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

instance FromJSON Quadrupole where
  parseJSON = genericParseJSON spicyJOption

type MultipoleR2 = Quadrupole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q20" k Quadrupole Quadrupole a b where
  labelOptic = lens q20 $ \s b -> s {q20 = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q21c" k Quadrupole Quadrupole a b where
  labelOptic = lens q21c $ \s b -> s {q21c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q21s" k Quadrupole Quadrupole a b where
  labelOptic = lens q21s $ \s b -> s {q21s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q22c" k Quadrupole Quadrupole a b where
  labelOptic = lens q22c $ \s b -> s {q22c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q22s" k Quadrupole Quadrupole a b where
  labelOptic = lens q22s $ \s b -> s {q22s = b}

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

instance FromJSON Octopole where
  parseJSON = genericParseJSON spicyJOption

type MultipoleR3 = Octopole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q30" k Octopole Octopole a b where
  labelOptic = lens q30 $ \s b -> s {q30 = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q31c" k Octopole Octopole a b where
  labelOptic = lens q31c $ \s b -> s {q31c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q31s" k Octopole Octopole a b where
  labelOptic = lens q31s $ \s b -> s {q31s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q32c" k Octopole Octopole a b where
  labelOptic = lens q32c $ \s b -> s {q32c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q32s" k Octopole Octopole a b where
  labelOptic = lens q32s $ \s b -> s {q32s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q33c" k Octopole Octopole a b where
  labelOptic = lens q33c $ \s b -> s {q33c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q33s" k Octopole Octopole a b where
  labelOptic = lens q33s $ \s b -> s {q33s = b}

----------------------------------------------------------------------------------------------------

-- | A spherical octopole moment.
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

instance FromJSON Hexadecapole where
  parseJSON = genericParseJSON spicyJOption

type MultipoleR4 = Hexadecapole

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q40" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q40 $ \s b -> s {q40 = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q41c" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q41c $ \s b -> s {q41c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q41s" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q41s $ \s b -> s {q41s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q42c" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q42c $ \s b -> s {q42c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q42s" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q42s $ \s b -> s {q42s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q43c" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q43c $ \s b -> s {q43c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q43s" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q43s $ \s b -> s {q43s = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q44c" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q44c $ \s b -> s {q44c = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "q44s" k Hexadecapole Hexadecapole a b where
  labelOptic = lens q44s $ \s b -> s {q44s = b}

{-
====================================================================================================
-}

-- | Available embedding methods for layer interaction in ONIOM.
data Embedding
  = -- | Mechanical embedding.
    Mechanical
  | -- | Electrostatic embedding. Scaling factors of charges in a
    --   given distance to a capped atom can be given.
    Electronic (Maybe (Seq Double))
  deriving (Eq, Show, Generic)

instance ToJSON Embedding where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Embedding where
  parseJSON = genericParseJSON spicyJOption

-- Prisms
_Mechanical :: Prism' Embedding ()
_Mechanical = prism' (const Mechanical) $ \s -> case s of
  Mechanical -> Just ()
  _ -> Nothing

_Electronic :: Prism' Embedding (Maybe (Seq Double))
_Electronic = prism' Electronic $ \s -> case s of
  Electronic b -> Just b
  _ -> Nothing

{-
====================================================================================================
-}

-- | Settings for optimisers. Those are specific for a given layer if microiterations are used.
-- If only macroiterations are used, the settings of the real system will be used.
data Optimisation = Optimisation
  { coordType :: CoordType,
    -- | Maximum number of geometry optimisation iterations.
    maxCycles :: Int,
    -- | Recalculate a true hessian every n steps. If this is 'Nothing' no hessians will be
    -- calculated
    hessianRecalc :: Maybe Int,
    -- | How the hessian is updated through the curse of the optimisation.
    hessianUpdate :: HessianUpdate,
    -- | Initial trust radius of the optimisation.
    trustRadius :: Double,
    -- | Maximum of the trust radius in optimisation.
    maxTrust :: Double,
    -- | Minimum of the trust radius in optimisation.
    minTrust :: Double,
    -- | Whether to perform a line search.
    lineSearch :: Bool,
    -- | Optimisation to either minima or saddle points.
    optType :: OptType,
    -- | Convergence criteria.
    convergence :: GeomConv,
    -- | Atom indices to freeze.
    freezes :: IntSet,
    -- | Pysisyphus connection settings.
    pysisyphus :: IPI,
    -- | Settings in case micro-optimisers are used.
    microStep :: MicroStep
  }

instance DefaultIO Optimisation where
  defIO = do
    defPysis <- defIO
    return
      Optimisation
        { coordType = DLC,
          maxCycles = 50,
          hessianRecalc = Nothing,
          hessianUpdate = BFGS,
          trustRadius = 0.3,
          minTrust = 0.1,
          maxTrust = 1.0,
          lineSearch = True,
          optType = Minimum RFO,
          pysisyphus = defPysis,
          convergence = def,
          freezes = mempty,
          microStep = LBFGS
        }

instance ToJSON Optimisation where
  toJSON
    Optimisation
      { coordType,
        maxCycles,
        hessianRecalc,
        hessianUpdate,
        trustRadius,
        maxTrust,
        minTrust,
        lineSearch,
        optType,
        convergence,
        freezes,
        microStep
      } =
      object
        [ "coordType" .= coordType,
          "maxCycles" .= maxCycles,
          "hessianRecalc" .= hessianRecalc,
          "hessianUpdate" .= hessianUpdate,
          "trustRadius" .= trustRadius,
          "maxTrust" .= maxTrust,
          "minTrust" .= minTrust,
          "lineSearch" .= lineSearch,
          "optType" .= optType,
          "convergence" .= convergence,
          "freezes" .= freezes,
          "microStep" .= microStep
        ]

-- Lenses
instance (k ~ A_Lens, a ~ CoordType, b ~ a) => LabelOptic "coordType" k Optimisation Optimisation a b where
  labelOptic = lens coordType $ \s b -> s {coordType = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "maxCycles" k Optimisation Optimisation a b where
  labelOptic = lens maxCycles $ \s b -> s {maxCycles = b}

instance (k ~ A_Lens, a ~ Maybe Int, b ~ a) => LabelOptic "hessianRecalc" k Optimisation Optimisation a b where
  labelOptic = lens hessianRecalc $ \s b -> s {hessianRecalc = b}

instance (k ~ A_Lens, a ~ HessianUpdate, b ~ a) => LabelOptic "hessianUpdate" k Optimisation Optimisation a b where
  labelOptic = lens hessianUpdate $ \s b -> s {hessianUpdate = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "trustRadius" k Optimisation Optimisation a b where
  labelOptic = lens trustRadius $ \s b -> s {trustRadius = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "maxTrust" k Optimisation Optimisation a b where
  labelOptic = lens maxTrust $ \s b -> s {maxTrust = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "minTrust" k Optimisation Optimisation a b where
  labelOptic = lens minTrust $ \s b -> s {minTrust = b}

instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "lineSearch" k Optimisation Optimisation a b where
  labelOptic = lens lineSearch $ \s b -> s {lineSearch = b}

instance (k ~ A_Lens, a ~ OptType, b ~ a) => LabelOptic "optType" k Optimisation Optimisation a b where
  labelOptic = lens optType $ \s b -> s {optType = b}

instance (k ~ A_Lens, a ~ GeomConv, b ~ a) => LabelOptic "convergence" k Optimisation Optimisation a b where
  labelOptic = lens convergence $ \s b -> s {convergence = b}

instance (k ~ A_Lens, a ~ IntSet, b ~ a) => LabelOptic "freezes" k Optimisation Optimisation a b where
  labelOptic = lens freezes $ \s b -> s {freezes = b}

instance (k ~ A_Lens, a ~ IPI, b ~ a) => LabelOptic "pysisyphus" k Optimisation Optimisation a b where
  labelOptic = lens pysisyphus $ \s b -> s {pysisyphus = b}

instance (k ~ A_Lens, a ~ MicroStep, b ~ a) => LabelOptic "microStep" k Optimisation Optimisation a b where
  labelOptic = lens microStep $ \s b -> s {microStep = b}

----------------------------------------------------------------------------------------------------

-- | Type of coordinates used for optimisation.
data CoordType
  = DLC
  | Cart
  | Redund
  deriving (Eq, Show)

instance FromJSON CoordType where
  parseJSON v = case v of
    String "dlc" -> pure DLC
    String "cart" -> pure Cart
    String "redund" -> pure Redund
    _ -> fail "encountered unknown field for CoordType"

instance ToJSON CoordType where
  toJSON v = case v of
    DLC -> toJSON @Text "dlc"
    Cart -> toJSON @Text "cart"
    Redund -> toJSON @Text "redund"

----------------------------------------------------------------------------------------------------

-- | Algorithm to keep an approximate hessian up to date between optimisation steps.
data HessianUpdate
  = BFGS
  | FlowChart
  | DampedBFGS
  | Bofill
  deriving (Eq, Show)

instance FromJSON HessianUpdate where
  parseJSON v = case v of
    String "bfgs" -> pure BFGS
    String "flowchart" -> pure FlowChart
    String "damped_bfgs" -> pure DampedBFGS
    String "bofill" -> pure Bofill
    _ -> fail "encountered unknown field for TSOptAlg"

instance ToJSON HessianUpdate where
  toJSON v = case v of
    BFGS -> toJSON @Text "bfgs"
    FlowChart -> toJSON @Text "flowchart"
    DampedBFGS -> toJSON @Text "damped_bfgs"
    Bofill -> toJSON @Text "bofill"

----------------------------------------------------------------------------------------------------

-- | Available step types for steps with the micro-cycles.
data MicroStep
  = LBFGS
  | SD
  | CG
  deriving (Eq, Show, Generic)

instance FromJSON MicroStep where
  parseJSON v = case v of
    String "lbfgs" -> pure LBFGS
    String "sd" -> pure SD
    String "cg" -> pure CG
    _ -> fail "encountered unknown field for MicroStep"

instance ToJSON MicroStep where
  toJSON v = case v of
    LBFGS -> toJSON @Text "lbfgs"
    SD -> toJSON @Text "sd"
    CG -> toJSON @Text "cg"

----------------------------------------------------------------------------------------------------

-- | Optimisation type.
data OptType
  = Minimum MinOptAlg
  | SaddlePoint TSOptAlg
  deriving (Eq, Show, Generic)

instance FromJSON OptType

instance ToJSON OptType

-- Prisms
_Minimum :: Prism' OptType MinOptAlg
_Minimum = prism' Minimum $ \s -> case s of
  Minimum b -> Just b
  _ -> Nothing

_SaddlePoint :: Prism' OptType TSOptAlg
_SaddlePoint = prism' SaddlePoint $ \s -> case s of
  SaddlePoint b -> Just b
  _ -> Nothing

----------------------------------------------------------------------------------------------------

-- | Optimisation algorithms for transition state/saddle point search.
data TSOptAlg
  = RS_I_RFO
  | RS_P_RFO
  | TRIM
  deriving (Eq, Show)

instance FromJSON TSOptAlg where
  parseJSON v = case v of
    String "rsirfo" -> pure RS_I_RFO
    String "rsprfo" -> pure RS_P_RFO
    String "trim" -> pure TRIM
    _ -> fail "encountered unknown field for TSOptAlg"

instance ToJSON TSOptAlg where
  toJSON v = case v of
    RS_I_RFO -> toJSON @Text "rsirfo"
    RS_P_RFO -> toJSON @Text "rsprfo"
    TRIM -> toJSON @Text "trim"

----------------------------------------------------------------------------------------------------

-- | Optimisation algorithm for geometry optimisation to minima.
data MinOptAlg
  = RFO
  deriving (Eq, Show)

instance FromJSON MinOptAlg where
  parseJSON v = case v of
    String "rfo" -> pure RFO
    _ -> fail "encountered unknown field for MinOptAlg"

instance ToJSON MinOptAlg where
  toJSON RFO = toJSON @Text "rfo"

----------------------------------------------------------------------------------------------------

-- | Convergence criteria of a geometry optimisation. 'Nothing' values are not checked for
-- convergce.
data GeomConv = GeomConv
  { rmsForce :: Maybe Double,
    maxForce :: Maybe Double,
    rmsDisp :: Maybe Double,
    maxDisp :: Maybe Double,
    eDiff :: Maybe Double
  }
  deriving (Eq, Show, Generic)

-- | 'Ord' instance, that can be used to check convergence. The left side is the value and the right
-- side the criterion.
instance Ord GeomConv where
  a `compare` b
    | a == b = EQ
    | all (== LT) allChecks = LT
    | otherwise = GT
    where
      allChecks =
        [ rmsForce a `compareMaybe` rmsForce b,
          maxForce a `compareMaybe` maxForce b,
          rmsDisp a `compareMaybe` rmsDisp b,
          maxDisp a `compareMaybe` maxDisp b,
          eDiff a `compareMaybe` eDiff b
        ]
      compareMaybe x y = case y of
        Nothing -> LT
        _ -> x `compare` y

instance ToJSON GeomConv where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON GeomConv where
  parseJSON = genericParseJSON spicyJOption

instance Default GeomConv where
  def =
    GeomConv
      { maxForce = Just $ 0.00045 * 1.88973,
        rmsForce = Just $ 0.00030 * 1.88973,
        maxDisp = Just $ 0.00180 * 1.88973,
        rmsDisp = Just $ 0.00120 * 1.88973,
        eDiff = Just 0.00001
      }

-- Labels
instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "rmsForce" k GeomConv GeomConv a b where
  labelOptic = lens rmsForce $ \s b -> s {rmsForce = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "maxForce" k GeomConv GeomConv a b where
  labelOptic = lens maxForce $ \s b -> s {maxForce = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "rmsDisp" k GeomConv GeomConv a b where
  labelOptic = lens rmsDisp $ \s b -> s {rmsDisp = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "maxDisp" k GeomConv GeomConv a b where
  labelOptic = lens maxDisp $ \s b -> s {maxDisp = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "eDiff" k GeomConv GeomConv a b where
  labelOptic = lens eDiff $ \s b -> s {eDiff = b}

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

instance FromJSON EnergyDerivatives where
  parseJSON = genericParseJSON spicyJOption

instance Default EnergyDerivatives where
  def =
    EnergyDerivatives
      { energy = Nothing,
        gradient = Nothing,
        hessian = Nothing
      }

-- Lenses
instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "energy" k EnergyDerivatives EnergyDerivatives a b where
  labelOptic = lens energy $ \s b -> s {energy = b}

instance (k ~ A_Lens, a ~ Maybe (VectorS Double), b ~ a) => LabelOptic "gradient" k EnergyDerivatives EnergyDerivatives a b where
  labelOptic = lens gradient $ \s b -> s {gradient = b}

instance (k ~ A_Lens, a ~ Maybe (MatrixS Double), b ~ a) => LabelOptic "hessian" k EnergyDerivatives EnergyDerivatives a b where
  labelOptic = lens hessian $ \s b -> s {hessian = b}

{-
====================================================================================================
-}

-- | A task the wrapper needs to perform.
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

instance FromJSON WrapperTask where
  parseJSON = genericParseJSON spicyJOption

----------------------------------------------------------------------------------------------------

-- | Context specific to a QM calculation.
data QMContext = QMContext
  { -- | Charge of the (sub)system as transparent to the QM software.
    charge :: !Int,
    -- | Multiplicity of the (sub)system as transparent to the QM software.
    mult :: !Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON QMContext where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON QMContext where
  parseJSON = genericParseJSON spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "charge" k QMContext QMContext a b where
  labelOptic = lens charge $ \s b -> s {charge = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "mult" k QMContext QMContext a b where
  labelOptic = lens mult $ \s b -> s {mult = b}

----------------------------------------------------------------------------------------------------

-- | Context specific to a MM calculation.
data MMContext = MMContext
  deriving (Show, Eq, Generic)

instance ToJSON MMContext where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON MMContext where
  parseJSON = genericParseJSON spicyJOption

----------------------------------------------------------------------------------------------------

-- | Information needed either by a QM or MM calculation.
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
_QM = prism' QM $ \s -> case s of
  QM b -> Just b
  _ -> Nothing

_MM :: Prism' QMMMSpec MMContext
_MM = prism' MM $ \s -> case s of
  MM b -> Just b
  _ -> Nothing

----------------------------------------------------------------------------------------------------

-- | The context of a calculation is the combination of its input and output values.
data CalcContext = CalcContext
  { -- | Necessary information to define a calculation.
    input :: !CalcInput,
    -- | Information produced by the calculation as
    --   defined by the 'CalcInput'.
    output :: !(Maybe CalcOutput)
  }
  deriving (Generic)

instance ToJSON CalcContext where
  toEncoding = genericToEncoding spicyJOption

instance (k ~ A_Lens, a ~ CalcInput, b ~ a) => LabelOptic "input" k CalcContext CalcContext a b where
  labelOptic = lens input $ \s b -> s {input = b}

instance (k ~ A_Lens, a ~ Maybe CalcOutput, b ~ a) => LabelOptic "output" k CalcContext CalcContext a b where
  labelOptic = lens output $ \s b -> s {output = b}

----------------------------------------------------------------------------------------------------

-- | The context for a wrapper calculation. These are information used by the system call to the wrapper
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
    -- | The embedding type for this calculation
    --   part. Might be ignored for Inherited
    --   calculations in ONIOM (low calculation
    --   level on model system).
    embedding :: !Embedding,
    -- | Settings for geometry optimisations on this layer. Always given and if not specified in the
    -- input defaulting.
    optimisation :: !Optimisation,
    -- | Additional input text, which will be inserted into the input file.
    additionalInput :: !(Maybe Text)
  }
  deriving (Generic)

instance ToJSON CalcInput where
  toEncoding = genericToEncoding spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ WrapperTask, b ~ a) => LabelOptic "task" k CalcInput CalcInput a b where
  labelOptic = lens task $ \s b -> s {task = b}

instance (k ~ A_Lens, a ~ Maybe JFilePathAbs, b ~ a) => LabelOptic "restartFile" k CalcInput CalcInput a b where
  labelOptic = lens restartFile $ \s b -> s {restartFile = b}

instance (k ~ A_Lens, a ~ Program, b ~ a) => LabelOptic "software" k CalcInput CalcInput a b where
  labelOptic = lens software $ \s b -> s {software = b}

instance (k ~ A_Lens, a ~ String, b ~ a) => LabelOptic "prefixName" k CalcInput CalcInput a b where
  labelOptic = lens prefixName $ \s b -> s {prefixName = b}

instance (k ~ A_Lens, a ~ JDirPathAbs, b ~ a) => LabelOptic "permaDir" k CalcInput CalcInput a b where
  labelOptic = lens permaDir $ \s b -> s {permaDir = b}

instance (k ~ A_Lens, a ~ JDirPathAbs, b ~ a) => LabelOptic "scratchDir" k CalcInput CalcInput a b where
  labelOptic = lens scratchDir $ \s b -> s {scratchDir = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "nProcs" k CalcInput CalcInput a b where
  labelOptic = lens nProcs $ \s b -> s {nProcs = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "nThreads" k CalcInput CalcInput a b where
  labelOptic = lens nThreads $ \s b -> s {nThreads = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "memory" k CalcInput CalcInput a b where
  labelOptic = lens memory $ \s b -> s {memory = b}

instance (k ~ A_Lens, a ~ QMMMSpec, b ~ a) => LabelOptic "qMMMSpec" k CalcInput CalcInput a b where
  labelOptic = lens qMMMSpec $ \s b -> s {qMMMSpec = b}

instance (k ~ A_Lens, a ~ Embedding, b ~ a) => LabelOptic "embedding" k CalcInput CalcInput a b where
  labelOptic = lens embedding $ \s b -> s {embedding = b}

instance (k ~ A_Lens, a ~ Optimisation, b ~ a) => LabelOptic "optimisation" k CalcInput CalcInput a b where
  labelOptic = lens optimisation $ \s b -> s {optimisation = b}

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "additionalInput" k CalcInput CalcInput a b where
  labelOptic = lens additionalInput $ \s b -> s {additionalInput = b}

----------------------------------------------------------------------------------------------------

-- | Results from a wrapper calculation on the given niveau. If for example multiple computations are
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

instance (k ~ A_Lens, a ~ EnergyDerivatives, b ~ a) => LabelOptic "energyDerivatives" k CalcOutput CalcOutput a b where
  labelOptic = lens (energyDerivatives :: CalcOutput -> EnergyDerivatives) $
    \s b -> (s {energyDerivatives = b} :: CalcOutput)

instance (k ~ A_Lens, a ~ IntMap Multipoles, b ~ a) => LabelOptic "multipoles" k CalcOutput CalcOutput a b where
  labelOptic = lens (multipoles :: CalcOutput -> IntMap Multipoles) $
    \s b -> (s {multipoles = b} :: CalcOutput)

{-
====================================================================================================
-}

-- | A known computational chemistry program to use, and some program-specific data.
data Program
  = Psi4 QCHamiltonian
  | XTB GFN
  | Turbomole QCHamiltonian
  deriving (Eq, Show, Generic)

instance ToJSON Program where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Program where
  parseJSON = genericParseJSON spicyJOption

_XTB :: Prism' Program GFN
_XTB = prism' XTB $ \s -> case s of
  XTB b -> Just b
  _ -> Nothing

_Psi4 :: Prism' Program QCHamiltonian
_Psi4 = prism' Psi4 $ \s -> case s of
  Psi4 info -> Just info
  _ -> Nothing

_Turbomole :: Prism' Program QCHamiltonian
_Turbomole = prism' Turbomole $ \s -> case s of
  Turbomole info -> Just info
  _ -> Nothing

-- | Extra prism, that gets the QC Hamiltonian, regardless of which constructor supplies it
-- (if it supplies ...).
_ProgramHamiltonian :: Prism' Program QCHamiltonian
_ProgramHamiltonian = prism' Turbomole $ \s -> case s of
  Turbomole h -> Just h
  Psi4 h -> Just h
  XTB _ -> Nothing

-- Auxilliary functions for working with Program data

-- | Returns true if the program is Psi4, and false otherwise.
isPsi4 :: Program -> Bool
isPsi4 (Psi4 _) = True
isPsi4 _ = False

-- | Returns true if the program is XTB, and false otherwise.
isXTB :: Program -> Bool
isXTB (XTB _) = True
isXTB _ = False

-- | Returns true if the program is Turbomole, and false otherwise.
isTurbomole :: Program -> Bool
isTurbomole (Turbomole _) = True
isTurbomole _ = False

----------------------------------------------------------------------------------------------------

-- | Version of the GFN-Hamiltonian in XTB calculations.
data GFN
  = GFNZero
  | GFNOne
  | GFNTwo
  deriving (Eq, Show, Generic)

-- FF hamiltonian still to do

instance ToJSON GFN where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON GFN where
  parseJSON = genericParseJSON spicyJOption

-- | Convert the enumeration type representation to the digit expected by XTB.
renderGFN :: GFN -> Text
renderGFN GFNZero = "0"
renderGFN GFNOne = "1"
renderGFN GFNTwo = "2"

----------------------------------------------------------------------------------------------------

-- | Program specific information for Turbomole.
data QCHamiltonian = QCHamiltonian
  { -- | Basis set specifications
    basis :: BasisSet,
    -- | SCF settings
    scf :: Maybe SCF,
    -- | Reference wavefunction settings
    ref :: RefWfn,
    -- | Settings for resolution of identity
    ri :: Maybe RI,
    -- | Correlation of the wavefunction and excited states
    corr :: Maybe Correlation,
    -- | Excited states
    exc :: Maybe Excitations,
    -- | Other keywords to use verbatim
    other :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

instance ToJSON QCHamiltonian where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON QCHamiltonian where
  parseJSON = genericParseJSON spicyJOption

instance (k ~ A_Lens, a ~ BasisSet, b ~ a) => LabelOptic "basis" k QCHamiltonian QCHamiltonian a b where
  labelOptic = lens (basis :: QCHamiltonian -> BasisSet) $ \s b -> (s {basis = b} :: QCHamiltonian)

instance (k ~ A_Lens, a ~ Maybe SCF, b ~ a) => LabelOptic "scf" k QCHamiltonian QCHamiltonian a b where
  labelOptic = lens scf $ \s b -> s {scf = b}

instance (k ~ A_Lens, a ~ RefWfn, b ~ a) => LabelOptic "ref" k QCHamiltonian QCHamiltonian a b where
  labelOptic = lens ref $ \s b -> s {ref = b}

instance (k ~ A_Lens, a ~ Maybe RI, b ~ a) => LabelOptic "ri" k QCHamiltonian QCHamiltonian a b where
  labelOptic = lens ri $ \s b -> s {ri = b}

instance (k ~ A_Lens, a ~ Maybe Correlation, b ~ a) => LabelOptic "corr" k QCHamiltonian QCHamiltonian a b where
  labelOptic = lens corr $ \s b -> s {corr = b}

instance (k ~ A_Lens, a ~ Maybe Excitations, b ~ a) => LabelOptic "exc" k QCHamiltonian QCHamiltonian a b where
  labelOptic = lens exc $ \s b -> s {exc = b}

instance (k ~ A_Lens, a ~ Maybe [Text], b ~ a) => LabelOptic "other" k QCHamiltonian QCHamiltonian a b where
  labelOptic = lens (other :: QCHamiltonian -> Maybe [Text]) $ \s b -> (s {other = b} :: QCHamiltonian)

----------------------------------------------------------------------------------------------------

-- | Specifications of various basis sets for quantum chemistry packages.
data BasisSet = BasisSet
  { -- | Basis from the turbomole basis set library
    basis :: Text,
    -- | Auxiliary coulomb-fitting basis from the library
    jbas :: Maybe Text,
    -- | Auxiliary JK-fitting basis from the library
    jkbas :: Maybe Text,
    -- | Effective core potential from the library
    ecp :: Maybe Text,
    -- | Correlation fitting basis from the library
    cbas :: Maybe Text,
    -- | Complementary auxiliary basis for explicitly correlated calculations
    cabs :: Maybe Text,
    -- | Other keywords to put verbatim in the @$atoms@ block
    other :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON BasisSet

instance ToJSON BasisSet

instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "basis" k BasisSet BasisSet a b where
  labelOptic = lens (basis :: BasisSet -> Text) $ \s b -> (s {basis = b} :: BasisSet)

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "jbas" k BasisSet BasisSet a b where
  labelOptic = lens jbas $ \s b -> s {jbas = b}

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "jkbas" k BasisSet BasisSet a b where
  labelOptic = lens jkbas $ \s b -> s {jkbas = b}

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "ecp" k BasisSet BasisSet a b where
  labelOptic = lens ecp $ \s b -> s {ecp = b}

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "cbas" k BasisSet BasisSet a b where
  labelOptic = lens cbas $ \s b -> s {cbas = b}

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "cabs" k BasisSet BasisSet a b where
  labelOptic = lens cabs $ \s b -> s {cabs = b}

instance (k ~ A_Lens, a ~ Maybe [Text], b ~ a) => LabelOptic "other" k BasisSet BasisSet a b where
  labelOptic = lens (other :: BasisSet -> Maybe [Text]) $ \s b -> (s {other = b} :: BasisSet)

----------------------------------------------------------------------------------------------------

-- | SCF settings. Serialise to multiple fields but control SCF cycles as common ground.
data SCF = SCF
  { -- | SCF convergence threshold as exponent.
    conv :: Natural,
    -- | Maximum number of iterations of the SCF.
    iter :: Natural,
    -- | Damping control.
    damp :: Maybe Damp,
    -- | Orbital level shift
    shift :: Maybe Double,
    -- | Other SCF settings to be used verbatim
    other :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON SCF

instance ToJSON SCF

instance Default SCF where
  def = SCF {conv = 8, iter = 200, damp = def, shift = Nothing, other = Nothing}

instance (k ~ A_Lens, a ~ Natural, b ~ a) => LabelOptic "conv" k SCF SCF a b where
  labelOptic = lens conv $ \s b -> s {conv = b}

instance (k ~ A_Lens, a ~ Natural, b ~ a) => LabelOptic "iter" k SCF SCF a b where
  labelOptic = lens (iter :: SCF -> Natural) $ \s b -> (s {iter = b} :: SCF)

instance (k ~ A_Lens, a ~ Maybe Damp, b ~ a) => LabelOptic "damp" k SCF SCF a b where
  labelOptic = lens damp $ \s b -> s {damp = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "shift" k SCF SCF a b where
  labelOptic = lens shift $ \s b -> s {shift = b}

instance (k ~ A_Lens, a ~ Maybe [Text], b ~ a) => LabelOptic "other" k SCF SCF a b where
  labelOptic = lens (other :: SCF -> Maybe [Text]) $ \s b -> (s {other = b} :: SCF)

----------------------------------------------------------------------------------------------------

-- | Settings of the SCF damping
data Damp = Damp
  { -- | Amount of old fock matrix to be mixed in.
    start :: Double,
    -- | On good convergece how much to reduce the mixing amount in each SCF step. Not used by all
    -- programs.
    step :: Double,
    -- | Minimum amount of the old fock matrix to be used in damping. Will not drop below this.
    lower :: Double
  }
  deriving (Eq, Show, Generic)

instance FromJSON Damp

instance ToJSON Damp

instance Default Damp where
  def = Damp {start = 0.75, step = 0.05, lower = 0.1}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "start" k Damp Damp a b where
  labelOptic = lens start $ \s b -> s {start = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "step" k Damp Damp a b where
  labelOptic = lens step $ \s b -> s {step = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "lower" k Damp Damp a b where
  labelOptic = lens (lower :: Damp -> Double) $ \s b -> s {lower = b}

----------------------------------------------------------------------------------------------------

-- | Type of RI approximation for the reference wavefunction.
data RI = RIJ | RIJK | OtherRI Text deriving (Eq, Show, Generic)

instance FromJSON RI

instance ToJSON RI

_OtherRI :: Prism' RI Text
_OtherRI = prism' (\t -> OtherRI t) $ \s -> case s of
  OtherRI t -> Just t
  _ -> Nothing

----------------------------------------------------------------------------------------------------

-- | Reference wavefunction specification.
data RefWfn
  = -- | Restricted Hartree-Fock
    RHF
  | -- | Unrestricted Hartree-Fock. Needs a multiplicity.
    UHF
  | -- | Restricted Kohn-Sham DFT
    RKS DFT
  | -- | Unrestricted Kohn-Sham DFT. Needs a multiplicity.
    UKS DFT
  deriving (Eq, Show, Generic)

instance FromJSON RefWfn

instance ToJSON RefWfn

_RKS :: Prism' RefWfn DFT
_RKS = prism' (\d -> RKS d) $ \s -> case s of
  RKS d -> Just d
  _ -> Nothing

_UKS :: Prism' RefWfn DFT
_UKS = prism' (\d -> UKS d) $ \s -> case s of
  UKS d -> Just d
  _ -> Nothing

----------------------------------------------------------------------------------------------------

-- | Settings for density functional theory.
data DFT = DFT
  { -- | Density functional to be used
    functional :: Text,
    -- | DFT grid specification
    grid :: Maybe Text,
    -- | Dispersion correction
    disp :: Maybe Text,
    -- | Other keywords to put verbatim in the DFT input.
    other :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON DFT

instance ToJSON DFT

instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "functional" k DFT DFT a b where
  labelOptic = lens functional $ \s b -> s {functional = b}

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "grid" k DFT DFT a b where
  labelOptic = lens grid $ \s b -> s {grid = b}

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "disp" k DFT DFT a b where
  labelOptic = lens disp $ \s b -> s {disp = b}

instance (k ~ A_Lens, a ~ Maybe [Text], b ~ a) => LabelOptic "other" k DFT DFT a b where
  labelOptic = lens (other :: DFT -> Maybe [Text]) $ \s b -> (s {other = b} :: DFT)

----------------------------------------------------------------------------------------------------

-- | Correlation on top of the reference wavefunction.
data Correlation = Correlation
  { -- | The program module, which to use for correlation calculation. Not used in all programs.
    corrModule :: Maybe Text,
    -- | Wavefunciton correlation model to be used
    method :: Text,
    -- | Maximum number of iterations in convergence of correlation
    iter :: Maybe Natural,
    -- | Other keywords to put verbatim in the input
    other :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Correlation

instance ToJSON Correlation

instance (k ~ A_Lens, a ~ Maybe Text, b ~ a) => LabelOptic "corrModule" k Correlation Correlation a b where
  labelOptic = lens corrModule $ \s b -> s {corrModule = b}

instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "method" k Correlation Correlation a b where
  labelOptic = lens method $ \s b -> s {method = b}

instance (k ~ A_Lens, a ~ Maybe Natural, b ~ a) => LabelOptic "iter" k Correlation Correlation a b where
  labelOptic = lens (iter :: Correlation -> Maybe Natural) $ \s b -> (s {iter = b} :: Correlation)

instance (k ~ A_Lens, a ~ Maybe [Text], b ~ a) => LabelOptic "other" k Correlation Correlation a b where
  labelOptic = lens (other :: Correlation -> Maybe [Text]) $ \s b -> (s {other = b} :: Correlation)

----------------------------------------------------------------------------------------------------

-- | Excitations. Either on top of the correlated wavefunction (if used) or ontop of the reference
-- wavefunction, if no correlation is given.
data Excitations = Excitations
  { -- | Number of states to be calculated.
    states :: Natural,
    -- | Possibly a target root, for which to calculate properties like gradients or densities
    target :: Maybe Natural,
    -- | Other information, that will be put verbatim into the input file.
    other :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Excitations

instance ToJSON Excitations

instance (k ~ A_Lens, a ~ Natural, b ~ a) => LabelOptic "states" k Excitations Excitations a b where
  labelOptic = lens states $ \s b -> s {states = b}

instance (k ~ A_Lens, a ~ Maybe Natural, b ~ a) => LabelOptic "target" k Excitations Excitations a b where
  labelOptic = lens target $ \s b -> s {target = b}

instance (k ~ A_Lens, a ~ Maybe [Text], b ~ a) => LabelOptic "other" k Excitations Excitations a b where
  labelOptic = lens (other :: Excitations -> Maybe [Text]) $ \s b -> (s {other = b} :: Excitations)

{-
####################################################################################################
-}

-- $localHelperTypes
-- These types are not meant to be returned or consumed by any exported function. Instead they are used
-- only locally to pass information around, which would otherwise need to be stored in unannotated
-- tuples or something.

-- | Type to pass atomwise information from parsers with atom-fragment associations around.
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

-- | Type just to handle the information obtained from parsing a single atom line of a TXYZ file.
data TXYZAtomInfo = TXYZAtomInfo
  { -- | Index of this atom in the TXYZ file. First number in a row.
    txyzIndex :: Int,
    -- | The atom of this line.
    txyzAtom :: Atom,
    -- | The indices of the atoms, to which this atom has a bond.
    txyzBondTargets :: [Int]
  }
