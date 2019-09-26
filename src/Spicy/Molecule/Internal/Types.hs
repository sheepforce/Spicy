{-|
Module      : Spicy.Molecule.Internal.Types
Description : Handling molecular informations
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module deals with the partitioning of the system, creation of bonds, assignment of
substructeres to layers and creation of ghost atoms. Following conventions shall apply:

    * highest level region has highest index

    * lowest level region has index 0 and contains the complete system
-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Molecule.Internal.Types
  ( -- * Exceptions
    -- $exceptions
    MolLogicException(..)
    -- * Types
    -- $types
  , Element(..)
  , AtomLabel
  , FFType(..)
  , Atom(..)
  , atom_Element
  , atom_Label
  , atom_IsPseudo
  , atom_FFType
  , atom_PCharge
  , atom_Coordinates
  , Molecule(..)
  , molecule_Label
  , molecule_Atoms
  , molecule_Bonds
  , molecule_SubMol
  , molecule_Energy
  , molecule_Gradient
  , molecule_Hessian
  , Trajectory
  )
where

import qualified Data.ByteString.Lazy.UTF8     as BL
import           Data.IntMap.Lazy               ( IntMap )
import           Data.IntSet                    ( IntSet )
import           Data.Sequence                  ( Seq )
import           Data.Text.Lazy                 ( Text )
import           Prelude                 hiding ( cycle
                                                , foldl1
                                                , foldr1
                                                , head
                                                , init
                                                , last
                                                , maximum
                                                , minimum
                                                , tail
                                                , take
                                                , takeWhile
                                                , (!!)
                                                )

import           GHC.Generics                   ( Generic )

import           Control.DeepSeq
import           Control.Exception.Safe
import           Control.Lens            hiding ( (.=) )
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Spicy.Generic

{-
####################################################################################################
-}
{- $exceptions
These types define composable exceptions for Spicy. The types are specific for a class of problems,
that can occur. These exception types are meant to be used in combination with
'Control.Exception.Safe' as described in
<https://tech.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell>.
-}
{-|
Exception type for operations on 'Molecule's, which lead to a logical error. This can be caused
because some Spicy assumptions are not met for example.
-}
data MolLogicException = MolLogicException
  { mlExcFunctionName :: String
  , mlExcDescription  :: String
  }

instance Show MolLogicException where
  show (MolLogicException f e) = "MoleculeLogicException in function \"" ++ f ++ "\":" ++ e

instance Exception MolLogicException

{-
####################################################################################################
-}
{- $types
Types to describe a molecule from a computational geometry point of view. The representation of a
molecule aims to be as descriptive as unambigous as possible and capture all quantities relevant for
molecular dynamics and geometry optimisations (energies, gradients, hessians, force fiekd types,
coordinates, connectivities). Electronic properties, such as dipoles, orbitals, the wavefunction and
so on, will be ignored, as Spicy is not aiming to be an analysis program, but a interface to create
multilayer calculations. Analysis of the results is up to postprocessing.
-}
{-|
All chemical elements. Have them very clear because force fields and pdb names may interfer and are
just arbitrary strings.
-}
data Element =
  H   |                                                                                                                                                                                     He  |
  Li  | Be                                                                                                                                                  | B   | C   | N   | O   | F   | Ne  |
  Na  | Mg                                                                                                                                                  | Al  | Si  | P   | S   | Cl  | Ar  |
  K   | Ca                                                                                      | Sc  | Ti  | V   | Cr  | Mn  | Fe  | Co  | Ni  | Cu  | Zn  | Ga  | Ge  | As  | Se  | Br  | Kr  |
  Rb  | Sr                                                                                      | Y   | Zr  | Nb  | Mo  | Tc  | Ru  | Rh  | Pd  | Ag  | Cd  | In  | Sn  | Sb  | Te  | I   | Xe  |
  Cs  | Ba  | La  | Ce  | Pr  | Nd  | Pm  | Sm  | Eu  | Gd  | Tb  | Dy  | Ho  | Er  | Tm  | Yb  | Lu  | Hf  | Ta  | W   | Re  | Os  | Ir  | Pt  | Au  | Hg  | Tl  | Pb  | Bi  | Po  | At  | Rn  |
  Fr  | Ra  | Ac  | Th  |  Pa | U   | Np  | Pu  | Am  | Cm  | Bk  | Cf  | Es  | Fm  | Md  | No  | Lr  | Rf  | Db  | Sg  | Bh  | Hs  | Mt  | Ds  | Rg  | Cn  | Uut | Fl  | Uup | Lv  | Uus | Uuo
  deriving ( Show, Eq, Read, Ord, Enum, Generic, NFData )

instance ToJSON Element where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Element

----------------------------------------------------------------------------------------------------
{-|
An atom label. They may come from pdb or force field parameter files or can be assigned by other
ways just to distinguish specific atoms.
-}
type AtomLabel = Text

----------------------------------------------------------------------------------------------------
{-|
These are labels for molecular mechanics software. The strings are basically arbitrary and depending
on the MM software used.
-}
data FFType = Mol2 Text | TXYZ Int | PDB Text | XYZ
  deriving ( Generic )

makeLenses ''FFType

instance Eq FFType where
  Mol2 _ == Mol2 _ = True
  Mol2 _ == _      = False
  TXYZ _ == TXYZ _ = True
  TXYZ _ == _      = False
  PDB  _ == PDB _  = True
  PDB  _ == _      = False
  XYZ    == XYZ    = True
  XYZ    == _      = False

instance ToJSON FFType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FFType

----------------------------------------------------------------------------------------------------
{-|
An Atom in a 'Molecule'. Atoms are compared by their indices only and they must therefore be unique.
The coordinates of the 'Atom' are defined as 'Seq', as this is extremely easy to concatenate when
building a coordinate vector.
-}
data Atom = Atom
  { _atom_Element     :: Element      -- ^ Chemical 'Element' of the atom.
  , _atom_Label       :: AtomLabel    -- ^ Label, e.g. from a pdb, just for identification, can
                                      --   be empty.
  , _atom_IsPseudo    :: Bool         -- ^ Boolean, telling if this is a pseudo atom,
                                      --   introduced because a bond was broken.
  , _atom_FFType      :: FFType       -- ^ Label depending on the MM software used, identifying
                                      --   topological atom.
  , _atom_PCharge     :: Maybe Double -- ^ Possibly a partial charge.
  , _atom_Coordinates :: Seq Double   -- ^ Coordinates of the atom, cartesian in R³. Relies on
                                      --   the parser to fill with exactly 3 values.
  }
  deriving ( Eq, Generic )

makeLenses ''Atom

instance Show Atom where
  show = BL.toString . encodePretty

instance ToJSON Atom where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Atom

----------------------------------------------------------------------------------------------------
{-|
A molecule, which might be the whole system, an ONIOM layer or a fragment of the system, each
containing possibly even higher layers for ONIOM or fragments. Stores all associated informations of
a layer.

Starting from a top level molecule, all atoms and bonds of the system are expected to be in the in
this top layer (except pseudoatoms of deeper layers). Therefore if atoms are in a deeper layers of
the recursion, their information is not used to describe a higher layer. Instead, all atoms of
deeper layers (except pseudoatoms) must be also replicated in a higher layer.
-}
data Molecule =
  Molecule { _molecule_Label    :: Text                     -- ^ Comment or identifier of a
                                                            --   molecule. Can be empty.
           , _molecule_Atoms    :: IntMap Atom              -- ^ An 'IntMap' of 'Atom's, 'Atom's
                                                            --   identified by their 'Int' index.
           , _molecule_Bonds    :: IntMap IntSet            -- ^ An IntMap, mapping the index of an
                                                            --   'Atom' in the 'Molecule' to the
                                                            --   indices of all 'Atom's, to which
                                                            --   it binds.
           , _molecule_SubMol   :: Seq Molecule             -- ^ A Molecule might contain other
                                                            --   molecules. These might be fragments
                                                            --   or higher level ONIOM layers.
           , _molecule_Energy   :: Maybe Double             -- ^ An energy, that might have been
                                                            --   calculated.
           , _molecule_Gradient :: Maybe (AccVector Double) -- ^ A gradient, that might have been
                                                            --   calculated.
           , _molecule_Hessian  :: Maybe (AccMatrix Double) -- ^ A hessian, that might have been
                                                            --   calculated.
           }
  deriving ( Eq, Generic )

makeLenses ''Molecule

instance Show Molecule where
  show = BL.toString . encodePretty

instance ToJSON Molecule where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Molecule

----------------------------------------------------------------------------------------------------
{-|
Trajectories are simply 'Seq'uences of 'Molecule's.
-}
type Trajectory = Seq Molecule
