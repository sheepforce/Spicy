{-|
Module      : Spicy.Molecule
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
module Spicy.Molecule
  ( -- * Exceptions
    MolLogicException
    -- * Types
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
    -- * Parser
  , parseXYZ
  , parseTXYZ
  , parseMOL2
  , parsePDB
    -- * Writer
  , writeXYZ
  , writeTXYZ
  , writeMOL2
  , writePDB
  , writeSpicy
  )
where
import           Spicy.Generic
import           Spicy.Molecule.Internal.Math
import           Spicy.Molecule.Internal.Parser
import           Spicy.Molecule.Internal.Types
import           Spicy.Molecule.Internal.Util
import           Spicy.Molecule.Internal.Writer

{-
====================================================================================================
-}
instance Check Molecule where
  check = checkMolecule
