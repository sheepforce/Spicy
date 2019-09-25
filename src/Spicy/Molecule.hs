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
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
--import           Spicy.Molecule.Internal.Math
import           Spicy.Molecule.Internal.Parser
import           Spicy.Molecule.Internal.Types
import           Spicy.Molecule.Internal.Util
import           Spicy.Molecule.Internal.Writer

{-
====================================================================================================
-}
{-|
As an exception allow for an orphan instance here, which very clearly belongs to the 'Molecule' type
as defined in 'Spicy.Molecule.Internal.Types'. This avoids circular imports.
-}
instance Check Molecule where
  check = checkMolecule
