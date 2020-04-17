{-|
Module      : Spicy.Molecule
Description : Handling molecular informations
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows
-}
module Spicy.Molecule
  ( -- * Parser
    parseXYZ
  , parseTXYZ
  , parseMOL2
  , parsePDB
    -- * Utility Functions
  , molMap
  , molMapWithMolID
  , molTraverse
  , molTraverseWithID
  , molFoldl
  , molFoldlWithMolID
  , molIDLensGen
  , calcIDLensGen
  , newSubLayer
  , createPseudoAtom
  , addAtom
  , removeAtom
  , BondOperation(..)
  , changeBond
  , neighbourList
  , guessBondMatrix
  , fragmentDetectionDetached
  , getPolarisationCloudFromAbove
    -- * Writer
  , writeXYZ
  , writeTXYZ
  , writeMOL2
  , writePDB
  , writeSpicy
  , writeLayout
  )
where
import           Spicy.Molecule.Internal.Parser
import           Spicy.Molecule.Internal.Util
import           Spicy.Molecule.Internal.Writer
