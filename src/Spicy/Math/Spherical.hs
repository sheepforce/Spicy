-- |
-- Module      : Spicy.Math.SparseArray
-- Description : Conversion from cartesian to solid harmonics
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows

module Spicy.Math.Spherical
  ()
where

import RIO

data CMultipoles = CMultipoles
  { monopoles :: !(Maybe CMonopole)
  , dipoles :: !(Maybe CDipole)
  , quadrupoles :: !(Maybe CQuadrupole)
  }

newtype CMonopole = CMonopole { c000 :: Double}
data CDipole = CDipole
  { c100 :: Double
  , c010 :: Double
  , c001 :: Double
  }
data CQuadrupole = CQuadrupole
  { c200 :: Double
  , c020 :: Double
  , c002 :: Double
  , c110 :: Double
  , c101 :: Double
  , c011 :: Double
  }