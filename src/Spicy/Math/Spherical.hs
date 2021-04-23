-- |
-- Module      : Spicy.Math.Spherical
-- Description : Conversion from cartesian to solid harmonics.
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
module Spicy.Math.Spherical
  ( CMultipoles (..),
    CMonopole (..),
    CDipole (..),
    CQuadrupole (..),
    cartesianToSpherical,
  )
where

import RIO
import Spicy.Molecule.Internal.Types

-- | Cartesian representation for multipoles up to quadrupoles.
data CMultipoles = CMultipoles
  { monopoles :: !(Maybe CMonopole),
    dipoles :: !(Maybe CDipole),
    quadrupoles :: !(Maybe CQuadrupole)
  }
  deriving (Show)

-- | A cartesian monopole, i.e. a partial charge.
newtype CMonopole = CMonopole {c000 :: Double} deriving (Show)

-- | A cartesian dipole. Field names refer to the cartesian quantum numbers.
data CDipole = CDipole
  { c100 :: Double,
    c010 :: Double,
    c001 :: Double
  }
  deriving (Show)

-- | A cartesian quadrupole. Field names refer to the cartesian quantum numbers.
data CQuadrupole = CQuadrupole
  { c200 :: Double,
    c020 :: Double,
    c002 :: Double,
    c110 :: Double,
    c101 :: Double,
    c011 :: Double
  }
  deriving (Show)

-- | Transforms multipoles from a cartesian (used by XTB)
-- to a spherical representation (used by other Spicy functions).
-- Currently supports multipoles up to quadrupoles, as xtb does not
-- calculate higher multipoles.
cartesianToSpherical :: CMultipoles -> Multipoles
cartesianToSpherical CMultipoles {..} =
  Multipoles
    { monopole = case monopoles of
        Nothing -> Nothing
        Just CMonopole {..} -> Just $ Monopole c000,
      dipole = case dipoles of
        Nothing -> Nothing
        Just CDipole {..} ->
          Just $
            Dipole
              { q10 = c001,
                q11c = c100,
                q11s = c010
              },
      quadrupole = case quadrupoles of
        Nothing -> Nothing
        Just CQuadrupole {..} ->
          Just $
            Quadrupole
              { q20 = c002,
                q21c = (2 / sqrt 3) * c101,
                q21s = (2 / sqrt 3) * c011,
                q22c = recip (sqrt 3) * (c200 - c020),
                q22s = (2 / sqrt 3) * c110
              },
      octopole = Nothing,
      hexadecapole = Nothing
    }
