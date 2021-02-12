-- |
-- Module      : Spicy.Math.SparseArray
-- Description : Conversion from cartesian to solid harmonics
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows

module Spicy.Math.Spherical
  ( CMultipoles(..),
    CMonopole(..),
    CDipole(..),
    CQuadrupole(..),
    cartesianToSpherical
  )
where

import RIO
import Spicy.Molecule.Internal.Types

data CMultipoles = CMultipoles
  { monopoles :: !(Maybe CMonopole)
  , dipoles :: !(Maybe CDipole)
  , quadrupoles :: !(Maybe CQuadrupole)
  } deriving Show

newtype CMonopole = CMonopole { c000 :: Double } deriving Show
data CDipole = CDipole
  { c100 :: Double
  , c010 :: Double
  , c001 :: Double
  } deriving Show
data CQuadrupole = CQuadrupole
  { c200 :: Double
  , c020 :: Double
  , c002 :: Double
  , c110 :: Double
  , c101 :: Double
  , c011 :: Double
  } deriving Show

-- For the moment, this supports multipoles up to quadrupoles
cartesianToSpherical :: CMultipoles -> Multipoles
cartesianToSpherical CMultipoles{..} = Multipoles
  { monopole = case monopoles of
      Nothing -> Nothing
      Just CMonopole{..} -> Just $ Monopole c000
  , dipole = case dipoles of
      Nothing -> Nothing
      Just CDipole{..} -> Just $ Dipole
        { q10 = c001
        , q11c = c100
        , q11s = c010 }
  , quadrupole = case quadrupoles of
      Nothing -> Nothing
      Just CQuadrupole{..} -> Just $ Quadrupole
        { q20 = c002
        , q21c = (2/sqrt 3) * c101
        , q21s = (2/sqrt 3) * c011
        , q22c = recip (sqrt 3) * (c200 - c020)
        , q22s = (2/sqrt 3) * c110 }
  , octopole = Nothing
  , hexadecapole = Nothing
  }

{-

It's possible to do to this translation for all indices.
this unfinished code is taken from old Beluga.

cartesianExpansion :: QSpherical -> [(Double,QCartesian)]
cartesianExpansion QS{..} = (n,cartesians)
  where
    n = sqrt (fromIntegral (2 * fac (l + abs m) * fac (l - abs m)) / fromIntegral ((2::Int) ^ delta 0 m)) / fromIntegral (2 ^ abs m * fac l)
    vm = if m >= 0 then 0 else 1%2
    c t u v = (-1)**fromRatio (t%1 +v-vm) * 0.25^t * fromIntegral (binom l t * binom (l - t) (abs m + t) * binom t u * binom (abs m) (2*numerator v `div` denominator v))
    fromRatio i = (fromIntegral.numerator) i / (fromIntegral.denominator) i
    makeXYZ t u v = (2*t+abs m-floor (2*(u%1+v)),floor (2*(u%1+v)),l-2*t-abs m)
    cartesians = do
      t <- [0..floor ((l - abs m) % 2)]
      u <- [0..t]
      v <- [vm..floor ((fromIntegral.abs) m / 2 - vm)%1 + vm]
      return (c t u v,CG ps o (makeXYZ t u v))

data QCartesian = QC
  { x :: Int
  , y :: Int
  , z :: Int }

data QSpherical = QS
  { l :: Int
  , m :: Int }
-}
