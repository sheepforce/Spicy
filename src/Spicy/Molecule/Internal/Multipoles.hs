-- |
-- Module      : Spicy.Molecule.Internal.Multipoles
-- Description : Operations and transformations of multipole data
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides operations for multipole data, including transformation to local frames and
-- conversion to point charges. For most of the definitions and conversion used see
-- [A Novel, Computationally Efficient Multipolar Model Employing Distributed Charges for Molecular Dynamics Simulations](https://pubs.acs.org/doi/10.1021/ct500511t).
--
-- \[
--
-- \]
module Spicy.Molecule.Internal.Multipoles () where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Massiv.Array as Massiv
import Optics
import RIO hiding (lens, view, (%~), (.~), (^.), (^?))
import qualified RIO.HashMap as HashMap
import Spicy.Common
import Spicy.Data
import Spicy.Molecule.Internal.Types

-- | The multipoles can be converted to an octahedral point charge model. Those charges might be
-- defined with respect to the principal axis of the spherical quadrupole tensor or in a local
-- reference frame of the molecule.
data OctahedralModel
  = SphericalRef
      { -- | Positive X
        qXu :: Double,
        -- | Negative X
        qXd :: Double,
        -- | Positive Y
        qYu :: Double,
        -- | Negative Y
        qYd :: Double,
        -- | Positive Z
        qZu :: Double,
        -- | Negative Z
        qZd :: Double
      }
  | LocalRef
      { -- | Positive X
        qXu :: Double,
        -- | Negative X
        qXd :: Double,
        -- | Positive Y
        qYu :: Double,
        -- | Negative Y
        qYd :: Double,
        -- | Positive Z
        qZu :: Double,
        -- | Negative Z
        qZd :: Double
      }

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qXu" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> qXu s) $ \s b -> s {qXu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qXd" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> qXd s) $ \s b -> s {qXd = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qYu" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> qYu s) $ \s b -> s {qYu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qYd" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> qYd s) $ \s b -> s {qYd = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qZu" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> qZu s) $ \s b -> s {qZu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qZd" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> qZd s) $ \s b -> s {qZd = b}

----------------------------------------------------------------------------------------------------

-- | Converts the atomic multipoles to the octahedral charge model in the coordinate system of the
-- spherical quadrupole tensor. All moments higher than quadrupole will be neglected.
toOctrahedralModel ::
  -- | The distance from the origin \(d_q\) in Angstrom. Default to \( \frac{1}{4} a_0 \)
  Maybe Double ->
  -- | The multipoles to convert.
  Multipoles ->
  -- | The octahedral point charge representation of the multipoles in the principla axis of the spherical quadrupole tensor.
  OctahedralModel
toOctrahedralModel dist mp =
  let d = fromMaybe (bohr2Angstrom 0.25) (bohr2Angstrom <$> dist)
      q00 = fromMaybe 0 $ mp ^? #monopole % _Just % #q00
      q11c = fromMaybe 0 $ mp ^? #dipole % _Just % #q11c
      q11s = fromMaybe 0 $ mp ^? #dipole % _Just % #q11s
      q20 = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q20
      q22c = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q22c
      q22s = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q22s
   in SphericalRef
        { qXu = (q00 / 6) + (q11c / (2 * d)) - (q20 / (6 * d ^ 2)) + (q22c / (2 * sqrt (3) * d ^ 2)),
          qYd = (q00 / 6) - (q11s / (2 * d)) - (q20 / (6 * d ^ 2)) - (q22c / (2 * sqrt (3) * d ^ 2)),
          qXd = (q00 / 6) - (q11c / (2 * d)) - (q20 / (6 * d ^ 2)) + (q22c / (2 * sqrt (3) * d ^ 2))
        }
