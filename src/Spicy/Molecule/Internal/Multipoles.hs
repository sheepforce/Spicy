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
module Spicy.Molecule.Internal.Multipoles
  ( OctahedralModel (..),
    toOctrahedralModel,
    octahedronToLocalFrame,
    sphericalToLocal,
  )
where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Massiv.Array as Massiv
import Optics
import RIO hiding (Vector, lens, view, (%~), (.~), (^.), (^?))
import qualified RIO.HashMap as HashMap
import Spicy.Common
import Spicy.Data
import Spicy.Molecule.Internal.Types hiding (S, coordinates)

data OctahedralValues = OctahedralValues
  { -- | Positive X
    qXu :: !Double,
    -- | Negative X
    qXd :: !Double,
    -- | Positive Y
    qYu :: !Double,
    -- | Negative Y
    qYd :: !Double,
    -- | Positive Z
    qZu :: !Double,
    -- | Negative Z
    qZd :: !Double
  }
  deriving (Eq, Show)

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qXu" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qXu s) $ \s b -> s {qXu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qXd" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qXd s) $ \s b -> s {qXd = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qYu" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qYu s) $ \s b -> s {qYu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qYd" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qYd s) $ \s b -> s {qYd = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qZu" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qZu s) $ \s b -> s {qZu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qZd" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qZd s) $ \s b -> s {qZd = b}

----------------------------------------------------------------------------------------------------

-- | The multipoles can be converted to an octahedral point charge model. Those charges might be
-- defined with respect to the principal axis of the spherical quadrupole tensor or in a local
-- reference frame of the molecule.
data OctahedralModel
  = SphericalRef
      { -- | The values of the point charges in the corners of the octahedron in the given axes system.
        values :: !OctahedralValues,
        -- | The local axes system.
        axesSystem :: !(Matrix S Double),
        -- | The \(d_q\) value in Angstrom.
        dq :: !Double,
        -- | The coordinates of the 6 point charges given in order (see 'OctahedralValues') as
        -- \(3 \times 6\) matrix with the coordinates given as column vectors of the matrix.
        -- | These are the global cartesian coordinates in angstrom (same as for 'Molecule').
        coordinates :: !(Maybe (Matrix S Double))
      }
  | LocalRef
      { -- | The values of the point charges in the corners of the octahedron in the given axes system.
        values :: !OctahedralValues,
        -- | The local axes system.
        axesSystem :: !(Matrix S Double),
        -- | The \(d_q\) value in Angstrom.
        dq :: !Double,
        -- | The coordinates of the 6 point charges given in order (see 'OctahedralValues') as
        -- \(3 \times 6\) matrix with the coordinates given as column vectors of the matrix.
        -- | These are the global cartesian coordinates in angstrom (same as for 'Molecule').
        coordinates :: !(Maybe (Matrix S Double))
      }

-- Lenses
instance (k ~ A_Lens, a ~ OctahedralValues, b ~ a) => LabelOptic "values" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> values s) $ \s b -> s {values = b}

instance (k ~ A_Lens, a ~ Matrix S Double, b ~ a) => LabelOptic "axesSystem" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> axesSystem s) $ \s b -> s {axesSystem = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "dq" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> dq s) $ \s b -> s {dq = b}

instance (k ~ A_Lens, a ~ Maybe (Matrix S Double), b ~ a) => LabelOptic "coordinates" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> coordinates s) $ \s b -> s {coordinates = b}

isSphericalRef :: OctahedralModel -> Bool
isSphericalRef ref = case ref of
  SphericalRef {} -> True
  _ -> False

----------------------------------------------------------------------------------------------------

-- | Converts the atomic multipoles to the octahedral charge model in the coordinate system of the
-- spherical quadrupole tensor. All moments higher than quadrupole will be neglected.
toOctrahedralModel ::
  -- | The distance from the origin \(d_q\) in Angstrom. Default to \( \frac{1}{4} a_0 \)
  Maybe Double ->
  -- | The multipoles to convert.
  Multipoles ->
  -- | The octahedral point charge representation of the multipoles in the principla axis of the
  -- spherical quadrupole tensor.
  OctahedralModel
toOctrahedralModel dist mp =
  let d = fromMaybe (bohr2Angstrom 0.25) (bohr2Angstrom <$> dist)
      q00 = fromMaybe 0 $ mp ^? #monopole % _Just % #q00
      q10 = fromMaybe 0 $ mp ^? #dipole % _Just % #q10
      q11c = fromMaybe 0 $ mp ^? #dipole % _Just % #q11c
      q11s = fromMaybe 0 $ mp ^? #dipole % _Just % #q11s
      q20 = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q20
      q22c = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q22c
      e2 = 2 :: Int
   in SphericalRef
        { axesSystem = Massiv.makeArray Seq (Sz (3 :. 3)) $ \(i :. j) -> if i == j then 1 else 0,
          dq = d,
          coordinates = Nothing,
          values =
            OctahedralValues
              { qXu = (q00 / 6) + (q11c / (2 * d)) - (q20 / (6 * d ^ e2)) + (q22c / (2 * sqrt (3) * d ^ e2)),
                qYd = (q00 / 6) - (q11s / (2 * d)) - (q20 / (6 * d ^ e2)) - (q22c / (2 * sqrt (3) * d ^ e2)),
                qXd = (q00 / 6) - (q11c / (2 * d)) - (q20 / (6 * d ^ e2)) + (q22c / (2 * sqrt (3) * d ^ e2)),
                qZu = (q00 / 6) + (q10 / (2 * d)) + (q20 / (3 * d ^ e2)),
                qYu = (q00 / 6) + (q11s / (2 * d)) - (q20 / (6 * d ^ e2)) - (q22c / (2 * sqrt (3) * d ^ e2)),
                qZd = (q00 / 6) - (q10 / (2 * d)) + (q20 / (3 * d ^ e2))
              }
        }

----------------------------------------------------------------------------------------------------

-- | Transformation of an octahedral charge model to the local reference frame defined by bonds.
-- Although in case of a non bonded atom the multipole orientation *should* not matter to much, this
-- function will fall back to define two bonds to arbitrarily chosen atoms. This should allow to
-- treat polarisation effects of monoatomic ions in solution or something like this.
octahedronToLocalFrame :: IntMap Atom -> BondMatrix -> IntMap OctahedralModel
octahedronToLocalFrame atoms bondMat =
  let
   in undefined

----------------------------------------------------------------------------------------------------

-- | Conversion of a vector from the spherical axis system to the local axis system.
--
-- The coordinate transfomation of a charge \(q\) with coordinates \( u \hat{\mathbf{p}}_x \),
-- \( v \hat{\mathbf{p}}_y \) and \( w \hat{\mathbf{p}}_z \), where \( \hat{\mathbf{p}}_x \),
-- \( \hat{\mathbf{p}}_y \) and \( \hat{\mathbf{p}}_z \) are the principal axes of the quadrupole
-- tensor, to the coordinates \( u' \hat{\mathbf{e}}_x \),- \( v' \hat{\mathbf{e}}_y \) and
-- \( w' \hat{\mathbf{e}}_z \) in the local atomic frame happens by the following equations:
-- \[ u' = u \hat{\mathbf{p}}_x \cdot \hat{\mathbf{e}}_x + v \hat{\mathbf{p}}_y \cdot \hat{\mathbf{e}}_x + w \hat{\mathbf{p}}_z \cdot \hat{\mathbf{e}}_x \]
-- \[ v' = u \hat{\mathbf{p}}_x \cdot \hat{\mathbf{e}}_y + v \hat{\mathbf{p}}_y \cdot \hat{\mathbf{e}}_y + w \hat{\mathbf{p}}_z \cdot \hat{\mathbf{e}}_x \]
-- \[ w' = u \hat{\mathbf{p}}_x \cdot \hat{\mathbf{e}}_z + v \hat{\mathbf{p}}_y \cdot \hat{\mathbf{e}}_z + w \hat{\mathbf{p}}_z \cdot \hat{\mathbf{e}}_z \]
-- With the local axes systems in \(3 \times 3 \) matrix form
-- \[ \mathbf{P} = \begin{bmatrix} \hat{\mathbf{p}}_x & \hat{\mathbf{p}}_y & \hat{\mathbf{p}}_z \end{bmatrix} \]
-- \[ \mathbf{E} = \begin{bmatrix} \hat{\mathbf{e}}_x & \hat{\mathbf{e}}_y & \hat{\mathbf{e}}_z \end{bmatrix} \]
-- and the coordinates of the point charges with column vectors of a matrix:
-- \[ \mathbf{C} = \begin{bmatrix}
--      d_q & -d_q & 0   &    0 &   0 &    0 \\
--        0 &    0 & d_q & -d_q &   0 &    0 \\
--        0 &    0 &   0 &    0 & d_q & -d_q
--    \end{bmatrix}
-- \]
-- the expression can be implemented with linear algebra:
-- \[ \mathbf{C}' = \mathbf{P}^T \mathbf{E} \mathbf{C} \]
sphericalToLocal ::
  (MonadThrow m) =>
  -- | The matrix \(\mathbf{E}\)
  Matrix S Double ->
  -- | Spherical representation of the octahedral charge model with the matrix \(\mathbf{P}\).
  OctahedralModel ->
  -- | The octahedral model rotated into the local reference frame with the matrix \(\mathbf{C}'\)
  -- as part of the result.
  m OctahedralModel
sphericalToLocal matE sphModel = do
  unless (isSphericalRef sphModel) . throwM . localExc $
    "Input reference system is not spherical."

  let matP = sphModel ^. #axesSystem
      matPT = computeAs S . setComp Seq . transpose $ matP
      dq = sphModel ^. #dq
  -- The 3 x 6 matrix with the octahedral coordinates. The coordinates appear as defined above
  -- and are column vectors.
  {- ORMOLU_DISABLE -}
  (matUVW :: Matrix S Double) <- Massiv.fromListsM Seq
    --  Xu,  Xd,  Yu,  Yd, Zu, Zd
    [ [ dq, -dq,   0,   0,  0,   0]
    , [  0,   0,  dq, -dq,  0,   0]
    , [  0,   0,   0,   0, dq, -dq]
    ]

  -- The matrix equation applied.
  newCoords <- matPT .><. matE >>= \pe -> (computeAs S pe) .><. matUVW

  return $
    LocalRef
      { values = sphModel ^. #values,
        axesSystem = matE,
        dq = dq,
        coordinates = Just . computeS $ newCoords
      }
  where
    vecsToMat :: Vector D Double -> Matrix D Double
    vecsToMat v = expandOuter (Sz 1) const . (computeS :: Vector D Double -> Vector S Double) $ v
    localExc = MolLogicException "sphericalToLocal"
