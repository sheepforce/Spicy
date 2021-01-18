{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Spicy.Math
-- Description : Basic mathematical operations
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module defines basic algebraic operations used throughout the program. Numerical heavy and most
-- other operations are implemented using Accelerate, to provide parallel operations. Note that all
-- 'VS.runQ' provided functions must be typed without typeclasses but by concrete types.
--
-- The operations here accept some insecurities (like not checking if both vectors of a dot product
-- have equal lenght) and trust the caller.
module Spicy.Math
  ( -- * Linear Algebra
    -- $linear
    -- $linearVector
    distance,
    magnitude,
    angle,
    cross3,
    lu,

    -- * Conversion

    -- ** Massiv-HMatrix
    vecM2H,
    vecH2M,
    matM2H,
    matH2M,

    -- ** Matrix-Vector
    ltMat2Square,
  )
where

import Data.Massiv.Array as Massiv
import Data.Massiv.Array.Manifest.Vector (ARepr, VRepr, fromVector', toVector)
import Data.Massiv.Core.Operations ()
import qualified Data.Vector.Generic as GenericVec
import Math.NumberTheory.Roots
import qualified Numeric.LinearAlgebra as LA
import RIO hiding (Vector, (%~))
import Spicy.Common

data MathException = MathException String deriving (Show)

instance Exception MathException

----------------------------------------------------------------------------------------------------

-- | The smallerst numerical value to accept as non-zero
ε :: Fractional e => e
ε = 1.0e-15

{-
####################################################################################################
-}

-- $linear
-- Implements useful linear algebra functions.

{-
====================================================================================================
-}

-- $linearVector

----------------------------------------------------------------------------------------------------

-- | Calculate the distance between two vectors of same size.
distance :: (Source r Ix1 a, NumericFloat r a, MonadThrow m) => Vector r a -> Vector r a -> m a
distance a b = do
  diffVec <- a .-. b
  let dist = sqrt . Massiv.sum . Massiv.map (** 2) $ diffVec
  return dist

----------------------------------------------------------------------------------------------------

-- | The magnitude/length of a vector.
magnitude :: (Floating e, Source r Ix1 e) => Vector r e -> e
magnitude a = sqrt . Massiv.sum . Massiv.map (^ (2 :: Int)) $ a

----------------------------------------------------------------------------------------------------

-- | Calculates the angle between two vectors of the same size in radian.
angle :: (Floating e, Numeric r e, Source r Ix1 e, MonadThrow m) => Vector r e -> Vector r e -> m e
angle a b = do
  numerator <- a `dotM` b
  return . acos $ numerator / (magnitude a * magnitude b)

----------------------------------------------------------------------------------------------------

-- | Cross product of two 3-dimensional vectors.
cross3 ::
  (Numeric r e, Manifest r Ix1 e, Mutable r' Ix1 e, MonadThrow m) =>
  Vector r e ->
  Vector r e ->
  m (Vector r' e)
cross3 a b = do
  let Sz nA = size a
      Sz nB = size b

  unless (nA == 3 && nB == 3) . throwM . MathException $ "Input vectors must both be of size 3"

  a1 <- a !? 0
  a2 <- a !? 1
  a3 <- a !? 2

  b1 <- b !? 0
  b2 <- b !? 1
  b3 <- b !? 2

  let c1 = a2 * b3 - a3 * b2
      c2 = a3 * b1 - a1 * b3
      c3 = a1 * b2 - a2 * b1
  return . Massiv.fromList Seq $ [c1, c2, c3]

----------------------------------------------------------------------------------------------------

-- | LUP decomposition backed up by LAPACK.
lu :: (Mutable r Ix2 e, Mutable r Ix1 e, LA.Field e) => Matrix r e -> (Matrix r e, Matrix r e, Matrix r e, e)
lu mat = (\(l, u, p, s) -> (matH2M l, matH2M u, matH2M p, s)) . LA.lu . matM2H $ mat

{-
####################################################################################################
-}

-- | Conversion of a massiv vector to the vector type used by HMatrix.
vecM2H ::
  ( Mutable (ARepr v) Ix1 e,
    GenericVec.Vector v e,
    Manifest r Ix1 e,
    VRepr (ARepr v) ~ v
  ) =>
  Vector r e ->
  v e
vecM2H vec = toVector vec

----------------------------------------------------------------------------------------------------

-- | Conversion of a vector as used by HMatrix to the Massiv vectors.
vecH2M ::
  ( GenericVec.Vector v a,
    Mutable (ARepr v) Ix1 a,
    Mutable r Ix1 a,
    Typeable v
  ) =>
  v a ->
  Array r Int a
vecH2M vec =
  let sz = GenericVec.length vec
   in fromVector' Par (Sz sz) vec

----------------------------------------------------------------------------------------------------

-- | Conversion of a matrix as used by Massiv to HMatrix.
matM2H :: (Storable e, Manifest r Ix2 e) => Matrix r e -> LA.Matrix e
matM2H mat =
  let Sz (m :. _) = size mat
   in LA.reshape m . toVector $ mat

----------------------------------------------------------------------------------------------------

-- | Conversion from HMatrix matrix to Massiv matrix.
matH2M :: (Mutable r Ix1 e, LA.Element e) => LA.Matrix e -> Matrix r e
matH2M mat =
  let nRows = LA.rows mat
      nCols = LA.cols mat
   in resize' (Sz $ nRows :. nCols) . fromVector' Par (Sz $ nRows * nCols) . LA.flatten $ mat

{-
====================================================================================================
-}

-- | Convert a lower triangular matrix represented as a 'VS.Vector' in row major order (columns index
-- changes first) back to the square matrix. The main diagonal is supposed to be included. The function
-- fails if the number of elements in the vector is not suitable to represent the lower triangular part
-- of a square matrix.
--
-- Given a square matrix of dimension \(n \times n\), the number of elements of its lower triangular
-- part (including main diagonal) would be:
--
-- \[ n_\mathrm{t} = \frac{n^2 + n}{2} \]
--
-- Solving this equation for \( n \) with a given \( n_t \) gives:
--
-- \[ n_{1,2} = \frac{1}{2} \left( \pm \sqrt{8 n_t + 1} - 1 \right) \]
--
-- for which only the positive solution is meaningful for us. The index of the the \(n \times n\)
-- matrix \( \mathbf{S}_{ij} \) (with \( i \) being the row index and  \( j \) being the column index)
-- can be linearised to map in the lower triangular matrix, which is represented as a vector \( \mathbf{T}_{k} \) as:
--
-- \[
-- k = \begin{cases}
--   \sum \limits_{m = 0}^i m + j, & i \geq j \\
--   \sum \limits_{m = 0}^j m + i, & \text{otherwise}
-- \end{cases}
-- \]
--
-- This index transformation can be used to reexpand the lower triangular matrix in vector form in row
-- major order back to the full symmetric square matrix.
ltMat2Square ::
  (MonadThrow m, Manifest r Ix1 a, Mutable r Ix1 a) => Vector r a -> m (Matrix r a)
ltMat2Square ltVec = do
  let -- Elements in the lower triangular part of the matrix (including main diagonal).
      nElemLT = Massiv.elemsCount ltVec

  -- sqrt(8 * n_t + 1)
  rootExpr <- case exactSquareRoot (8 * nElemLT + 1) of
    Just x -> return x
    Nothing -> throwM . localExc $ ("Cannot take the square root of " <> show (8 * nElemLT + 1))

  -- (sqrt(8 * n_t + 1) + 1) / 2
  let nElemSquare = (rootExpr - 1) `div` 2

  -- Expanded lower triangular square matrix with all elements annotated with their future index
  -- in the matrix.
  annoLT <-
    fmap snd
      <$> traverse
        ( \((r, c), v) -> case v of
            Just val -> return ((r, c), val)
            Nothing -> throwM . localExc $ ""
        )
        [ ((r, c), ltVec Massiv.!? lineariseIndex (r, c))
          | r <- [0 .. nElemSquare - 1],
            c <- [0 .. nElemSquare - 1]
        ]

  -- Square matrix in Massiv's representation.
  Massiv.resizeM (Sz (nElemSquare :. nElemSquare)) . Massiv.fromList Par $ annoLT
  where
    localExc = DataStructureException "ltMat2Square"

    -- Given the indices of the square matrix (0 based), convert to index position in the linearised
    -- lower triangular matrix.
    lineariseIndex :: (Int, Int) -> Int
    lineariseIndex (ixR, ixC) =
      if ixR >= ixC
        then -- We are in the lower triangular matrix part (including the main diagonal).
          RIO.sum [0 .. ixR] + ixC
        else -- We are in the upper triangular part (excluding the main diagonal) of the square matrix.
          RIO.sum [0 .. ixC] + ixR
