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

    -- * Conversion

    -- ** Matrix-Vector
    ltMat2Square,
  )
where

import Data.Massiv.Array as Massiv
import Data.Massiv.Core.Operations()
import Math.NumberTheory.Roots
import RIO hiding (Vector)
import Spicy.Common

{-
####################################################################################################
-}

-- $linear

-- |
-- This aims at implementing an interface as close as possible to the one of the [Linear
-- package](https://hackage.haskell.org/package/linear) for the 'Massiv' types.

{-
====================================================================================================
-}

-- $linearVector

----------------------------------------------------------------------------------------------------
{-
Calculate the distance between two vectors of same size
-}
distance :: (Source r Ix1 a, NumericFloat r a, MonadThrow m) => Vector r a -> Vector r a -> m a
distance a b = do
  diffVec <- a .-. b
  let dist = sqrt . Massiv.sum . Massiv.map (** 2) $ diffVec
  return dist

{-
####################################################################################################
-}

-- |
-- Convert a lower triangular matrix represented as a 'VS.Vector' in row major order (columns index
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
  (MonadThrow m, Manifest r Ix1 a, Mutable r Ix1 a {- ,Resize r Ix1-}) => Vector r a -> m (Matrix r a)
ltMat2Square ltVec = do
  let -- Elements in the lower triangular part of the matrix (including main diagonal).
      nElemLT = Massiv.elemsCount ltVec

  -- sqrt(8 * n_t + 1)
  rootExpr <- case exactSquareRoot (8 * nElemLT + 1) of
    Just x -> return x
    Nothing ->
      throwM $
        DataStructureException "ltMat2Square" $
          "Cannot take the square root of "
            <> show
              (8 * nElemLT + 1)

  -- (sqrt(8 * n_t + 1) + 1) / 2
  let nElemSquare = (rootExpr - 1) `div` 2

  -- Expanded lower triangular square matrix with all elements annotated with their future index
  -- in the matrix.
  annoLT <-
    fmap snd
      <$> traverse
        ( \((r, c), v) -> case v of
            Just val -> return ((r, c), val)
            Nothing -> throwM $ DataStructureException "ltMat2Square" ""
        )
        [ ((r, c), ltVec Massiv.!? lineariseIndex (r, c))
          | r <- [0 .. nElemSquare - 1],
            c <- [0 .. nElemSquare - 1]
        ]

  -- Square matrix in Massiv's representation.
  Massiv.resizeM (Sz (nElemSquare :. nElemSquare)) . Massiv.fromList Par $ annoLT
  where
    -- Given the indices of the square matrix (0 based), convert to index position in the linearised
    -- lower triangular matrix.
    lineariseIndex :: (Int, Int) -> Int
    lineariseIndex (ixR, ixC) =
      if ixR >= ixC
        then -- We are in the lower triangular matrix part (including the main diagonal).
          RIO.sum [0 .. ixR] + ixC
        else -- We are in the upper triangular part (excluding the main diagonal) of the square matrix.
          RIO.sum [0 .. ixC] + ixR
