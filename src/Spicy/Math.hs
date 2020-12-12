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
    swapRows,
    luDecomp,
    det,

    -- * Conversion

    -- ** Matrix-Vector
    ltMat2Square,
  )
where

import Data.Massiv.Array as Massiv
import Data.Massiv.Core.Operations ()
import Math.NumberTheory.Roots
import RIO hiding (Vector)
import Spicy.Common

data MathException = MathException String deriving (Show)

instance Exception MathException

{-
####################################################################################################
-}

-- $linear

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

-- | LU decomposition of square matrices by Doolittle algorithm.
luDecomp ::
  ( Ord e,
    Show e,
    Storable e,
    Fractional e,
    Numeric r e,
    Show (Matrix r e),
    Source (R r) Ix1 e,
    Mutable r Ix2 e,
    InnerSlice r Ix2 e,
    Extract r Ix2 e,
    Source (R r) Ix2 e,
    MonadThrow m,
    MonadUnliftIO m,
    PrimMonad m
  ) =>
  Matrix r e ->
  m ((Matrix r e, Int), Matrix r e, Matrix r e)
luDecomp a = do
  let Sz (m :. n) = size a
  unless (m == n) . throwM . MathException $ "LU decomposition works for square matrices only"

  -- Initialise l and p as identity matrix and A(0) = A
  let p = computeP . identityMatrix $ Sz m
      lAcc = makeArray Par (Sz $ m :. 0) (const 0)

  go 0 (p, 0) lAcc a
  where
    go ::
      ( Show (Matrix r e),
        Show e,
        Ord e,
        Storable e,
        Fractional e,
        Numeric r e,
        Source (R r) Ix1 e,
        Mutable r Ix2 e,
        InnerSlice r Ix2 e,
        Extract r Ix2 e,
        Source (R r) Ix2 e,
        MonadThrow m,
        MonadUnliftIO m,
        PrimMonad m
      ) =>
      Int ->
      (Matrix r e, Int) ->
      Matrix r e ->
      Matrix r e ->
      m ((Matrix r e, Int), Matrix r e, Matrix r e)
    go n (p', s) lAcc' a'
      | n < 0 = throwM . MathException $ "n < 0, Must start with 0."
      | n == m - 1 = do
        let lastCol = makeArray @S Par (Sz $ m :. 1) (\(r :. _) -> if r == m -1 then 1 else 0)
        lAcc <- computeP @S <$> appendM 1 lAcc' lastCol
        let l = computeP . Massiv.imap (\(r :. c) e -> if r == c then 1 else negate e) $ lAcc
        return ((p', s), l, a')
      | n >= 0 && n < m - 1 = do
        aColN <- computeAs S . Massiv.map abs <$> (a' <!? n)
        minAn <- minimumM aColN
        k <-
          let elFold acc@(_, accE) ix e = if e > accE && ix > n then (ix, e) else acc
              neutral = (0, minAn)
              chFold acc@(_, accE) v@(ix, e) = if e > accE && ix > n then v else acc
           in fst <$> ifoldlP elFold neutral chFold neutral aColN
        pn <- swapRows n k p'
        aPivot <- swapRows n k a'
        let ln =
              makeArray @S Par (Sz $ m :. m) $ \(r :. c) ->
                if r == c
                  then 1
                  else
                    if c == n
                      then - (elemL r c aPivot)
                      else 0

        an <- computeP <$> (ln .><. aPivot)

        lAcc <- do
          lAccPivot <- swapRows n k lAcc'
          rowNln <- ln <!? n
          computeP <$> appendM 1 lAccPivot (expandInner (Sz 1) const rowNln)

        -- traceM $ ">>>>>>>>>> " <> tShow n <> "<<<<<<<<<<<<<<"
        -- traceM $ "A(n-1) = " <> tShow a'
        -- traceM $ "P(n) = " <> tShow pn
        -- traceM $ "L(n) = " <> tShow ln
        -- traceM $ "A(p) = " <> tShow aPivot
        -- traceM $ "A(n) = " <> tShow an

        go (n + 1) (pn, if n == k then s else s + 1) lAcc an
      | otherwise = throwM . MathException $ "Number of iterations exceeded."
      where
        Sz (m :. _) = size a

        elemL :: (Fractional e, Manifest r Ix2 e) => Int -> Int -> Matrix r e -> e
        elemL r c matA'
          | r >= c = (matA' ! r :. c) / (matA' ! c :. c)
          | otherwise = 0

-- | Calculates the determinant of a square matrix from an LUP decomposition.
det ::
  ( RealFrac e,
    Load r Ix2 e,
    Manifest r Ix2 e,
    MonadThrow m
  ) =>
  ((Matrix r e, Int), Matrix r e, Matrix r e) ->
  m e
det ((p, s), l, u) = do
  -- Check if all matrices have the same size.
  let Sz (mP :. nP) = size p
      Sz (mL :. nL) = size l
      Sz (mU :. nU) = size u
      ixVec = makeArray @U Par (Sz mP) $ \i -> i :. i
      lMainDiagProd = Massiv.product . Massiv.map (l !) $ ixVec
      uMainDiagProd = Massiv.product . Massiv.map (u !) $ ixVec
  unless (RIO.all (== mP) [nP, mL, nL, mU, nU]) . throwM . MathException $ "Matrix mismatch size"

  return $ (-1) ^ s * lMainDiagProd * uMainDiagProd

-- | The smallest value close to zero not yet to accept as zero.
ε :: Fractional e => e
ε = 1e-15

-- | Swap rows \(k\) and \(l\) of a matrix and \(m \times \n\) matrix with \(m, n \geq 0\) and
-- \(0 \leq k, l \leq m, n\).
swapRows ::
  (Mutable r Ix2 e, MonadUnliftIO m, PrimMonad m, MonadThrow m) =>
  -- | Row index
  Int ->
  Int ->
  Matrix r e ->
  m (Matrix r e)
swapRows k l a =
  backpermuteM (size a) (\ix@(m :. n) -> if m == k then l :. n else if m == l then k :. n else ix) a

{-
-- |
luDecomp :: (MonadThrow m, PrimMonad m, MonadUnliftIO m) => Matrix S Double -> m (Vector S Double, Matrix S Double)
luDecomp matA = do
  let Sz (m :. n) = size matA
  unless (m == n) . throw . MathException $ "LU decomposition is only implemented for square matrices."

  let π = makeArrayLinear @S Par (Sz n) fromIntegral

  (π, a) <- go 0 (undefined :: Vector S Double) matA
  return (π, a)
  where
    go ::
      ( -- element
        Fractional e,
        Ord e,
        Storable e,
        -- general
        Numeric r e,
        -- vector
        Source (R r) Ix1 e,
        Mutable r Ix1 e,
        -- matrix
        InnerSlice r Ix2 e,
        Mutable r Ix2 e,
        Extract r Ix2 e,
        Numeric (R r) e,
        Mutable (R r) Ix2 e,
        -- monad
        MonadThrow m,
        PrimMonad m,
        MonadUnliftIO m
      ) =>
      Int ->
      Vector r e ->
      Matrix r e ->
      m (Vector r e, Matrix r e)
    go k π' a'
      | k >= 0 && k < n = do
        -- Find the largest value in the current column of a' (a__k) and the row index of it (k').
        a__k <- a' <!? k
        minAik <- minimumM a__k
        let elFold acc@(_, p) i a_ik = if i >= k && a_ik > p then (i, a_ik) else acc
            chFold acc@(_, p) (i, a_ik) = if a_ik > p then (i, a_ik) else acc
            neutral = (k, minAik)
        (k', p) <- ifoldlP elFold neutral chFold neutral a__k

        -- Safety check that p is not to small.
        unless (p >= ε) . throwM . MathException $ "matrix is singular"

        -- Update the permutation matrix π' by exchanging π'_k with π'_k'
        π <- backpermuteM (Sz n) (\ix -> if ix == k then k' else if ix == k' then k else ix) π'

        -- Swap rows in matrix a.
        aPivot <- swapRows k k' a'

        -- Calculate the Schur complement for the lower right part of the matrix.
        v <- extractFromToM (k + 1 :. k) (k + 1 :. n - 1) aPivot
        w <- extractFromToM (k :. k + 1) (k :. n - 1) aPivot
        a_kk <- aPivot !? (k :. k)
        vw <- computeP @S . Massiv.map (/ a_kk) <$> (v .><. w)
        aLR <- computeP @S <$> extractFromToM (k + 1 :. k + 1) (n - 1 :. n - 1) aPivot
        schur <- aLR .-. vw

        -- Change to mutations to avoid too many operations.
        aPivotM <- loadArrayS @S aPivot

        -- Generate column indices for current k.
        let nElems = n - k
            columInds = makeArrayLinear @U Par (Sz nElems) (\c -> k + 1 + c)
            blockIdx = makeArray @U Par (Sz $ nElems :. nElems) (\(r :. c) -> k + 1 + r :. k + 1 + c)

        -- Monadically and with mutation change the column of a to new values.
        _ <-
          mapIO @S
            ( \i -> do
                let ik = i :. k
                modifyM aPivotM (\a_ik -> return $ a_ik / a_kk) ik
            )
            columInds

        -- Update the lower right part of the matrix with the Schur complement
        _ <-
          mapIO @S
            ( \(i :. j) -> do
                let schurIdx = i - k - 1 :. j - k - 1
                a_ij <- schur !? schurIdx
                writeM aPivotM schurIdx a_ij
            )
            blockIdx

        a <- computeP <$> freezeS aPivotM
        go (k + 1) π a
      | k == n = return (π', a')
      | otherwise = throwM . MathException $ "Wrong iteration number and index given."
      where
        Sz (n :. _) = size matA
-}

{-
####################################################################################################
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
