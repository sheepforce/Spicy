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
  ( MathException (..),
    ε,

    -- * Linear Algebra
    -- $linear
    -- $linearVector
    distance,
    magnitude,
    angle,
    cross3,
    swapRows,
    lupDecomp,
    lupDecomp',
    det,
    LUP (..),

    -- * Conversion

    -- ** Matrix-Vector
    ltMat2Square,

    -- * Mutable Interface
    -- $mutableHelper
    swapRowsM,
    findPivotRow,
  )
where

import Data.Massiv.Array as Massiv
import Data.Massiv.Core.Operations ()
import Math.NumberTheory.Roots
import RIO hiding (Vector)
import RIO.Partial (succ)
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

-- | LU decomposition of square matrices by Doolittle algorithm.
lupDecomp ::
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
lupDecomp a = do
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

----------------------------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------------------------

-- | Results of a LUP decomposition. \(\mathbf{L}\) and \(\mathbf{U}\) are stored together, while
-- the permutation matrix is given as memory efficient vector representation in the first place.
-- Functions to expand the memory efficient values to their full representations are provided
-- alongside.
data LUP r e = LUP
  { lu :: Matrix r e,
    π :: Vector r Int,
    s :: Int,
    getL :: LUP r e -> Matrix r e,
    getU :: LUP r e -> Matrix r e,
    getP :: LUP r e -> Matrix r e
  }

-- | LUP decomposition of a square matrix.
lupDecomp' ::
  ( Ord e,
    Fractional e,
    Numeric r e,
    -- vector
    Mutable r Ix1 Int,
    -- matrix
    Mutable r Ix2 e,
    -- monads
    MonadThrow m,
    PrimMonad m,
    MonadUnliftIO m,
    PrimState m ~ RealWorld
  ) =>
  Matrix r e ->
  m (LUP r e)
lupDecomp' matA
  | m /= n = throwM . MathException $ "LUP decomposition requires a square matrix"
  | otherwise = do
    -- Initialise values for iterations.
    aIn <- thaw matA
    πIn <- thaw $ makeArrayLinear Par (Sz n) id

    -- Enter iterations.
    (luM, πM, s) <- go 0 aIn πIn 0

    -- Freeze results.
    lu <- freeze Par luM
    π <- freeze Par πM

    -- Return results.
    return $
      LUP
        { lu = lu,
          π = π,
          s = s,
          getL = getLF,
          getU = getUF,
          getP = getPF
        }
  where
    -- Size of the input matrix.
    Sz (m :. n) = size matA

    -- Function to obtain L from the combined LU matrix.
    getLF =
      computeP
        . Massiv.imap
          ( \(r :. c) e -> case r `compare` c of
              LT -> 0
              EQ -> 1
              GT -> e
          )
        . lu

    -- Function to obtain U from the combined LU matrix.
    getUF =
      computeP
        . Massiv.imap
          ( \(r :. c) e -> case r `compare` c of
              LT -> e
              EQ -> e
              GT -> 0
          )
        . lu

    -- Function to obtain a representation of P from π.
    getPF lup =
      makeArray
        Par
        (Sz $ m :. m)
        ( \(r :. c) ->
            let piC = (π lup) ! r
             in if c == piC then 1 else 0
        )

    -- Iterative decomposition solver.
    go ::
      ( Ord e,
        Fractional e,
        Numeric r e,
        -- vector
        Mutable r Ix1 Int,
        -- matrix
        Mutable r Ix2 e,
        -- monads
        PrimMonad m,
        MonadThrow m,
        MonadUnliftIO m
      ) =>
      Int ->
      MArray (PrimState m) r Ix2 e ->
      MArray (PrimState m) r Ix1 Int ->
      Int ->
      m (MArray (PrimState m) r Ix2 e, MArray (PrimState m) r Ix1 Int, Int)
    go k a π s
      | k >= 0 && k < n - 1 = do
        {-
        -- DEBUG
        traceM $ ">>>>>>>>>>>>>>> " <> tShow k <> " <<<<<<<<<<<<<<<<<"
        matA <- freeze Par a
        traceM $ "A(n) = " <> tShow matA
        -}

        -- Generate index vectors for current step.
        let nElems = n - k - 1
            -- Column k below the main diagonal.
            ixV = makeArrayLinear @U Par (Sz $ nElems) (\i -> i + 1 + k)
            -- The remaining lower right matrix below k,k.
            ixA' = makeArray @U Par (Sz $ nElems :. nElems) (\(r :. c) -> (r + k + 1 :. c + k + 1))

        -- Find the row which to use for the pivot.
        (rowPivot :. _, pivotElem) <- findPivotRow k k a
        unless (pivotElem >= ε) . throwM . MathException $ "Singular matrix in LUP decomposition"

        -- Swap the rows and update the permutation matrix and its signature.
        unless (rowPivot == k) $ swapRowsM k rowPivot a
        _ <- swapM π k rowPivot
        let s' = if rowPivot /= k then succ s else s

        {-
        -- DEBUG
        traceM $ "Pivot:"
        traceM $ "  Row: " <> tShow rowPivot
        traceM $ "  Element: " <> tShow pivotElem
        vecP <- freeze Par π
        traceM $ "  π: " <> tShow vecP
        matAP <- freeze Par a
        traceM $ "A(p) = " <> tShow matAP
        -}

        -- Perform the column update.
        a_kk <- readM a (k :. k)
        forIO_ ixV (\i -> modify a (\a_ik -> return $ a_ik / a_kk) (i :. k))

        {-
        -- DEBUG
        matAPV <- freeze Par a
        traceM $ "Column update: " <> tShow matAPV
        -}

        -- Perform the submatrix update.
        forIO_
          ixA'
          ( \ix@(i :. j) -> do
              a_ik <- readM a (i :. k)
              a_kj <- readM a (k :. j)
              modify a (\a_ij -> return $ a_ij - (a_ik * a_kj)) ix
          )

        {-
        -- DEBUG
        matAPVA <- freeze Par a
        traceM $ "Submatrix update:" <> tShow matAPVA
        -}

        go (succ k) a π (s')
      | k == n - 1 = return (a, π, s)
      | otherwise = throwM . MathException $ "Wrong iteration in LUP decomposition"

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

{-
####################################################################################################
-}

-- $helper

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
====================================================================================================
-}

-- $mutable

-- | Swap rows of a mutable matrix in place.
swapRowsM ::
  ( Mutable r Ix2 e,
    MonadThrow m,
    PrimMonad m,
    MonadUnliftIO m
  ) =>
  Int ->
  Int ->
  MArray (PrimState m) r Ix2 e ->
  m ()
swapRowsM i j a = do
  forIO_ ixC (\c -> swapM_ a (i :. c) (j :. c))
  where
    Sz (_ :. n) = msize a
    ixC = makeArrayLinear @U Par (Sz n) id

----------------------------------------------------------------------------------------------------

-- | Given a matrix, a column in which to look and a starting index of a row, look for the element
-- (and its index in the matrix) with the largest absolute numerical value, only taking into account
-- elements including and below the row index.
findPivotRow ::
  ( Num e,
    Ord e,
    Mutable r Ix2 e,
    MonadThrow m,
    PrimMonad m,
    MonadUnliftIO m
  ) =>
  -- | Column.
  Int ->
  -- | Row index from which to start.
  Int ->
  -- | Array in which to search.
  MArray (PrimState m) r Ix2 e ->
  m (Ix2, e)
findPivotRow cIx sIx a = do
  neutralVal <- readM a (cIx :. sIx)
  let neutralElem = (cIx :. sIx, neutralVal)
  ifoldlIO elFoldF neutralElem chFoldF neutralElem ixV
  where
    -- Size of the array.
    Sz (m :. _) = msize a

    -- Vector of the element indices which to check.
    ixV = makeArrayLinear @U Par (Sz $ m - sIx) (\rIx -> sIx + rIx :. cIx)

    -- Element folding function.
    elFoldF acc@(_, accE) _ ix = do
      matEl <- abs <$> readM a ix
      return $ if matEl > accE then (ix, matEl) else acc

    -- Chunk folding function.
    chFoldF acc@(_, accE) val@(_, valE) = return $ if valE > accE then val else acc
