-- |
-- Module      : Spicy.Math.Square
-- Description : The Square type for typesafe squareity
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- A Type to encode a "squareness" property, together with
-- smart constructors that guarantee that property.
module Spicy.Math.Square
  ( Square,
    getSquare,
    squareMatrix,
    squareSz,
  )
where

import Data.Massiv.Array
import RIO
import Spicy.Common

-- | Newtype wrapper that serves as a guarantee that the
-- wrapped value is square. The UnsafeSquare constructor
-- is not meant to be exported. Values of type Square a are
-- constructed by dedicated safe functions.
newtype Square a = UnsafeSquare {getSquare :: a}

-- | Guarantee that a matrix is square. Will throw a 'DataStructureException'
-- if it is not.
squareMatrix :: (Load r Ix2 e, MonadThrow m) => Matrix r e -> m (Square (Matrix r e))
squareMatrix matrix =
  let Sz (n :. m) = size matrix
   in if n == m
        then return . UnsafeSquare $ matrix
        else throwM $ DataStructureException "squareMatrix" "Matrix not square!"

-- | Guarantee that a 2-dimensional size is square. Will throw a
-- 'DataStructureException' if it is not.
squareSz :: MonadThrow m => Sz Ix2 -> m (Square (Sz Ix2))
squareSz sz@(Sz (n :. m)) =
  if n == m
    then return . UnsafeSquare $ sz
    else throwM $ DataStructureException "squareSz" "Size not square!"