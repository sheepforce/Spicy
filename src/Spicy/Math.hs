{-|
Module      : Spicy.Math
Description : Basic mathematical operations
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jenVS.de
Stability   : experimental
Portability : POSIX, Windows

This module defines basic algebraic operations used throughout the program. Numerical heavy and most
other operations are implemented using Accelerate, to provide parallel operations. Note that all
'VS.runQ' provided functions must be typed without typeclasses but by concrete types.

The operations here accept some insecurities (like not checking if both vectors of a dot product
have equal lenght) and trust the caller.
-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Math
  ( -- * Linear Algebra
    -- $linearAlgebra
    (<.>)
  , vLength
  , vDistance
  , vAngle
  , vCross
  )
where

import           Control.Exception.Safe
import           Data.Array.Accelerate          ( Matrix )
import qualified Data.Foldable                 as F
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Prelude                 hiding ( cycle
                                                , foldl1
                                                , foldr1
                                                , head
                                                , init
                                                , last
                                                , maximum
                                                , minimum
                                                , tail
                                                , take
                                                , takeWhile
                                                , (!!)
                                                )
import           Spicy.Generic
import qualified Spicy.Math.Internal           as MI
import           Spicy.Molecule

{-
####################################################################################################
-}
{- $linearAlgebra
Functions for common linear algebra tasks. They are implemented using 'Seq' for simple functions and
for more heavily numerical functions with Accelerate or 'Control.Parallel.Strategies'.
-}
{-|
Dot product of two 'Seq's.
-}
(<.>) :: (Num a) => Seq a -> Seq a -> a
a <.> b = F.sum $ S.zipWith (*) a b

----------------------------------------------------------------------------------------------------
{-|
Length of a 'Seq'.
-}
vLength :: (Floating a) => Seq a -> a
vLength a = sqrt $ a <.> a

----------------------------------------------------------------------------------------------------
{-|
Distance between 2 points ('Seq's).
-}
vDistance :: (Floating a) => Seq a -> Seq a -> a
vDistance a b = vLength $ S.zipWith (-) a b

----------------------------------------------------------------------------------------------------
{-|
Angle in radian between 2 'Seq's.
-}
vAngle :: (Floating a) => Seq a -> Seq a -> a
vAngle a b = acos $ (a <.> b) / ((vLength a) * (vLength b))

----------------------------------------------------------------------------------------------------
{-|
3D cross product of 2 'Seq's.
-}
vCross :: MonadThrow m => Seq Double -> Seq Double -> m (Seq Double)
vCross a b =
  let crossProduct :: Maybe (Seq Double)
      crossProduct = do
        a1 <- a S.!? 0
        a2 <- a S.!? 1
        a3 <- a S.!? 2
        b1 <- b S.!? 0
        b2 <- b S.!? 1
        b3 <- b S.!? 2
        let c1 = a2 * b3 - a3 * b2
            c2 = a3 * b1 - a1 * b3
            c3 = a1 * b2 - a2 * b1
        return $ S.fromList [c1, c2, c3]
  in  case crossProduct of
        Nothing ->
          throwM $ DataStructureException "vCross" "Could not get an element from input sequence"
        Just cp -> return cp
