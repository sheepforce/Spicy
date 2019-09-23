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
{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Math
( (<.>)
, vLength
, vDistance
, vAngle
, vCross
) where
import           Control.Exception.Safe
import           Data.Array.Accelerate             (Matrix)
import qualified Data.Foldable                     as F
import           Data.Sequence                     (Seq)
import qualified Data.Sequence                     as S
import           Prelude                           hiding (cycle, foldl1,
                                                    foldr1, head, init, last,
                                                    maximum, minimum, tail,
                                                    take, takeWhile, (!!))
import qualified Spicy.Math.Internal               as MI
import           Spicy.Types
#ifdef CUDA
import           Data.Array.Accelerate.LLVM.PTX
#else
import           Data.Array.Accelerate.LLVM.Native
#endif


{-
(<!!>) :: (VS.Shape sh, VS.Elt e) => VS.Array sh e -> Int -> e
arr <!!> ix =
  let accAtIx = VS.runQ $ VS.unit $ arr VS.!! ix :: VS.Scalar e
      atIx = head . VS.toList $ accAtIx
  in  atIx
  -}

{-|
Dot product of two 'Seq's.
-}
(<.>) :: (Num a) => Seq a -> Seq a -> a
a <.> b = F.sum $ S.zipWith (*) a b

{-|
Length of a 'Seq'.
-}
vLength :: (Floating a) => Seq a -> a
vLength a = sqrt $ a <.> a

{-|
Distance between 2 points ('Seq's).
-}
vDistance :: (Floating a) => Seq a -> Seq a -> a
vDistance a b = vLength $ S.zipWith (-) a b

{-|
Angle in radian between 2 'Seq's.
-}
vAngle :: (Floating a) => Seq a -> Seq a -> a
vAngle a b = acos $ (a <.> b) / ((vLength a) * (vLength b))

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

{-|
__PROOF OF CONCEPT FOR ACCELERATE. NOT TO BE TAKEN AS FINAL FUNCION.
-}
distMat' :: Molecule -> Matrix Double
distMat' mol =
  let coordVec = MI.getCoordinates Serial mol
  in  dM coordVec
  where
    dM = $(runQ MI.distMat)
{-
-- | Defines the normal vector of a plane, defined by 3 points
r3VecNormalVecOfPlane3Points :: (Vector R, Vector R, Vector R) -> Vector R
r3VecNormalVecOfPlane3Points (a, b, c) = (b - a) `cross` (c - a)

-- | Dihedral angle between 4 atoms
r3VecDihedral :: (Vector R, Vector R, Vector R, Vector R) -> R
r3VecDihedral (a, b, c, d) = hmVecAngle (p1Normal, p2Normal)
  where
    p1Normal = r3VecNormalVecOfPlane3Points (a, b, c)
    p2Normal = r3VecNormalVecOfPlane3Points (b, c, d)

-- vectorProduct :: Seq Double -> Seq Double -> VS.Scalar Double
-- vectorProduct = $( VS.runQ vectorProduct' )
-}
