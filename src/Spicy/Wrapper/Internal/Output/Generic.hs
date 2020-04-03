{-|
Module      : Spicy.Wrapper.Internal.Output.Generic
Description : Parsers for generic outputs
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for genric output files such as simple matrices in files.
-}
module Spicy.Wrapper.Internal.Output.Generic
  ( doubleVector
  , doubleSquareMatrix
  )
where
import           Data.Attoparsec.Text
import           Data.Massiv.Array             as Massiv
import           Math.NumberTheory.Powers.Squares
import           RIO
import           Spicy.Class

{-|
Parses plain text representations of vectors.
-}
doubleVector :: Parser (VectorS Double)
doubleVector = do
  numbers <- many1 $ skipSpace *> double
  return . VectorS . Massiv.fromList Par $ numbers

----------------------------------------------------------------------------------------------------
{-|
Parses a plain text representation of a square matrix. Will fail if the number of elements is not a
square number.
-}
doubleSquareMatrix :: Parser (MatrixS Double)
doubleSquareMatrix = do
  vec <- doubleVector
  let nElems    = Massiv.elemsCount . getVectorS $ vec
      matrixDim = exactSquareRoot nElems
  case matrixDim of
    Just dim -> return . MatrixS . Massiv.resize' (Sz (dim :. dim)) . getVectorS $ vec
    Nothing ->
      fail
        $  "Wrong number of elements to construct a square matrix. "
        <> show nElems
        <> " is not a square number."
