-- |
-- Module      : Spicy.Wrapper.Internal.Output.Turbomole
-- Description : Parsers for Turbomole output files
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides parsers for Turbomole outputs.
module Spicy.Wrapper.Internal.Output.Turbomole
  ( energy,
    gradient,
    hessian,
    basis,
  )
where

import Data.Attoparsec.Text
import Data.Char (toUpper)
import qualified Data.Map as Map
import Data.Massiv.Array as Massiv
import Math.NumberTheory.Roots
import RIO
import Spicy.Common
import Spicy.Molecule (Element)

-- | @$energy@ block
energy :: Parser Double
energy = do
  void $ string "$energy" <* skipHorizontalSpace
  void $ string "SCF" <* skipHorizontalSpace
  void $ string "SCFKIN" <* skipHorizontalSpace
  void $ string "SCFPOT" <* endOfLine

  void $ skipHorizontalSpace *> decimal @Int <* skipHorizontalSpace
  scfEnergy <- double <* skipHorizontalSpace
  _kin <- double <* skipHorizontalSpace
  _pot <- double <* skipHorizontalSpace <* endOfLine

  return scfEnergy

----------------------------------------------------------------------------------------------------

-- | @$gradient@ block
gradient :: Parser (VectorS Double)
gradient = do
  -- Header
  void $ string "$grad" <* skipHorizontalSpace
  void $ string "cartesian gradients" <* endOfLine

  -- Summary stuff
  _cycle <- skipHorizontalSpace *> string "cycle =" *> skipHorizontalSpace *> decimal @Int <* skipHorizontalSpace
  _scfEnergy <- string "SCF energy =" *> skipHorizontalSpace *> double <* skipHorizontalSpace
  _gradMag <- string "|dE/dxyz| =" *> skipHorizontalSpace *> double <* endOfLine

  -- @$coord@ style coordinates for which the gradient is printed
  void . many1 $ do
    _xyz <- count 3 (skipHorizontalSpace *> double) <* skipHorizontalSpace
    _element <- many1 letter <* endOfLine
    return ()

  -- Cartesian gradients, xyz component per atom and line
  grad <- fmap (flatten . Massiv.fromLists' @S @Ix2 Par) . many1 $ do
    xyz <- count 3 $ skipHorizontalSpace *> fortranDouble
    endOfLine
    return xyz

  return . VectorS $ grad

----------------------------------------------------------------------------------------------------

-- | @nprhessian@ block. This requires non-projected hessians, with translational and rotational
-- components still present.
hessian :: Parser (MatrixS Double)
hessian = do
  void $ string "$nprhessian" <* optional " (projected)" <* endOfLine

  -- Iterate over lines
  allElements <- fmap (compute @S . sconcat) . many1 $ do
    -- Indices. We don't care about them.
    void . count 2 $ skipHorizontalSpace *> decimal @Int

    -- Parse a single line. Might (!) contain up to 5 elements per line
    elements <- do
      els <- many1 $ skipHorizontalSpace *> fortranDouble
      endOfLine
      return . sfromList $ els

    return elements

  -- Reshape into a square hessian matrix.
  let Sz m = size allElements
  n <- case exactSquareRoot m of
    Nothing -> fail "The number of elements in the $hessian block is not a square number. Therefore no Hessian can be constructed."
    Just x -> return x
  let hess = resize' (Sz $ n :. n) allElements

  return . MatrixS $ hess

----------------------------------------------------------------------------------------------------

-- | Parser for the primary orbital basis in Turbomole format.
basis :: Parser (Map Element [[(Double, Double)]])
basis = do
  void $ string "$basis" <* endOfLine

  -- Parse basis sets per element.
  basisPerElement <- many1 elementBlock

  return . Map.fromList $ basisPerElement
  where
    -- Parser for the basis set of one element.
    elementBlock = do
      -- Start of a block.
      void $ char '*' <* endOfLine

      -- Element and name of the basis set
      element <- many1 letter <* skipHorizontalSpace >>= tmElString2El
      _name <- takeTill isEndOfLine <* endOfLine

      -- Comment. Often indicates contraction style
      _comment <- takeTill isEndOfLine <* endOfLine

      -- End of the header of a basis block.
      void $ char '*' <* endOfLine

      -- Contraction blocks.
      cGTOs <- many1 contractionBlock

      return (element, cGTOs)

    -- Convert a lower letter Turbomole element string into an element.
    tmElString2El :: MonadFail m => String -> m Element
    tmElString2El [] = fail "Element string empty. Cannot parse."
    tmElString2El elS@(e : es) =
      let elM = readMaybe @Element $ toUpper e : es
       in case elM of
            Nothing -> fail $ "Could not parse element string " <> elS
            Just el -> return el

    -- Parses a cGTO by parsing all pGTOs.
    contractionBlock = do
      -- Header of a cGTO. Indicates number of pGTOs and angular momentum.
      nPrim <- skipHorizontalSpace *> decimal @Int
      angMom <- skipHorizontalSpace *> many1 letter <* endOfLine

      -- Parse given number of pGTOs with exponent and contraction coefficient.
      pGTOs <- count nPrim $ do
        expo <- skipHorizontalSpace *> double
        coeff <- skipHorizontalSpace *> double <* endOfLine
        return (expo, coeff)

      return pGTOs
