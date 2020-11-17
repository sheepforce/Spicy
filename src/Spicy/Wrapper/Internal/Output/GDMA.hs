{-|
Module      : Spicy.Wrapper.Internal.Output.GDMA
Description : Parsers for GDMA output files
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for GDMA outputs.
-}
module Spicy.Wrapper.Internal.Output.GDMA
  ( -- * Analyser
  -- * Parser
    gdmaMultipoles
  )
where
import           Control.Applicative
import           RIO
import           RIO.Text
import           Data.Attoparsec.Text
import           Spicy.Class
import           Spicy.Generic
import           Spicy.Data
import           Data.Char

gdmaMultipoles :: Parser (IntMap Multipoles)
gdmaMultipoles = do

  return undefined

-- Parse all multipole data of an expansion centre.
multipoleCentre :: Parser (Atom, Multipoles)
multipoleCentre = do
  symbol <- takeTill isSpace <* skipHorizontalSpace
  xCoord <- string "x =" *> skipHorizontalSpace *> (angstrom2Bohr <$> double) <* skipHorizontalSpace
  yCoord <- string "y =" *> skipHorizontalSpace *> (angstrom2Bohr <$> double) <* skipHorizontalSpace
  zCoord <- string "z =" *> skipHorizontalSpace *> (angstrom2Bohr <$> double) <* skipHorizontalSpace
  _ <- string "bohr" <* endOfLine

  maxRank <- skipHorizontalSpace *> string "Maximum rank =" *> skipHorizontalSpace *> decimal
  radius <-
    skipHorizontalSpace
    *> string "Radius ="
    *> skipHorizontalSpace
    *> double
    <* skipHorizontalSpace
    <* string "bohr"
    <* endOfLine

  -- Rank 0 - Monopoles
  q0 <- if (not $ maxRank >= 0) then return Nothing else labelField "|Q1|" <* endOfLine

  -- Rank 1 - Dipoles
  q1 <- if (not $ maxRank >= 1)
    then return Nothing
    else do
      q1Mag <- labelField "|Q1|"
      q11c  <- labelField "Q11c"
      q11s <- labelField "Q11s" <* endOfLine

  return (undefined, undefined)
 where
  labelField :: Text -> Parser Double
  labelField label =
    (string $ label <> " =") *> skipHorizontalSpace *> double <* skipHorizontalSpace
