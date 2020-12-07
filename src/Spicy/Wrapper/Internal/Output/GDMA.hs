-- |
-- Module      : Spicy.Wrapper.Internal.Output.GDMA
-- Description : Parsers for GDMA output files
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides parsers for GDMA outputs.
module Spicy.Wrapper.Internal.Output.GDMA
  ( -- * Parser
    gdmaMultipoles,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import RIO hiding (take)
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import Spicy.Common
import Spicy.Data
import Spicy.Molecule hiding (C, S)

-- | Parses all multipole expansion centres from the GDMA output as a list. The second part of the
-- tuple is the multipole at the origin.
gdmaMultipoles :: Parser [Multipoles]
gdmaMultipoles = do
  _ <- manyTill anyChar (string "Positions and radii in bohr") <* endOfLine
  _ <- string "Multipole moments in atomic units, ea_0^k for rank k" <* count 2 endOfLine

  -- Parse all expansion centres.
  multipoleCentres <- many' multipoleCentre

  -- The total multipole could be parsed here but for now we skip over it.

  return multipoleCentres

----------------------------------------------------------------------------------------------------

-- | A data type to hold the parser results of GDMA in an ordered fashion.
data MultipoleIdx = MultipoleIdx
  { rank :: Int,
    idx :: SphTIdx
  }
  deriving (Eq, Ord)

data SphTIdx = SphTIdx Int ComponentL
  deriving (Eq, Ord)

data ComponentL = None | S | C
  deriving (Eq, Ord)

-- | Parse all multipole data of an expansion centre.
multipoleCentre :: Parser Multipoles
multipoleCentre = do
  _symbol <- takeTill isSpace <* skipHorizontalSpace
  _xCoord <- string "x =" *> skipHorizontalSpace *> (angstrom2Bohr <$> double) <* skipHorizontalSpace
  _yCoord <- string "y =" *> skipHorizontalSpace *> (angstrom2Bohr <$> double) <* skipHorizontalSpace
  _zCoord <- string "z =" *> skipHorizontalSpace *> (angstrom2Bohr <$> double) <* skipHorizontalSpace
  _ <- string "bohr" <* endOfLine

  maxRank <- skipHorizontalSpace *> string "Maximum rank =" *> skipHorizontalSpace *> (decimal :: Parser Int)
  _radius <-
    skipHorizontalSpace
      *> string "Radius ="
      *> skipHorizontalSpace
      *> double
      <* skipHorizontalSpace
      <* string "bohr"
      <* endOfLine

  -- Rank 0 - Monopoles
  q0 <- do
    (mpIdx, q00') <- skipHorizontalSpace *> labeledComponent
    case mpIdx of
      MultipoleIdx {rank = 0, idx = SphTIdx 0 None} -> return . Just $ Monopole {q00 = q00'}
      _ -> fail "Rank 0 (monopole) could not be parsed correctly."

  -- Other ranks.
  multipoles <- Map.unions <$> many' rankBlock

  -- Expansion centres are separated by a blank line.
  endOfLine

  return
    Multipoles
      { monopole = q0,
        dipole = undefined,
        quadrupole = undefined,
        octopole = undefined,
        hexadecapole = undefined
      }
  where
    -- A helper function to feed the result of one parser into another.
    nextParse :: Parser a -> Text -> Parser a
    nextParse nextParser t = case parseOnly nextParser t of
      Left err -> fail err
      Right res -> return res

    -- Parsing the magnitude field of a multipole. Looks like @|Qx|@, where x is the multipole rank.
    -- Returns the rank and the magnitude of this multipole moment.
    magnitudeField :: Parser (Int, Double)
    magnitudeField = do
      rank <- char '|' *> char 'Q' *> decimal <* char '|' <* char ' ' <* char '='
      mag <- skipMany (char ' ') *> double
      return (rank, mag)

    -- Parses a labeled component field of a multipole. Gives @(Rank, (Component, s/c), Value) for
    -- each field.
    labeledComponent :: Parser (MultipoleIdx, Double)
    labeledComponent = do
      rank' <- char 'Q' *> take 1 >>= nextParse (decimal <|> (char 'A' *> pure 10))
      cmpntN <- decimal <|> (char 'A' *> pure 10)
      cmpntL <- option None $ ((char 's' *> pure S) <|> (char 'c' *> pure C))
      value' <- skipHorizontalSpace *> char '=' *> skipHorizontalSpace *> double
      return (MultipoleIdx {rank = rank', idx = SphTIdx cmpntN cmpntL}, value')

    -- Parse an entire block of a rank /= 0.
    rankBlock :: Parser (Map MultipoleIdx Double)
    rankBlock = do
      _ <- magnitudeField
      assocMap <- Map.fromList <$> (many' $ skipHorizontalSpace *> labeledComponent <* skipSpace)
      return assocMap
