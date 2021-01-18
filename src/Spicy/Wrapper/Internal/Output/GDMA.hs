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
import Spicy.Common
import Spicy.Molecule hiding (C, S)

-- | Parses all multipole expansion centres from the GDMA output as a list. The second part of the
-- tuple is the multipole at the origin.
gdmaMultipoles :: Parser [Multipoles]
gdmaMultipoles = do
  _ <- manyTill anyChar (string "Positions and radii in angstrom") <* endOfLine
  _ <- string "Multipole moments in atomic units, ea_0^k for rank k" <* count 2 endOfLine

  -- Parse all expansion centres.
  multipoleCentres <- many1 multipoleCentre

  -- The total multipole could be parsed here but for now we skip over it.

  return multipoleCentres

----------------------------------------------------------------------------------------------------

-- | A data type to hold the parser results of GDMA in an ordered fashion.
data MultipoleIdx = MultipoleIdx
  { rank :: Int,
    idx :: SphTIdx
  }
  deriving (Eq, Ord, Show)

data SphTIdx = SphTIdx Int ComponentL
  deriving (Eq, Ord, Show)

data ComponentL = None | S | C
  deriving (Eq, Ord, Show)

-- | Parse all multipole data of an expansion centre.
multipoleCentre :: Parser Multipoles
multipoleCentre = do
  _symbol <- takeTill isSpace <* skipHorizontalSpace
  _xCoord <- string "x =" *> skipHorizontalSpace *> double <* skipHorizontalSpace
  _yCoord <- string "y =" *> skipHorizontalSpace *> double <* skipHorizontalSpace
  _zCoord <- string "z =" *> skipHorizontalSpace *> double <* skipHorizontalSpace
  _ <- (string "bohr" <|> string "angstrom") <* endOfLine

  _maxRank <- skipHorizontalSpace *> string "Maximum rank =" *> skipHorizontalSpace *> (decimal :: Parser Int)
  _radius <-
    skipHorizontalSpace
      *> string "Radius ="
      *> skipHorizontalSpace
      *> double
      <* skipHorizontalSpace
      <* (string "bohr" <|> string "angstrom")
      <* endOfLine

  -- Rank 0 - Monopoles
  q0 <- do
    (mpIdx, q00') <- skipHorizontalSpace *> labeledComponent <* endOfLine
    case mpIdx of
      MultipoleIdx {rank = 0, idx = SphTIdx 0 None} -> return . Just $ Monopole {q00 = q00'}
      _ -> fail "Rank 0 (monopole) could not be parsed correctly."

  -- Other ranks.
  multipoles <- Map.unions <$> many' rankBlock

  -- Construct the values.
  let dipole =
        let q10' = multipoles Map.!? MultipoleIdx {rank = 1, idx = SphTIdx 0 None}
            q11c' = multipoles Map.!? MultipoleIdx {rank = 1, idx = SphTIdx 1 C}
            q11s' = multipoles Map.!? MultipoleIdx {rank = 1, idx = SphTIdx 1 S}
         in if (all isNothing [q10', q11c', q11s'])
              then Nothing
              else
                Just $
                  Dipole
                    { q10 = fromMaybe 0 q10',
                      q11c = fromMaybe 0 q11c',
                      q11s = fromMaybe 0 q11s'
                    }
      quadrupole =
        let q20' = multipoles Map.!? MultipoleIdx {rank = 2, idx = SphTIdx 0 None}
            q21c' = multipoles Map.!? MultipoleIdx {rank = 2, idx = SphTIdx 1 C}
            q21s' = multipoles Map.!? MultipoleIdx {rank = 2, idx = SphTIdx 1 S}
            q22c' = multipoles Map.!? MultipoleIdx {rank = 2, idx = SphTIdx 2 C}
            q22s' = multipoles Map.!? MultipoleIdx {rank = 2, idx = SphTIdx 2 S}
         in if (all isNothing [q20', q21c', q21s', q22c', q22s'])
              then Nothing
              else
                Just $
                  Quadrupole
                    { q20 = fromMaybe 0 q20',
                      q21c = fromMaybe 0 q21c',
                      q21s = fromMaybe 0 q21s',
                      q22c = fromMaybe 0 q22c',
                      q22s = fromMaybe 0 q22s'
                    }
      octopole =
        let q30' = multipoles Map.!? MultipoleIdx {rank = 3, idx = SphTIdx 0 None}
            q31c' = multipoles Map.!? MultipoleIdx {rank = 3, idx = SphTIdx 1 C}
            q31s' = multipoles Map.!? MultipoleIdx {rank = 3, idx = SphTIdx 1 S}
            q32c' = multipoles Map.!? MultipoleIdx {rank = 3, idx = SphTIdx 2 C}
            q32s' = multipoles Map.!? MultipoleIdx {rank = 3, idx = SphTIdx 2 S}
            q33c' = multipoles Map.!? MultipoleIdx {rank = 3, idx = SphTIdx 3 C}
            q33s' = multipoles Map.!? MultipoleIdx {rank = 3, idx = SphTIdx 3 S}
         in if (all isNothing [q30', q31c', q31s', q32c', q32s', q33c', q33s'])
              then Nothing
              else
                Just $
                  Octopole
                    { q30 = fromMaybe 0 q30',
                      q31c = fromMaybe 0 q31c',
                      q31s = fromMaybe 0 q31s',
                      q32c = fromMaybe 0 q32c',
                      q32s = fromMaybe 0 q32s',
                      q33c = fromMaybe 0 q33c',
                      q33s = fromMaybe 0 q33s'
                    }
      hexadecapole =
        let q40' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 0 None}
            q41c' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 1 C}
            q41s' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 1 S}
            q42c' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 2 C}
            q42s' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 2 S}
            q43c' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 3 C}
            q43s' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 3 S}
            q44c' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 4 C}
            q44s' = multipoles Map.!? MultipoleIdx {rank = 4, idx = SphTIdx 4 S}
         in if (all isNothing [q40', q41c', q41s', q42c', q42s', q43c', q43s', q44c', q44s'])
              then Nothing
              else
                Just $
                  Hexadecapole
                    { q40 = fromMaybe 0 q40',
                      q41c = fromMaybe 0 q41c',
                      q41s = fromMaybe 0 q41s',
                      q42c = fromMaybe 0 q42c',
                      q42s = fromMaybe 0 q42s',
                      q43c = fromMaybe 0 q43c',
                      q43s = fromMaybe 0 q43s',
                      q44c = fromMaybe 0 q44c',
                      q44s = fromMaybe 0 q44s'
                    }
  return
    Multipoles
      { monopole = q0,
        dipole = dipole,
        quadrupole = quadrupole,
        octopole = octopole,
        hexadecapole = hexadecapole
      }
  where
    -- Parsing the magnitude field of a multipole. Looks like @|Qx|@, where x is the multipole rank.
    -- Returns the rank and the magnitude of this multipole moment.
    magnitudeField :: Parser (Int, Double)
    magnitudeField = do
      rank <- char '|' *> char 'Q' *> (decimal <|> (char 'A' *> pure 10)) <* char '|' <* char ' ' <* char '='
      mag <- skipHorizontalSpace *> double
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
      _mf <- magnitudeField
      assocMap <-
        (endOfLine *> return mempty)
          <|> (many1 $ skipHorizontalSpace *> labeledComponent <* skipSpace)

      return . Map.fromList $ assocMap
