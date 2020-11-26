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
import RIO
import Spicy.Common
import Spicy.Data
import Spicy.Molecule

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
  q0 <-
    if (not $ maxRank >= 0)
      then return Nothing
      else do
        q00' <- labelField "Q00" <* endOfLine
        return . Just $ Monopole {q00 = q00'}

  -- Rank 1 - Dipoles
  q1 <-
    if (not $ maxRank >= 1)
      then return Nothing
      else do
        _q1Mag <- labelField "|Q1|"
        q11c' <- labelField "Q11c"
        q11s' <- labelField "Q11s" <* endOfLine
        return . Just $
          Dipole
            { q11c = q11c',
              q11s = q11s'
            }

  -- Rank 2 - Quadrupoles
  q2 <-
    if (not $ maxRank >= 2)
      then return Nothing
      else do
        _q2Mag <- labelField "|Q2|"
        q20' <- labelField "Q20"
        q22c' <- labelField "Q22c"
        q22s' <- labelField "Q22s" <* endOfLine
        return . Just $
          Quadrupole
            { q20 = q20',
              q22c = q22c',
              q22s = q22s'
            }

  -- Rank 3 - Octopole
  q3 <-
    if (not $ maxRank >= 3)
      then return Nothing
      else do
        _q3Mag <- labelField "|Q3|"
        q31c' <- labelField "Q31c"
        q31s' <- labelField "Q31s"
        q33c' <- labelField "Q33c" <* endOfLine
        q33s' <- labelField "Q33s" <* endOfLine
        return . Just $
          Octopole
            { q31c = q31c',
              q31s = q31s',
              q33c = q33c',
              q33s = q33s'
            }

  -- Rank 4 - Hexadecapole
  q4 <-
    if (not $ maxRank >= 4)
      then return Nothing
      else do
        _q4Mag <- labelField "|Q4|"
        q40' <- labelField "Q40"
        q42c' <- labelField "Q42c"
        q42s' <- labelField "Q42s" <* endOfLine
        q44c' <- labelField "Q44c"
        q44s' <- labelField "Q44s" <* endOfLine
        return . Just $
          Hexadecapole
            { q40 = q40',
              q42c = q42c',
              q42s = q42s',
              q44c = q44c',
              q44s = q44s'
            }

  -- Rank 5
  _q5 <-
    if (not $ maxRank >= 5)
      then return Nothing
      else do
        _q5Mag <- labelField "|Q5|"
        _q51c' <- labelField "Q51c"
        _q51s' <- labelField "Q51s"
        _q53c' <- labelField "Q53c" <* endOfLine
        _q53s' <- labelField "Q53s"
        _q55c' <- labelField "Q55c"
        _q55s' <- labelField "Q55s" <* endOfLine
        return Nothing

  -- Rank 6
  _q6 <-
    if (not $ maxRank >= 6)
      then return Nothing
      else do
        _q6Mag <- labelField "|Q6|"
        _q60' <- labelField "Q60"
        _q62c' <- labelField "Q62c"
        _q62s' <- labelField "Q62s" <* endOfLine
        _q64c' <- labelField "Q64c"
        _q64s' <- labelField "Q64s"
        _q66c' <- labelField "Q66c" <* endOfLine
        _q66s' <- labelField "Q66s" <* endOfLine
        return Nothing

  -- Rank 7
  _q7 <-
    if (not $ maxRank >= 7)
      then return Nothing
      else do
        _q7Mag <- labelField "|Q7|"
        _q71c' <- labelField "Q71c"
        _q71s' <- labelField "Q71s"
        _q73c' <- labelField "Q73c" <* endOfLine
        _q73s' <- labelField "Q73s"
        _q75c' <- labelField "Q75c"
        _q75s' <- labelField "Q75s" <* endOfLine
        _q77c' <- labelField "Q77c"
        _q77s' <- labelField "Q77s" <* endOfLine
        return Nothing

  -- Ranke 8
  _q8 <-
    if (not $ maxRank >= 8)
      then return Nothing
      else do
        _q8Mag <- labelField "|Q8|"
        _q80' <- labelField "Q80"
        _q82c' <- labelField "Q82c"
        _q82s' <- labelField "Q82s" <* endOfLine
        _q84c' <- labelField "Q84c"
        _q84s' <- labelField "Q84s"
        _q86c' <- labelField "Q86c" <* endOfLine
        _q86s' <- labelField "Q86s"
        _q88c' <- labelField "Q88c"
        _q88s' <- labelField "Q88s" <* endOfLine
        return Nothing

  -- Rank 9
  _q9 <-
    if (not $ maxRank >= 9)
      then return Nothing
      else do
        _q9Mag <- labelField "|Q9|"
        _q91c' <- labelField "Q91c"
        _q91s' <- labelField "Q91s"
        _q93c' <- labelField "Q93c" <* endOfLine
        _q93s' <- labelField "Q93s"
        _q95c' <- labelField "Q95c"
        _q95s' <- labelField "Q95s" <* endOfLine
        _q97c' <- labelField "Q97c"
        _q97s' <- labelField "Q97s"
        _q99c' <- labelField "Q99c" <* endOfLine
        _q99s' <- labelField "Q99s" <* endOfLine
        return Nothing

  -- Rank 10
  _q10 <-
    if (not $ maxRank >= 10)
      then return Nothing
      else do
        _qAMag <- labelField "|QA|"
        _qA0' <- labelField "QA0"
        _qA2c' <- labelField "QA2c"
        _qA2s' <- labelField "QA2s" <* endOfLine
        _qA4c' <- labelField "QA4c"
        _qA4s' <- labelField "QA4s"
        _qA6c' <- labelField "QA6c" <* endOfLine
        _qA6s' <- labelField "QA6s"
        _qA8c' <- labelField "QA8c"
        _qA8s' <- labelField "QA8s" <* endOfLine
        _qAAc' <- labelField "QAAc"
        _qAAs' <- labelField "QAAs" <* endOfLine
        return Nothing

  -- Expansion centres are separated by a blank line.
  endOfLine

  return
    Multipoles
      { monopole = q0,
        dipole = q1,
        quadrupole = q2,
        octopole = q3,
        hexadecapole = q4
      }
  where
    labelField :: Text -> Parser Double
    labelField label =
      skipHorizontalSpace *> string label *> skipHorizontalSpace *> char '=' *> skipHorizontalSpace
        *> double <* skipHorizontalSpace
