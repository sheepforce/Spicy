{-|
Module      : Spicy.Wrapper.Internal.Output.FChk
Description : Parsers for Gaussian FChk files.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for FChk outputs.
-}
module Spicy.Wrapper.Internal.Output.FChk
  ( -- * Analyser
    getResultsFromFChk
    -- * Parser
  , fChk
  )
where
import           Control.Applicative
import           Control.Lens            hiding ( Empty )
import           Data.Attoparsec.Text
import           Data.Default
import           RIO                     hiding ( take
                                                , takeWhile
                                                , (^.)
                                                , (^?)
                                                )
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as Text
import           Spicy.Class
import           Spicy.Generic
import           Spicy.Math
import           Data.Massiv.Array             as Massiv
                                         hiding ( take )

{-|
A function that uses information from FChk files to obtain 'CalcOutput'. The function will not
fill in a value ('Just') if it cannot be found in the FChk and fail if the FChk cannot be parsed or
a block occurs multiple times.
-}
getResultsFromFChk :: MonadThrow m => Text -> m CalcOutput
getResultsFromFChk content = do
  -- Parse the complete FChk. The Hessian is obtained in row major order as a vector representing
  -- the lower triangular matrix and needs to be expanded to the full square matrix here.
  fchk <- parse' fChk content
  let fchkBlocks     = fchk ^. blocks
      energy         = fchkBlocks Map.!? "Total Energy"
      gradient       = fchkBlocks Map.!? "Cartesian Gradient"
      hessianContent = fchkBlocks Map.!? "Cartesian Force Constants"

  let hessianLTVec = hessianContent ^? _Just . _Array . _ArrayDouble
  hessian <- case hessianLTVec of
    Just vec -> Just . MatrixS <$> ltMat2Square vec
    Nothing  -> return Nothing

  return CalcOutput
    { _calcOutput_Multipoles        = def
    , _calcOutput_EnergyDerivatives = EnergyDerivatives
                                        { _energyDerivatives_Energy   = energy
                                                                        ^? _Just
                                                                        .  _Scalar
                                                                        .  _ScalarDouble
                                        , _energyDerivatives_Gradient = VectorS
                                                                        <$> gradient
                                                                        ^?  _Just
                                                                        .   _Array
                                                                        .   _ArrayDouble
                                        , _energyDerivatives_Hessian  = hessian
                                        }
    }

{-
####################################################################################################
-}
{-|
Parser for Gaussian Formatted Checkpoint files version 3. See
<http://wild.life.nctu.edu.tw/~jsyu/compchem/g09/g09ur/f_formchk.htm> for details.
-}
fChk :: Parser FChk
fChk = do
  -- Line 1: "Initial 72 characters of the title section"
  initTitle  <- takeWhile (not <$> isEndOfLine) <* endOfLine

  -- Line2: "Type, Method, Basis"
  -- Calculation type (format: A10)
  typeString <- Text.toUpper . Text.strip <$> take 10
  readType   <- case typeString of
    "SP"                   -> return SP
    "FOPT"                 -> return FOPT
    "POPT"                 -> return POPT
    "FTS"                  -> return FTS
    "PTS"                  -> return PTS
    "FSADDLE"              -> return FSADDLE
    "PSADDLE"              -> return PSADDLE
    "FORCE"                -> return FORCE
    "FREQ"                 -> return FREQ
    "SCAN"                 -> return SCAN
    "GUESS=ONLY"           -> return GUESS
    "LST"                  -> return LST
    "STABILITY"            -> return STABILITY
    "REARCHIVE"            -> return REARCHIVE
    "MS-RESTART"           -> return MSRESTART
    "REARCHIVE/MS-RESTART" -> return REARCHIVE
    "MIXED"                -> return MIXED
    _                      -> fail "Could not assign calculation string to calculation type."
  -- Calculation method such as MP2 (format: A30)
  methodString <- Text.strip <$> take 30
  -- Basis set (format: A30)
  basisString  <- Text.strip <$> (takeWhile (not <$> isEndOfLine) <* endOfLine)
  -- An arbitrary amount of scalar fields or array blocks with labels
  content      <- many1 (scalar <|> array)
  return FChk { _title    = initTitle
              , _calcType = readType
              , _basis    = basisString
              , _method   = methodString
              , _blocks   = Map.fromList content
              }

----------------------------------------------------------------------------------------------------
{-|
Parser for 'Scalar' fields in FChk files.
-}
scalar :: Parser (Text, Content)
scalar = do
  label    <- Text.strip <$> take 40 <* count 3 (char ' ')
  typeChar <- take 1 <* count 5 (char ' ')
  value    <- case typeChar of
    "I" -> ScalarInt <$> (skipHorizontalSpace *> decimal)
    "R" -> ScalarDouble <$> (skipHorizontalSpace *> double)
    "C" -> ScalarText <$> (skipHorizontalSpace *> takeWhile (not <$> isEndOfLine))
    "L" -> do
      textBool <- skipHorizontalSpace *> (string "T" <|> string "F")
      case textBool of
        "T" -> return $ ScalarLogical True
        "F" -> return $ ScalarLogical False
        _   -> fail "Could not parse boolean scalar expression."
    _ -> fail "Unknown identifier for scalar expression."
  skipHorizontalSpace
  endOfLine
  return (label, Scalar value)

----------------------------------------------------------------------------------------------------
{-|
Parser for 'Array' fields in FChk files.
-}
-- Text arrays appear in no way to be actual arrays of strings. Nevertheles for Fortran they are, as
-- the format for those is 5A12 per line (C) or 9A8 (H). In the FChk they appear as a single normal
-- string, but need to be parsed in those chunks to comply with the strange Fortran string handling.
-- This format makes me wanna cry ...
array :: Parser (Text, Content)
array = do
  label     <- Text.strip <$> take 40 <* count 3 (char ' ')
  typeChar  <- (char 'I' <|> char 'R' <|> char 'C' <|> char 'H' <|> char 'L') <* count 3 (char ' ')
  _         <- string "N="
  nElements <- skipHorizontalSpace *> decimal <* endOfLine
  values    <- case typeChar of
    'I' -> do -- Integer arrays, safely separated by spaces.
      intVals <- count nElements $ skipSpace *> signed decimal
      endOfLine
      return . ArrayInt . Massiv.fromList Par $ intVals
    'R' -> do -- Real arrays, safely separated by spaces.
      doubleVals <- count nElements $ skipSpace *> double
      endOfLine
      return . ArrayDouble . Massiv.fromList Par $ doubleVals
    'C' -> do -- The horror of Fortran string parsing begins ...
      -- let linesToExcpect = nElements `div` 5 + if nElements `mod` 5 /= 0 then 1 else 0
      --     elementsInLastLine = nElements `mod` 5
      textChunks <- count nElements (take 12 <* option () endOfLine)
      return . ArrayText . Text.concat $ textChunks
    'H' -> do -- Another strange fortran format for chunks of text.
      textChunks <- count nElements (take 9 <* option () endOfLine)
      return . ArrayText . Text.concat $ textChunks
    'L' -> do -- Logical arrays. Single characters and no separation.
      boolChars <- count nElements ((char 'T' <|> char 'F') <* option () endOfLine)
      bools     <- traverse
        (\c -> case c of
          'T'   -> return True
          'F'   -> return False
          other -> fail $ "Character \"" <> [other] <> "\" cannot be parsed as Boolean."
        )
        boolChars
      return . ArrayLogical . Massiv.fromList Par $ bools
    _ -> fail $ "Character \"" <> [typeChar] <> " \" is not a valid type character. Cannot parse."
  return (label, Array values)
