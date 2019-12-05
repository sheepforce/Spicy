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
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Wrapper.Internal.Output.FChk
  ( -- * Analyser
    getResultsFromFChk
    -- * Parser
  , fChk
    -- * Types
  , ScalarVal(..)
  , _ScalarInt
  , _ScalarDouble
  , _ScalarText
  , _ScalarLogical
  , ArrayVal(..)
  , _ArrayInt
  , _ArrayDouble
  , _ArrayText
  , _ArrayLogical
  , CalcType(..)
  , _SP
  , _FOPT
  , _POPT
  , _FTS
  , _PTS
  , _FSADDLE
  , _PSADDLE
  , _FORCE
  , _FREQ
  , _SCAN
  , _GUESS
  , _LST
  , _STABILITY
  , _REARCHIVE
  , _MSRESTART
  , _MIXED
  , Content(..)
  , _Scalar
  , scalarLabel
  , scalarValue
  , _Array
  , arrayLabel
  , arrayValue
  , FChk(..)
  , title
  , calcType
  , basis
  , method
  , blocks
  )
where
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Lens            hiding ( Empty )
import qualified Data.Array.Accelerate         as A
import           Data.Attoparsec.Text.Lazy
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as S
import qualified Data.Text                     as TS
import           Data.Text.Lazy                 ( Text )
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
import           Spicy.Wrapper.Internal.Types.Shallow

{-|
Formatted Checkpoint files can contain an arbitrary amount of sections with scalar values. Those
scalar values are either 'Int', 'Double', 'Text' or 'Bool'.
-}
data ScalarVal
  = ScalarInt Int
  | ScalarDouble Double
  | ScalarText Text
  | ScalarLogical Bool
  deriving ( Eq, Show )

makePrisms ''ScalarVal

----------------------------------------------------------------------------------------------------
{-|
Data types an array can hold.
-}
-- The text value is very strange. A normal text is split into chunks of 12 or 8 characters and
-- print without separator. Therefore it appears as a normal text but for Fortran it is actually an
-- array of text fragments. I don't care about the crazy Fortran internals and treat it as a single
-- text, which it actually is.
data ArrayVal
  = ArrayInt (AccVector Int)
  | ArrayDouble (AccVector Double)
  | ArrayText Text
  | ArrayLogical (AccVector Bool)
  deriving ( Eq, Show )

makePrisms ''ArrayVal

----------------------------------------------------------------------------------------------------
{-|
Possible types of calculation types from an FChk file.
-}
data CalcType
  = SP        -- ^ Single point
  | FOPT      -- ^ Full optimisation to minimum
  | POPT      -- ^ Partial optimisation to an minimum
  | FTS       -- ^ Full optimisation to a transition state
  | PTS       -- ^ Partial optimisation to a transition state
  | FSADDLE   -- ^ Full optimisation to a saddle point of higher order
  | PSADDLE   -- ^ Partial optimisation to a saddle point of higher order
  | FORCE     -- ^ Energy + gradient calculation
  | FREQ      -- ^ Frequency calculation (2nd derivative)
  | SCAN      -- ^ Potential energy surface scan
  | GUESS     -- ^ Generates molecular orbitals only, also orbital localisation
  | LST       -- ^ Linear synchronous transit calculation
  | STABILITY -- ^ SCF stability analysis
  | REARCHIVE -- ^ Generate archive information from checkpoint file
  | MSRESTART -- ^ Generate archive information from checkpoint file
  | MIXED     -- ^ Compound models such as G2, G3, ...
  deriving ( Eq, Show )

makePrisms ''CalcType

----------------------------------------------------------------------------------------------------
{-|
In the FChk format after 2 lines of header an arbitrary amount of contents will follow. These are
either scalar values or arrays of values. The arrays are always printed as vectors but might
actually refer to matrices. The meaning of the content blocks must be obtained by the label fields.
-}
data Content
  = Scalar
      { _scalarLabel :: Text
      , _scalarValue :: ScalarVal
      }
  | Array
      { _arrayLabel :: Text
      , _arrayValue :: ArrayVal
      }
  deriving ( Eq, Show )

makeLenses ''Content
makePrisms ''Content

----------------------------------------------------------------------------------------------------
{-|
A format fully representing the contents of an formatted checkpoint file version 3.
-}
data FChk = FChk
  { _title    :: Text        -- ^ Short title from the header of the FChk.
  , _calcType :: CalcType    -- ^ Calculation type from the header.
  , _basis    :: Text        -- ^ Primary orbital basis from the FChk header.
  , _method   :: Text        -- ^ Calculation method (such as CCSD, HF, MP2) from the header.
  , _blocks   :: Seq Content -- ^ Blocks of 'Content'.
  }
  deriving ( Eq, Show )

makeLenses ''FChk

{-
####################################################################################################
-}
{-|
A function that uses information from FChk files to obtain 'WrapperOutput'. The function will not
fill in a value ('Just') if it cannot be found in the FChk and fail if the FChk cannot be parsed or
a block occurs multiple times.
-}
getResultsFromFChk :: MonadThrow m => Text -> m WrapperOutput
getResultsFromFChk content = do
  -- Parse the complete FChk.
  fchk <- case parse fChk content of
    Done _ result -> return result
    Fail _ _ err  -> throwM . ParserException $ "Cannot parse contents of FChk file with: " <> err
  let fchkBlocks     = fchk ^. blocks
      energyBlocks   = filterBlocks "Total Energy" (^? scalarLabel) fchkBlocks
      gradientBlocks = filterBlocks "Cartesian Gradient" (^? arrayLabel) fchkBlocks
      hessianBlocks  = filterBlocks "Cartesian Force Constants" (^? arrayLabel) fchkBlocks
  energy     <- checkResults energyBlocks
  gradient   <- checkResults gradientBlocks
  _hessianVec <- checkResults hessianBlocks
  return WrapperOutput { _wrapperOutput_Energy   = energy ^? _Just . _Scalar . _2 . _ScalarDouble
                       , _wrapperOutput_Gradient = gradient ^? _Just . _Array . _2 . _ArrayDouble
                       , _wrapperOutput_Hessian  = undefined -- hessian ^? _Just . _Scalar . _2. _ArrayDouble
                       , _wrapperOutput_PCharges = Nothing
                       }
 where
  filterBlocks :: Text -> (Content -> Maybe Text) -> Seq Content -> Seq Content
  filterBlocks filterString lensFunc block = S.filter (\b -> lensFunc b == Just filterString) block
  checkResults :: MonadThrow m => Seq a -> m (Maybe a)
  checkResults res = case res of
    S.Empty     -> return Nothing
    e :<| Empty -> return . Just $ e
    _ :<| _     -> throwM $ WrapperGenericException "getResultsFromFChk"
                                                    "FChk contains multiple defintions of a block."


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
  typeString <- TS.toUpper . TS.strip <$> take 10
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
  methodString <- TS.strip <$> take 30
  -- Basis set (format: A30)
  basisString  <- TS.strip <$> (takeWhile (not <$> isEndOfLine) <* endOfLine)
  -- An arbitrary amount of scalar fields or array blocks with labels
  content      <- many1 (scalar <|> array)
  return FChk { _title    = textS2L initTitle
              , _calcType = readType
              , _basis    = textS2L basisString
              , _method   = textS2L methodString
              , _blocks   = S.fromList content
              }

----------------------------------------------------------------------------------------------------
{-|
Parser for 'Scalar' fields in FChk files.
-}
scalar :: Parser Content
scalar = do
  label    <- TS.strip <$> take 40 <* count 3 (char ' ')
  typeChar <- take 1 <* count 5 (char ' ')
  value    <- case typeChar of
    "I" -> ScalarInt <$> (skipHorizontalSpace *> decimal)
    "R" -> ScalarDouble <$> (skipHorizontalSpace *> double)
    "C" -> ScalarText . textS2L <$> (skipHorizontalSpace *> takeWhile (not <$> isEndOfLine))
    "L" -> do
      textBool <- skipHorizontalSpace *> (string "T" <|> string "F")
      case textBool of
        "T" -> return $ ScalarLogical True
        "F" -> return $ ScalarLogical False
        _   -> fail "Could not parse boolean scalar expression."
    _ -> fail "Unknown identifier for scalar expression."
  skipHorizontalSpace
  endOfLine
  return Scalar { _scalarLabel = textS2L label, _scalarValue = value }

----------------------------------------------------------------------------------------------------
{-|
Parser for 'Array' fields in FChk files.
-}
-- Text arrays appear in no way to be actual arrays of strings. Nevertheles for Fortran they are, as
-- the format for those is 5A12 per line (C) or 9A8 (H). In the FChk they appear as a single normal
-- string, but need to be parsed in those chunks to comply with the strange Fortran string handling.
-- This format makes me wanna cry ...
array :: Parser Content
array = do
  label     <- TS.strip <$> take 40 <* count 3 (char ' ')
  typeChar  <- (char 'I' <|> char 'R' <|> char 'C' <|> char 'H' <|> char 'L') <* count 3 (char ' ')
  _         <- string "N="
  nElements <- skipHorizontalSpace *> decimal <* endOfLine
  values    <- case typeChar of
    'I' -> do -- Integer arrays, safely separated by spaces.
      intVals <- count nElements $ skipSpace *> signed decimal
      endOfLine
      return . ArrayInt . AccVector . A.fromList (A.Z A.:. nElements) $ intVals
    'R' -> do -- Real arrays, safely separated by spaces.
      doubleVals <- count nElements $ skipSpace *> double
      endOfLine
      return . ArrayDouble . AccVector . A.fromList (A.Z A.:. nElements) $ doubleVals
    'C' -> do -- The horror of Fortran string parsing begins ...
      -- let linesToExcpect = nElements `div` 5 + if nElements `mod` 5 /= 0 then 1 else 0
      --     elementsInLastLine = nElements `mod` 5
      textChunks <- count nElements (take 12 <* option () endOfLine)
      return . ArrayText . textS2L . TS.concat $ textChunks
    'H' -> do -- Another strange fortran format for chunks of text.
      textChunks <- count nElements (take 9 <* option () endOfLine)
      return . ArrayText . textS2L . TS.concat $ textChunks
    'L' -> do -- Logical arrays. Single characters and no separation.
      boolChars <- count nElements ((char 'T' <|> char 'F') <* option () endOfLine)
      bools     <- traverse
        (\c -> case c of
          'T'   -> return True
          'F'   -> return False
          other -> fail $ "Character \"" <> [other] <> "\" cannot be parsed as Boolean."
        )
        boolChars
      return . ArrayLogical . AccVector . A.fromList (A.Z A.:. nElements) $ bools
    _ -> fail $ "Character \"" <> [typeChar] <> " \" is not a valid type character. Cannot parse."
  return Array { _arrayLabel = textS2L label, _arrayValue = values }