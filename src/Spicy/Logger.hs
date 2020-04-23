{-|
Module      : Spicy.Logger
Description : Custom logging facilities built on top of RIO
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module implements the styling of Spicy's logging functions. It is built on top of RIO's logger
and it's ideas, but uses a custom Options type, as the original one is too deeply hidden in RIO.
-}
module Spicy.Logger
  ( -- * Logging Behaviour and Appearance
    newLogFunction
  , logInfoF
    -- * Printing & Human Readable
  , formatPadFoldable
  , getModelAsBuilder
  , molID2OniomHumandID
  , map2Human
  )
where
import qualified Data.ByteString.Builder       as ByteString
import qualified Data.ByteString.Lazy          as ByteString
import           Data.Foldable
import           Data.List.Split
import qualified Data.Text.IO                  as Text
import           Formatting
import           RIO
import qualified RIO.Partial                   as RIO'
import           RIO.Seq                        ( Seq(..) )
import qualified RIO.Seq                       as Seq
import qualified RIO.Text                      as Text
import qualified RIO.Time                      as Time
import           Spicy.Class
import           Spicy.Generic
import           System.Console.ANSI
import qualified System.Path                   as Path
import qualified RIO.Map                       as Map

{-|
Maximum width of a log message, if applicable.
-}
maxWidth :: Int
maxWidth = 100

----------------------------------------------------------------------------------------------------
{-|
Prefix strings for Spicy's logging function.
-}
logLevelString :: LogLevel -> Text
logLevelString level = case level of
  LevelDebug     -> "Debug"
  LevelInfo      -> "Info"
  LevelWarn      -> "Warn"
  LevelError     -> "Error"
  LevelOther lvl -> lvl

----------------------------------------------------------------------------------------------------
{-|
Additional space after the log level opening to add, to have a body ad consistent indentation.
-}
logLevelPostSpacing :: LogLevel -> Text
logLevelPostSpacing level =
  let openingLength = Text.length . logLevelString $ level
  in  Text.replicate (10 - openingLength) " "

----------------------------------------------------------------------------------------------------
{-|
Colour styling for different printing levels for the line opening.
-}
logLevelStyle :: LogLevel -> [SGR]
logLevelStyle level = case level of
  LevelDebug   -> [SetColor Foreground Dull White]
  LevelInfo    -> [SetColor Foreground Vivid Green]
  LevelWarn    -> [SetColor Foreground Vivid Yellow]
  LevelError   -> [SetColor Foreground Vivid Red]
  LevelOther _ -> [SetColor Foreground Vivid Cyan]

----------------------------------------------------------------------------------------------------
{-|
Style of the body of a log message.
-}
logStyleDefault :: [SGR]
-- logStyleDefault = [SetColor Background Vivid Black, SetColor Foreground Vivid White]
logStyleDefault = [SetColor Foreground Vivid White]

----------------------------------------------------------------------------------------------------
{-|
Style of the verbose part of a log message.
-}
logStyleVeboseInfo :: [SGR]
logStyleVeboseInfo = [SetColor Foreground Dull White]

----------------------------------------------------------------------------------------------------
{-|
Creates a new logging function from 'MyLogOptions'.
-}
newLogFunction :: MyLogOptions -> LogFunc
newLogFunction options = mkLogFunc $ \_callStack logSource logLevel' message -> do
  -- Get option information.
  doVerboseLogging <- _logVerbose options
  logThresh        <- _logMinLevel options
  doColor          <- _logUseColor options
  let useTime = _logUseTime options
  -- Create a formatted logging function.
  when (logLevel' >= logThresh) $ do
    when doColor $ setSGR logStyleDefault
    Text.putStr "["
    when doColor . setSGR . logLevelStyle $ logLevel'
    Text.putStr . logLevelString $ logLevel'
    when doColor $ setSGR logStyleDefault
    Text.putStr "]"
    Text.putStr . logLevelPostSpacing $ logLevel'
    when doColor $ setSGR logStyleDefault
    Text.putStrLn
      . RIO.decodeUtf8With RIO.lenientDecode
      . ByteString.toStrict
      . ByteString.toLazyByteString
      . getUtf8Builder
      . _logFormat options
      $ message
    when doVerboseLogging $ do
      when doColor $ setSGR logStyleVeboseInfo
      when useTime $ do
        currentTime <- Time.getZonedTime
        let localTime  = Time.zonedTimeToLocalTime currentTime
            timeString = Time.formatTime Time.defaultTimeLocale "%d.%m.%Y-%T" localTime
        Text.putStr . Text.pack $ timeString
        Text.putStr "  "
      if logSource == ""
        then return ()
        else do
          Text.putStr . textDisplay $ logSource
          Text.putStrLn ""
    when doColor $ setSGR [Reset]

----------------------------------------------------------------------------------------------------
{-|
Using RIO's logging at the Info level to also write the LogFile.
-}
logInfoF :: (HasLogFile env, HasLogFunc env) => Utf8Builder -> RIO env ()
logInfoF info = do
  logInfo info
  logPath <- Path.toAbsRel <$> view logFileL
  appendFileUTF8 logPath . flip Text.snoc '\n' . utf8BuilderToText $ info

{-
####################################################################################################
-}
{-|
For logging purposes build lines of formatted values. Custom formatters for the values and custom
delimiters can be specified. This function will try to obey the maximum width of the log messages
but at least one value per line will be printed.
-}
formatPadFoldable
  :: (Functor t, Foldable t)
  => Int                     -- ^ Width of a single field in a line of values.
  -> Format Text (a -> Text) -- ^ A formatter for the values in the structure.
  -> Text                    -- ^ A delimiter of the fields.
  -> t a                     -- ^ Structure holding the values.
  -> [Utf8Builder]
formatPadFoldable padSize formatter delimiter values =
  fmap (text2Utf8Builder . Text.concat)
    . chunksOf nChunks
    . toList
    . fmap (\v -> sformat (left padSize ' ' %. formatter) v <> sformat stext delimiter)
    $ values
 where
  nChunks        = max (maxWidth `div` (padSize + delimiterWidth)) 1
  delimiterWidth = Text.length delimiter

----------------------------------------------------------------------------------------------------
{-|
A calculation/hamiltonian model has been defined by 'SpicyEnv' in the '_sCalculation' field. Get a
string for printing it.
-}
getModelAsBuilder :: (MonadReader env m, HasInputFile env) => m Utf8Builder
getModelAsBuilder = do
  inputFile <- view inputFileL
  let calcModel = inputFile ^. model
  case calcModel of
    ONIOMn{} -> return "ONIOM-n"

----------------------------------------------------------------------------------------------------
{-|
Translation of a 'MolID' to a more human readable text. Valid in the context of ONIOM calculations.
-}
molID2OniomHumandID :: MolID -> Utf8Builder
molID2OniomHumandID Empty = "Layer 0 (real system)"
molID2OniomHumandID molID =
  let depth  = Seq.length molID
      idTree = foldr'
        (\currentID textAcc ->
          let offSet   = fromEnum 'A'
              idLetter = RIO'.toEnum $ currentID + offSet
          in  textAcc `Text.snoc` idLetter
        )
        ("Layer " <> tShow depth <> " ")
        molID
  in  display idTree

----------------------------------------------------------------------------------------------------
{-|
Print a 'Map' in a nice form, every entry as its own line. Does not respect the maximum line length.
-}
map2Human :: (Show a, Show b) => Map a b -> [Utf8Builder]
map2Human dataMap =
  let associations  = Map.toList dataMap
      keys          = fst <$> associations
      values        = snd <$> associations
      keysText      = tShow <$> keys
      valuesText    = tShow <$> values
      maxKeysLength = maximum $ fmap Text.length keysText
      keysFormatted = sformat ((left maxKeysLength ' ' %. stext) % " = ") <$> keysText
      keyValuesText = zipWith (<>) keysFormatted valuesText
  in  display <$> keyValuesText
