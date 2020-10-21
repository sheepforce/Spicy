{-|
Module      : Spicy.Initiator
Description : Translator of inputs to initial state
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides translation from the initial input structure to the initial spicy state.
-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Initiator
  ( spicyStartUp
  )
where
import           Control.Lens
import           Data.Char
import           Data.FileEmbed
import           Data.Maybe
import           GitHash
import           RIO                     hiding ( to
                                                , view
                                                , (^.)
                                                , (.~)
                                                )
import           Spicy.Class
import           Spicy.Generic
import           Spicy.Molecule.Internal.Parser

import           Data.Default
import           Data.Yaml.Pretty
import           RIO.Process
import qualified RIO.Text                      as Text
import           Spicy.JobDriver
import           Spicy.Logger
import           System.Console.CmdArgs  hiding ( def )
import           System.Path                    ( (</>) )
import qualified System.Path                   as Path
import qualified System.Path.Directory         as Dir

{-|
The Spicy Logo as ASCII art.
-}
spicyLogo :: ByteString
spicyLogo = $(embedFile . Path.toString . Path.relFile $ "data/Fonts/SpicyLogo.txt")

----------------------------------------------------------------------------------------------------
{-|
Get information from the build by TemplateHaskell to be able to show a reproducible version.
-}
gitInfo :: GitInfo
gitInfo = $$tGitInfoCwd

----------------------------------------------------------------------------------------------------
--spicyStartUp :: RIO SimpleApp SpicyEnv
spicyStartUp :: IO ()
spicyStartUp = runSimpleApp $ do
  -- Greet with the Spicy logo and emit some version information.
  logInfo . displayBytesUtf8 $ spicyLogo
  mapM_
    logInfo
    [ "Running the Spicy program:"
    , "  Commit: " <> (displayShow . giHash $ gitInfo)
    , "  Branch: " <> (displayShow . giBranch $ gitInfo)
    , "  Date  : " <> (displayShow . giCommitDate $ gitInfo)
    , ""
    ]

  -- Get command line arguments to Spicy.
  cmdArgsAndMode <- liftIO $ cmdArgs spicyModes
  let doVerboseLogging = verbose cmdArgsAndMode

  -- Prepare the logging function for the "real" spicy execution.
  let logOptions =
        def
          &  logVerbose
          .~ return doVerboseLogging
          &  logUseLoc
          .~ True
          &  logUseTime
          .~ False
          &  logMinLevel
          .~ if doVerboseLogging then return LevelDebug else return LevelInfo
      logFunction = newLogFunction logOptions

  -- Assemble everything for the "real" Spicy run in a SpicyEnv and use the custom logging function.
  let initEnv = InitEnv { _logFunction = logFunction, _spicyArgs = cmdArgsAndMode }
  runRIO initEnv $ do
    spicyArgs <- view cmdArgsL
    case spicyArgs of
      Exec{} -> do
        spicyEnv <- prepareSpicyExec
        runRIO spicyEnv spicyExecMain
      Translate{} -> logInfo "Running Spicy in translator mode:"

----------------------------------------------------------------------------------------------------
{-|
Prepare 'SpicyEnv' for a normal execution run by reading necessary files from disk and converting
into data structures according to input format.

This function will behave in the following way:

- The pre-startup scripts will be read from the file specified by @spicy exec --startupconf=$FILE@,
  or, if not given, try to read @${HOME}/.spicyrc@. If none of both can be used, Spicy will exit.
- If no log file was given by @spicy exec --logfile=$FILE@, the default name @Spicy.log@ in the
  current directory will be used.
- The molecule will be read from a file as specified in the input file. It will be used directly as
  obtained from the parser. Layouting for subsequent calculations is subject to the main call of
  spicy.
-}
prepareSpicyExec :: (HasLogFunc env, HasSpicyArgs env) => RIO env SpicyEnv
prepareSpicyExec = do
  spicyArgs <- view cmdArgsL
  logFunc   <- view logFuncL

  -- Check if Spicy took the right turn and we got an exec argument from the command line.
  case spicyArgs of
    Exec{} -> return ()
    _      -> do
      logError
        "Spicy was attempting to prepare for a normal execution run, but was given a different mode."
      throwM $ SpicyIndirectionException
        "prepareSpicyExec"
        "Wrong mode on command line specified for normal execution run preparation."

  -- Setup the logfile. If none was given on the command line, this will default to "Spicy.log" in
  -- the current directory. If it already exists, a warning will be given and then it will be
  -- overwritten
  spicyEnvLogFile <- liftIO . Path.dynamicMakeAbsoluteFromCwd . Path.filePath $ fromMaybe
    "Spicy.log"
    (logfile spicyArgs)
  logFileExists <- liftIO $ Dir.doesFileExist spicyEnvLogFile
  when logFileExists $ do
    logWarn "Log file does already exist. Will be overwritten."
    liftIO $ Dir.removeFile spicyEnvLogFile

  -- Read pre-startup configuration file. If no argument has been given, try finding
  -- @${HOME}/.spicyrc@. If nothing works, we must fail here!
  let logSource = "Pre-Startup scripts"
  homeDir         <- liftIO Dir.getHomeDirectory
  pathToPSEnvFile <- case startupconf spicyArgs of
    Just psusPath -> do
      path' <- liftIO . Path.dynamicMakeAbsoluteFromCwd . Path.filePath $ psusPath
      logDebugS logSource
        $  "Using "
        <> path2Utf8Builder path'
        <> " as pre-startup configuration file."
      return path'
    Nothing -> do
      let defaultPath = homeDir </> Path.relFile ".spicyrc"
      logDebugS logSource
        $  "No custom pre-startup configuration supplied. Trying "
        <> path2Utf8Builder defaultPath
        <> "."
      return defaultPath
  spicyPreStartUpConf <- parseYamlFile pathToPSEnvFile

  -- Read the input file.
  let pathToInput = input spicyArgs
  case pathToInput of
    "" -> do
      mapM_
        logError
        [ "No input file was specified but for the Spicy execution this is required."
        , "Specify an input file as: spicy exec --input"
        ]
      throwM $ SpicyIndirectionException "prepareSpicyExec"
                                         "No log file given but log file is required."
    _ -> return ()
  inputFilePath <- liftIO . Path.dynamicMakeAbsoluteFromCwd . Path.filePath $ pathToInput
  inputFile     <- parseYamlFile inputFilePath

  -- Read the input molecule as referenced in the input file.
  let inputMolecule = inputFile ^. molecule
  originalMolecule <- loadInputMolecule inputMolecule

  -- Create a default process context.
  defProcContext   <- mkDefaultProcessContext

  -- Some final logging information before entering the Spicy main function.
  mapM_
    logInfo
    [ "Spicy Execution run:"
    , "  Pre-startup configuration file: " <> path2Utf8Builder pathToPSEnvFile
    , "  Spicy input file              : " <> path2Utf8Builder inputFilePath
    , "  Molecule file                 : "
      <> (path2Utf8Builder . getFilePath $ inputFile ^. molecule . path)
    , "  Log file                      : " <> path2Utf8Builder spicyEnvLogFile
    ]
  -- Input file information.
  logDebug "  Input file:"
  mapM_ (logDebug . text2Utf8Builder . ("    " <>))
    . Text.lines
    . decodeUtf8Lenient
    . encodePretty defConfig
    $ inputFile

  return SpicyEnv { _sLogFile     = spicyEnvLogFile
                  , _sMolecule    = MoleculeEnv { _meNative = originalMolecule }
                  , _sCalculation = inputFile
                  , _sPreStartUp  = spicyPreStartUpConf
                  , _sLogFunction = logFunc
                  , _sProcContext = defProcContext
                  }

----------------------------------------------------------------------------------------------------
{-|
Load the whole molecular system from an external file. If a filetype is not provided, try to guess
it by the file extension. If the extension cannot be understood, try the desperate choice of
defaulting to XYZ. If nothing works, this will simply fail.
-}
loadInputMolecule :: InputMolecule -> RIO env Molecule
loadInputMolecule inputMolecule = do
  let pathToMolFile    = getFilePath $ inputMolecule ^. path
      molFileExtension = filter (/= '.') . Path.takeExtension $ pathToMolFile
      fileFormat       = fromMaybe (guessType molFileExtension) (inputMolecule ^. fileType)

  moleculeText <- readFileUTF8 pathToMolFile
  case fileFormat of
    XYZ  -> parse' parseXYZ moleculeText
    PDB  -> parse' parsePDB moleculeText
    MOL2 -> parse' parseMOL2 moleculeText
    TXYZ -> parse' parseTXYZ moleculeText
 where
  guessType :: String -> FileType
  guessType ext' =
    let ext = toLower <$> ext'
    in  case ext of
          "xyz"  -> XYZ
          "pdb"  -> PDB
          "mol2" -> MOL2
          "txyz" -> TXYZ
          _      -> XYZ

----------------------------------------------------------------------------------------------------
{-|
This functions restructures the 'Molecule' initially loaded from file
-}
