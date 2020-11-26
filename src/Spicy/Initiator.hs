{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Spicy.Initiator
-- Description : Translator of inputs to initial state
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides translation from the initial input structure to the initial spicy state.
module Spicy.Initiator
  ( spicyMain,
  )
where

import Data.Char
import Data.FileEmbed
import Data.Maybe
import Data.Version (showVersion)
import Optics hiding (view)
import Paths_spicy (version)
import RIO hiding
  ( to,
    view,
    (.~),
    (^.),
  )
import RIO.Process
import Spicy.CmdArgs
import Spicy.Common
import Spicy.InputFile
import Spicy.Molecule
-- import           Spicy.JobDriver
-- import           Spicy.Logger

import Spicy.RuntimeEnv
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Dir

-- | The Spicy Logo as ASCII art.
spicyLogo :: Utf8Builder
spicyLogo = displayBytesUtf8 $(embedFile . Path.toString . Path.relFile $ "data/Fonts/SpicyLogo.txt")

----------------------------------------------------------------------------------------------------

-- | Get information from the build by TemplateHaskell to be able to show a reproducible version.
versionInfo :: Utf8Builder
versionInfo = displayShow . showVersion $ version

----------------------------------------------------------------------------------------------------
spicyMain :: IO ()
spicyMain = runSimpleApp $ do
  -- Greet with the Spicy logo and emit some version information.
  logInfo spicyLogo
  logInfo $ "Spicy version " <> versionInfo

  -- Get command line arguments to Spicy.
  spicyArgs <- liftIO $ cmdArgs spicyModes

  case spicyArgs of
    Exec {} -> do
      spicyEnv <- inputToEnv
      runRIO spicyEnv $ do
        -- spicyExecMain
        logError "Not implemented yet."
    Translate {} -> do
      logError "Not implemented yet."

----------------------------------------------------------------------------------------------------

-- | Prepare 'SpicyEnv' for a normal execution run by reading necessary files from disk and
-- converting into data structures according to input format.
--
-- This function will behave in the following way:
--
-- - The pre-startup scripts will be read from the file specified by @spicy exec --startupconf=$FILE@,
--   or, if not given, try to read @${HOME}/.spicyrc@. If none of both can be used, Spicy will exit.
-- - If no log file was given by @spicy exec --logfile=$FILE@, the default name @Spicy.log@ in the
--   current directory will be used.
-- - The molecule will be read from a file as specified in the input file. It will be used directly as
--   obtained from the parser. Layouting for subsequent calculations is subject to the main call of
--   spicy.
inputToEnv :: (HasLogFunc env) => RIO env SpicyEnv
inputToEnv = do
  spicyArgs <- liftIO $ cmdArgs spicyModes

  -- Look for a the spicyrc in the environment or alternatively in the home directory.
  homeDir <- liftIO $ Dir.getHomeDirectory
  maybeSpicyrcPath <- liftIO . lookupEnv $ "SPICYRC"
  let spicyrcPath = case maybeSpicyrcPath of
        Nothing -> homeDir </> (Path.relFile ".spicyrc")
        Just p -> Path.absFile p
  wrapperConfigs' <- parseYamlFile spicyrcPath

  -- Read the input file.
  let inputPathRel = Path.toAbsRel . Path.relFile $ spicyArgs ^. #input
  inputPathAbs <- liftIO $ Path.dynamicMakeAbsoluteFromCwd inputPathRel
  inputFile <- parseYamlFile inputPathAbs

  -- Read the input molecule.
  molecule' <- loadInputMolecule $ inputFile ^. #molecule

  -- Construct the process context
  procCntxt' <- mkDefaultProcessContext

  -- Construct the LogFunction and return the runtime environment
  logOptions' <- logOptionsHandle stdout (spicyArgs ^. #verbose)
  let logOptions = setLogUseTime True $ logOptions'
  withLogFunc logOptions $ \lf -> do
    let spicyEnv =
          SpicyEnv
            { molecule = molecule',
              calculation = inputFile,
              wrapperConfigs = wrapperConfigs',
              motion = Nothing,
              procCntxt = procCntxt',
              logFunc = lf
            }
    runRIO spicyEnv $ ask >>= return

----------------------------------------------------------------------------------------------------

-- |
-- Load the whole molecular system from an external file. If a filetype is not provided, try to guess
-- it by the file extension. If the extension cannot be understood, try the desperate choice of
-- defaulting to XYZ. If nothing works, this will simply fail.
loadInputMolecule :: InputMolecule -> RIO env Molecule
loadInputMolecule inputMolecule = do
  let pathToMolFile = getFilePath $ inputMolecule ^. #path
      molFileExtension = filter (/= '.') . Path.takeExtension $ pathToMolFile
      fileFormat = fromMaybe (guessType molFileExtension) (inputMolecule ^. #fileType)

  moleculeText <- readFileUTF8 pathToMolFile
  case fileFormat of
    XYZ -> parse' xyz moleculeText
    PDB -> parse' pdb moleculeText
    MOL2 -> parse' mol2 moleculeText
    TXYZ -> parse' txyz moleculeText
  where
    guessType :: String -> FileType
    guessType ext' =
      let ext = toLower <$> ext'
       in case ext of
            "xyz" -> XYZ
            "pdb" -> PDB
            "mol2" -> MOL2
            "txyz" -> TXYZ
            _ -> XYZ
