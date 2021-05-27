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
import qualified Data.Map as Map
import Data.Maybe
import Optics hiding (view)
import RIO hiding
  ( to,
    view,
    (.~),
    (^.),
  )
import RIO.List (repeat)
import RIO.Process
import Spicy.CmdArgs
import Spicy.Common
import Spicy.InputFile
import Spicy.JobDriver
import Spicy.Molecule
import Spicy.Outputter hiding (Motion)
import Spicy.RuntimeEnv
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Dir

logSource :: LogSource
logSource = "Initiator"

----------------------------------------------------------------------------------------------------
spicyMain :: IO ()
spicyMain = runSimpleApp inputToEnvAndRun

----------------------------------------------------------------------------------------------------

-- | Prepare 'SpicyEnv' for a normal execution run and then start Spicy. Reads necessary files from
--  disk and converts them into data structures according to input format.
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
inputToEnvAndRun :: (HasLogFunc env) => RIO env ()
inputToEnvAndRun = do
  inputArgs <- liftIO $ cmdArgs spicyArgs

  -- Look for a the spicyrc in the environment or alternatively in the home directory.
  homeDir <- liftIO Dir.getHomeDirectory
  maybeSpicyrcPath <- liftIO . lookupEnv $ "SPICYRC"
  let spicyrcPath = case maybeSpicyrcPath of
        Nothing -> homeDir </> Path.relFile ".spicyrc"
        Just p -> Path.absFile p
  wrapperConfigs <- parseYamlFile spicyrcPath

  -- Read the input file.
  let inputPathRel = Path.toAbsRel . Path.relFile $ inputArgs ^. #input
  inputPathAbs <- liftIO $ Path.dynamicMakeAbsoluteFromCwd inputPathRel
  inputFile <- parseYamlFile inputPathAbs

  -- Read the input molecule.
  molecule' <- loadInputMolecule $ inputFile ^. #molecule
  moleculeT <- newTVarIO molecule'

  -- Construct the process context.
  procCntxt' <- mkDefaultProcessContext

  -- Construct the motion state.
  let allMolIDs = toList $ getAllMolIDsHierarchically molecule'
      motion' =
        Motion
          { outerCycle = 0,
            innerCycles = Map.fromList $ zip allMolIDs (repeat 0)
          }
  motionT <- newTVarIO motion'

  -- Create the input and output slots of the companion threads.
  -- The calculation slot, running the QC wrappers.
  calcSlot <- defIO

  -- Directory settings.
  let scratchDir = getDirPath $ inputFile ^. #scratch
  scratchDirAbs <- liftIO $ Path.genericMakeAbsoluteFromCwd scratchDir

  -- Make sure the scratch is empty.
  scratchExists <- liftIO . Dir.doesDirectoryExist $ scratchDirAbs
  when scratchExists . liftIO . Dir.removeDirectoryRecursive $ scratchDirAbs

  -- Initialise the outputter thread and make sure to remove old log files.
  outQ <- newTBQueueIO 100
  let outfile = fromMaybe (Path.file "spicy.out") (Path.file <$> inputArgs ^. #logfile)
      printVerb = defPrintVerbosity . fromMaybe Medium $ inputFile ^. #printLevel
      outputter =
        Outputter
          { outChan = outQ,
            outFile = outfile,
            printVerbosity = printVerb
          }
  hasOldOutput <- liftIO . Dir.doesFileExist $ outfile
  when hasOldOutput . liftIO . Dir.removeFile $ outfile
  logThread <- async $ runReaderT loggingThread outputter
  link logThread

  -- Construct the LogFunction and return the runtime environment
  logOptions' <- logOptionsHandle stdout (inputArgs ^. #verbose)
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \lf -> do
    let spicyEnv =
          SpicyEnv
            { molecule = moleculeT,
              calculation = inputFile,
              wrapperConfigs = wrapperConfigs,
              motion = motionT,
              procCntxt = procCntxt',
              logFunc = lf,
              calcSlot = calcSlot,
              outputter = outputter
            }

    -- The spicy main thread.
    runRIO spicyEnv spicyExecMain

----------------------------------------------------------------------------------------------------

-- | Load the whole molecular system from an external file. If a filetype is not provided, try to guess
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
