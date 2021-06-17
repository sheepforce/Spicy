-- |
-- Module      : Spicy.JobDriver
-- Description : Combination of steps to full ONIOM jobs
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides the main glue for fragment method logics.
module Spicy.JobDriver
  ( spicyExecMain,
  )
where

import Data.List.Split
import qualified Data.Map as Map
import Data.Yaml.Pretty
import Optics hiding (view)
import RIO hiding (view, (.~), (^.))
import RIO.ByteString (hPutStr)
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import RIO.Process
import Spicy.Common
import Spicy.Data
import Spicy.InputFile
import Spicy.Molecule
import Spicy.ONIOM.AtomicDriver
import Spicy.ONIOM.Collector
import Spicy.ONIOM.Layout
import Spicy.Outputter hiding (Macro, Micro)
import Spicy.RuntimeEnv
import Spicy.Wrapper
import System.Path ((</>))
import qualified System.Path as Path

logSource :: LogSource
logSource = "JobDriver"

----------------------------------------------------------------------------------------------------
spicyExecMain ::
  ( HasMolecule env,
    HasInputFile env,
    HasLogFunc env,
    HasWrapperConfigs env,
    HasProcessContext env,
    HasCalcSlot env,
    HasOutputter env,
    HasMotion env
  ) =>
  RIO env ()
spicyExecMain = do
  -- Starting the output log.
  hPutStr stderr spicyLogoColour
  inputFile <- view inputFileL
  inputPrintEnv <- getCurrPrintEnv
  printSpicy $
    spicyLogo
      <> ("Spicy Version " <> versionInfo <> "\n\n\n")
      <> txtInput
      <> sep
      <> (displayBytesUtf8 . encodePretty defConfig $ inputFile)
      <> sep
      <> ( renderBuilder . spicyLog inputPrintEnv $ do
             printCoords ONIOM
             printTopology ONIOM
         )
  -- Writing the ONIOM tree to a file
  let initLayoutFile = getDirPath (inputFile ^. #permanent) </> Path.relFile "Input.mol2"
  logInfoS logSource $ "Writing input structure as parsed to: " <> displayShow (Path.toString initLayoutFile)
  writeFileUTF8
    (getDirPath (inputFile ^. #permanent) </> Path.relFile "Input.mol2")
    =<< writeONIOM (inputPrintEnv ^. #mol)

  -- Start the companion threads for i-PI, Pysis and the calculations.
  calcSlotThread <- async provideCalcSlot
  link calcSlotThread

  -- Building an inital neighbourlist for large distances.
  logInfoS logSource "Constructing initial neighbour list for molecule ..."
  initNeighbourList

  -- Apply topology updates as given in the input file to the molecule.
  logInfoS logSource "Applying changes to the input topology ..."
  changeTopologyOfMolecule

  -- The molecule as loaded from the input file must be layouted to fit the current calculation
  -- type.
  logInfoS logSource "Preparing layout for a MC-ONIOMn calculation ..."
  layoutMoleculeForCalc

  -- setupPhase printing. Show the layouted molecules and topologies in hierarchical order.
  setupPrintEnv <- getCurrPrintEnv
  let molIDHierarchy = getAllMolIDsHierarchically $ setupPrintEnv ^. #mol
  printSpicy $
    txtSetup
      <> ( renderBuilder . spicyLog setupPrintEnv . forM_ molIDHierarchy $ \i -> do
             printCoords (Layer i)
             printTopology (Layer i)
         )
  writeFileUTF8
    (getDirPath (inputFile ^. #permanent) </> Path.relFile "Setup.mol2")
    =<< writeONIOM (setupPrintEnv ^. #mol)

  -- Perform the specified tasks on the input file.
  tasks <- view $ inputFileL % #task
  forM_ tasks $ \t -> do
    case t of
      Energy -> do
        -- Logging.
        energyStartPrintEnv <- getCurrPrintEnv
        printSpicy txtSinglePoint
        printSpicy . renderBuilder . spicyLog energyStartPrintEnv $
          spicyLogMol (HashSet.fromList [Always, Task Start]) All

        -- Actual calculation
        multicentreOniomNDriver WTEnergy *> multicentreOniomNCollector

        -- Final logging
        energyEndPrintEnv <- getCurrPrintEnv
        printSpicy . renderBuilder . spicyLog energyEndPrintEnv $
          spicyLogMol (HashSet.fromList [Always, Task End, FullTraversal]) All
      Optimise Macro -> geomMacroDriver
      Optimise Micro -> geomMicroDriver
      Frequency -> do
        -- Logging.
        hessStartPrintEnv <- getCurrPrintEnv
        printSpicy txtSinglePoint
        printSpicy . renderBuilder . spicyLog hessStartPrintEnv $
          spicyLogMol (HashSet.fromList [Always, Task Start]) All

        -- Actual calculation
        multicentreOniomNDriver WTHessian *> multicentreOniomNCollector

        -- Final logging.
        hessEndPrintEnv <- getCurrPrintEnv
        printSpicy txtSinglePoint
        printSpicy . renderBuilder . spicyLog hessEndPrintEnv $
          spicyLogMol (HashSet.fromList [Always, Task End, FullTraversal]) All
      MD -> do
        logErrorS logSource "A MD run was requested but MD is not implemented yet."
        throwM $ SpicyIndirectionException "spicyExecMain" "MD is not implemented yet."

  -- Final logging.
  finalPrintEnv <- getCurrPrintEnv
  printSpicy . renderBuilder . spicyLog finalPrintEnv $
    spicyLogMol (HashSet.fromList [Spicy End, Always]) All
  writeFileUTF8
    (getDirPath (inputFile ^. #permanent) </> Path.relFile "Final.mol2")
    =<< writeONIOM (finalPrintEnv ^. #mol)
  printSpicy $ sep <> "Spicy execution finished. May the sheep be with you and your results!"

  -- Kill the companion threads after we are done.
  cancel calcSlotThread

  -- LOG
  logInfoS logSource "Spicy execution finished. May the sheep be with you and your results!"

----------------------------------------------------------------------------------------------------

-- | Construct an initial neighbour list with large distances for all atoms. Can be reduced to
-- smaller values efficiently.
initNeighbourList :: (HasMolecule env) => RIO env ()
initNeighbourList = do
  molT <- view moleculeL
  mol <- readTVarIO molT
  nL <- neighbourList 15 mol
  atomically . writeTVar molT $ mol & #neighbourlist .~ Map.singleton 15 nL

----------------------------------------------------------------------------------------------------

-- | Application of the topology updates, that were requested in the input file. The order of
-- application is:
--
--   1. Guess a new bond matrix if requested.
--   2. Remove bonds between pairs of atoms.
--   3. Add bonds between pairs of atoms.
changeTopologyOfMolecule ::
  (HasInputFile env, HasMolecule env, HasLogFunc env) => RIO env ()
changeTopologyOfMolecule = do
  -- Get the molecule to manipulate
  molT <- view moleculeL
  mol <- readTVarIO molT

  -- Get the input file information.
  inputFile <- view inputFileL

  case inputFile ^. #topology of
    Nothing -> return ()
    Just topoChanges -> do
      -- Resolve the defaults to final values.
      let covScalingFactor = fromMaybe defCovScaling $ topoChanges ^. #radiusScaling
          removalPairs = fromMaybe [] $ topoChanges ^. #bondsToRemove
          additionPairs = fromMaybe [] $ topoChanges ^. #bondsToAdd

      -- Show what will be done to the topology:
      logInfoS logSource $
        "Guessing new bonds (scaling factor): "
          <> if topoChanges ^. #guessBonds
            then display covScalingFactor
            else "No"
      logInfoS logSource $
        "Removing bonds between atom pairs: "
          <> utf8Show (chunksOf 5 removalPairs)
      logInfoS logSource $
        "Adding bonds between atom pairs: "
          <> utf8Show (chunksOf 5 additionPairs)

      -- Apply changes to the topology
      let bondMatrixFromInput = mol ^. #bonds
      unless (HashMap.null bondMatrixFromInput) $
        logWarnS
          logSource
          "The input file format contains topology information such as bonds\
          \ but manipulations were requested anyway."

      -- Completely guess new bonds if requested.
      molNewBondsGuess <-
        if topoChanges ^. #guessBonds
          then do
            bondMatrix <- guessBondMatrix (Just covScalingFactor) mol
            return $ mol & #bonds .~ bondMatrix
          else return mol

      -- Remove all bonds between atom pairs requested.
      molNewBondsRemoved <-
        foldl'
          ( \molAcc' atomPair -> do
              molAcc <- molAcc'
              molAccNew <- changeBond Remove molAcc atomPair
              return molAccNew
          )
          (return molNewBondsGuess)
          removalPairs

      -- Add all bonds between atom pairs requested.
      molNewBondsAdded <-
        foldl'
          ( \molAcc' atomPair -> do
              molAcc <- molAcc'
              molAccNew <- changeBond Add molAcc atomPair
              return molAccNew
          )
          (return molNewBondsRemoved)
          additionPairs

      -- The molecule after all changes to the topology have been applied.
      atomically . writeTVar molT $ molNewBondsAdded

----------------------------------------------------------------------------------------------------

-- | Perform transformation of the molecule data structure as obtained from the input to match the
-- requirements for the requested calculation type.
layoutMoleculeForCalc ::
  (HasInputFile env, HasMolecule env) => RIO env ()
layoutMoleculeForCalc = do
  inputFile <- view inputFileL
  case inputFile ^. #model of
    ONIOMn {} -> mcOniomNLayout
