{-|
Module      : Spicy.JobDriver
Description : Combination of steps to full ONIOM jobs
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides the main glue for fragment method logics.
-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.JobDriver
  ( spicyExecMain
  )
where
import           Control.Lens
import           Data.FileEmbed
import           Data.List.Split
import           RIO                     hiding ( view
                                                , (^.)
                                                )
import qualified RIO.HashMap                   as HashMap
import           RIO.Process
import qualified RIO.Text                      as Text
import           Spicy.Class
import           Spicy.Data
import           Spicy.Generic
import           Spicy.Logger
import           Spicy.Molecule
import           Spicy.ONIOM.AtomicDriver
import           Spicy.ONIOM.Layout
import qualified System.Path                   as Path

{-|
The Spicy Logo as ASCII art.
-}
jobDriverText :: Text
jobDriverText =
  decodeUtf8Lenient $(embedFile . Path.toString . Path.relFile $ "data/Fonts/JobDriver.txt")

----------------------------------------------------------------------------------------------------
spicyExecMain
  :: ( HasMolecule env
     , HasInputFile env
     , HasLogFunc env
     , HasLogFile env
     , HasPreStartUpEnv env
     , HasProcessContext env
     )
  => RIO env ()
spicyExecMain = do
  inputFile   <- view inputFileL

  -- Get a string builder to print the model name for logging.
  modelString <- getModelAsBuilder

  -- Open the log file by starting with the job driver headline.
  mapM_ (logInfoF . text2Utf8Builder) $ Text.lines jobDriverText

  -- Apply topology updates as given in the input file to the molecule.
  logInfoF "Applying changes to the input topology:"
  molTopologyAdaptions <- changeTopologyOfMolecule

  -- The molecule as loaded from the input file must be layouted to fit the current calculation
  -- type.
  logInfoF $ "Preparing layout for a " <> modelString <> " calculation ..."
  molLayouted <- local (& moleculeL .~ molTopologyAdaptions) layoutMoleculeForCalc
  logInfoF
    $  "Layouting done. Writing layout to "
    <> (path2Utf8Builder . getDirPath $ inputFile ^. permanent)
    <> "."
  local (& moleculeL .~ molLayouted) $ writeLayout writeXYZ

  -- Perform the specified tasks on the input file.
  -- TODO (phillip|p=100|#Unfinished) - This is here for testing purposes. The real implementation should use a more abstract driver, that composes atomic tasks.
  _molProcessed <- local (& moleculeL .~ molLayouted) (multicentreOniomNDriver WTGradient)

  return ()

----------------------------------------------------------------------------------------------------
{-|
Application of the topology updates, that were requested in the input file. The order of application
is:

  1. Guess a new bond matrix if requested.
  2. Remove bonds between pairs of atoms.
  3. Add bonds between pairs of atoms.
-}
changeTopologyOfMolecule
  :: (HasInputFile env, HasMolecule env, HasLogFunc env, HasLogFile env) => RIO env Molecule
changeTopologyOfMolecule = do
  -- Get the molecule to manipulate
  mol       <- view moleculeL

  -- Get the input file information.
  inputFile <- view inputFileL

  case inputFile ^. topology of
    Nothing          -> return mol
    Just topoChanges -> do
      -- Resolve the defaults to final values.
      let covScalingFactor = fromMaybe defCovScaling $ topoChanges ^. radiusScaling
          removalPairs     = fromMaybe [] $ topoChanges ^. bondsToRemove
          additionPairs    = fromMaybe [] $ topoChanges ^. bondsToAdd

      -- Show what will be done to the topology:
      logInfoF $ "  Guessing new bonds (scaling factor): " <> if topoChanges ^. guessBonds
        then display covScalingFactor
        else "No"
      logInfoF "  Removing bonds between atom pairs:"
      mapM_ (logInfo . ("    " <>) . utf8Show) $ chunksOf 5 removalPairs
      logInfoF "  Adding bonds between atom pairs:"
      mapM_ (logInfo . ("    " <>) . utf8Show) $ chunksOf 5 additionPairs

      -- Apply changes to the topology
      let bondMatrixFromInput = mol ^. molecule_Bonds
      unless (HashMap.null bondMatrixFromInput)
        $ logWarn
            "The input file format contains topology information such as bonds\
                \ but manipulations were requested anyway."

      -- Completely guess new bonds if requested.
      molNewBondsGuess <- if topoChanges ^. guessBonds
        then do
          bondMatrix <- guessBondMatrix (Just covScalingFactor) mol
          return $ mol & molecule_Bonds .~ bondMatrix
        else return mol

      -- Remove all bonds between atom pairs requested.
      molNewBondsRemoved <- foldl'
        (\molAcc' atomPair -> do
          molAcc    <- molAcc'
          molAccNew <- changeBond Remove molAcc atomPair
          return molAccNew
        )
        (return molNewBondsGuess)
        removalPairs

      -- Add all bonds between atom pairs requested.
      molNewBondsAdded <- foldl'
        (\molAcc' atomPair -> do
          molAcc    <- molAcc'
          molAccNew <- changeBond Add molAcc atomPair
          return molAccNew
        )
        (return molNewBondsRemoved)
        additionPairs

      -- The molecule after all changes to the topology have been applied.
      return molNewBondsAdded

----------------------------------------------------------------------------------------------------
{-|
Perform transformation of the molecule data structure as obtained from the input to match the
requirements for the requested calculation type.
-}
layoutMoleculeForCalc
  :: (HasInputFile env, HasMolecule env, HasLogFunc env, HasLogFile env) => RIO env Molecule
layoutMoleculeForCalc = do
  inputFile <- view inputFileL
  case inputFile ^. model of
    ONIOMn{} -> oniomNLayout

----------------------------------------------------------------------------------------------------
{-|
Supposed to run all tasks, that were specified in the input file in a stateful fashion. (Result of
the last task will be used as start for the next task.)
-}
runTasks :: RIO env Molecule
runTasks = undefined
