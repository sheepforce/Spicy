{-|
Module      : Spicy.ONIOM.AtomicDriver
Description : Preparation, running an analysis of ONIOM jobs on layouted systems
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

The driver method for ONIOM applications. This modules uses the layouted molecules and performs an
atomic task with them (energy, gradient, hessian or property calculation).

- Updates all calculation contexts to perform the correct task
- Call wrappers to perform the calculations specified, get their output and update all 'CalcOutput's
- Combine all data from 'CalcOutput' to a final ONIOM result, according to the task.
-}
module Spicy.ONIOM.AtomicDriver
  ( multicentreOniomNDriver
  )
where
import           Control.Lens
import           Data.Default
import           RIO                     hiding ( view
                                                , (^.)
                                                )
import           RIO.Process
import           Spicy.Class
import           Spicy.Logger
import           Spicy.Molecule
import           Spicy.ONIOM.Collector
import           Spicy.Wrapper

{-|
A driver function for an atomic step in Multicentre-ONIOM-n methods. Performs a single point energy
calculation, a gradient calculation or a hessian calculation on a given layout and builds the ONIOM
result from the individual results of the calculations on the layers.
-}
multicentreOniomNDriver
  :: ( HasMolecule env
     , HasLogFile env
     , HasLogFunc env
     , HasPreStartUpEnv env
     , HasProcessContext env
     , HasInputFile env
     )
  => WrapperTask
  -> RIO env Molecule
multicentreOniomNDriver atomicTask = do
  mol       <- view moleculeL
  inputFile <- view inputFileL

  let modelOK = case inputFile ^. model of
        ONIOMn{} -> True

  logInfoF "************************************"
  logInfoF "* Multicentre ONIOM-N AtomicDriver *"
  logInfoF "************************************"

  -- Check if this driver is suitable for the layout.
  unless modelOK . throwM $ MolLogicException
    "multicentreOniomNDriver"
    "This driver assumes a multicentre ONIOM-n layout,\
    \ but the input file specifies a different layout type."

  -- Clean all calculation outputs from old results and set the tasks for all wrappers.
  let molWithEmptyOutputs = molMap (& molecule_CalcContext . each . calcContext_Output .~ def) mol
      molWithTasks        = molMap
        (& molecule_CalcContext . each . calcContext_Input . calcInput_Task .~ atomicTask)
        molWithEmptyOutputs

  -- Perform all the wrapper tasks.
  logInfoF "Performing wrapper calculations on molecule ..."
  -- TODO (phillip|p=100|#Unfinished) - Running calculations like this is only possible, if they do not depend on each other. For electronic embedding, the layers need to communicate top down.
  molWithOutputs <- local (& moleculeL .~ molWithTasks) runAllCalculations

  -- Collect all the results from the calculation outputs and combine bottom up all the results
  logInfoF "Collecting the results from individual calculations ..."
  molCollectedResult <- local (& moleculeL .~ molWithOutputs) multicentreOniomNCollector

  return molCollectedResult
