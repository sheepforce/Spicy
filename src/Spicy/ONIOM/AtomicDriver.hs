-- |
-- Module      : Spicy.ONIOM.AtomicDriver
-- Description : Preparation, running an analysis of ONIOM jobs on layouted systems
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- The driver method for ONIOM applications. This modules uses the layouted molecules and performs an
-- atomic task with them (energy, gradient, hessian or property calculation).
--
-- - Updates all calculation contexts to perform the correct task
-- - Call wrappers to perform the calculations specified, get their output and update all 'CalcOutput's
-- - Combine all data from 'CalcOutput' to a final ONIOM result, according to the task.
module Spicy.ONIOM.AtomicDriver
  ( multicentreOniomNDriver,
  )
where

import Data.Default
import Optics hiding (Empty, view)
import RIO hiding
  ( view,
    (.~),
    (^.),
    (^..),
    (^?),
  )
import qualified RIO.Map as Map
import RIO.Process
import RIO.Seq (Seq (..))
import Spicy.Common
import Spicy.Data
import Spicy.InputFile
import Spicy.Logger
import Spicy.Molecule
import Spicy.ONIOM.Collector
import Spicy.RuntimeEnv
import Spicy.Wrapper


-- TODO (phillip|p=100|#Wrong) - The way the molecule calculation traverses does not support electronic embedding.

-- | A driver function for an atomic step in Multicentre-ONIOM-n methods. Performs a single point energy
-- calculation, a gradient calculation or a hessian calculation on a given layout and builds the ONIOM
-- result from the individual results of the calculations on the layers.
--
-- *Note:* The way polarisation can be used here is a general form. It can polarise layerwise top down,
-- but only the layer directly above the layer currently handled can polarise. For exmaple in the case
-- of a 3 layer ONIOM model with real, intermediate and model system, the model system can only be
-- polarised by the intermediate system, but not the real system. The real system might influence the
-- model system by polarising the intermediate system, which then polarises the model system. Therefore
-- this is like a propagation form the outer to the inner layers. This different from the special cases
-- described in [The ONIOM Method and Its Applications](https://doi.org/10.1021/cr5004419), section
-- 2.1.4.1. Also, the scheme employed here allows QM-QM polarisation, if the QM method provides
-- charges.
multicentreOniomNDriver ::
  ( HasMolecule env,
    HasLogFunc env,
    HasWrapperConfigs env,
    HasProcessContext env,
    HasInputFile env
  ) =>
  WrapperTask ->
  RIO env Molecule
multicentreOniomNDriver atomicTask = do
  mol <- view moleculeL
  inputFile <- view inputFileL

  let modelOK = case inputFile ^. #model of
        ONIOMn {} -> True

  logInfo "************************************"
  logInfo "* Multicentre ONIOM-N AtomicDriver *"
  logInfo "************************************"

  -- Check if this driver is suitable for the layout.
  unless modelOK . throwM $
    MolLogicException
      "multicentreOniomNDriver"
      "This driver assumes a multicentre ONIOM-n layout,\
      \ but the input file specifies a different layout type."

  -- Clean all calculation outputs from old results and set the tasks for all wrappers.
  let molWithEmptyOutputs = molMap (& #calcContext % each % #output .~ def) mol
      molWithTasks =
        molMap
          (& #calcContext % each % #input % #task .~ atomicTask)
          molWithEmptyOutputs

  -- Perform all the wrapper tasks.
  logInfo "Performing wrapper calculations on molecule ..."
  -- Apply the calculations top down. The results from the layer above can be used to polarise.
  molWithOutputs <-
    molTraverseWithID
      ( \currentMolID currentMol -> do
          -- Generate a lens for the current layer.
          let currentMolLens = molIDLensGen currentMolID

          -- Some logging information about the current layer.
          logInfo $ "ONIOM " <> (display . molID2OniomHumandID $ currentMolID)
          logInfo $ "  Embedding type: " <> "electronic"

          -- Polarise the layer if applicable and requested by electronic embedding
          thisLayerMaybeWithPolarisation <-
            if currentMolID == Empty
              then return currentMol
              else do
                -- TODO (phillip|p=5|#Improvement) - https://aip.scitation.org/doi/10.1063/1.4972000 contains a version applicable to small systems, which does not need scaling factors.
                let embeddingOfThisOrignalLayer =
                      currentMol
                        ^? #calcContext
                        % ix (ONIOMKey Original)
                        % #input
                        % #embedding
                case embeddingOfThisOrignalLayer of
                  Just (Electronic scalingFactors) ->
                    getPolarisationCloudFromAbove
                      molWithTasks
                      currentMolID
                      (fromMaybe defElectronicScalingFactors scalingFactors)
                  Just Mechanical -> return currentMol
                  Nothing ->
                    throwM $
                      MolLogicException
                        "multicentreOniomNDriver"
                        "The calculation input does not contain any embedding information for a high level calculation on a model system or this no ONIOM calculation was specified."

          -- DEBUG
          logDebug $ "Polarised local molecule: " <> displayShow thisLayerMaybeWithPolarisation
          -- END DEBUG

          -- The calculation is performed on the local molecule (no other layer present than the current
          -- one). Therefore a local context of calculation keys is provided, to carry out the calculation
          -- on the local molecule of this traverse.
          -- All ONIOM calculations will be performed.
          currentLayerOutputs <-
            Map.traverseWithKey
              ( \calcK _ -> case calcK of
                  ONIOMKey Inherited -> do
                    molUpdatedCtxt <-
                      local (& moleculeL .~ molWithTasks) $
                        runCalculation
                          (CalcID {molID = currentMolID, calcKey = ONIOMKey Inherited})
                    return
                      . join
                      $ ( molUpdatedCtxt
                            ^? currentMolLens
                            % #calcContext
                            % ix calcK
                            % #output
                        )
                  ONIOMKey Original -> do
                    let molWithPolarisedLayer =
                          molWithTasks & molIDLensGen currentMolID .~ thisLayerMaybeWithPolarisation
                    molUpdatedCtxt <-
                      local (& moleculeL .~ molWithPolarisedLayer) $
                        runCalculation
                          (CalcID {molID = currentMolID, calcKey = ONIOMKey Original})
                    return
                      . join
                      $ ( molUpdatedCtxt
                            ^? currentMolLens
                            % #calcContext
                            % ix calcK
                            % #output
                        )
                        {-
                        -- Will become necessary if other calculation types should be implemented at some point.
                        _ -> throwM $ MolLogicException
                          "multicentreOniomNDriver"
                          "This layer does not only contain ONIOM-type calculation contexts,\
                          \ but this is the ONIOM atomic driver. Cannot continue."
                        -}
              )
              (currentMol ^. #calcContext)

          -- The calculation on the current layer has been performed now. This should provide
          -- 'CalcOutput' for both the high and the low calculation on the current layer. Extract this
          -- output and update the only the calcoutput of the traverse-local molecule.
          let oldCalcContext = currentMol ^. #calcContext
              -- currentUpdatedCalcOutput =
              --   Map.map (^. calcContext_Output) $ currentLayerDone ^. #calcContext
              newCalcContext =
                Map.foldlWithKey'
                  ( \contextAcc key outputVal ->
                      Map.adjust (& #output .~ outputVal) key contextAcc
                  )
                  oldCalcContext
                  currentLayerOutputs
              currentMolWithOutput = currentMol & #calcContext .~ newCalcContext

          -- Finally check if for all calculation contexts of the traverse-local current molecule an
          -- output has been obtained. Return this layer updated if everything is fine, throw an
          -- exception otherwise.
          let layerHasAllOutput =
                all isJust $ currentMolWithOutput ^.. #calcContext % each % #output
          if layerHasAllOutput
            then return currentMolWithOutput
            else
              throwM $
                MolLogicException
                  "multicentreOniomNDriver"
                  "All calculations of the current layer should have been performed but this seems\
                  \ not to be the case. Some calculation output is missing."
      )
      molWithTasks

  -- Collect all the results from the calculation outputs and combine bottom up all the results.
  logInfo "Collecting the results from individual calculations ..."
  molCollectedResult <-
    local (& moleculeL .~ molWithOutputs) $
      multicentreOniomNCollector atomicTask

  return molCollectedResult
