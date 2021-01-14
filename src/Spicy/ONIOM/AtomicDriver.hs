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

  -- LOG
  logInfo "Performing wrapper calculations on molecule ..."

  -- Apply the calculations top down. The results from the layer above can be used to polarise.
  molWithOutputs <-
    molFoldlWithMolID
      ( \molAcc' layerID layerMol -> do
          -- Unwrap the molecule accumulator.
          molAcc <- molAcc'

          -- Generate a lens for the current layer.
          let layerLens = molIDLensGen layerID

          -- LOG
          logInfo $ "ONIOM " <> (display . molID2OniomHumandID $ layerID)

          -- Polarise the layer if applicable and requested by electronic embedding
          thisLayerMaybeWithPolarisation <-
            if layerID == Empty
              then return layerMol
              else do
                -- TODO (phillip|p=5|#Improvement) - https://aip.scitation.org/doi/10.1063/1.4972000 contains a version applicable to small systems, which does not need scaling factors.
                let embeddingOfThisOrignalLayer =
                      layerMol ^? #calcContext % ix (ONIOMKey Original) % #input % #embedding
                case embeddingOfThisOrignalLayer of
                  Just (Electronic scalingFactors) -> do
                    let eeScalingFactors = fromMaybe defElectronicScalingFactors scalingFactors

                    -- LOG
                    logInfo $ "Using electronic with scaling factors = " <> displayShow eeScalingFactors

                    -- Construct the polarised molecule.
                    getPolarisationCloudFromAbove molAcc layerID eeScalingFactors
                  Just Mechanical -> do
                    -- LOG
                    logInfo "Using mechanical embedding"
                    return layerMol
                  Nothing ->
                    throwM . localExc $
                      "The calculation input does not contain any embedding information\n\
                      \ for a high level calculation on a model system\n\
                      \ or this no ONIOM calculation was specified."

          -- After the current layer has been potentially polarised, re-insert this layer in the
          -- full ONIOM structure.
          let molWithPolarisation = molAcc & layerLens .~ thisLayerMaybeWithPolarisation

          -- The calculation is performed on the current layer. The runCalculation function updates
          -- the outputs of the calculation layer on the fly.
          let calcCntxts = layerMol ^. #calcContext
          molWithLayerOutputsAndPoles <-
            Map.foldlWithKey
              ( \localAccMol' calcK _ -> do
                  -- Unwrap the molecule accumulator, which will subsequently be updated.
                  localAccMol <- localAccMol'

                  -- Construct the local calcID
                  let localCalcID = CalcID {molID = layerID, calcKey = calcK}

                  -- LOG
                  logInfo $ case calcK of
                    ONIOMKey Original -> "High level calculation ..."
                    ONIOMKey Inherited -> "Low level calculation ..."

                  -- Run the calculation for this context.
                  thisMolWithCntxt <- local (& moleculeL .~ localAccMol) $ runCalculation localCalcID

                  -- Transfer the multipoles from the calculation output to the atom data.
                  thisMolWithCntxtAndAtoms <- multipoleTransfer localCalcID thisMolWithCntxt

                  -- Use the context obtained from this calculation for the next iteration,
                  -- accumulating more and more results.
                  return thisMolWithCntxtAndAtoms
              )
              (pure molWithPolarisation)
              calcCntxts

          return molWithLayerOutputsAndPoles
      )
      (pure molWithTasks)
      molWithTasks

  -- Collect all the results from the calculation outputs and combine bottom up all the results.
  logInfo "Collecting the results from individual calculations ..."
  molCollectedResult <-
    local (& moleculeL .~ molWithOutputs) $
      multicentreOniomNCollector atomicTask

  return molCollectedResult
  where
    localExc = MolLogicException "multicentreOniomNDriver"
