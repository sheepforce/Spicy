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
  RIO env ()
multicentreOniomNDriver atomicTask = do
  molT <- view moleculeL
  mol <- atomically . readTVar $ molT
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

  -- LOG
  logInfo "Performing wrapper calculations on molecule ..."

  -- Obtain all calculations IDs.
  let allMolIDs = getAllMolIDsHierarchically mol

  -- Prepares the molecule for a new call of the atomic driver.
  atomically $ do
    cleanOutputs molT
    assignTasks molT atomicTask

  -- Traverse the current molecule: perform all calculations and collect the results.
  forM_ allMolIDs $ \layerID -> do
    -- Obtain information specific for this iteration.
    let layerLens = molIDLensGen layerID
    currMol <- atomically . readTVar $ molT
    layerMol <- maybe2MThrow (localExc "Layer cannot be found.") $ currMol ^? layerLens
    let calcKeys = Map.keys $ layerMol ^. #calcContext

    -- LOG
    logInfo $ "Layer " <> molID2OniomHumandID layerID

    -- If electronic embedding is active for this layer, polarise it. Updates this layer in the full
    -- system.
    molPolarised <- maybePolariseLayer currMol layerID
    atomically . writeTVar molT $ molPolarised

    -- Run original and inherited calculation.
    flip traverse_ calcKeys $ \calcK -> do
      -- LOG
      logInfo $ case calcK of
        ONIOMKey Original -> "High level calculation ..."
        ONIOMKey Inherited -> "Low level calculation ..."

      -- Run the calculation.
      let calcID = CalcID {molID = layerID, calcKey = calcK}
      runCalculation calcID

    -- Transfer the multipoles of the just executed calculation immediately to the data of this
    -- layer.
    let calcID = CalcID {molID = layerID, calcKey = ONIOMKey Original}
    currMolAfterCalc <- atomically . readTVar $ molT
    molWithPolesTransfered <- multipoleTransfer calcID currMolAfterCalc

    -- Update the molecule after the calculations on the current layer have been finished and
    -- the multipoles have been collected.
    atomically . writeTVar molT $ molWithPolesTransfered
  where
    localExc = MolLogicException "multicentreOniomNDriver"

----------------------------------------------------------------------------------------------------

-- | Cleans all outputs from the molecule.
cleanOutputs :: TVar Molecule -> STM ()
cleanOutputs molT = do
  mol <- readTVar molT
  let molWithEmptyOutputs = molMap (& #calcContext % each % #output .~ def) mol
  writeTVar molT molWithEmptyOutputs

----------------------------------------------------------------------------------------------------

-- | Assigns the given task to each calculation in the molecule.
assignTasks :: TVar Molecule -> WrapperTask -> STM ()
assignTasks molT task = do
  mol <- readTVar molT
  let molWithTasks =
        molMap
          (& #calcContext % each % #input % #task .~ task)
          mol
  writeTVar molT molWithTasks

----------------------------------------------------------------------------------------------------

-- | Polarises the current layer with all layers hierarchically above. The resulting molecule will
-- just be the layer specified by the 'MolID'. The function assumes, that all layers above already
-- have been properly polarised.
maybePolariseLayer :: MonadThrow m => Molecule -> MolID -> m Molecule
maybePolariseLayer molFull molID
  | molID == Empty = return molFull
  | otherwise = do
    let layerLens = molIDLensGen molID
    molLayer <- maybe2MThrow (localExc "Layer cannot be found") $ molFull ^? layerLens
    embedding <-
      maybe2MThrow (localExc "Original calculation cannot be found") $
        molLayer ^? #calcContext % ix (ONIOMKey Original) % #input % #embedding

    polarisedLayer <- case embedding of
      Mechanical -> return molLayer
      Electronic scalingFactors -> do
        let eeScalings = fromMaybe defElectronicScalingFactors scalingFactors
        getPolarisationCloudFromAbove molFull molID eeScalings

    return $ molFull & layerLens .~ polarisedLayer
  where
    localExc = MolLogicException "polariseLayer"
