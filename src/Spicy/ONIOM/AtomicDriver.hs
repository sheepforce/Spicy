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
  ( oniomCalcDriver,
    multicentreOniomNDriver,
    geomMacroDriver,
  )
where

import Data.Default
import Data.Massiv.Array as Massiv hiding (forM_)
import Data.Massiv.Array.Manifest.Vector as Massiv
import Optics hiding (Empty, view)
import RIO hiding
  ( view,
    (.~),
    (^.),
    (^..),
    (^?),
  )
import qualified RIO.Map as Map
import RIO.Seq (Seq (..))
import qualified RIO.Vector.Storable as VectorS
import Spicy.Common
import Spicy.Data
import Spicy.InputFile
import Spicy.Logger
import Spicy.Molecule
import Spicy.ONIOM.Collector
import Spicy.RuntimeEnv
import Spicy.Wrapper.IPI.Protocol
import Spicy.Wrapper.IPI.Types

-- | A primitive driver, that executes a given calculation on a given layer. No results will be
-- transered from the calculation output to the actual fields of the molecule.
oniomCalcDriver ::
  ( HasMolecule env,
    HasLogFunc env,
    HasCalcSlot env
  ) =>
  CalcID ->
  WrapperTask ->
  RIO env ()
oniomCalcDriver calcID wTask = do
  -- Obtain infos from the environment.
  molT <- view moleculeL
  mol <- atomically . readTVar $ molT
  calcSlotT <- view calcSlotL

  let layerID = calcID ^. #molID
      calcK = calcID ^. #calcKey

  -- LOG
  logInfo $
    "Layer " <> molID2OniomHumandID layerID <> ", " <> case calcK of
      ONIOMKey Original -> "high level calculation"
      ONIOMKey Inherited -> "low level calculation"

  -- Cleanup the calculation data on this layer and set the task to perform.
  atomically $ do
    cleanOutputOfCalc molT calcID
    assignTaskToCalc molT calcID wTask

  -- Polarise the layer with all information that is available above.
  molWithPol <- maybePolariseLayer mol layerID
  atomically . writeTVar molT $ molWithPol

  -- Run the specified calculation in the calculation slot. Then wait for its results.
  atomically $ putTMVar (calcSlotT ^. #input) calcID
  atomically $ do
    molWithResults <- takeTMVar (calcSlotT ^. #output)
    writeTVar molT molWithResults

----------------------------------------------------------------------------------------------------

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
    HasInputFile env,
    HasCalcSlot env
  ) =>
  WrapperTask ->
  RIO env ()
multicentreOniomNDriver atomicTask = do
  -- Obtain environment information
  molT <- view moleculeL
  mol <- atomically . readTVar $ molT
  inputFile <- view inputFileL

  let modelOK = case inputFile ^. #model of
        ONIOMn {} -> True

  -- Check if this driver is suitable for the layout.
  unless modelOK . throwM . localExc $
    "This driver assumes a multicentre ONIOM-n layout,\
    \ but the input file specifies a different layout type."

  -- Obtain all calculations IDs.
  let allMolIDs = getAllMolIDsHierarchically mol

  -- Iterate over all layers and calculate all of them. After each high level calculation the
  -- multipoles will be collected and transfered from the high level calculation output to the
  -- actual multipole fields of the atoms, to allow for electronic embedding in deeper layers.
  forM_ allMolIDs $ \layerID -> do
    -- Iteration dependent iterations.
    molCurr <- atomically . readTVar $ molT
    let layerLens = molIDLensGen layerID
    calcKeys <-
      maybe2MThrow (localExc "Invalid layer specified") $
        Map.keys <$> molCurr ^? layerLens % #calcContext

    -- Run the original and the inherited calculation on this layer. For the original high level
    -- calculation transfer the multipoles from the calculation otuput to the corresponding fields
    -- of the atoms.
    forM_ calcKeys $ \calcK -> do
      -- Construct the current CalcID.
      let calcID = CalcID {molID = layerID, calcKey = calcK}

      -- Perform the current calculation on the current calculation of the molecule.
      oniomCalcDriver calcID atomicTask

      -- If this was a high level original calculation transfer the multipoles to the atoms.
      when (calcK == ONIOMKey Original) $ do
        molWithPolOutput <- atomically . readTVar $ molT
        molWithPolTrans <- multipoleTransfer calcID molWithPolOutput
        atomically . writeTVar molT $ molWithPolTrans
  where
    localExc = MolLogicException "multicentreOniomNDriver"

----------------------------------------------------------------------------------------------------

-- | A driver for geometry optimisations without microiteration. Does one full traversal of the
-- ONIOM molecule before a geometry displacement step.  Updates can happen by an arbitrary i-PI
-- server
geomMacroDriver ::
  ( HasMolecule env,
    HasLogFunc env,
    HasInputFile env,
    HasCalcSlot env
  ) =>
  -- | The i-PI settings that correspond to the i-PI client and server, that shall do the updates.
  IPI ->
  RIO env ()
geomMacroDriver ipi = do
  return ()
  {-
  -- Check if the client is still runnning and expects us to provide new data.
  let clientWantsMore = True

  when clientWantsMore $ do
    -- Get communication vars with the server.
    let ipiForceIn = ipi ^. #input
        ipiPosOut = ipi ^. #output

    -- Get the current molecule and position data from the i-PI server.
    molT <- view moleculeL
    molOld <- atomically . readTVar $ molT


    -- Get a new Position data from the server and update the molecule.
    logDebug "Works A1"
    posData <- atomically . takeTMVar $ ipiPosOut
    logDebug "Works A2"
    posVec <- case posData ^. #coords of
      NetVec vec -> Massiv.fromVectorM Par (Sz $ VectorS.length vec) vec
    molNewStruct <- updateMolWithPosVec posVec molOld
    atomically . writeTVar molT $ molNewStruct

    -- Do a full traversal of the ONIOM tree and obtain the full ONIOM gradient.
    multicentreOniomNDriver WTGradient

    -- Process the results and collect the gradients for a geometry optimisation step.
    multicentreOniomNCollector WTGradient

    -- Place the coordinates in the Pysis input variable to trigger a geometry optimisation step by
    -- Pysisyphus.
    molWithForces <- atomically . readTVar $ molT
    forceData <- molToForceData molWithForces
    atomically . putTMVar ipiForceIn $ forceData

    -- Reiterate for the next geometry.
    geomMacroDriver ipi
    -}

----------------------------------------------------------------------------------------------------

-- | Cleans all outputs from the molecule.
cleanOutputs :: TVar Molecule -> STM ()
cleanOutputs molT = do
  mol <- readTVar molT
  let molWithEmptyOutputs = molMap (& #calcContext % each % #output .~ def) mol
  writeTVar molT molWithEmptyOutputs

----------------------------------------------------------------------------------------------------

-- | Cleans all outputs from the molecule.
cleanOutputOfCalc :: TVar Molecule -> CalcID -> STM ()
cleanOutputOfCalc molT calcID = do
  mol <- readTVar molT
  let molWithEmptyOutputs = molMap (& calcLens % #output .~ def) mol
  writeTVar molT molWithEmptyOutputs
  where
    calcLens = calcIDLensGen calcID

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

-- | Assigns the given task to the given calculation id.
assignTaskToCalc :: TVar Molecule -> CalcID -> WrapperTask -> STM ()
assignTaskToCalc molT calcID task = do
  mol <- readTVar molT
  let molWithTasks =
        molMap
          (& calcLens % #input % #task .~ task)
          mol
  writeTVar molT molWithTasks
  where
    calcLens = calcIDLensGen calcID

----------------------------------------------------------------------------------------------------

-- | Polarises a layer specified by its 'MolID' with all layers hierarchically above. The resulting
-- molecule will just be the layer specified by the 'MolID'. The function assumes, that all layers
-- above already have been properly polarised.
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
