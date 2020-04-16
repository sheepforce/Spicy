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
import           Control.Lens            hiding ( Empty )
import           Data.Default
import           RIO                     hiding ( view
                                                , (^.)
                                                )
import qualified RIO.Map                       as Map
import           RIO.Process
import           RIO.Seq                        ( Seq(..) )
import qualified RIO.Seq                       as Seq
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
  -- Apply the calculations top down. The results from the layer above
  molWithOutputs <- molTraverseWithID
    (\currentMolID currentMol -> do
      -- Some logging information about the current layer.
      logInfoF $ "ONIOM " <> (display . molID2OniomHumandID $ currentMolID)
      logInfoF $ "  Embedding type: " <> "electronic"

      -- Polarise the layer if applicable and requested by electronic embedding
      thisLayerMaybeWithPolarisation <- if currentMolID == Empty
        then return currentMol
        -- TODO (phillip|p=100|#Unfinished) - These values are here just for testing. We need to get them from the reader context somehow.
        -- TODO (phillip|p=100|#Unfinished) - Electronic embedding now happens always but shouldn't. Instead obtain somehow from a reader context.
        else getPolarisationCloudFromAbove molWithTasks
                                           currentMolID
                                           (Seq.fromList [0.2, 0.4, 0.6, 0.8])

      -- DEBUG
      logDebug $ "Polarised local molecule: " <> displayShow thisLayerMaybeWithPolarisation
      -- END DEBUG

      -- The calculation is performed on the local molecule (no other layer present than the current
      -- one). Therefore a local context of calculation keys is provided, to carry out the calculation
      -- on the local molecule of this traverse.
      -- All ONIOM calculations will be performed.
      currentLayerOutputs <- Map.traverseWithKey
        (\calcK _ -> case calcK of
          ONIOMKey Inherited -> do
            molUpdatedCtxt <- local (& moleculeL .~ currentMol) $ runCalculation
              (CalcID { _calcID_MolID = Empty, _calcID_calcKey = ONIOMKey Inherited })
            return . join $ (molUpdatedCtxt ^? molecule_CalcContext . ix calcK . calcContext_Output)
          ONIOMKey Original -> do
            molUpdatedCtxt <- local (& moleculeL .~ thisLayerMaybeWithPolarisation) $ runCalculation
              (CalcID { _calcID_MolID = Empty, _calcID_calcKey = ONIOMKey Original })
            return . join $ (molUpdatedCtxt ^? molecule_CalcContext . ix calcK . calcContext_Output)
          {-
          -- Will become necessary if other calculation types should be implemented at some point.
          _ -> throwM $ MolLogicException
            "multicentreOniomNDriver"
            "This layer does not only contain ONIOM-type calculation contexts,\
            \ but this is the ONIOM atomic driver. Cannot continue."
          -}
        )
        (currentMol ^. molecule_CalcContext)

      -- The calculation on the current layer has been performed now. This should provide
      -- 'CalcOutput' for both the high and the low calculation on the current layer. Extract this
      -- output and update the only the calcoutput of the traverse-local molecule.
      let oldCalcContext = currentMol ^. molecule_CalcContext
          -- currentUpdatedCalcOutput =
          --   Map.map (^. calcContext_Output) $ currentLayerDone ^. molecule_CalcContext
          newCalcContext = Map.foldlWithKey'
            (\contextAcc key outputVal ->
              Map.adjust (& calcContext_Output .~ outputVal) key contextAcc
            )
            oldCalcContext
            currentLayerOutputs
          currentMolWithOutput = currentMol & molecule_CalcContext .~ newCalcContext

      -- Finally check if for all calculation contexts of the traverse-local current molecule an
      -- output has been obtained. Return this layer updated if everything is fine, throw an
      -- exception otherwise.
      let layerHasAllOutput =
            all isJust $ currentMolWithOutput ^.. molecule_CalcContext . each . calcContext_Output
      if layerHasAllOutput
        then return currentMolWithOutput
        else throwM $ MolLogicException
          "multicentreOniomNDriver"
          "All calculations of the current layer should have been performed but this seems\
          \ not to be the case. Some calculation output is missing."
    )
    molWithTasks

  -- Collect all the results from the calculation outputs and combine bottom up all the results.
  logInfoF "Collecting the results from individual calculations ..."
  molCollectedResult <- local (& moleculeL .~ molWithOutputs) multicentreOniomNCollector

  return molCollectedResult
