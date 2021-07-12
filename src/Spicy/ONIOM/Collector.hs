-- |
-- Module      : Spicy.ONIOM.Collector
-- Description : Transforming individual information from calculations to an ONIOM result.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Collector functions to gather the results from the 'CalcOutput's of a molecule and build the ONIOM
-- result from it.
module Spicy.ONIOM.Collector
  ( multicentreOniomNCollector,
    collector,
    collectorDepth,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Data.Massiv.Array as Massiv hiding
  ( mapM,
    sum,
  )
import Optics hiding (view)
import RIO hiding
  ( Vector,
    view,
    (%~),
    (.~),
    (^.),
    (^..),
    (^?),
  )
import qualified RIO.Seq as Seq
import Spicy.Common
import Spicy.Molecule hiding (S)

-- | Collector for multicentre ONIOM-N calculations.
{-# SCC multicentreOniomNCollector #-}
multicentreOniomNCollector :: HasMolecule env => RIO env ()
multicentreOniomNCollector = do
  molT <- view moleculeL
  mol <- readTVarIO molT
  molEDeriv <- collector mol
  atomically . writeTVar molT $ molEDeriv

----------------------------------------------------------------------------------------------------

-- | MC-ONIOMn collector for the full ONIOM tree. Transforms the energy and its derivatives to the
-- the ONIOM representation. Collects all derivatives as far as possible from bottom to top.
--
-- \[
--     E^\mathrm{MC-ONIOM2} = E^\mathrm{real} + \sum\limits_c E^\mathrm{model, high}_c - \sum\limits_c E^\mathrm{model, low}_c
-- \]
-- \[
--     \nabla E^\mathrm{MC-ONIOM2} = E^\mathrm{real} + \sum\limits_c \nabla E_c^\mathrm{model, high} \mathbf{J}_c - \sum\limits_c \nabla E_c^\mathrm{model, low} \mathbf{J}_c
-- \]
-- \[
--     \mathbf{H}^\mathrm{MC-ONIOM2} = \mathbf{H}^\mathrm{real}
--                                   - \sum\limits_c \mathbf{J}_c^T \mathbf{H}_c^\mathrm{model, low} \mathbf{J}_c
--                                   + \sum\limits_c \mathbf{J}_c^T \mathbf{H}_c^\mathrm{model, high} \mathbf{J}_c
-- \]
--
-- The idea is to express the full ONIOM finger tree as a recursion over local multi-centre ONIOM2
-- setups, where the deepest layer of each branch forms the base case of the recursion.
{-# SCC collector #-}
collector :: MonadThrow m => Molecule -> m Molecule
collector mol = collectorDepth maxDepth mol
  where
    maxDepth =
      Seq.length $
        molFoldlWithMolID
          (\iAcc i _ -> if Seq.length i > Seq.length iAcc then i else iAcc)
          mempty
          mol

----------------------------------------------------------------------------------------------------

-- | Energy derivative collector up to a given depth of the ONIOM tree. If the maximum depth or
-- larger is given, this will collect the full ONIOM tree.
{-# SCC collectorDepth #-}
collectorDepth :: MonadThrow m => Int -> Molecule -> m Molecule
collectorDepth depth molecule
  | depth < 0 = throwM . localExc $ "Cannot collect for negative depth."
  | otherwise = go 0 molecule
  where
    localExc = MolLogicException "energyCollectorD"

    -- Recursive bottom up collector.
    go :: MonadThrow m => Int -> Molecule -> m Molecule
    go currDepth mol = do
      -- Model centres below this layer.
      let modelCentres = mol ^. #subMol

      -- If we are at the maximum collection depth or nothing deeper exists we found the model
      -- system and assign the high level calculation output as the true energy of this model.
      -- Otherwise collect deeper layes first.
      if IntMap.null modelCentres || currDepth >= depth
        then eDerivTransfer (ONIOMKey Original) mol >>= multipoleTransformation IntMap.empty
        else do
          -- Recursion into deeper layers happens first. The collector works bottom up.
          modelsCollected <- mapM (go (currDepth + 1)) modelCentres

          -- Get the transformed derivatives of all model centres.
          let eModelHigh = fmap (^. #energyDerivatives) modelsCollected
          eModelLow <- mapM (getEDeriv (ONIOMKey Inherited)) modelsCollected
          modelJacobian <-
            maybe2MThrow (localExc "Missing Jacobian for molecule") $
              mapM (^? #jacobian % _Just) modelCentres
          let modelEDs =
                IntMap.intersectionWith (\j (h, l) -> (h, l, j)) modelJacobian $
                  IntMap.intersectionWith (,) eModelHigh eModelLow

          -- Get the high level energy of this system.
          eReal <- getEDeriv (ONIOMKey Original) mol

          -- Perform the transformations of the energy derivatives.
          eDerivs <- eDerivTransformation modelEDs eReal

          -- Collect the multipoles.
          molMPoles <- multipoleTransformation modelCentres mol

          return $ molMPoles
            & #energyDerivatives .~ eDerivs
            & #subMol .~ modelsCollected

----------------------------------------------------------------------------------------------------

-- | Transformer for energy derivatives of local MC-ONIOM2 setups.
{-# SCC eDerivTransformation #-}
eDerivTransformation ::
  ( MonadThrow m,
    Traversable t
  ) =>
  -- | High level energy derivatives, low level energy derivatives and the Jacobian matrix per model
  -- centre.
  t (EnergyDerivatives, EnergyDerivatives, MatrixS Double) ->
  -- | Energy derivatives of the real system.
  EnergyDerivatives ->
  m EnergyDerivatives
eDerivTransformation eModelHighLow eReal = ff (pure eReal) eModelHighLow
  where
    ff = foldl' $ \accM (eDHigh, eDLow, MatrixS j) -> do
      -- Unwrap the accumulator.
      acc <- accM

      -- Transform the energy.
      let newEAcc :: Maybe Double
          newEAcc = do
            accE <- acc ^. #energy
            eHigh <- eDHigh ^. #energy
            eLow <- eDLow ^. #energy
            return $ accE + eHigh - eLow

      -- Transform the gradient.
      let newGAcc :: Maybe (VectorS Double)
          newGAcc = do
            (VectorS accG) <- acc ^. #gradient
            gHighT <- (eDHigh ^. #gradient) >>= \(VectorS g) -> g ><. j
            gLowT <- (eDLow ^. #gradient) >>= \(VectorS g) -> g ><. j
            VectorS <$> (accG .+. gHighT >>= (.-. gLowT))

      -- Transform the hessian.
      let newHAcc :: Maybe (MatrixS Double)
          newHAcc = do
            (MatrixS accH) <- acc ^. #hessian
            let jT = compute . transpose $ j
            hHighT <- (eDHigh ^. #hessian) >>= \(MatrixS h) -> jT .><. h >>= (.><. j)
            hLowT <- (eDLow ^. #hessian) >>= \(MatrixS h) -> jT .><. h >>= (.><. j)
            MatrixS . compute <$> (accH .+. hHighT >>= (.-. hLowT))

      return $
        EnergyDerivatives
          { energy = newEAcc,
            gradient = newGAcc,
            hessian = newHAcc
          }

----------------------------------------------------------------------------------------------------

-- | Energy derivative transfer from a high or low level calculation to the real system. Works on
-- the current top layer.
eDerivTransfer :: MonadThrow m => CalcK -> Molecule -> m Molecule
eDerivTransfer oniomKey mol =
  getEDeriv oniomKey mol >>= \eD -> return $ mol & #energyDerivatives .~ eD

----------------------------------------------------------------------------------------------------

-- | Get the energy derivatives of the current layer from the given calculation key.
getEDeriv :: MonadThrow m => CalcK -> Molecule -> m EnergyDerivatives
getEDeriv oniomKey mol =
  maybe2MThrow localExc $
    mol ^? #calcContext % ix oniomKey % #output % _Just % #energyDerivatives
  where
    localExc = MolLogicException "getEDeriv" $
      "Energy derivatives for the calculation key " <> show oniomKey <> " could not be found."

----------------------------------------------------------------------------------------------------

-- | Collector for multipole moments. Uses a charge/moment redistribution scheme. Moments of a link
-- atom in the layer will normally get the multipole moment from the calculation output. But if this
-- layer is later used to provide multipoles for a real system, the link atom moments will first be
-- distributed to the real atoms.
{-# SCC multipoleTransformation #-}
multipoleTransformation :: (Foldable t, Functor t, MonadThrow m) => t Molecule -> Molecule -> m Molecule
multipoleTransformation models real = do
  -- Get the multipoles of the real system from the high level calculation on this layer.
  mpRealOut <-
    maybe2MThrow (localExc "High level calculation with multipole information for this layer not found") $
      real ^? #calcContext % ix (ONIOMKey Original) % #output % _Just % #multipoles

  let -- Atoms of the real system with poles from the high level calculation on the real system.
      atomsRealWithOutPoles = updateAtomsWithPoles atomsReal mpRealOut
      -- Update again with the poles from the model centres.
      atomsRealWithModelPoles = updateAtomsWithPoles atomsRealWithOutPoles mpModels
      -- Reconstruct all atoms of the real system with highest level poles that are available.
      atomsUpdated = atomsRealWithModelPoles `IntMap.union` (atomsRealWithOutPoles `IntMap.union` atomsReal)

  return $ real & #atoms .~ atomsUpdated
  where
    localExc = MolLogicException "multipoleTransformation"
    atomsReal = real ^. #atoms

    -- Redistribute link atom moments of the model systems
    modelsRedis = fmap redistributeLinkMoments models

    -- Redistributed but otherwise collected multipole moments of the model systems.
    mpModels = IntMap.unions . fmap (\m -> fmap (^. #multipoles) $ m ^. #atoms) $ modelsRedis

----------------------------------------------------------------------------------------------------

-- | Update atoms with available multipoles. Atoms, that have no multipoles assigned to will be kept
-- unchanged.
updateAtomsWithPoles :: IntMap Atom -> IntMap Multipoles -> IntMap Atom
updateAtomsWithPoles atoms poles =
  let atomsWithPoles = IntMap.intersectionWith (\a p -> a & #multipoles .~ p) atoms poles
   in atomsWithPoles `IntMap.union` atoms
