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
    eDerivCollector,
    eDerivCollectorDepth,
    multipoleTransferCollector,
    multipoleTransfer,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
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
multicentreOniomNCollector :: HasMolecule env => RIO env ()
multicentreOniomNCollector = do
  molT <- view moleculeL
  mol <- readTVarIO molT
  molEDeriv <- eDerivCollector mol
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
eDerivCollector :: MonadThrow m => Molecule -> m Molecule
eDerivCollector mol = eDerivCollectorDepth maxDepth mol
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
eDerivCollectorDepth :: MonadThrow m => Int -> Molecule -> m Molecule
eDerivCollectorDepth depth molecule
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
        then eDerivTransfer (ONIOMKey Original) mol
        else do
          -- Recursion into deeper layers happens first. The collector works bottom up.
          modelsCollected <- mapM (go (currDepth + 1)) modelCentres

          -- Get the transformed derivatives of all model centres.
          let eModelHigh = fmap (^. #energyDerivatives) modelsCollected
          eModelLow <- mapM (getEDeriv (ONIOMKey Inherited)) modelsCollected
          modelJacobian <-
            maybe2MThrow (localExc "Missing Jacobian for molecule") $
              mapM (^? #jacobian % _Just) modelCentres
          let models =
                IntMap.intersectionWith (\j (h, l) -> (h, l, j)) modelJacobian $
                  IntMap.intersectionWith (,) eModelHigh eModelLow

          -- Get the high level energy of this system.
          eReal <- getEDeriv (ONIOMKey Original) mol

          -- Perform the transformations of the energy derivatives, assign to the molecule and
          -- return.
          eDerivs <- eDerivTransformation models eReal
          return $ mol & #energyDerivatives .~ eDerivs

----------------------------------------------------------------------------------------------------

-- | Transformer for energy derivatives of local MC-ONIOM2 setups.
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
  maybe2MThrow (localExc "Energy derivatives for the calculation key could not be found.") $
    mol ^? #calcContext % ix oniomKey % #output % _Just % #energyDerivatives
  where
    localExc = MolLogicException "getEDeriv"

{-
====================================================================================================
-}

-- | This collector does not perform any actual calculation, but rather sets the multipoles of the
-- atoms. In similiar spirit to the other collectors, this happens in a local MC-ONIOM2 recursion.
-- The multipole moments of the current real layer are taken from the local model if this atom was
-- part of the local model system and from this real layers original calculation context otherwise.
--
-- The final toplayer allows to calculate the electrostatic energy from all its atoms directly then.
multipoleTransferCollector :: MonadThrow m => Molecule -> m Molecule
multipoleTransferCollector mol = do
  let subMols = mol ^. #subMol

  -- Update this layer of the molecule with its multipoles, as if this layer would be the real
  -- system.
  thisLayerAsReal <-
    if IntMap.null subMols
      then -- If no deeper layers are present, the multipoles of this layer depends on nothing else and can
      -- be taken directly from the high level original calculation context.
        thisOriginalMultipolesOutputAsRealMultipoles mol
      else multiCentreONIOM2Collector mol subMols

  return thisLayerAsReal
  where
    -- For the bottom layer (highest calculation level, most model system), the multipoles of the
    -- original calcoutput will be used to fill in the values for the atoms.
    thisOriginalMultipolesOutputAsRealMultipoles :: MonadThrow m => Molecule -> m Molecule
    thisOriginalMultipolesOutputAsRealMultipoles mol' = do
      maybeOriginalCalcMultipoles <-
        maybe2MThrow
          ( MolLogicException
              "multipoleTransferCollector"
              "The original calculation output for a layer does not exist."
          )
          $ mol'
            ^? #calcContext
              % ix (ONIOMKey Original)
              % #output
              % _Just
              % #multipoles
      let realAtomsNoPoles =
            IntMap.filter
              (\a -> not $ a ^. #isDummy || isAtomLink (a ^. #isLink))
              $ mol' ^. #atoms

      -- Check that the wrapper produced outputs for the correct atoms and that the output is
      -- complete.
      unless
        (IntMap.keysSet maybeOriginalCalcMultipoles == IntMap.keysSet realAtomsNoPoles)
        . throwM
        $ MolLogicException
          "multipoleTransferCollector"
          "The atoms from the calculation output and the molecule layer itself seem to mismatch."

      -- Combine the Atom Intmap with the multipole IntMap. The intersection is used here (which
      -- will not shrink the size of the IntMap, if the keys are completely the same), to allow for
      -- different IntMaps to be combined.
      let realAtomsPoles =
            IntMap.intersectionWith
              (\atom poles -> atom & #multipoles .~ poles)
              realAtomsNoPoles
              maybeOriginalCalcMultipoles
      return $ mol' & #atoms .~ realAtomsPoles

    -- In a local MC-ONIOM2 setup calculate the current layer as if it would have been real. The
    -- multipoles of link atoms, that are only in the local model system, are being redistributed.
    -- The best case scenario is to have GDMA multipoles, where the link atoms were not allowed to
    -- carry a pole.
    multiCentreONIOM2Collector :: MonadThrow m => Molecule -> IntMap Molecule -> m Molecule
    multiCentreONIOM2Collector realMol modelCentres = do
      -- Remove link tags from atoms, that were already links in the local real system from the
      -- model systems.
      let modelLinksClean = fmap (removeRealLinkTagsFromModel realMol) modelCentres

      -- Make sure that the set 1 atoms (model system atoms without link atoms in the model system)
      -- are a subset of this real system.
      let set1ModelAtoms =
            IntMap.keysSet
              . IntMap.unions
              . fmap (\mc -> IntMap.filter (not . isAtomLink . isLink) $ mc ^. #atoms)
              $ modelLinksClean
          realAtomsNoPoles = IntMap.keysSet $ realMol ^. #atoms
      unless (set1ModelAtoms `IntSet.isSubsetOf` realAtomsNoPoles) . throwM $
        MolLogicException
          "multipoleTransferCollector"
          "The model centres seem to contain non-link atoms, which are not part of the real layer.\
          \ This must not happen."

      -- First get all multipoles of this local real layer from the calculation output of this layer.
      realMolWithMultipoles <- thisOriginalMultipolesOutputAsRealMultipoles realMol

      -- Make sure the model layers all got their multipoles updated.
      modelCentresWithTheirRealMultipoles <- mapM multipoleTransferCollector modelLinksClean

      -- Redistribute the multipoles of model system specific link atoms over the the set 1 atoms of
      -- the model.
      let modelCentresRedistributed =
            fmap redistributeLinkMoments modelCentresWithTheirRealMultipoles

      -- Use the model system atom's multipoles instead of the ones of the real system calculation for
      -- the real system here (model system multipoles have priority).
      let realAtoms = realMolWithMultipoles ^. #atoms
          modelSet1Atoms = IntMap.unions . fmap atoms $ modelCentresRedistributed
          realAtomsUpdated =
            IntMap.unionWith
              (\realAtom modelAtom -> realAtom & #multipoles .~ (modelAtom ^. #multipoles))
              realAtoms
              modelSet1Atoms

      return $
        realMol
          & (#atoms .~ realAtomsUpdated)
          & (#subMol .~ modelCentresWithTheirRealMultipoles)

----------------------------------------------------------------------------------------------------

-- | Not a collector in the sense of the others. Simply transfers the multipoles from a given CalcID
-- to the corresponding fields of the molecule.
multipoleTransfer :: MonadThrow m => CalcID -> Molecule -> m Molecule
multipoleTransfer calcID mol = do
  let layerID = calcID ^. #molID
      calcLens = calcIDLensGen calcID
      layerLens = molIDLensGen layerID

  -- Obtain the atoms of this layer.
  atoms <- maybe2MThrow (localExc "The specified layer does not exist.") $ mol ^? layerLens % #atoms
  let multipolesInAtoms = fmap (^. #multipoles) atoms

  -- Obtain multipoles from the output of the calculation.
  multiPolesInOutput <-
    maybe2MThrow (localExc "Requested calculation does not exist or did not produce output.") $
      mol ^? calcLens % #output % _Just % #multipoles

  -- Check that not too many or mismatching multipoles have been obtained.
  let atomKeys = IntMap.keysSet multipolesInAtoms
      atomsWithMPAvailable = IntMap.keysSet multiPolesInOutput
  unless (atomsWithMPAvailable `IntSet.isSubsetOf` atomKeys) . throwM . localExc $
    "The calculation seems to have produced multipoles for non existing atoms."

  -- Update the multipoles and rejoin with the atoms.
  let updatedPoles = IntMap.union multiPolesInOutput multipolesInAtoms
      updatedAtoms = IntMap.intersectionWith (\atom mp -> atom & #multipoles .~ mp) atoms updatedPoles

  -- Check that we did not loose atoms.
  unless (IntMap.keysSet updatedAtoms == atomKeys) . throwM . localExc $
    "Lost atoms during the multipole transfer."

  return $ mol & layerLens % #atoms .~ updatedAtoms
  where
    localExc = MolLogicException "multipoleTransfer"
