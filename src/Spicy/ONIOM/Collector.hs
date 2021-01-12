-- |
-- Module      : Spicy.ONIOM.Collector
-- Description : Transforming individual information from calculations to an ONIOM result.
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Collector functions to gather the results from the 'CalcOutput's of a molecule and build the ONIOM
-- result from it.
module Spicy.ONIOM.Collector
  ( multicentreOniomNCollector,
    energyCollector,
    gradientCollector,
    hessianCollector,
    multipoleTransferCollector,
    multipoleTransfer
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
    (^?),
  )
import Spicy.Common
import Spicy.InputFile
import Spicy.Molecule hiding (S)

-- |
-- Collector for multicentre ONIOM-N calculations.
multicentreOniomNCollector :: (HasMolecule env, HasInputFile env) => WrapperTask -> RIO env Molecule
multicentreOniomNCollector atomicTask = do
  mol <- view moleculeL
  _inputFile <- view inputFileL

  -- TODO (phillip|p=100|#Unfinished) - A multipole collector needs to run first to get embedding.
  molEnergy <- energyCollector mol
  molGradient <- case atomicTask of
    WTGradient -> gradientCollector molEnergy
    _ -> return molEnergy
  molHessian <- case atomicTask of
    WTHessian -> hessianCollector molGradient
    _ -> return molGradient

  return molHessian

----------------------------------------------------------------------------------------------------

-- | __Use the 'multicentreOniomNCollector', which calls this collector.__
--
-- Collector for the energy of a multicentre ONIOM-n molecule. Can fail if some energies or
-- electrostatic information are not available. Each layer will get an updated field for
-- @molecule_EnergyDerivatives . energyDerivatives_Energy@ and will 'Just' contain the energy as if the
-- corresponding layer would be the real system top layer.
--
-- /The idea here works as follows:/
-- The highest calculation level model system does not contain any
-- deeper layers. It's energy, if it would be a real system, is simply the energy of the high
-- calculation level. Therefore, the data from the calculation output of this deepest layers will be
-- used as the result from this layers and copied to the '_molecule_EnergyDerivatives' field of this
-- layer. This is the base case of the recursion.
--
-- Now the energy of an intermediate layer or the real system depends on the 'Original' calculation
-- context, the energy of the system below at the given level for the layer below (in
-- '_molecule_EnergyDerivatives') and the energy of the model layer below this layer on the calculation
-- niveau of this layer ('Inherited' from calculation output).
--
-- The recursion steps from top (real system) through the layers (model system) until it hits the
-- bottom. When the bottom has been processed, the energy expressions can then be evaluated bottom up
-- again.
--
-- The result is, that the molecule can be treated as series of ONIOM2 multicentre calculations
-- (current layer as real system) and the layer directly below as model systems.
--
-- The energy expression for such a multicentre ONIOM2 calculations is:
--
-- \[
--     E^\mathrm{MC-ONIOM2} = E^\mathrm{real} + \sum\limits_c E^\mathrm{model, high}_c - \sum\limits_c E^\mathrm{model, low}_c
-- \]
--
-- where \(c\) refers to the centre.

-- TODO (phillip|p=100|#Unfinished) - This takes embedding currently not into consideration.
energyCollector :: MonadThrow m => Molecule -> m Molecule
energyCollector mol = do
  let subMols = mol ^. #subMol

  -- Update this layer of the molecule with its energy, as if this layer would be the real system.
  thisLayerAsReal <-
    if IntMap.null subMols
      then -- If no deeper layers are present, the energy of this layer depends on nothing else and can be
      -- directly taken from the 'Original' calculation output of this layer.
        thisOriginalEnergyOutputAsRealEnergy mol
      else multiCentreONIOM2Collector mol subMols

  return thisLayerAsReal
  where
    -- For a molecule, that does not contain any deeper layers, the 'Original' calcoutput can be
    -- used as the energy of this system. This function copies the energy from the 'Original'
    -- calcoutput to this layer.
    thisOriginalEnergyOutputAsRealEnergy :: MonadThrow m => Molecule -> m Molecule
    thisOriginalEnergyOutputAsRealEnergy mol' = do
      energy <-
        maybe2MThrow
          ( MolLogicException
              "energyCollector"
              "The original calculation output for a layer does not exist or provides no energy."
          )
          $ mol'
            ^? #calcContext
              % ix (ONIOMKey Original)
              % #output
              % _Just
              % #energyDerivatives
              % #energy
              % _Just
      return $ mol' & #energyDerivatives % #energy ?~ energy

    -- Collects the energy of a local ONIOM2 setup. There is the real system with its 'Original'
    -- calculation output (E(Real)) and the model centres with their high level results
    -- (E(model, high)) in '_molecule_EnergyDerivatives' and their low level results in the 'Inherited'
    -- calculation context.
    multiCentreONIOM2Collector :: MonadThrow m => Molecule -> IntMap Molecule -> m Molecule
    multiCentreONIOM2Collector realMol modelCentres = do
      -- Make sure the model centres have their energies already collected.
      modelCentresWithTheirRealEnergies <- mapM energyCollector modelCentres

      -- Get the set of E(model, high).
      modelCentresHighLevelEnergies <-
        maybe2MThrow
          ( MolLogicException
              "energyCollector"
              "To calculate the ONIOM energy, the high level energies of all model centres must be\
              \ present but are not."
          )
          $ traverse
            (\modelCentre -> modelCentre ^. #energyDerivatives % #energy)
            modelCentresWithTheirRealEnergies

      -- Get the set of E(model, low).
      modelCentresLowLevelEnergies <-
        maybe2MThrow
          ( MolLogicException
              "energyCollector"
              "To calculate the ONIOM energy, the low level energies of all model centres must be\
              \ present but are not."
          )
          $ traverse
            ( \modelCentre ->
                modelCentre
                  ^? #calcContext
                    % ix (ONIOMKey Inherited)
                    % #output
                    % _Just
                    % #energyDerivatives
                    % #energy
                    % _Just
            )
            modelCentresWithTheirRealEnergies

      -- Get E(real) (this layers energy) from the original calculation of this layer.
      realSystemEnergy <-
        maybe2MThrow
          ( MolLogicException
              "energyCollector"
              "The energy of the local real system could not be found in the \"Original\" calculation\
              \ context but is required."
          )
          $ realMol
            ^? #calcContext
              % ix (ONIOMKey Original)
              % #output
              % _Just
              % #energyDerivatives
              % #energy
              % _Just

      -- Calculate the MC-ONIOM2 energy.
      let oniom2Energy =
            realSystemEnergy + sum modelCentresHighLevelEnergies - sum modelCentresLowLevelEnergies

      -- Update this layer molecule with the ONIOM2 energy of this layer and the submolecules with
      -- their evaluated versions and then return.
      return $
        realMol
          & (#energyDerivatives % #energy ?~ oniom2Energy)
          & (#subMol .~ modelCentresWithTheirRealEnergies)

----------------------------------------------------------------------------------------------------

-- |
-- __Use the 'multicentreOniomNCollector', which calls this collector.__
--
-- Collector for multicentre ONIOM-n gradients. Implemented in the local ONIOM-2 formulation, that is
-- used by 'energyCollector'. For link atoms the formulation of [A new ONIOM implementation in
-- Gaussian98. Part I. The calculation of energies, gradients, vibrational frequencies and electric
-- field derivatives](https://doi.org/10.1016/S0166-1280\(98\)00475-8) is used, which maintains the
-- degrees of freedom of the system by distributing the gradient of the link atom to the capped atom
-- and the host atom is used.
--
-- The gradient expression for such a multicentre ONIOM2 calculation is:
--
-- \[
--     \nabla E^\mathrm{MC-ONIOM2} = E^\mathrm{real} + \sum\limits_c \nabla E_c^\mathrm{model, high} \mathbf{J}_c - \sum\limits_c \nabla E_c^\mathrm{model, low} \mathbf{J}_c
-- \]
--
-- For the definition of the Jacobian matrix, see 'getJacobian'.

-- TODO (phillip|p=100|#Unfinished) - The effect of embedding is not direclty considered here but potentially must.
gradientCollector :: MonadThrow m => Molecule -> m Molecule
gradientCollector mol = do
  let subMols = mol ^. #subMol

  -- Update this layer of the molecule with its gradient, as if this layer would be the real system.
  thisLayerAsReal <-
    if IntMap.null subMols
      then thisOriginalGradientAsRealGradient mol
      else multiCentreONIOM2Collector mol subMols

  return thisLayerAsReal
  where
    -- For a molecule, that does not contain any deeper layers, the 'Original' calcoutput can be
    -- used as the gradient of this system. This function therefore copies the gradient from the
    -- 'Original' calcoutput to this layer's 'EnergyDerivatives'.
    thisOriginalGradientAsRealGradient :: MonadThrow m => Molecule -> m Molecule
    thisOriginalGradientAsRealGradient mol' = do
      originalGradient <-
        maybe2MThrow
          ( MolLogicException
              "gradientCollector"
              "Could not find the gradient from the original calculation of this layer."
          )
          $ mol'
            ^? #calcContext
              % ix (ONIOMKey Original)
              % #output
              % _Just
              % #energyDerivatives
              % #gradient
              % _Just
      return $ mol' & #energyDerivatives % #gradient ?~ originalGradient

    -- Collects the gradients of a local ONIOM2 setup. There is the real system with its 'Original'
    -- calculation output (∇E(real)) and the model centres with their high level results
    -- (∇E(model, high)) in '_molecule_EnergyDerivatives' and their low level results in the
    -- 'Inherited' calculation context.
    multiCentreONIOM2Collector :: MonadThrow m => Molecule -> IntMap Molecule -> m Molecule
    multiCentreONIOM2Collector realMol modelCentres = do
      -- Make sure the model centres have their gradients already collected.
      modelCentresWithTheirRealGradients <- mapM gradientCollector modelCentres

      -- Get ∇E(real) (this layers gradient) from the original calculation of this layer.
      realSystemGradient <-
        maybe2MThrow
          ( MolLogicException
              "gradientCollector"
              "The original calculation output of a layer did not provide gradient information."
          )
          $ getVectorS
            <$> realMol
            ^? #calcContext
              % ix (ONIOMKey Original)
              % #output
              % _Just
              % #energyDerivatives
              % #gradient
              % _Just

      -- Get the set of ∇E(model, low)*J.
      modelCentresWithTransformedLowLevelGradients <-
        maybe2MThrow
          ( MolLogicException
              "gradientCollector"
              "While calculating the transformed low level gradient of a model system, something went\
              \ wrong. This can be caused by a missing low level gradient for the model system, a\
              \ missing Jacobian for the model system, or a dimension mismatch in the Jacobian and\
              \ the gradient."
          )
          $ traverse
            ( \modelCentre -> do
                lowOutputGradient <-
                  modelCentre
                    ^? #calcContext
                      % ix (ONIOMKey Inherited)
                      % #output
                      % _Just
                      % #energyDerivatives
                      % #gradient
                      % _Just
                -- To be able to use matrix-matrix multiplication, this formally transforms the
                -- gradient vector to a 1x3M matrix, which again is a row vector.
                let outputGradientAsRowVec =
                      Massiv.computeAs Massiv.S
                        . Massiv.setComp Par
                        . Massiv.expandOuter (Sz 1) const
                        . getVectorS
                        $ lowOutputGradient
                jacobian <- getMatrixS <$> modelCentre ^. #jacobian
                -- Calculate the transformed gradient.
                transformedGradient <- outputGradientAsRowVec .><. jacobian
                transformedGradient !?> 0
            )
            modelCentresWithTheirRealGradients

      -- Get the set of ∇E(mode, high)*J
      modelCentresWithTransformedHighLevelGradient <-
        maybe2MThrow
          ( MolLogicException
              "gradientCollector"
              "While calculating the transformed high level gradient of a model system, something\
              \ went wrong. This can be caused by a missing high level gradient for the model system,\
              \ a missing Jacobian for the model system, or a dimension mismatch in the Jacobian and\
              \ the gradient."
          )
          $ traverse
            ( \modelCentre -> do
                extractedGradient <-
                  modelCentre ^. #energyDerivatives % #gradient
                let extractedGradientAsRowVec =
                      Massiv.computeAs Massiv.S
                        . Massiv.setComp Par
                        . Massiv.expandOuter (Sz 1) const
                        . getVectorS
                        $ extractedGradient
                jacobian <- getMatrixS <$> modelCentre ^. #jacobian
                transformedGradient <- extractedGradientAsRowVec .><. jacobian
                transformedGradient !?> 0
            )
            modelCentresWithTheirRealGradients

      -- Sum up the transformed the groups of model gradients.
      let gradientSize = Massiv.size realSystemGradient
          zeroGradient :: Vector Massiv.S Double
          zeroGradient = Massiv.makeArray Par gradientSize (const 0)
      modelHighLevelGradients <-
        foldl'
          (\sumAcc g -> sumAcc >>= (.+. Massiv.delay g))
          (return . Massiv.delay $ zeroGradient)
          modelCentresWithTransformedHighLevelGradient
      modelLowLevelGradients <-
        foldl'
          (\sumAcc g -> sumAcc >>= (.+. Massiv.delay g))
          (return . Massiv.delay $ zeroGradient)
          modelCentresWithTransformedLowLevelGradients

      -- Calculate the MC-ONIOM2 gradient.
      oniom2GradientD <-
        Massiv.delay realSystemGradient .-. modelLowLevelGradients >>= (.+. modelHighLevelGradients)
      let oniom2Gradient = VectorS . Massiv.compute . Massiv.setComp Par $ oniom2GradientD

      -- Update this layer molecule with the ONIOM2 gradient of this layer and the submolecule with
      -- their evaluated versions and return.
      return $
        realMol
          & (#energyDerivatives % #gradient ?~ oniom2Gradient)
          & (#subMol .~ modelCentresWithTheirRealGradients)

----------------------------------------------------------------------------------------------------

-- |
-- __Use the 'multicentreOniomNCollector', which calls this collector.__
--
-- Multicentre ONIOMn collector for the hessian. Implemented in a local ONIOM-2 formulation, that is
-- also used by 'energyCollector'. For link atoms, the formulation of [A new ONIOM implementation in
-- Gaussian98. Part I. The calculation of energies, gradients, vibrational frequencies and electric
-- field derivatives](https://doi.org/10.1016/S0166-1280\(98\)00475-8) is used, which maintains the
-- degrees of freedom of the system by distributing the gradient of the link atom to the capped atom
-- and the host atom is used.
--
-- The hessian expression for such a multicentre ONIOM2 calculation is:
--
-- \[
--     \mathbf{H}^\mathrm{MC-ONIOM2} = \mathbf{H}^\mathrm{real}
--                                   - \sum\limits_c \mathbf{J}_c^T \mathbf{H}_c^\mathrm{model, low} \mathbf{J}_c
--                                   + \sum\limits_c \mathbf{J}_c^T \mathbf{H}_c^\mathrm{model, high} \mathbf{J}_c
-- \]
--
-- For the definition of the Jacobian matrix, see 'getJacobian'.
hessianCollector :: MonadThrow m => Molecule -> m Molecule
hessianCollector mol = do
  let subMols = mol ^. #subMol

  -- Update this layer of the molecule with its hessian, af if this layer would be the real system.
  thisLayerAsReal <-
    if IntMap.null subMols
      then thisOriginalHessianAsRealHessian mol
      else multiCentreONIOM2Collector mol subMols

  return thisLayerAsReal
  where
    -- For a molecule, that does not contain any deeper layers, the 'Original' calcoutput can be used
    -- as the gradient for this system. This function therefore copies the hessian from the 'Original'
    -- calcoutput to this layers 'EnergyDerivatives'.
    thisOriginalHessianAsRealHessian :: MonadThrow m => Molecule -> m Molecule
    thisOriginalHessianAsRealHessian mol' = do
      originalCalcHessian <-
        maybe2MThrow
          ( MolLogicException
              "hessianCollector"
              "Could not find the hessian from the original calcoutput of this layer."
          )
          $ mol'
            ^? #calcContext
              % ix (ONIOMKey Original)
              % #output
              % _Just
              % #energyDerivatives
              % #hessian
              % _Just

      return $ mol & #energyDerivatives % #hessian ?~ originalCalcHessian

    multiCentreONIOM2Collector :: MonadThrow m => Molecule -> IntMap Molecule -> m Molecule
    multiCentreONIOM2Collector realMol modelCentres = do
      -- Make sure the model centres have their hessians already collected.
      modelCentresWithTheirRealHessians <- mapM hessianCollector modelCentres

      -- Get H(real) (this layer's hessian) from the original calcoutput.
      realSystemHessian <-
        maybe2MThrow
          ( MolLogicException
              "hessianCollector"
              "The hessian of the local real system could not be found in the \"Original\" calculation\
              \ context, but is required."
          )
          $ getMatrixS
            <$> realMol
            ^? #calcContext
              % ix (ONIOMKey Original)
              % #output
              % _Just
              % #energyDerivatives
              % #hessian
              % _Just

      -- Get the set of J^T H(model, low) J.
      modelCentresWithTransformedLowLevelHessian <-
        maybe2MThrow
          ( MolLogicException
              "hessianCollector"
              "While calculating the transformed low level hessian of a model system, something went\
              \ wrong. This can be cause by a missing low level hessian for the model system, a\
              \ missing Jacobian for the model system, or a dimension mismatch in the Jacobian and\
              \ the Hessian."
          )
          $ traverse
            ( \modelCentre ->
                let lowOutputHessian =
                      getMatrixS
                        <$> modelCentre
                        ^? #calcContext
                          % ix (ONIOMKey Inherited)
                          % #output
                          % _Just
                          % #energyDerivatives
                          % #hessian
                          % _Just
                    jacobian = getMatrixS <$> modelCentre ^. #jacobian
                    jacobianT =
                      Massiv.computeAs Massiv.S . Massiv.setComp Par . Massiv.transpose <$> jacobian
                    transformedHessian = do
                      jT <- jacobianT
                      j <- jacobian
                      h <- lowOutputHessian
                      (Massiv.computeAs S . Massiv.setComp Par <$> jT .><. h) >>= (.><. j)
                 in transformedHessian
            )
            modelCentresWithTheirRealHessians

      -- Get the set of J^T H(model, low) J.
      modelCentresWithTransformedHighLevelHessian <-
        maybe2MThrow
          ( MolLogicException
              "hessianCollector"
              "While calculating the transformed high level hessian of a model system, something went\
              \ wrong. This can be cause by a missing high level hessian for the model system, a\
              \ missing Jacobian for the model system, or a dimension mismatch in the Jacobian and\
              \ the Hessian."
          )
          $ traverse
            ( \modelCentre ->
                let maybeExtractedHessian =
                      getMatrixS
                        <$> modelCentre
                        ^. #energyDerivatives
                          % #hessian
                    jacobian = getMatrixS <$> modelCentre ^. #jacobian
                    jacobianT =
                      Massiv.computeAs S . Massiv.setComp Par . Massiv.transpose <$> jacobian
                    transformedHessian = do
                      jT <- jacobianT
                      j <- jacobian
                      h <- maybeExtractedHessian
                      (Massiv.computeAs S . Massiv.setComp Par <$> jT .><. h) >>= (.><. j)
                 in transformedHessian
            )
            modelCentresWithTheirRealHessians

      -- Sum up the transformed groups of model hessians.
      let hessianSize = Massiv.size realSystemHessian
          zeroHessian :: Matrix Massiv.S Double
          zeroHessian = Massiv.makeArray Par hessianSize (const 0)
      modelHighLevelHessian <-
        foldl'
          (\sumAcc h -> sumAcc >>= (.+. Massiv.delay h))
          (return . Massiv.delay $ zeroHessian)
          modelCentresWithTransformedHighLevelHessian
      modelLowLevelHessian <-
        foldl'
          (\sumAcc h -> sumAcc >>= (.+. Massiv.delay h))
          (return . Massiv.delay $ zeroHessian)
          modelCentresWithTransformedLowLevelHessian

      -- Calculate the MC-ONIOM2 gradient.
      oniom2HessianD <-
        Massiv.delay realSystemHessian .-. modelLowLevelHessian >>= (.+. modelHighLevelHessian)
      let oniom2Hessian = MatrixS . Massiv.compute . Massiv.setComp Par $ oniom2HessianD

      -- Update this layer molecule with the hessian of this layer and the submolecule with their
      -- evaluated versions and return.
      return $
        realMol
          & (#energyDerivatives % #hessian ?~ oniom2Hessian)
          & (#subMol .~ modelCentresWithTheirRealHessians)

----------------------------------------------------------------------------------------------------

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
              (\a -> not $ a ^. #isDummy || (isAtomLink $ a ^. #isLink))
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
      calcK = calcID ^. #calcKey
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
