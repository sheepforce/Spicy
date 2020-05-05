{-|
Module      : Spicy.ONIOM.Collector
Description : Transforming individual information from calculations to an ONIOM result.
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

Collector functions to gather the results from the 'CalcOutput's of a molecule and build the ONIOM
result from it.
-}
module Spicy.ONIOM.Collector
  ( multicentreOniomNCollector
  )
where
import           RIO                     hiding ( (^.)
                                                , view
                                                )
import           Control.Lens
import           Spicy.Class
import qualified Data.IntMap.Strict            as IntMap

{-|
Collector for multicentre ONIOM-N calculations.
-}
multicentreOniomNCollector :: (HasMolecule env, HasInputFile env) => WrapperTask -> RIO env Molecule
multicentreOniomNCollector atomicTask = do
  mol        <- view moleculeL
  _inputFile <- view inputFileL

  molEnergy  <- energyCollector mol
  -- Collect gradient information if requested.
  -- Collect hessian information if requested.

  -- TODO (phillip|p=100|#Unfinished) - Obviously processing needs to be implemented here before returning any molecule.
  return mol

----------------------------------------------------------------------------------------------------
{-|
Collector for the energy of a multicentre ONIOM-n molecule. Can fail if some energies or
electrostatic information are not available. Each layer will get an updated field for
@molecule_EnergyDerivatives . energyDerivatives_Energy@ and will 'Just' contain the energy as if the
corresponding layer would be the real system top layer.

*The idea here works as follows:*

The highest calculation level model system does not contain any
deeper layers. It's energy, if it would be a real system, is simply the energy of the high
calculation level. Therefore, the data from the calculation output of this deepest layers will be
used as the result from this layers and copied to the '_molecule_EnergyDerivatives' field of this
layer. This is the base case of the recursion.

Now the energy of an intermediate layer or the real system depends on the 'Original' calculation
context, the energy of the system below at the given level for the layer below (in
'_molecule_EnergyDerivatives') and the energy of the model layer below this layer on the calculation
niveau of this layer ('Inherited' from calculation output).

The recursion steps from top (real system) through the layers (model system) until it hits the
bottom. When the bottom has been processed, the energy expressions can then be evaluated bottom up
again.

The result is, that the molecule can be treated as series of ONIOM2 multicentre calculations
(current layer as real system) and the layer directly below as model systems.

The energy expression for such a multicentre ONIOM2 calculations is:
\[
E^\mathrm{MC-ONIOM2} = E^\mathrm{real} + \sum\limits_c E^\mathrm{model, high}_c - \sum\limits_c E^\mathrm{model, low}_c
\]
where \(c\) refers to the centre.
-}
-- TODO (phillip|p=100|#Unfinished) - This takes embedding currently not into consideration.
energyCollector :: MonadThrow m => Molecule -> m Molecule
energyCollector mol = do
  let subMols = mol ^. molecule_SubMol

  -- Update this layer of the molecule with its energy, as if this layer would be the real system.
  thisLayerAsReal <- if IntMap.null subMols
    -- If no deeper layers are present, the energy of this layer depends on nothing else and can be
    -- directly taken from the 'Original' calculation output of this layer.
    then thisOriginalEnergyOutputAsRealEnergy mol
    else multiCentreONIOM2Collector mol subMols

  return thisLayerAsReal
 where
  -- For a molecule, that does not contain any deeper layers, the 'Original' calcoutput can be
  -- used as the energy of this system. This function copies the energy from the 'Original'
  -- calcoutput to this layer.
  thisOriginalEnergyOutputAsRealEnergy :: MonadThrow m => Molecule -> m Molecule
  thisOriginalEnergyOutputAsRealEnergy mol' = do
    let maybeOriginalCalcEnergy =
          mol'
            ^? molecule_CalcContext
            .  ix (ONIOMKey Original)
            .  calcContext_Output
            .  _Just
            .  calcOutput_EnergyDerivatives
            .  energyDerivatives_Energy
            .  _Just
    case maybeOriginalCalcEnergy of
      Nothing -> throwM $ MolLogicException
        "energyCollector"
        "Could not find the energy from the original calculation of this layer."
      Just energy ->
        return $ mol' & molecule_EnergyDerivatives . energyDerivatives_Energy ?~ energy

  -- Collects the energy of a local ONIOM2 setup. There is the real system with its 'Original'
  -- calculation output (E(Real)) and the model centres with their high level results
  -- (E(model, high)) in 'molecule_EnergyDerivatives' and their low level results in the 'Inherited'
  -- calculation context.
  multiCentreONIOM2Collector :: MonadThrow m => Molecule -> IntMap Molecule -> m Molecule
  multiCentreONIOM2Collector realMol modelCentres = do
    -- Make sure the model centres have their energy calculated.
    modelCentresWithTheirRealEnergies <- mapM energyCollector modelCentres

    -- Get the set of E(model, high).
    modelCentresHighLevelEnergies     <- do
      let maybeExtractedEnergies = traverse
            (\modelCentre -> modelCentre ^. molecule_EnergyDerivatives . energyDerivatives_Energy)
            modelCentresWithTheirRealEnergies
      case maybeExtractedEnergies of
        Nothing -> throwM $ MolLogicException
          "energyCollector"
          "To calculate the ONIOM energy, the high level energies of all model centres must be\
          \ present but are not."
        Just energies -> return energies

    -- Get the set of E(model, low).
    modelCentresLowLevelEnergies <- do
      let maybeExtractedEnergies = traverse
            (\modelCentre ->
              modelCentre
                ^? molecule_CalcContext
                .  ix (ONIOMKey Inherited)
                .  calcContext_Output
                .  _Just
                .  calcOutput_EnergyDerivatives
                .  energyDerivatives_Energy
                .  _Just
            )
            modelCentresWithTheirRealEnergies
      case maybeExtractedEnergies of
        Nothing -> throwM $ MolLogicException
          "energyCollector"
          "To calculate the ONIOM energy, the low level energies of all model centres must be\
          \ present but are not."
        Just energies -> return energies

    -- Get E(real) (this layers energy) from the original calculation of this layer.
    realSystemEnergy <- do
      let maybeOriginalRealEnergy =
            realMol
              ^? molecule_CalcContext
              .  ix (ONIOMKey Original)
              .  calcContext_Output
              .  _Just
              .  calcOutput_EnergyDerivatives
              .  energyDerivatives_Energy
              .  _Just
      case maybeOriginalRealEnergy of
        Nothing -> throwM $ MolLogicException
          "energyCollector"
          "The energy of the local real system could not be found in the \"Original\" calculation\
          \ context but is required."
        Just energy -> return energy

    -- Calculate the MC-ONIOM2 energy.
    let oniom2Energy =
          realSystemEnergy + sum modelCentresHighLevelEnergies - sum modelCentresLowLevelEnergies

    -- Update this layer molecule with the ONIOM2 energy of this layer and and the submolecules with
    -- their evaluated versions and then return.
    return
      $ realMol
      & (molecule_EnergyDerivatives . energyDerivatives_Energy ?~ oniom2Energy)
      & (molecule_SubMol .~ modelCentresWithTheirRealEnergies)

----------------------------------------------------------------------------------------------------
{-|
Collector for multicentre ONIOM-n gradients. Implemented in the local ONIOM-2 formulation, that is
used by 'energyCollector'. For link atoms the formulation of [A new ONIOM implementation in
Gaussian98. Part I. The calculation of energies, gradients, vibrational frequencies and electric
field derivatives](https://doi.org/10.1016/S0166-1280(98)00475-8) is used, which maintains the
degrees of freedom of the system by distributing the gradient of the link atom to the capped atom
and the host atom is used.
-}
-- TODO (phillip|p=100|#Unfinished) - The effect of embedding is not direclty considered here but potentially must.
gradientCollector :: MonadThrow m => Molecule -> m Molecule
gradientCollector mol = do
  let subMols = mol ^. molecule_SubMol

  -- Update this layer of the molecule with its gradient, as if this layer would be the real system.
  thisLayerAsReal <- if IntMap.null subMols
    then thisOriginalGradientAsRealGradient mol
    else multiCentreONIOM2Collector mol subMols
  return undefined
 where
    -- For a molecule, that does not contain any deeper layers, the 'Original' calcoutput can be
    -- used as the gradient of this system. This function therefore copies the energy from the
    -- 'Original' calcoutput to this layer's 'EnergyDerivatives'.
  thisOriginalGradientAsRealGradient :: MonadThrow m => Molecule -> m Molecule
  thisOriginalGradientAsRealGradient mol' = do
    let maybeOriginalCalcGradient =
          mol'
            ^? molecule_CalcContext
            .  ix (ONIOMKey Original)
            .  calcContext_Output
            .  _Just
            .  calcOutput_EnergyDerivatives
            .  energyDerivatives_Gradient
            .  _Just
    case maybeOriginalCalcGradient of
      Nothing -> throwM $ MolLogicException
        "gradientCollector"
        "Could not find the gradient from the original calculation of this layer."
      Just gradient ->
        return $ mol' & molecule_EnergyDerivatives . energyDerivatives_Gradient ?~ gradient

  multiCentreONIOM2Collector :: MonadThrow m => Molecule -> IntMap Molecule -> m Molecule
  multiCentreONIOM2Collector realMol modelCentres = do
    return undefined
