{-|
Module      : Spicy.Wrapper.Internal.Shallow
Description : Simple type, used to modify template input files.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides a container type for the most basic informations, that will be replaced in a
computational chemistry program. This is a "shallow" wrapper to a quantum chemistry program, as a
lot of knowledge for the input in the underlying program is required.

The other version is the "deep" wrapper, where all details of the computational chemistry input are
actually layered in Spicy.
-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Wrapper.Internal.Shallow
  ( WrapperGenericException(..)
  , WrapperInput(..)
  , wrapperInput_Molecule
  , wrapperInput_Task
  , wrapperInput_CalculationInput
  , Task(..)
  , _Energy
  , _Gradient
  , _Hessian
  , _Property
  , NumericalEfficiency(..)
  , _Analytical
  , _Numerical
  , CalculationInput(..)
  , _CalculationInput_QuantumMechanics
  , _CalculationInput_MolecularMechanics
  , QuantumMechanics(..)
  , quantumMechanics_Charge
  , quantumMechanics_Multiplicity
  , quantumMechanics_WFReference
  , quantumMechanics_Molecule
  , WFReference(..)
  , _RHF
  , _ROHF
  , _UKS
  , MolecularMechanics(..)
  , molecularMechanics_Molecule
  , MMEngine(..)
  )
where
import           Control.Exception.Safe
import           Control.Lens
import           Prelude                 hiding ( cycle
                                                , foldl1
                                                , foldr1
                                                , head
                                                , init
                                                , last
                                                , maximum
                                                , minimum
                                                , tail
                                                , take
                                                , takeWhile
                                                , (!!)
                                                )
import           Spicy.Generic
import           Spicy.Molecule.Internal.Types
import           Spicy.Molecule.Internal.Util

{-|
Exceptions for a computational chemistry wrapper, that are unspecific to a program.
-}
data WrapperGenericException = WrapperGenericException
  { wgExcDescription :: String
  }

instance Show WrapperGenericException where
  show (WrapperGenericException e) = "WrapperGenericException: " ++ e

instance Exception WrapperGenericException

----------------------------------------------------------------------------------------------------
{-|
Contents, that a Spicy wrapper will replace in the input file. First, distinguish between quantum
mechanics and molecular mechanics.
-}
data WrapperInput = WrapperInput
  { _wrapperInput_Molecule         :: Molecule         -- ^ The 'Molecule' on which to perform
                                                       --   calculations. Willonly use the highest
                                                       --   layer in '_molecule_SubMol'.
  , _wrapperInput_Task             :: Task             -- ^ A 'Task' the wrapper needs to perform.
  , _wrapperInput_CalculationInput :: CalculationInput -- ^ Values specific to the calculation
                                                       --   niveau, that need to be replaced.
  }
  deriving ( Eq, Show )

instance Check WrapperInput where
  check input =
    let mol = _wrapperInput_Molecule input
    in  case (_wrapperInput_CalculationInput input) of
          CalculationInput_QuantumMechanics qm -> do
            _qm <- checkQM qm mol
            return input
          CalculationInput_MolecularMechanics _mm -> return input

----------------------------------------------------------------------------------------------------
{-|
A task the wrapper needs to perform.
-}
data Task
  = Energy                       -- ^ Single point energy calculation.
  | Gradient NumericalEfficiency -- ^ Gradient calculation.
  | Hessian  NumericalEfficiency -- ^ Hessian calculation.
  | Property                     -- ^ Property calculation.
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
How efficient a task can be performed. Used for gradient calculations mainly.
-}
data NumericalEfficiency
  = Analytical
  | Numerical
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
The molecule independent parts of a calculation, to be replaced in the input.
-}
data CalculationInput
  = CalculationInput_QuantumMechanics QuantumMechanics
  | CalculationInput_MolecularMechanics MolecularMechanics
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Input, that needs to be replaced in a shallow wrapper input for quantum chemistry.
-}
data QuantumMechanics = QuantumMechanics
  { _quantumMechanics_Charge       :: Int         -- ^ Charge of the 'Molecule'.
  , _quantumMechanics_Multiplicity :: Int         -- ^ Multiplicity of the 'Molecule'.
  , _quantumMechanics_WFReference  :: WFReference -- ^ Single determinant reference type.
  , _quantumMechanics_Molecule     :: Molecule    -- ^ The 'Molecule', which will be put into the
                                                  --   input.
  }
  deriving ( Eq, Show )

{-|
'check' like function for the quantum mechanics part of 'WrapperInput'.
-}
checkQM :: MonadThrow m => QuantumMechanics -> Molecule -> m QuantumMechanics
checkQM qm mol =
  let
    charge               = _quantumMechanics_Charge qm
    multiplicity         = _quantumMechanics_Multiplicity qm
    reference            = _quantumMechanics_WFReference qm
    nElectrons           = getNElectrons mol charge
    maxMultiplicityCheck = if (nElectrons + 1 < multiplicity) then False else True
    wfCheck              = case (even nElectrons, reference) of
      (False, RHF) -> False
      (_    , _  ) -> True
    multiplicityCheck = case (even nElectrons, even multiplicity) of
      (True , False) -> True
      (False, True ) -> True
      (_    , _    ) -> False
  in
    case (maxMultiplicityCheck, wfCheck, multiplicityCheck) of
      (True, True, True) -> return qm
      (False, _, _) ->
        throwM $ WrapperGenericException "Not enough electrons to have this multiplicity."
      (_, False, _) ->
        throwM $ WrapperGenericException "Multiplicity not possible with this wavefunction."
      (_, _, False) ->
        throwM $ WrapperGenericException "Invalid combination of multiplicity and charge."

----------------------------------------------------------------------------------------------------
{-|
Single determinant wavefunction type.
-}
data WFReference
  = RHF  -- ^ Restricted Hartree-Fock/Kohn-Sham DFT.
  | ROHF -- ^ Restricted open shell Hartree-Fock/Kohn-Sham DFT.
  | UKS  -- ^ Unrestricted Hartree-Fock/Kohn-Sham DFT.
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Informations, that need to be replaced in a shallow molecular mechanics wrapper.
-}
data MolecularMechanics = MolecularMechanics
  { _molecularMechanics_Molecule :: Molecule -- ^ The 'Molecule', that will be used as the input.
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Defines a selection of MM programs in the context of force field types.
-}
data MMEngine
  = Tinker
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
makeLenses ''WrapperInput
makePrisms ''Task
makePrisms ''NumericalEfficiency
makePrisms ''CalculationInput
makeLenses ''QuantumMechanics
makePrisms ''WFReference
makeLenses ''MolecularMechanics
