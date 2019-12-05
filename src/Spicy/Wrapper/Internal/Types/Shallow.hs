{-|
Module      : Spicy.Wrapper.Internal.Types.Shallow
Description : Simple types, used to modify template input files.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides a container type for the most basic informations, that will be replaced in a
computational chemistry program. This is a "shallow" wrapper to a quantum chemistry program, as
knowledge for the input in the underlying program is required. Therefore a shallow wrapper utilises
Ginger (Jinja syntax) input files for software.

The other version is the "deep" wrapper, where all details of the computational chemistry input are
actually layered in Spicy.
-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Wrapper.Internal.Types.Shallow
  ( -- * Exceptions
    WrapperGenericException(..)
    -- * Shallow Wrapper Input Types
    -- $wrapperInput
  , WrapperInput(..)
  , wrapperInput_Molecule
  , wrapperInput_Task
  , wrapperInput_CalculationInput
  , wrapperInput_Restart
  , wrapperInput_Software
  , wrapperInput_PrefixOutName
  , wrapperInput_PermanentDir
  , wrapperInput_ScratchDir
  , wrapperInput_NProcesses
  , wrapperInput_NThreads
  , wrapperInput_Memory
  , Task(..)
  , _Energy
  , _Gradient
  , _Hessian
  , _Property
  , Software(..)
  , _Psi4
  , NumericalEfficiency(..)
  , _Analytical
  , _Numerical
  , CalculationInput(..)
  , _CalculationInput_QuantumMechanics
  , _CalculationInput_MolecularMechanics
    -- ** Quantum Mechanics
    -- $quantumMechanics
  , QuantumMechanics(..)
  , quantumMechanics_Charge
  , quantumMechanics_Multiplicity
  , quantumMechanics_Method
    -- ** Molecular Mechanics
    -- $molecularMechanics
  , MolecularMechanics(..)
  , molecularMechanics_Molecule
    -- ** Wrapper Output Types
    -- $wrapperOutput
  , WrapperOutput(..)
  , wrapperOutput_Energy
  , wrapperOutput_Gradient
  , wrapperOutput_Hessian
  , wrapperOutput_PCharges
  )
where
import           Control.Exception.Safe
import           Control.Lens
import           Data.Default
import           Data.Sequence                  ( Seq )
import           Data.Text.Lazy                 ( Text )
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
import           System.Path


{-|
Exceptions for a computational chemistry wrapper, that are unspecific to a program.
-}
data WrapperGenericException = WrapperGenericException
  { wgExcFunction    :: String
  , wgExcDescription :: String
  }

instance Show WrapperGenericException where
  show (WrapperGenericException f e) = "WrapperGenericException: " ++ f ++ e

instance Exception WrapperGenericException

{-
####################################################################################################
-}
{- $wrapperInput
Defines a general scheme for input of a general shallow wrapper, may it be a quantum chemistry
programm or a molecular mechanics program.
-}
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
  , _wrapperInput_Restart          :: Maybe AbsFile    -- ^ A 'FilePath' to a file, from which
                                                       --   restart information can be read (e.g.
                                                       --   WF) if already present.
  , _wrapperInput_Software         :: Software         -- ^ The calculation 'Software' to be used
                                                       --   for the calculation.
  , _wrapperInput_PrefixOutName    :: String           -- ^ A prefix name to 'FilePath' to be used
                                                       --   in inputs, which should be saved
                                                       --   somewhere. Consider this somehow the
                                                       --   "project name".
  , _wrapperInput_PermanentDir     :: AbsDir           -- ^ Directory for permanent output files to
                                                       --   be kept.
  , _wrapperInput_ScratchDir       :: AbsDir           -- ^ Directory for scratch files, depending
                                                       --   on the program context shared or
                                                       --   replicated
  , _wrapperInput_NProcesses       :: Int              -- ^ Number of processes (MPI, DDI, whatever)
                                                       --   to launch.
  , _wrapperInput_NThreads         :: Int              -- ^ Number of threads to be launched per
                                                       --   process. Might not have any effect if
                                                       --   the underlying program doesn't support
                                                       --   it.
  , _wrapperInput_Memory           :: Int              -- ^ The memory in MB per process for the
                                                       --   program if applicable.
  }
  deriving ( Eq, Show )

instance Check WrapperInput where
  check input =
    let mol = _wrapperInput_Molecule input
    in  -- Check for QM and MM informations separately
      case _wrapperInput_CalculationInput input of
        CalculationInput_QuantumMechanics qm -> do
          -- Perform independent checks above the QM level of the input.
          let isQmSoftware = (_wrapperInput_Software input) `elem` qmSoftware
          -- Verify all checks.
          if all (== True) [isQmSoftware]
            then do
              -- Check the QM part separately.
              _qm <- checkQM qm mol
              return input
            else throwM $ WrapperGenericException
              "check"
              "Found inconsistency in your wrapper input. The QM part is not passing a check."
        CalculationInput_MolecularMechanics _mm -> return input
    where qmSoftware = [Psi4]

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
A software, which is being wrapped.
-}
data Software
  = Psi4
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

{-
====================================================================================================
-}
{- $quantumMechanics
Input specific for quantum chemistry calculations.
-}
{-|
Input, that needs to be replaced in a shallow wrapper input for quantum chemistry.
-}
data QuantumMechanics = QuantumMechanics
  { _quantumMechanics_Charge       :: Int  -- ^ Charge of the 'Molecule'.
  , _quantumMechanics_Multiplicity :: Int  -- ^ Multiplicity of the 'Molecule'.
  , _quantumMechanics_Method       :: Text -- ^ A program-specific "method-string".
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
'check' like function for the quantum mechanics part of 'WrapperInput'.
-}
checkQM :: MonadThrow m => QuantumMechanics -> Molecule -> m QuantumMechanics
checkQM qm mol =
  let charge               = _quantumMechanics_Charge qm
      multiplicity         = _quantumMechanics_Multiplicity qm
      nElectrons           = getNElectrons mol charge
      maxMultiplicityCheck = nElectrons + 1 >= multiplicity
      multiplicityCheck    = case (even nElectrons, even multiplicity) of
        (True , False) -> True
        (False, True ) -> True
        (_    , _    ) -> False
  in  case (maxMultiplicityCheck, multiplicityCheck) of
        (True, True) -> return qm
        (False, _) ->
          throwM $ WrapperGenericException "check" "Not enough electrons to have this multiplicity."
        (_, False) ->
          throwM $ WrapperGenericException "check" "Invalid combination of multiplicity and charge."

{-
====================================================================================================
-}
{- $molecularMechanics
Input specific for a molecular mechanics program.
-}
{-|
Informations, that need to be replaced in a shallow molecular mechanics wrapper.
-}
data MolecularMechanics = MolecularMechanics
  { _molecularMechanics_Molecule :: Molecule -- ^ The 'Molecule', that will be used as the input.
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
makeLenses ''WrapperInput
makePrisms ''Task
makePrisms ''Software
makePrisms ''NumericalEfficiency
makePrisms ''CalculationInput
makeLenses ''QuantumMechanics
makeLenses ''MolecularMechanics

{-
####################################################################################################
-}
{- $wrapperOutput
Results to be expected from a wrapper calculation.
-}
{-|
A collection of values, that could be obtained from a wrapper calculation.
-}
data WrapperOutput = WrapperOutput
  { _wrapperOutput_Energy   :: Maybe Double             -- ^ Potentially a calculated energy given
                                                        --   in Hartree.
  , _wrapperOutput_Gradient :: Maybe (AccVector Double) -- ^ Potentially a calculated gradient given
                                                        --   in Hartree/Angstrom.
  , _wrapperOutput_Hessian  :: Maybe (AccMatrix Double) -- ^ Potentially a mass-weightes Hessian.
  , _wrapperOutput_PCharges :: Maybe (Seq Double)       -- ^ Potentially partial charges for the
                                                        --   atoms.
  }
  deriving ( Eq, Show )

instance Default WrapperOutput where
  def = WrapperOutput { _wrapperOutput_Energy   = Nothing
                      , _wrapperOutput_Gradient = Nothing
                      , _wrapperOutput_Hessian  = Nothing
                      , _wrapperOutput_PCharges = Nothing
                      }

----------------------------------------------------------------------------------------------------
makeLenses ''WrapperOutput