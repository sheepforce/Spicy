{-|
Module      : Spicy.Wrapper.Input.Shallow
Description : Provides writers for shallow wrappers to quantum chemistry and molecular mechanics software.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides methods to write input files to quantum chemistry programs by means of shallow
wrappers. That means this wrapper tries to be as agnostic as possible of the underlying program and
simply fills in placeholders into a template file.
-}
module Spicy.Wrapper.Internal.Input.Shallow
  ( translate2Input
  )
where
import           Control.Exception.Safe
import           Control.Lens
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Text                     as TS
import qualified Data.Text.Lazy                as TL
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
import           Spicy.Molecule.Internal.Writer
import           Spicy.Wrapper.Internal.Types.Shallow
import           Text.Ginger

{-|
This function takes a template file and replaces the values. The following context is provided:

  - @molecule@: program specific representation of the molecule (most often cartesian in angstrom)
  - @charge@: the molecular charge
  - @multiplicity@: spin multiplicity of the system
  - @openshells@: the number of open MO shells in an unrestricted calculations
  - @task@: a task string to tell the program which quantity to calculate
-}
translate2Input
  :: (MonadThrow m)
  => TL.Text      -- ^ A template file for the computational chemistry code ('Software') with ginger
                  --   placeholders.
  -> WrapperInput -- ^ Data structure with the relevant informations.
  -> m TL.Text    -- ^ Output file.
translate2Input template rawWrapperInput = do
  -- Check if the input for the wrapper is sane.
  input <- check rawWrapperInput
  -- Build the context for Ginger.
  let qmInput'       = input ^? wrapperInput_CalculationInput . _CalculationInput_QuantumMechanics
      _mmInput'      = input ^? wrapperInput_CalculationInput . _CalculationInput_MolecularMechanics
      charge'        = _quantumMechanics_Charge <$> qmInput'
      multiplicity'  = _quantumMechanics_Multiplicity <$> qmInput'
      nOpenShells'   = (\x -> x - 1) <$> multiplicity'
      restartContext = HM.singleton "restart" $ case input ^. wrapperInput_Restart of
        Just file -> tsShow file
        Nothing   -> "Nothing"
  moleculeContext     <- toMol input
  chargeContext       <- HM.singleton "charge" . tsShow <$> maybe2MonadThrow charge'
  multiplicityContext <- HM.singleton "multiplicity" . tsShow <$> maybe2MonadThrow multiplicity'
  nOpenShellsContext  <- HM.singleton "openshells" . tsShow <$> maybe2MonadThrow nOpenShells'
  taskContext         <- toTask input
  let context =
        moleculeContext
          <> chargeContext
          <> multiplicityContext
          <> nOpenShellsContext
          <> taskContext
          <> restartContext
  -- Parse the Ginger template.
  parsed <- parseGinger (const . return $ Nothing) Nothing (TL.unpack template)
  -- Render the Ginger document with context.
  let gingerResult = textS2L . easyRender context <$> parsed
  case gingerResult of
    Left e ->
      throwM
        $  WrapperGenericException "translate2Input"
        $  "Failed parsing the Ginger template with:"
        ++ show e
    Right r -> return r
 where
  maybe2MonadThrow :: MonadThrow m => Maybe a -> m a
  maybe2MonadThrow Nothing = throwM
    $ WrapperGenericException "translate2Input" "A value wasn't present in the wrapper input."
  maybe2MonadThrow (Just a) = return a


----------------------------------------------------------------------------------------------------
{-|
Converts a 'Molecule' to a program specific representation.
-}
toMol :: MonadThrow m => WrapperInput -> m (HashMap TS.Text TS.Text)
toMol wrapperInput
  | software == Psi4 = angstromXYZ
  | otherwise = throwM
  $ WrapperGenericException "toMol" "Cannot write a moleucle format for the software requested."
 where
  molecule        = wrapperInput ^. wrapperInput_Molecule
  software        = wrapperInput ^. wrapperInput_Software
  angstromXYZBody = textL2S . TL.unlines . drop 2 . TL.lines <$> writeXYZ molecule
  angstromXYZ     = HM.singleton "molecule" <$> angstromXYZBody

----------------------------------------------------------------------------------------------------
{-|
Generates a "task" string specific for computational chemistry software.
-}
toTask :: MonadThrow m => WrapperInput -> m (HashMap TS.Text TS.Text)
toTask wrapperInput
  | software == Psi4 = HM.singleton "task" <$> psi4Task
  | otherwise = throwM
  $ WrapperGenericException "toTask" "Cannot create a task string for the chosen software."
 where
  software = wrapperInput ^. wrapperInput_Software
  task     = wrapperInput ^. wrapperInput_Task
  psi4Task = case task of
    Energy     -> return "energy"
    Gradient _ -> return "gradient"
    Hessian  _ -> return "hessian"
    _          -> throwM $ WrapperGenericException "toTask" "Cannot handle Psi4 property jobs yet."
