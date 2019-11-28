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
This function takes a template file and replaces the values. There are values which are not always
provided by the wrapper input ('Maybe'). Those values will have a @Nothing@ as a Ginger context and
appear as this in the input file. It is therefore easy to check with the Ginger language against
those values in the template. Contexts which can behave like this will be declared as such.

The following context is provided (@Maybe@ declares contexts, which might not have an actual value
and will apear as @Nothing@ in Ginger.):

  - @molecule@: program specific representation of the molecule (most often cartesian in angstrom).
  - @task@: a task string to tell the program which quantity to calculate.
  - @charge@: @Maybe@ the molecular charge.
  - @multiplicity@: @Maybe@ the spin multiplicity of the system.
  - @openshells@: @Maybe@ the number of open MO shells in an unrestricted calculations.
  - @prefix@: Is a name prefix somehow labeling the calculation. This is usually set by Spicy.
  - @permanent@: A path to a directory where permanent files can be written to. Files that go there
    should be allowed to be written to a shared network file system.
  - @scratch@: A path to a directory where scratch files can be written to.
  - @nprocs@: Number of distributet MPI/DDI/... processes to start.
  - @nthreads@: Number of threads __per__ process.
  - @memory@: Memory assigned to the program in MiB.
  - @restart@: @Maybe@ a path to the restart file.

For example a Psi4 input file could look like this:

@
memory {{ memory }} MiB

molecule Spicy {
{% indent %}
{% indent %}
  {{ charge }} {{ multiplicity }}
  {{ molecule }}
{% endindent %}
{% endindent %}
}

set {
  scf_type df
  reference rhf
  basis cc-pVTZ
  molden_write true
}

{% if restart == \"Nothing\" %}
{{ task }}("omp2")
{% else %}
{{ task }}("omp2", restart_file="{{ restart }}")
{% endif %}
@

The @{% indent %} ... {% endindent %}@ blocks are fully optional but will result in indentation in
the Ginger render result. The @restart@ value is being checked in the Ginger template itself. If
the restart file is a @'Nothing'@ on the Haskell side, the Ginger context @restart@ will be a
string @Nothing@. Only in case on the Haskell side the restart information is a @'Just file'@, the
Ginger context @restart@ will be a filepath, which can be used.
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
  let qmInput       = input ^? wrapperInput_CalculationInput . _CalculationInput_QuantumMechanics
      _mmInput      = input ^? wrapperInput_CalculationInput . _CalculationInput_MolecularMechanics
      charge        = maybeContext $ _quantumMechanics_Charge <$> qmInput
      multiplicity' = _quantumMechanics_Multiplicity <$> qmInput
      multiplicity  = maybeContext multiplicity'
      nOpenShells   = maybeContext $ (\x -> x - 1) <$> multiplicity'
      prefix        = TS.pack $ input ^. wrapperInput_PrefixOutName
      permanentDir  = TS.pack $ input ^. wrapperInput_PermanentDir
      scratchDir    = TS.pack $ input ^. wrapperInput_ScratchDir
      nProcesses    = textL2S . tShow $ input ^. wrapperInput_NProcesses
      nThreads      = textL2S . tShow $ input ^. wrapperInput_NThreads
      memory        = textL2S . tShow $ input ^. wrapperInput_Memory
      restart       = case input ^. wrapperInput_Restart of
        Just file -> TS.pack file
        Nothing   -> "Nothing"
  molecule <- toMol input
  task     <- toTask input
  let context =
        HM.fromList
          [ ("molecule"   , molecule)
          , ("task"       , task)
          , ("charge"     , charge)
          , ("muliplicity", multiplicity)
          , ("nopenshells", nOpenShells)
          , ("prefix"     , prefix)
          , ("permanent"  , permanentDir)
          , ("scratch"    , scratchDir)
          , ("nprocs"     , nProcesses)
          , ("nthreads"   , nThreads)
          , ("memory"     , memory)
          , ("restart"    , restart)
          ] :: HashMap TS.Text TS.Text
  -- Parse the Ginger template.
  parsed <- parseGinger (const . return $ Nothing) Nothing (TL.unpack template)
  -- Render the Ginger document with context.
  let gingerResult = textS2L . easyRender context <$> parsed
  case gingerResult of
    Left e ->
      throwM
        $  WrapperGenericException "translate2Input"
        $  "Failed parsing the Ginger template with: "
        ++ show e
    Right r -> return r
 where
  maybeContext :: (Show a) => Maybe a -> TS.Text
  maybeContext mVal = case mVal of
    Just val -> textL2S . tShow $ val
    Nothing  -> "Nothing"


----------------------------------------------------------------------------------------------------
{-|
Converts a 'Molecule' to a program specific representation.
-}
toMol :: MonadThrow m => WrapperInput -> m TS.Text
toMol wrapperInput
  | software == Psi4 = angstromXYZ
  | otherwise = throwM
  $ WrapperGenericException "toMol" "Cannot write a moleucle format for the software requested."
 where
  molecule    = wrapperInput ^. wrapperInput_Molecule
  software    = wrapperInput ^. wrapperInput_Software
  angstromXYZ = textL2S . TL.unlines . drop 2 . TL.lines <$> writeXYZ molecule

----------------------------------------------------------------------------------------------------
{-|
Generates a "task" string specific for computational chemistry software.
-}
toTask :: MonadThrow m => WrapperInput -> m TS.Text
toTask wrapperInput
  | software == Psi4 = psi4Task
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
