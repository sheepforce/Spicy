{-|
Module      : Spicy.Wrapper.Psi4.Internal.InputWriter
Description : Generator for Psi4 input files
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides a translator from 'Wrapper' to an actual Psi4 input file. The translation is
carefully checked to be sensible and actually doable by Psi4.
-}

module Spicy.Wrapper.Psi4.Internal.ShallowWriter
  ( makeInput
  )
where
import           Control.Exception.Safe
import qualified Data.Text                     as TS
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
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
import           Spicy.Wrapper.Internal.Shallow
import           Text.Ginger
import           Control.Lens
import qualified Data.HashMap.Lazy             as HM
import           Spicy.Generic
import           Spicy.Wrapper.Psi4.Internal.Generic

{-|
Takes a template input and replaces its placeholders with Ginger.
-}
makeInput :: (MonadThrow m) => Text -> WrapperInput -> m Text
makeInput psiTemplate wrapperInput = do
  -- Get the quantum chemistry wrapper informations and convert it to MonadThrow.
  let qmInputCandidate =
        wrapperInput ^? wrapperInput_CalculationInput . _CalculationInput_QuantumMechanics
      qmInput' = case qmInputCandidate of
        Nothing -> throwM $ WrapperPsi4Exception
          "makeInput"
          "The input is not providing informations for a QM calculation."
        Just qm -> return qm
  qmInput <- qmInput'
  -- Build the context for Ginger from Wrapper informations.
  mol     <- molecule2HashMap $ wrapperInput ^. wrapperInput_Molecule
  let spinMult = quantumMechanics2HashMap qmInput
      wfFile   = HM.singleton "restartFile" (TS.pack $ wrapperInput ^. wrapperInput_Restart)
      context  = mol <> spinMult <> wfFile
  -- Parse the template input file for Ginger.
  template <- parseGinger (const $ return Nothing) Nothing (T.unpack psiTemplate)
  -- Generate the new file.
  case template of
    Left parseError -> throwM $ WrapperPsi4Exception
      "makeInput"
      ("Ginger could not parse the Psi4 input file template with: " ++ show parseError)
    Right t -> return . textS2L $ easyRender context t
