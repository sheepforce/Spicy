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
  ()
where
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
import           Control.Exception.Safe
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy                 ( Text )
import           Text.Ginger
import           Control.Monad.IO.Class
import Control.Lens
import Spicy.Wrapper.Psi4.Internal.Generic
import qualified Data.HashMap.Lazy as HM

makeInput :: (MonadThrow m) => Text -> WrapperInput -> m Text
makeInput template input =
  let qmInput = input ^? wrapperInput_CalculationInput . _CalculationInput_QuantumMechanics
  in  case qmInput of
        Nothing -> throwM $ WrapperPsi4Exception "makeInput" "The input is not providing informations for a QM calculation."
        Just qm -> do
          context <- parseGinger (const $ return Nothing) Nothing (T.unpack template)
          return ""
