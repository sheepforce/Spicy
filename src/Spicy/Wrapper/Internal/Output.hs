{-|
Module      : Spicy.Wrapper.Internal.Output
Description : Parsers for calculation output
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for wrapper outputs to obtain molecular informations, such as energies,
gradients and hessians.
-}
module Spicy.Wrapper.Internal.Output
  ( --getResultsFromOutput
  )
where
{-
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Lens
import           Data.Attoparsec.Text.Lazy
import           Data.Functor
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import           Debug.Trace
import           Spicy.Generic
import           Spicy.Wrapper.Internal.Types.Shallow
-}

{-|
A parser for energies from Psi4 calculations. This parser tries to be as aware as possible of the
calculation niveau.
-}
{-
getResultsFromOutput :: MonadThrow m => Text -> WrapperInput -> m WrapperOutput
getResultsFromOutput content wrapperInput
  | software == Psi4 = undefined
  | otherwise        = fail "No parser available for chosen software."
  where software = wrapperInput ^. wrapperInput_Software
-}
