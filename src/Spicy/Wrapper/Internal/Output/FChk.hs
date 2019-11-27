{-|
Module      : Spicy.Wrapper.Internal.Output.FChk
Description : Parsers for Gaussian FChk files.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for FChk outputs.
-}
module Spicy.Wrapper.Internal.Output.FChk
  (
  )
where
import           Control.Applicative
import           Control.Exception.Safe
import           Data.Attoparsec.Text.Lazy
import           Data.Functor
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import qualified Data.Text                     as TS
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import           Spicy.Generic
import           Spicy.Wrapper.Internal.Types.Shallow

getResultsFromFChk :: MonadThrow m => Text -> m WrapperOutput
getResultsFromFChk = undefined
