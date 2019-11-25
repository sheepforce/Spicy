{-|
Module      : Spicy.Wrapper.Internal.Input
Description : Provides functionality regarding the manipulation of input to wrapped software.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

A module providing manipulation facilities for input creation and/or manipulation for wrapped
quantum chemistry and molecular mechanics programs.
-}
module Spicy.Wrapper.Internal.Input
  ( WrapperInputException(..)
  )
where
import           Control.Exception.Safe

{-|
Input specific errors.
-}
data WrapperInputException = WrapperInputException
  { wiFunctionName :: String
  , wIDescription  :: String
  }

instance Show WrapperInputException where
  show (WrapperInputException f e) = "WrapperInputException in function \"" ++ f ++ "\":" ++ e

instance Exception WrapperInputException
