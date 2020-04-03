{-|
Module      : Spicy.Wrapper
Description : Abstract routines to perform calculations with external software.
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

Abstract methods to perform computational chemistry tasks by calling external programs.
-}
module Spicy.Wrapper
  ( runAllCalculations
  , runCalculation
  )
where
import           Spicy.Wrapper.Internal.Executor
