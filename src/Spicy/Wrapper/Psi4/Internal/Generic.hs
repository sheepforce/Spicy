{-|
Module      : Spicy.Wrapper.Psi4.Internal.Generic
Description : Shared functions of the Psi4 wrapper
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides commong functions for the Psi4 wrapper.
-}
module Spicy.Wrapper.Psi4.Internal.Generic
  ( WrapperPsi4Exception(..)
   -- * Mapper
   -- $mapper
  , molecule2HashMap
  )
where
import           Control.Exception.Safe
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Text                     as TS
import           Spicy.Generic
import           Spicy.Molecule.Internal.Types
import           Spicy.Molecule.Internal.Writer

{-|
Psi4 specific exceptions.
-}
data WrapperPsi4Exception = WrapperPsi4Exception
  { wPsi4ExcFunctionName :: String
  , wPsi4Description     :: String
  }

instance Show WrapperPsi4Exception where
  show (WrapperPsi4Exception f e) = "WrapperPsi4Exception in function \"" ++ f ++ "\":" ++ e

instance Exception WrapperPsi4Exception

{-
####################################################################################################
-}
{- $mapper
Ginger/Psi4 specific translations.
-}
{-|
Convert a 'Molecule' to its Psi4-appropiate 'Text' representation and put it in a 'HashMap'.
-}
molecule2HashMap :: MonadThrow m => Molecule -> m (HashMap TS.Text TS.Text)
molecule2HashMap mol = do
  xyzText <- writeXYZ mol
  let molRep = TS.unlines . drop 2 . TS.lines . textL2S $ xyzText
  return $ HM.singleton "molecule" molRep
