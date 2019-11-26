{-|
Module      : Spicy.Wrapper.Internal.Output
Description : Parsers for calculation output
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for wrapper outputs to obtain molecular informations, such as energies,
gradients and hessians.
-}
module Spicy.Wrapper.Internal.Output
  ( parseOutput
  )
where
import           Data.Attoparsec.Text.Lazy
import           Spicy.Wrapper.Internal.Types.Shallow

{-|
A parser for energies from Psi4 calculations. This parser tries to be as aware as possible of the
calculation niveau.
-}
parseOutput :: WrapperInput -> Parser WrapperOutput
parseOutput wrapperInput = undefined
