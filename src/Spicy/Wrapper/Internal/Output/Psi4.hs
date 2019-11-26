{-|
Module      : Spicy.Wrapper.Internal.Output.Psi4
Description : Parsers for calculation output
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for Psi4 outputs to obtain molecular informations, such as energies,
gradients and hessians.
-}
module Spicy.Wrapper.Internal.Output.Psi4
  ( getEnergy
  )
where
import           Data.Attoparsec.Text.Lazy

{-|
A parser for energies from Psi4 calculations. This parser tries to be as aware as possible of the
calculation niveau.
-}
getEnergy :: Parser Double
getEnergy = do
  -- Look for the input specification and find out which calculation niveau this was.
  _ <- manyTill anyChar $ string "==> Input File <=="

  return undefined
