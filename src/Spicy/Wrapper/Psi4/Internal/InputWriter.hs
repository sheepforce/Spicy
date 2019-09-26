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

module Spicy.Wrapper.Psi4.Internal.InputWriter
() where
import Spicy.Wrapper.Internal.InputTypes
import Control.Lens
