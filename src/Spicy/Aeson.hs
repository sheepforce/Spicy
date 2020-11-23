-- |
-- Module      : Spicy.Aeson
-- Description : Settings for JSON instance derivations.
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides settings to overwrite the Aeson JSON fields.
module Spicy.Aeson
  ( spicyJOption,
  )
where

import Data.Aeson.TH
import RIO
import qualified RIO.Char as Char

-- |
-- Aeson options for nice parsing of JSON/YAML fields in combination with nice ability to still create
-- lenses.
spicyJOption :: Options
spicyJOption =
  defaultOptions
    { constructorTagModifier = map Char.toLower,
      omitNothingFields = True
    }
