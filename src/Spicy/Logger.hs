-- |
-- Module      : Spicy.Logger
-- Description : Custom logging facilities built on top of RIO
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module implements the styling of Spicy's logging functions. It is built on top of RIO's logger
-- and it's ideas, but uses a custom Options type, as the original one is too deeply hidden in RIO.
module Spicy.Logger
  ( -- * Printing & Human Readable
    formatPadFoldable,
    molID2OniomHumandID,
    map2Human,
  )
where

import Data.Foldable
import Data.List.Split
import Formatting
import RIO hiding (view)
import qualified RIO.Map as Map
import qualified RIO.Partial as RIO'
import qualified RIO.Seq as Seq
import qualified RIO.Text as Text
import Spicy.Common
import Spicy.Molecule

-- | The maximum width in characters a message might be wide.
maxWidth :: Int
maxWidth = 100

-- |
-- For logging purposes build lines of formatted values. Custom formatters for the values and custom
-- delimiters can be specified. This function will try to obey the maximum width of the log messages
-- but at least one value per line will be printed.
formatPadFoldable ::
  (Functor t, Foldable t) =>
  -- | Width of a single field in a line of values.
  Int ->
  -- | A formatter for the values in the structure.
  Format Text (a -> Text) ->
  -- | A delimiter of the fields.
  Text ->
  -- | Structure holding the values.
  t a ->
  [Utf8Builder]
formatPadFoldable padSize formatter delimiter values =
  fmap (text2Utf8Builder . Text.concat)
    . chunksOf nChunks
    . toList
    . fmap (\v -> sformat (left padSize ' ' %. formatter) v <> sformat stext delimiter)
    $ values
  where
    nChunks = max (maxWidth `div` (padSize + delimiterWidth)) 1
    delimiterWidth = Text.length delimiter

----------------------------------------------------------------------------------------------------

-- |
-- Translation of a 'MolID' to a more human readable text. Valid in the context of ONIOM calculations.
molID2OniomHumandID :: MolID -> Utf8Builder
molID2OniomHumandID Seq.Empty = "0"
molID2OniomHumandID molID =
  let depth = Seq.length molID
      idTree =
        foldr'
          ( \currentID textAcc ->
              let offSet = fromEnum 'A'
                  idLetter = RIO'.toEnum $ currentID + offSet
               in textAcc `Text.snoc` idLetter
          )
          (tShow depth)
          molID
   in display idTree

----------------------------------------------------------------------------------------------------

-- |
-- Print a 'Map' in a nice form, every entry as its own line. Does not respect the maximum line length.
map2Human :: (Show a, Show b) => Map a b -> [Utf8Builder]
map2Human dataMap =
  let associations = Map.toList dataMap
      keys = fst <$> associations
      values = snd <$> associations
      keysText = tShow <$> keys
      valuesText = tShow <$> values
      maxKeysLength = maximum $ fmap Text.length keysText
      keysFormatted = sformat ((left maxKeysLength ' ' %. stext) % " = ") <$> keysText
      keyValuesText = zipWith (<>) keysFormatted valuesText
   in display <$> keyValuesText
