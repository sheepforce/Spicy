{-|
Module      : Spicy.Wrapper.Internal.Output.Psi4
Description : Parsers for Psi4
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for Psi4 outputs.
-}
module Spicy.Wrapper.Internal.Output.Psi4
  ( getResultsFromOutput
  , getInputEcho
  )
where
import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text.Lazy
import           Data.Functor
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import           Debug.Trace
import           Spicy.Wrapper.Internal.Types.Shallow
import           Spicy.Generic
import           Control.Exception.Safe

{-|
A Psi4 specific parser to get informations from the output file. It parses the input from top to
bottom and uses non consumed 'Text' after a 'Parser' was applied, to remove the 'Text' that has
already been consumed by Attoparsec. This is being supplied to the next parser then, which needs to
search less and less content. This reduces the amount of checks for "search all" parsers.
-}
getResultsFromOutput :: MonadThrow m => Text -> m WrapperOutput
getResultsFromOutput content = do
  -- Parse the echo of the input file.
  Continue inputEchoRemain inputEcho <- getInputEcho content
  return undefined

----------------------------------------------------------------------------------------------------
{-|
Find the echo of the original input, from which the output file was produced.
-}
getInputEcho :: MonadThrow m => Text -> m (Continue Text)
getInputEcho content = parseMT getInputEchoParser content
 where
  getInputEchoParser :: Parser Text
  getInputEchoParser = do
    _start <-
      manyTill anyChar
      $  string "==> Input File <=="
      <* skipSpace
      <* string "--------------------------------------------------------------------------"
      <* endOfLine
    echoInput <- manyTill anyChar
      $ string "--------------------------------------------------------------------------"
    return . T.pack $ echoInput
