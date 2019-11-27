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
  (
  )
where
-- import           Control.Applicative
-- import           Control.Exception.Safe
-- import           Data.Attoparsec.Text.Lazy
-- import           Data.Functor
-- import           Data.Map.Lazy                  ( Map )
-- import qualified Data.Map.Lazy                 as M
-- import qualified Data.Text                     as TS
-- import           Data.Text.Lazy                 ( Text )
-- import qualified Data.Text.Lazy                as T
-- import           Spicy.Generic
-- import           Spicy.Wrapper.Internal.Types.Shallow


{-
----------------------------------------------------------------------------------------------------
{-|
A Psi4 specific parser to get informations from the output file. It parses the input from top to
bottom and uses non consumed 'Text' after a 'Parser' was applied, to remove the 'Text' that has
already been consumed by Attoparsec. This is being supplied to the next parser then, which needs to
search less and less content. This reduces the amount of checks for "search all" parsers.
-}
getResultsFromOutput :: MonadThrow m => Text -> m WrapperOutput
getResultsFromOutput content = do
  -- Parse the echo of the input file and get the method strings from it.
  Continue inputEchoRemain inputEcho <- getInputEcho content
  methodStrings                      <- getMethodStrings inputEcho
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

----------------------------------------------------------------------------------------------------
{-|
Parse the actual calulation tasks from the echo of the input. As Psi4 automatically selects numeric
or analytical derivatives and the parser cannot distinguis from the echo of the input alone, we will
always assume the 'Analytical' version here, but there is no meaning in it.
-}
getMethodStrings :: MonadThrow m => Text -> m [(Task, Text)]
getMethodStrings content = do
  let methodStrings :: Either String [(Task, Text)]
      methodStrings = concat <$> traverse
        (\taskCombi -> eitherResult $ parse (many' $ getMethodStringsParser taskCombi) content)
        [ (return Energy               , "energy")
        , (return $ Gradient Analytical, "gradient")
        , (return $ Hessian Analytical , "hessian")
        , (return $ Hessian Analytical , "frequency")
        , (fail "Cannot handle optimisation outputs of Psi4.", "optimize")
        ]
  case methodStrings of
    Left  e -> throwM $ ParserException e
    Right r -> return r

----------------------------------------------------------------------------------------------------
{-|
This is a generic parser for arbitrary Psithon functions, that takes combinations of 'Task' in
a 'Parser' context and a Psithon function name, that performs this 'Task' (such as @energy@).
The @Parser Task@ is not actually meant to be a parser but provide the monadic context. If a
Psithon task can't be handled (such as @optimize@), it should simply be a @fail e@. If this is
a valid combination, then it should be just a @return Task@.
-}
getMethodStringsParser :: (Parser Task, TS.Text) -> Parser (Task, Text)
getMethodStringsParser (taskP, functionName) = do
  task   <- taskP
  _      <- manyTill anyChar (string $ functionName `TS.append` "(") $> task
  method <- (char '"' <|> char '\'') *> manyTill anyChar (char '"' <|> char '\'')
  return (task, T.pack method)

-}
