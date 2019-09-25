{-|
Module      : Spicy.Generic
Description : Common data types and functions
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jenVS.de
Stability   : experimental
Portability : POSIX, Windows

Definition of generic functions, agnostic of a Spicy specific data type, and generic classes.
-}
{-# LANGUAGE DeriveGeneric #-}
module Spicy.Generic
  ( -- * Classes
    -- $classes
    Check(..)
    -- * Exceptions
    -- $exceptions
  , DataStructureException(..)
  , ParserException(..)
    -- * Types
    -- $types
    -- ** Parallelism
    -- $typesParallel
  , ParallelStrategy(..)
    -- ** Accelerate
    -- $typesAccelerate
  , AccVector(..)
  , AccMatrix(..)
    -- * Parser Helper Functions
    -- $parserHelper
  , parse'
  , skipHorizontalSpace
  , maybeOption
    -- * Text
    -- $text
  , textS2L
  )
where
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.Array.Accelerate         as A
import           Data.Attoparsec.Text.Lazy
import qualified Data.Text                     as TS
import qualified Data.Text.Lazy                as TL
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( cycle
                                                , foldl1
                                                , foldr1
                                                , head
                                                , init
                                                , last
                                                , maximum
                                                , minimum
                                                , tail
                                                , take
                                                , takeWhile
                                                , (!!)
                                                )

{-
####################################################################################################
-}
{- $classes
-}
{-|
A class, which checks if a data structure matches the assumptions, that Spicy is using. If the check
passes, the data structure is returned unmodified. If the check fails, an exception in 'MonadThrow'
will be raised.
-}
class Check a where
  check :: MonadThrow m => a -> m a

{-
####################################################################################################
-}
{- $exceptions
-}
{-|
Exception type for operations on data structures, which are not meeting necessary criteria for the
operation to perform.
-}
data DataStructureException = DataStructureException
  { dsExcFunctionName :: String -- ^ Function which is causing the exception.
  , dsExcDescription  :: String -- ^ Description of the problem.
  }

instance Show DataStructureException where
  show (DataStructureException f e) = "DataStructureException in function \"" ++ f ++ "\":" ++ e

instance Exception DataStructureException

----------------------------------------------------------------------------------------------------
{-|
Exception type for textual or binary data, that could not be parsed.
-}
data ParserException = ParserException String

instance Show ParserException where
  show (ParserException e) = "ParserException in parser: \"" ++ e ++ "\""

instance Exception ParserException

{-
####################################################################################################
-}
{- $types
-}

{-
====================================================================================================
-}
{- $typesParallel
Type to control the parallelity of a function in terms of 'Control.Parallel.Strategies'.
-}
data ParallelStrategy
  = Serial
  | Parallel
  deriving ( Eq )

{-
====================================================================================================
-}
{- $typesAccelerate
Custom 'newtype' wrappers are defined around the 'Data.Array.Accelerate' module, which add
additional behaviours to Accelerate types.
-}
{-|
'newtype' wrapper to Accelerate's 'A.Vector's.
-}
newtype AccVector a = AccVector { getAccVector :: A.Vector a }
    deriving ( Generic, Show, Eq )

instance ( ToJSON a, A.Elt a ) => ToJSON (AccVector a) where
  toJSON vec =
    let plainVec        = getAccVector vec
        (A.Z A.:. xDim) = A.arrayShape plainVec
        elements        = A.toList plainVec
    in  object ["shape" .= xDim, "elements" .= elements]

instance ( FromJSON a, A.Elt a ) => FromJSON (AccVector a) where
  parseJSON = withObject "AccVector" $ \vec -> do
    xDim     <- vec .: "shape"
    elements <- vec .: "elements"
    return . AccVector $ A.fromList (A.Z A.:. xDim) elements

----------------------------------------------------------------------------------------------------
{-|
'newtype' wrapper to Accelerate's 'A.Matrix's.
-}
newtype AccMatrix a = AccMatrix { getAccMatrix :: A.Matrix a }
    deriving ( Generic, Show, Eq )

instance ( ToJSON a, A.Elt a ) => ToJSON (AccMatrix a) where
  toJSON mat =
    let plainMat                  = getAccMatrix mat
        (A.Z A.:. xDim A.:. yDim) = A.arrayShape plainMat
        elements                  = A.toList plainMat
    in  object ["shape" .= (xDim, yDim), "elements" .= elements]

instance ( FromJSON a, A.Elt a ) => FromJSON (AccMatrix a) where
  parseJSON = withObject "AccVector" $ \mat -> do
    (xDim, yDim) <- mat .: "shape"
    elements     <- mat .: "elements"
    return . AccMatrix $ A.fromList (A.Z A.:. xDim A.:. yDim) elements

{-
####################################################################################################
-}
{- $parserHelper
-}
{-|
This is a wrapper around Attoparsec's 'parse' function. Contrary to 'parse', this function fails
with  an composable error type in 'MonadThrow'.
-}
parse' :: MonadThrow m => Parser a -> TL.Text -> m a
parse' p t = case parse p t of
  Done _ r   -> return r
  Fail _ _ e -> throwM $ ParserException e

----------------------------------------------------------------------------------------------------
{-|
As Attoparsec's 'skipSpace', but skips horizintal space only.
-}
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = do
  _ <- takeWhile (`elem` [' ', '\t', '\f', '\v'])
  return ()

----------------------------------------------------------------------------------------------------
{-|
Make a parser optional and wrap it in a 'Maybe'.
-}
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

{-
####################################################################################################
-}
{- $text
-}
{-|
Convert strict text to lazy text.
-}
textS2L :: TS.Text -> TL.Text
textS2L = TL.pack . TS.unpack
