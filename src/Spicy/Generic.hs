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
    -- * Operations on Data Structures
    -- $dataOperations
    -- ** Text
    -- $textOperations
  , textS2L
    -- ** Sequence
    -- $sequenceOperations
  , groupBy
    -- ** IntMap and IntSet Structures
    -- $imisOperationns
  , imisBidirectorial
  , iMdisjoint
  , checkRepMapCompleteIS
  , checkRepMapCompleteIM
  , checkRepMapCompleteIMIS
  , replaceIS
  , replaceIMKeys
  , replaceIMIS
  , groupTupleSeq
  , imisFromGroupedSequence
  , makeIMISUnidirectorial
  , removeInverseFromIMIS
  , removeEmptyIMIS
  )
where
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.Array.Accelerate         as A
import           Data.Attoparsec.Text.Lazy
import           Data.Foldable
import           Data.IntMap.Lazy               ( IntMap )
import qualified Data.IntMap.Lazy              as IM
import           Data.IntSet                    ( IntSet
                                                , (\\)
                                                )
import qualified Data.IntSet                   as IS
import           Data.Maybe
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as S
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
{- $dataOperations
Operations on generic data formats, which are commonly used in Spicy.
-}

{-
====================================================================================================
-}
{- $textOperations
Operations on strict and lazy 'TS.Text'.
-}
{-|
Convert strict text to lazy text.
-}
textS2L :: TS.Text -> TL.Text
textS2L = TL.pack . TS.unpack

{-
====================================================================================================
-}
{- $sequenceOperations
Operations on 'Seq'uneces.
-}
{-|
This function implements
[groupBy](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:groupBy) as in
Data.List:
"The group function takes a list and returns a list of lists such that the concatenation of the
result is equal to the argument. Moreover, each sublist in the result contains only equal elements."
-}
groupBy :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupBy _ S.Empty    = S.empty
groupBy f (x :<| xs) = (x :<| ys) :<| groupBy f zs where (ys, zs) = S.spanl (f x) xs

{-
====================================================================================================
-}
{- $imisOperations
Operations on 'IntMap' and 'IntSet' data structures, as they are commonly used in Spicy.
-}
{-|
Check wether an 'IntMap' 'IntSet' structure is bidirectorial.
-}
imisBidirectorial :: IntMap IntSet -> Bool
imisBidirectorial imis = IM.foldrWithKey'
  (\key valIS bool ->
    let -- Look for the IntSet, that can be found when looking up all values from an IntSet of Keys.
        targetIS :: Seq IntSet
        targetIS = IS.foldr'
          (\k acc -> case imis IM.!? k of
            Nothing  -> acc :|> IS.empty
            Just tIS -> acc :|> tIS
          )
          S.empty
          valIS
        -- Check for all in the Seq of found IntSet, if the current key is also a member.
        keyInTargets :: Seq Bool
        keyInTargets = fmap (key `IS.member`) targetIS
    in  -- If the current key is a member of all target IntSet, we are fine. If not, we have a
        -- problem.
        all (== True) keyInTargets && bool
  )
  True
  imis

----------------------------------------------------------------------------------------------------
{-|
Check if 2 'IntMap' are disjoint in their 'IM.Key's.
-}
iMdisjoint :: IntMap a -> IntMap b -> Bool
iMdisjoint a b = IM.null $ a `IM.intersection` b

----------------------------------------------------------------------------------------------------
{-|
Check if 'IntMap' is complete to replace all values in an 'IntSet' (any old 'IS.Key' can be replaced
by a new 'IS.Key').  Gives 'True' if complete, 'False' if the replacement 'IntMap' 'Int' has holes.
-}
checkRepMapCompleteIS
  :: IntMap Int -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntSet     -- ^ 'IntSet' in which values should be replaced.
  -> Bool       -- ^ Result.
checkRepMapCompleteIS repMap is =
  let -- Keys, that can be replaced
      repKeys  = IM.keysSet repMap
      -- Keys that shall be replaced minus keys that can be replaced
      lostKeys = is \\ repKeys
  in  IS.null lostKeys

----------------------------------------------------------------------------------------------------
{-|
Check if 'IntMap' is complete to replace all 'IM.Key's from the old 'IntMap' by new 'IM.Key's. Gives
'True' if the 'IntMap' with replacements is complete and 'False' otherwise.
-}
checkRepMapCompleteIM
  :: IntMap Int -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntMap a   -- ^ 'IntMap' in which 'IM.Key's should be replaced.
  -> Bool       -- ^ Result.
checkRepMapCompleteIM repMap im =
  let -- Keys, that can be replaced
      repKeys  = IM.keysSet repMap
      -- Keys that shall be replaced
      oldKeys  = IM.keysSet im
      -- Keys that cannot be replaced
      lostKeys = oldKeys \\ repKeys
  in  IS.null lostKeys

----------------------------------------------------------------------------------------------------
{-|
Check if 'IntMap' is complete to replace all values in an 'IntMap' 'IntSet' type construction
(replacing both the lookup keys in the 'IntMap', as well as all values in the 'IntSet'). Gives
'True' if complete and 'False' otherwise.
-}
checkRepMapCompleteIMIS
  :: IntMap Int    -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntMap IntSet -- ^ 'IntMap' 'IntSet' in which values and keys should be replaced.
  -> Bool          -- ^ Result.
checkRepMapCompleteIMIS repMap imis =
  let -- All values, that appear in the union of all IntSet
      oldSets = IS.unions imis
  in  checkRepMapCompleteIM repMap imis && checkRepMapCompleteIS repMap oldSets

----------------------------------------------------------------------------------------------------
{-|
Replace all 'IS.Key's from an 'IntSet' according to mappings from an 'IntMap' 'Int'. Entries, that
cannot be found in the 'IntMap' will no be changed.
-}
replaceIS
  :: IntMap Int -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntSet     -- ^ 'IntSet' to be modified.
  -> IntSet     -- ^ Resulting new 'IntSet'.
replaceIS repMap is = IS.map (\oK -> let nK = repMap IM.!? oK in fromMaybe oK nK) is

----------------------------------------------------------------------------------------------------
{-|
Replace all 'IM.Key's in an 'IntMap' according to mappings from an 'IntMap' 'Int'. Entries that
cannot be found in the 'IntMap' will not be changed.
-}
replaceIMKeys
  :: IntMap Int -- ^ 'IntMap' containing the mapping from old 'IM.Key's to new 'IM.Key's.
  -> IntMap a   -- ^ 'IntMap' in which 'IM.Key's shall be replaced.
  -> IntMap a   -- ^ Resulting new 'IntMap' with replaced 'IM.Key's.
replaceIMKeys repMap im = IM.mapKeys (\oK -> let nK = repMap IM.!? oK in fromMaybe oK nK) im

----------------------------------------------------------------------------------------------------
{-|
Replace all lookup-'IM.Key's of an 'IntMap' 'IntSet' and all values in the 'IntSet' by a given
mapping from an 'IntMap'. Entries that cannot be found in the 'IntMap' will not be changed.
-}
replaceIMIS
  :: IntMap Int    -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntMap IntSet -- ^ Original structure, which to replace both keys and values.
  -> IntMap IntSet -- ^ modified structure.
replaceIMIS repMap imis =
  -- Replace all values in all IntSet
  IM.map (replaceIS repMap)
  -- Replace all lookup keys
    . replaceIMKeys repMap
    $ imis

----------------------------------------------------------------------------------------------------
{-|
Group by the first tuple element and within this group build an IntSet of the the second tuple
elements.
-}
groupTupleSeq :: Seq (Int, Int) -> IntMap IntSet
groupTupleSeq a =
  let -- Build groups of tuples with same keys.
      keyValGroups :: Seq (Seq (Int, Int))
      keyValGroups = groupBy (\x y -> fst x == fst y) . S.sortOn fst $ a
      -- Transform the grouped key value structures to a Seq (IntMap IntSet), where each IntMap has
      -- just one key.
      atomicIntMaps :: MonadThrow m => m (Seq (IntMap IntSet))
      atomicIntMaps = traverse imisFromGroupedSequence keyValGroups
      -- Fold all atom IntMap in the sequence into one.
      completeMap   = foldl' (<>) IM.empty <$> atomicIntMaps
  in  -- The only way this function can fail, is if keys would not properly be groupled. This cannot
      -- happen if 'groupBy' is called correclty before 'imisFromGroupedSequence'. Therefore default
      -- to the empty IntMap if this case, that cannot happen, happens.
      case completeMap of
        Left  _  -> IM.empty
        Right im -> im

----------------------------------------------------------------------------------------------------
{-|
Create the IntMap IntSet structure from a group of 'IM.Key' value pairs. This means, that the first
elements of the tuple, all need to be the same 'IM.key'. If they are not the assumptions of this
function are not met and a 'Left' 'String' as error will be returned. The result will be an IntMap
with a single 'IM.Key'.
-}
imisFromGroupedSequence :: MonadThrow m => Seq (Int, Int) -> m (IntMap IntSet)
imisFromGroupedSequence group
  | S.null group = return IM.empty
  | keyCheck = case headKey of
    Nothing -> return IM.empty
    Just k  -> return $ IM.fromList [(k, values)]
  | otherwise = throwM
  $ DataStructureException "imisFromGroupedSequence" "The keys are not all the same."
 where
  headGroup = group S.!? 0
  keys      = fst <$> group
  headKey   = fst <$> headGroup
  keyCheck  = all (== headKey) (pure <$> keys)
  values    = IS.fromList . toList . fmap snd $ group

----------------------------------------------------------------------------------------------------
{-|
The bond structure, which is defined bidirectorially can be reduced to be defined unidirectorial. If
you imagine this structure as the bond matrix, this is the same as taking just the upper right
triangular matrix without the main diagonal.
-}
makeIMISUnidirectorial :: IntMap IntSet -> IntMap IntSet
makeIMISUnidirectorial imis = removeEmptyIMIS $ IM.foldrWithKey
  (\key valIS acc -> IM.update (\_ -> Just $ IS.filter (> key) valIS) key acc)
  imis
  imis

----------------------------------------------------------------------------------------------------
{-|
This function takes and 'IntMap' 'IntSet' structure and a single update tuple. All values from the
'IntSet' will be looked up in the 'IntMap' as 'IM.Key', and the 'IM.Key' from the tuple will be
removed from the so obtained pairs.
Example:
@removeInverseFromIMIS map (5, IS.fromList [1,2,3])@ would remove the value 5 from the 'IntMap'
entries ('IntSet's) with the 'IM.Key's 1, 2 and 3.
-}
-- "val2Rem" = value to remove
removeInverseFromIMIS
  :: IntMap IntSet    -- ^ Original structure.
  -> (IM.Key, IntSet) -- ^ The update tuple. 'IM.Key' is the value to be removed from the 'IntSet's,
                      --   that are found, when looking up all values from the 'IntSet' in the
                      --   'IntMap'.
  -> IntMap IntSet    -- ^ Updated structure.
removeInverseFromIMIS imis (val2Rem, keys) = IM.foldrWithKey'
  (\key _ acc ->
    if key `IS.member` keys then IM.update (Just <$> IS.delete val2Rem) key acc else acc
  )
  imis
  imis

----------------------------------------------------------------------------------------------------
{-|
Remove 'IM.Key' value pairs from the 'IntMap', where the 'IntSet' is empty.
-}
removeEmptyIMIS :: IntMap IntSet -> IntMap IntSet
removeEmptyIMIS imis = IM.foldrWithKey'
  (\key is acc -> IM.update (\_ -> if IS.null is then Nothing else Just is) key acc)
  imis
  imis
