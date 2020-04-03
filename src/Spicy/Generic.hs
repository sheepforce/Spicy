{-|
Module      : Spicy.Generic
Description : Common data types and functions
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

Definition of generic functions, agnostic of a Spicy specific data type, and generic classes.
-}
module Spicy.Generic
  ( -- * Parser Helper Functions
    -- $parserHelper
    parse'
  , skipHorizontalSpace
  , maybeOption
  , parseYamlFile
  , parseJSONFile
    -- * Operations on Data Structures
    -- $dataOperations
    -- ** Path Types
    -- $pathOperations
  , path2Text
  , path2Utf8Builder
  , makeJFilePathAbsFromCwd
  , makeJDirPathAbsFromCwd
  , replaceProblematicChars
    -- ** Text
    -- $textOperations
  , readFileUTF8
  , writeFileUTF8
  , appendFileUTF8
  , tShow
  , text2Utf8Builder
  , removeWhiteSpace
    -- ** UTF8 Builder
    -- $utf8builderOperations
  , utf8Show
    -- ** ByteString
    -- $byteStringOperations
  , byteStringLazy2Strict
  , byteStringStrict2Lazy
  , byteString2Utf8Builder
    -- ** Sequence
    -- $sequenceOperations
  , groupBy
    -- ** Map and Set Structures
    -- $mapSetOperations
  , mapSetIsBidirectorial
  , intMapSetIsBidirectorial
  , mapDisjoint
  , intMapDisjoint
  , isRepMapCompleteForSet
  , intIsRepMapCompleteForSet
  , isRepMapCompleteForMap
  , intIsRepMapCompleteForMap
  , isRepMapCompleteForMapSet
  , intIsRepMapCompleteForMapSet
  , replaceSet
  , intReplaceSet
  , replaceMapKeys
  , intReplaceMapKeys
  , replaceMapSet
  , intReplaceMapSet
  , groupTupleSeq
  , intGroupTupleSeq
  , mapSetFromGroupedSequence
  , intMapSetFromGroupedSequence
  , makeMapSetUnidirectorial
  , intMakeMapSetUnidirectorial
  , removeInverseFromMapSet
  , intRemoveInverseFromMapSet
  , removeEmptyMapSet
  , intRemoveEmptyMapSet
  , mapSetAddConnectionBidirectorial
  , intMapSetAddConnectionBidirectorial
  , mapSetRemoveConnectionBidirectorial
  , intMapSetRemoveConnectionBidirectorial
    -- ** Bond Matrix Operations
    -- $bondMatrixOperations
  , isBondMatrixBidirectorial
  , isRepMapCompleteforBondMatrix
  , replaceBondMatrixInds
  , cleanBondMatByAtomInds
  , removeBondsByAtomIndsFromBondMat
  , removeBondFromBondMat
  , addBondToBondMat
  , bondMat2ImIs
  , makeBondMatUnidirectorial
    -- ** Massiv
    -- $massivOperations
  , takeWhileV'
  , takeWhileV
  , vectorToVectorGroups
  , vectorToGroups
  , matrixFromGroupVector
  , chunksOfNColumns
  )
where
import           Data.Attoparsec.Text
import qualified Data.ByteString.Builder       as Builder
import qualified Data.ByteString.Lazy          as BL
import           Data.Foldable
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Data.Maybe
import qualified Data.Text.IO                  as T
import           RIO                     hiding ( takeWhile
                                                , Vector
                                                )
import qualified RIO.Map                       as Map
import           RIO.Seq                        ( Seq(..)
                                                , (<|)
                                                , (|>)
                                                )
import qualified RIO.Seq                       as Seq
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as Text
import           System.IO                      ( hSetEncoding
                                                , utf8
                                                )
import qualified System.Path                   as Path
import           System.Path.IO                as Path
import           System.Path.PartClass
import           Spicy.Class
import qualified RIO.HashMap                   as HashMap
import           Data.Massiv.Array             as Massiv
                                         hiding ( toList
                                                , all
                                                , mapM_
                                                )
import           Data.Aeson
import           Data.Yaml.Include              ( decodeFileWithWarnings )

{-
####################################################################################################
-}
{- $parserHelper
-}
{-|
This is a wrapper around Attoparsec's 'parse' function. Contrary to 'parse', this function fails
with  an composable error type in 'MonadThrow'.
-}
parse' :: MonadThrow m => Parser a -> Text -> m a
parse' p t = case parse p t of
  Done _ r   -> return r
  Fail _ _ e -> throwM $ ParserException e
  Partial f  -> case f "" of
    Done _ r   -> return r
    Fail _ _ e -> throwM $ ParserException e
    Partial _  -> throwM $ ParserException "Obtained a Partial result twice. Cannot continue."

----------------------------------------------------------------------------------------------------
{-|
As Attoparsec's 'skipSpace', but skips horizintal space only.
-}
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = do
  _ <- takeWhile (`elem` [' ', '\t', '\f', '\v'])
  return ()

----------------------------------------------------------------------------------------------------x
{-|
Make a parser optional and wrap it in a 'Maybe'.
-}
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

----------------------------------------------------------------------------------------------------
{-|
Parse a YAML file in a RIO monad with logging and print the warnings.
-}
parseYamlFile :: (FromJSON a, HasLogFunc env) => Path.AbsFile -> RIO env a
parseYamlFile yamlPath = do
  let logSource = "Parse YAML file"
  decodeCandidate <- liftIO . decodeFileWithWarnings . Path.toString $ yamlPath
  case decodeCandidate of
    Left exception -> do
      logErrorS logSource
        $  "While parsing "
        <> path2Utf8Builder yamlPath
        <> " an error was encountered."
      throwM exception
    Right ([]      , dat) -> return dat
    Right (warnings, dat) -> do
      logWarnS logSource
        $  "While parsing "
        <> path2Utf8Builder yamlPath
        <> " the following warnings were raised:"
      mapM_ (logWarnS logSource . text2Utf8Builder . tShow) warnings
      return dat

----------------------------------------------------------------------------------------------------
{-|
Parse a YSON file in a RIO monad with logging and print the warnings.
-}
parseJSONFile :: (FromJSON a, HasLogFunc env) => Path.AbsFile -> RIO env a
parseJSONFile jsonPath = do
  let logSource = "Parse YAML file"
  decodeCandidate <- liftIO . eitherDecodeFileStrict . Path.toString $ jsonPath
  case decodeCandidate of
    Left errorMsg -> do
      logErrorS logSource
        $  "While parsing "
        <> path2Utf8Builder jsonPath
        <> " an error was encountered."
      throwM $ DataStructureException "eitherDecodeFileStrict" errorMsg
    Right dat -> return dat

{-
####################################################################################################
-}
{- $dataOperations
Operations on generic data formats, which are commonly used in Spicy.
-}
{-
====================================================================================================
-}
{- $pathOperations
-}
{-|
Converts a pathtype Path to a textual representation as obtained with 'Path.toString' in 'TL.Text'
representation instead of 'String'.
-}
path2Text :: (AbsRel ar, FileDir fd) => Path.Path ar fd -> Text
path2Text path' = Text.pack . Path.toString $ path'

----------------------------------------------------------------------------------------------------
{-|
Convert a typed path to an Utf8Builder, which is often useful for printing log messages.
-}
path2Utf8Builder :: (AbsRel ar, FileDir fd) => Path.Path ar fd -> Utf8Builder
path2Utf8Builder = text2Utf8Builder . path2Text

----------------------------------------------------------------------------------------------------
{-|
Make a 'JFilePath' absolute.
-}
makeJFilePathAbsFromCwd :: JFilePath -> IO JFilePathAbs
makeJFilePathAbsFromCwd jFilePath = do
  absolutePath <- Path.genericMakeAbsoluteFromCwd . getFilePath $ jFilePath
  return . JFilePathAbs $ absolutePath

----------------------------------------------------------------------------------------------------
{-|
Make a 'JDirPath' absolute.
-}
makeJDirPathAbsFromCwd :: JDirPath -> IO JDirPathAbs
makeJDirPathAbsFromCwd jDirPath = do
  absolutePath <- Path.genericMakeAbsoluteFromCwd . getDirPath $ jDirPath
  return . JDirPathAbs $ absolutePath

----------------------------------------------------------------------------------------------------
{-|
Replaces problematic characters from a string before parsing it to a path, so that for example even
a string with slashes can be parsed as a file without directories prepended.
-}
replaceProblematicChars :: String -> String
replaceProblematicChars path2Sanitise = fmap
  (\c -> if c `elem` Path.pathSeparators then '_' else c
  )
  path2Sanitise

{-
====================================================================================================
-}
{- $textOperations
Operations on strict 'Text'.
-}
{-|
Wrapper around RIO's writing of unicode formatted text to a file ('writeFileUtf8'), compatible with
typed paths.
-}
writeFileUTF8 :: MonadIO m => Path.AbsRelFile -> Text -> m ()
writeFileUTF8 path' text = writeFileUtf8 (Path.toString path') text

----------------------------------------------------------------------------------------------------
{-|
Wrapper around RIO's reading of unicode formatted text to a file ('readFileUtf8'), compatible with
typed paths.
-}
readFileUTF8 :: MonadIO m => Path.AbsRelFile -> m Text
readFileUTF8 path' = readFileUtf8 (Path.toString path')

----------------------------------------------------------------------------------------------------
{-|
Appending for UTF-8 encoded files in RIO's style of writing formatted text to a file, compatible
with typed paths.
-}
appendFileUTF8 :: MonadIO m => Path.AbsRelFile -> Text -> m ()
appendFileUTF8 path' text = liftIO . Path.withFile path' Path.AppendMode $ \h -> do
  hSetEncoding h utf8
  T.hPutStr h text

----------------------------------------------------------------------------------------------------
{-|
Converting things to show to a text.
-}
tShow :: Show a => a -> Text
tShow = utf8BuilderToText . displayShow

----------------------------------------------------------------------------------------------------
{-|
Convert a 'Text' to an Utf8Builder as used by RIO.
-}
text2Utf8Builder :: Text -> Utf8Builder
text2Utf8Builder = Utf8Builder . Builder.byteString . Text.encodeUtf8

----------------------------------------------------------------------------------------------------
{-|
Removes all white space, even between words, from a text.
-}
removeWhiteSpace :: Text -> Text
removeWhiteSpace = Text.concat . Text.words

{-
====================================================================================================
-}
{- $utf8builderOperations
Operation on and with RIO's 'Utf8Builder' strings.
-}
utf8Show :: Show a => a -> Utf8Builder
utf8Show = text2Utf8Builder . tShow

{-
====================================================================================================
-}
{- $byteStringOperations
Operations on Bytestrings.
-}
{-|
Convert a lazy bytestring to a strict one.
-}
byteStringLazy2Strict :: BL.ByteString -> ByteString
byteStringLazy2Strict = BL.toStrict

----------------------------------------------------------------------------------------------------
{-|
Convert a strict bytestring to a lazy one.
-}
byteStringStrict2Lazy :: ByteString -> BL.ByteString
byteStringStrict2Lazy = BL.fromStrict

----------------------------------------------------------------------------------------------------
{-|
Converts a strict bytestring to an UTF8Builder.
-}
byteString2Utf8Builder :: ByteString -> Utf8Builder
byteString2Utf8Builder = Utf8Builder . Builder.byteString

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
{-# INLINE groupBy #-}
groupBy :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupBy _ Seq.Empty  = Seq.empty
groupBy f (x :<| xs) = (x :<| ys) :<| groupBy f zs where (ys, zs) = Seq.spanl (f x) xs

{-
====================================================================================================
-}
{- $mapSetOperations
Operations on 'Map' and 'Set' data structures, as they are commonly used in Spicy. For every
function a general version is implemented and a version which does the same on 'IntSet' and 'IntMap'
structures. Those functions specific to the 'Int' types are prefixed with Int, but otherwise behave
the same.
-}
{-|
Check wether a 'Map' 'Set' structure is bidirectorial.
-}
{-# INLINE mapSetIsBidirectorial #-}
mapSetIsBidirectorial :: Ord a => Map a (Set a) -> Bool
mapSetIsBidirectorial mapSet = Map.foldrWithKey'
  (\key valSet testBool ->
    let -- Look for the IntSet, that can be found when looking up all values from an IntSet of Keys.
        targetSet = Set.foldr'
          (\k acc -> case mapSet Map.!? k of
            Nothing  -> acc :|> Set.empty
            Just tIS -> acc :|> tIS
          )
          Seq.empty
          valSet
        -- Check for all in the Seq of found IntSet, if the current key is also a member.
        keyInTargets :: Seq Bool
        keyInTargets = fmap (key `Set.member`) targetSet
    in                     -- If the current key is a member of all target IntSet, we are fine. If not, we have a
        -- problem.
        all (== True) keyInTargets && testBool
  )
  True
  mapSet

{-# INLINE intMapSetIsBidirectorial #-}
intMapSetIsBidirectorial :: IntMap IntSet -> Bool
intMapSetIsBidirectorial mapSet = IntMap.foldrWithKey'
  (\key valSet testBool ->
    let -- Look for the IntSet, that can be found when looking up all values from an IntSet of Keys.
        targetSet = IntSet.foldr'
          (\k acc -> case mapSet IntMap.!? k of
            Nothing  -> acc :|> IntSet.empty
            Just tIS -> acc :|> tIS
          )
          Seq.empty
          valSet
        -- Check for all in the Seq of found IntSet, if the current key is also a member.
        keyInTargets :: Seq Bool
        keyInTargets = fmap (key `IntSet.member`) targetSet
    in                     -- If the current key is a member of all target IntSet, we are fine. If not, we have a
        -- problem.
        all (== True) keyInTargets && testBool
  )
  True
  mapSet

----------------------------------------------------------------------------------------------------
{-|
Check if 2 'Map's are disjoint in their keys.
-}
{-# INLINE mapDisjoint #-}
mapDisjoint :: Ord a => Map a b -> Map a c -> Bool
mapDisjoint a b = Map.null $ a `Map.intersection` b

{-# INLINE intMapDisjoint #-}
intMapDisjoint :: IntMap a -> IntMap a -> Bool
intMapDisjoint a b = IntMap.null $ a `IntMap.intersection` b

----------------------------------------------------------------------------------------------------
{-|
Check if a 'Map' is complete to replace all values in an 'Set' (any old key in the 'Set' can be
replaced by a new key).  Gives 'True' if complete, 'False' if the replacement 'Map' has holes.
-}
{-# INLINE isRepMapCompleteForSet #-}
isRepMapCompleteForSet
  :: Ord a
  => Map a b -- ^ 'Map' containing the mapping from old keys to new keys of the set.
  -> Set a   -- ^ 'Set' in which values should be replaced.
  -> Bool    -- ^ Result.
isRepMapCompleteForSet repMap set' =
  let -- Keys, that can be replaced
      repKeys  = Map.keysSet repMap
      -- Keys that shall be replaced minus keys that can be replaced
      lostKeys = set' Set.\\ repKeys
  in  Set.null lostKeys

{-# INLINE intIsRepMapCompleteForSet #-}
intIsRepMapCompleteForSet
  :: IntMap b -- ^ 'IntMap' containing the mapping from old keys to new keys of the set.
  -> IntSet   -- ^ 'IntSet' in which values should be replaced.
  -> Bool     -- ^ Result.
intIsRepMapCompleteForSet repMap set' =
  let -- Keys, that can be replaced
      repKeys  = IntMap.keysSet repMap
      -- Keys that shall be replaced minus keys that can be replaced
      lostKeys = set' IntSet.\\ repKeys
  in  IntSet.null lostKeys

----------------------------------------------------------------------------------------------------
{-|
Check if a 'Map' is complete to replace all keys from the old 'Map' by new keys. Gives
'True' if the 'Map' with replacements is complete and 'False' otherwise.
-}
{-# INLINE isRepMapCompleteForMap #-}
isRepMapCompleteForMap
  :: Ord a
  => Map a b -- ^ 'Map' containing the mapping from old key to new key.
  -> Map a c -- ^ 'Map' in which keys should be replaced.
  -> Bool    -- ^ Result.
isRepMapCompleteForMap repMap map' =
  let -- Keys, that can be replaced
      repKeys  = Map.keysSet repMap
      -- Keys that shall be replaced
      oldKeys  = Map.keysSet map'
      -- Keys that cannot be replaced
      lostKeys = oldKeys Set.\\ repKeys
  in  Set.null lostKeys

{-# INLINE intIsRepMapCompleteForMap #-}
intIsRepMapCompleteForMap
  :: IntMap a -- ^ 'IntMap' containing the mapping from old key to new key.
  -> IntMap b -- ^ 'IntMap' in which keys should be replaced.
  -> Bool    -- ^ Result.
intIsRepMapCompleteForMap repMap map' =
  let -- Keys, that can be replaced
      repKeys  = IntMap.keysSet repMap
      -- Keys that shall be replaced
      oldKeys  = IntMap.keysSet map'
      -- Keys that cannot be replaced
      lostKeys = oldKeys IntSet.\\ repKeys
  in  IntSet.null lostKeys

----------------------------------------------------------------------------------------------------
{-|
Check if a 'Map' is complete to replace all values in a 'Map a Set a' type construction
(replacing both the lookup keys in the 'Map', as well as all values in the 'Set'). Gives
'True' if complete and 'False' otherwise.
-}
{-# INLINE isRepMapCompleteForMapSet #-}
isRepMapCompleteForMapSet
  :: Ord a
  => Map a b       -- ^ 'IntMap' containing the mapping from old keys to new keys.
  -> Map a (Set a) -- ^ 'IntMap' 'IntSet' in which values and keys should be replaced.
  -> Bool          -- ^ Result.
isRepMapCompleteForMapSet repMap mapSet =
  let -- All values, that appear in the union of all IntSet
      oldSets = Set.unions mapSet
  in  isRepMapCompleteForMap repMap mapSet && isRepMapCompleteForSet repMap oldSets

{-# INLINE intIsRepMapCompleteForMapSet #-}
intIsRepMapCompleteForMapSet
  :: IntMap Int    -- ^ 'IntMap' containing the mapping from old keys to new keys.
  -> IntMap IntSet -- ^ 'IntMap' 'IntSet' in which values and keys should be replaced.
  -> Bool          -- ^ Result.
intIsRepMapCompleteForMapSet repMap mapSet =
  let -- All values, that appear in the union of all IntSet
      oldSets = IntSet.unions mapSet
  in  intIsRepMapCompleteForMap repMap mapSet && intIsRepMapCompleteForSet repMap oldSets

----------------------------------------------------------------------------------------------------
{-|
Replace all keys from a 'Set a' according to mappings from an 'Map a b'. Entries, that
cannot be found in the 'Map' will no be changed.
-}
{-# INLINE replaceSet #-}
replaceSet
  :: Ord a
  => Map a a -- ^ 'Map' containing the mapping from old keys to new keys.
  -> Set a   -- ^ 'Set' to be modified.
  -> Set a   -- ^ Resulting new 'IntSet'.
replaceSet repMap set' = Set.map (\oK -> let nK = repMap Map.!? oK in fromMaybe oK nK) set'

{-# INLINE intReplaceSet #-}
intReplaceSet
  :: IntMap Int -- ^ 'IntMap' containing the mapping from old keys to new keys.
  -> IntSet     -- ^ 'IntSet' to be modified.
  -> IntSet     -- ^ Resulting new 'IntSet'.
intReplaceSet repMap set' =
  IntSet.map (\oK -> let nK = repMap IntMap.!? oK in fromMaybe oK nK) set'

----------------------------------------------------------------------------------------------------
{-|
Replace all keys in a 'Map' according to mappings from a different 'Map'. Entries that
cannot be found in the 'Map' with the replacements will not be changed.
-}
{-# INLINE replaceMapKeys #-}
replaceMapKeys
  :: Ord a
  => Map a a -- ^ 'Map' containing the mapping from old keys to new keys.
  -> Map a b -- ^ 'Map' in which keys shall be replaced.
  -> Map a b -- ^ Resulting new 'Map' with replaced keys.
replaceMapKeys repMap map' =
  Map.mapKeys (\oK -> let nK = repMap Map.!? oK in fromMaybe oK nK) map'

{-# INLINE intReplaceMapKeys #-}
intReplaceMapKeys
  :: IntMap Int -- ^ 'IntMap' containing the mapping from old keys to new keys.
  -> IntMap a   -- ^ 'IntMap' in which keys shall be replaced.
  -> IntMap a   -- ^ Resulting new 'Map' with replaced keys.
intReplaceMapKeys repMap map' =
  IntMap.mapKeys (\oK -> let nK = repMap IntMap.!? oK in fromMaybe oK nK) map'

----------------------------------------------------------------------------------------------------
{-|
Replace all lookup-keyss of a 'Map a Set a' and all values in the 'Set' by a given mapping from a
different 'Map'. Entries that cannot be found in the replacement-'Map' will not be changed.
-}
{-# INLINE replaceMapSet #-}
replaceMapSet
  :: Ord a
  => Map a a       -- ^ 'Map' containing the mapping from old keys to new keys.
  -> Map a (Set a) -- ^ Original structure, which to replace both keys and values.
  -> Map a (Set a) -- ^ Modified structure.
replaceMapSet repMap mapSet =
  -- Replace all values in all IntSet
  Map.map (replaceSet repMap)
  -- Replace all lookup keys
    . replaceMapKeys repMap
    $ mapSet

{-# INLINE intReplaceMapSet #-}
intReplaceMapSet
  :: IntMap Int    -- ^ 'IntMap' containing the mapping from old keys to new keys.
  -> IntMap IntSet -- ^ Original structure, which to replace both keys and values.
  -> IntMap IntSet -- ^ Modified structure.
intReplaceMapSet repMap mapSet =
  -- Replace all values in all IntSet
  IntMap.map (intReplaceSet repMap)
  -- Replace all lookup keys
    . intReplaceMapKeys repMap
    $ mapSet

----------------------------------------------------------------------------------------------------
{-|
Group by the first tuple element and within this group build an 'Set' of the the second tuple
elements.
-}
{-# INLINE groupTupleSeq #-}
groupTupleSeq :: Ord a => Seq (a, a) -> Map a (Set a)
groupTupleSeq a =
  let -- Build groups of tuples with same keys.
      --keyValGroups :: Seq (Seq (a, a))
      keyValGroups =
          groupBy (\b c -> fst b == fst c) . Seq.sortBy (\b c -> fst b `compare` fst c) $ a

      -- Transform the grouped key value structures to a Seq (IntMap IntSet), where each IntMap has
      -- just one key.
      -- atomicIntMaps :: MonadThrow m => m (Seq (Map Int (Set Int)))
      atomicIntMaps = traverse mapSetFromGroupedSequence keyValGroups
      -- Fold all atom IntMap in the sequence into one.
      completeMap   = foldl' (<>) Map.empty <$> atomicIntMaps
  in                     -- The only way this function can fail, is if keys would not properly be groupled. This cannot
      -- happen if 'groupBy' is called correclty before 'mapSetFromGroupedSequence'. Therefore
      -- default to the empty Map if this case, that cannot happen, happens.
      case completeMap of
        Left  _    -> Map.empty
        Right map' -> map'

{-# INLINE intGroupTupleSeq #-}
intGroupTupleSeq :: Seq (Int, Int) -> IntMap IntSet
intGroupTupleSeq a =
  let -- Build groups of tuples with same keys.
      --keyValGroups :: Seq (Seq (a, a))
      keyValGroups =
          groupBy (\b c -> fst b == fst c) . Seq.sortBy (\b c -> fst b `compare` fst c) $ a

      -- Transform the grouped key value structures to a Seq (IntMap IntSet), where each IntMap has
      -- just one key.
      -- atomicIntMaps :: MonadThrow m => m (Seq (Map Int (Set Int)))
      atomicIntMaps = traverse intMapSetFromGroupedSequence keyValGroups
      -- Fold all atom IntMap in the sequence into one.
      completeMap   = foldl' (<>) IntMap.empty <$> atomicIntMaps
  in                     -- The only way this function can fail, is if keys would not properly be groupled. This cannot
      -- happen if 'groupBy' is called correclty before 'mapSetFromGroupedSequence'. Therefore
      -- default to the empty Map if this case, that cannot happen, happens.
      case completeMap of
        Left  _    -> IntMap.empty
        Right map' -> map'

----------------------------------------------------------------------------------------------------
{-|
Create the 'Map a (Set a)' structure from a group of key value pairs. This means, that the first
elements of the tuple, all need to be the same key. If they are not the assumptions of this function
are not met and an error will be returned. The result will be a 'Map' with a single keys.
-}
{-# INLINE mapSetFromGroupedSequence #-}
mapSetFromGroupedSequence :: (MonadThrow m, Ord a) => Seq (a, a) -> m (Map a (Set a))
mapSetFromGroupedSequence group
  | Seq.null group = return Map.empty
  | keyCheck = case headKey of
    Nothing -> return Map.empty
    Just k  -> return $ Map.fromList [(k, values)]
  | otherwise = throwM
  $ DataStructureException "mapSetFromGroupedSequence" "The keys are not all the same."
 where
  headGroup = group Seq.!? 0
  keys      = fst <$> group
  headKey   = fst <$> headGroup
  keyCheck  = all (== headKey) (pure <$> keys)
  values    = Set.fromList . toList . fmap snd $ group

{-# INLINE intMapSetFromGroupedSequence #-}
intMapSetFromGroupedSequence :: MonadThrow m => Seq (Int, Int) -> m (IntMap IntSet)
intMapSetFromGroupedSequence group
  | Seq.null group = return IntMap.empty
  | keyCheck = case headKey of
    Nothing -> return IntMap.empty
    Just k  -> return $ IntMap.fromList [(k, values)]
  | otherwise = throwM
  $ DataStructureException "mapSetFromGroupedSequence" "The keys are not all the same."
 where
  headGroup = group Seq.!? 0
  keys      = fst <$> group
  headKey   = fst <$> headGroup
  keyCheck  = all (== headKey) (pure <$> keys)
  values    = IntSet.fromList . toList . fmap snd $ group

----------------------------------------------------------------------------------------------------
{-|
The bond structure, which is defined bidirectorially can be reduced to be defined unidirectorial. If
you imagine this structure as the bond matrix, this is the same as taking just the upper right
triangular matrix without the main diagonal.
-}
{-# INLINE makeMapSetUnidirectorial #-}
makeMapSetUnidirectorial :: Ord a => Map a (Set a) -> Map a (Set a)
makeMapSetUnidirectorial mapSet = removeEmptyMapSet $ Map.foldrWithKey
  (\key valSet acc -> Map.update (\_ -> Just $ Set.filter (> key) valSet) key acc)
  mapSet
  mapSet

{-# INLINE intMakeMapSetUnidirectorial #-}
intMakeMapSetUnidirectorial :: IntMap IntSet -> IntMap IntSet
intMakeMapSetUnidirectorial mapSet = intRemoveEmptyMapSet $ IntMap.foldrWithKey
  (\key valSet acc -> IntMap.update (\_ -> Just $ IntSet.filter (> key) valSet) key acc)
  mapSet
  mapSet

----------------------------------------------------------------------------------------------------
{-|
This function takes a 'Map a (Set a)' structure and a single update tuple. All values from the 'Set'
in the tuple will be looked up in the 'Map' as key, and the keys from the tuple will be removed from
the so obtained pairs.

Example:
@
    removeInverseFromMapSet map' (5, Set.fromList [1,2,3])
@
would remove the value 5 from the 'Map' values ('Set') with the lookup keys 1, 2 and 3.
-}
-- "val2Rem" = value to remove
{-# INLINE removeInverseFromMapSet #-}
removeInverseFromMapSet
  :: (Ord a, Ord b)
  => Map a (Set b) -- ^ Original structure.
  -> (b, Set a)    -- ^ The update tuple. @b@ is the value to be removed from the 'Map's 'Set'
                   --   values, that are found, when looking up all values from the 'Set' in the
                   --   original 'Map'.
  -> Map a (Set b) -- ^ Updated structure.
removeInverseFromMapSet mapSet (val2Rem, keys) = Map.foldrWithKey'
  (\key _ acc ->
    if key `Set.member` keys then Map.update (Just <$> Set.delete val2Rem) key acc else acc
  )
  mapSet
  mapSet

{-# INLINE intRemoveInverseFromMapSet #-}
intRemoveInverseFromMapSet
  :: IntMap IntSet -- ^ Original structure.
  -> (Int, IntSet) -- ^ The update tuple. @fst@ is the value to be removed from the 'IntMap's
                   --   'IntSet' values, that are found, when looking up all values from the 'Set'
                   --   in the original 'IntMap'.
  -> IntMap IntSet -- ^ Updated structure.
intRemoveInverseFromMapSet mapSet (val2Rem, keys) = IntMap.foldrWithKey'
  (\key _ acc -> if key `IntSet.member` keys
    then IntMap.update (Just <$> IntSet.delete val2Rem) key acc
    else acc
  )
  mapSet
  mapSet

----------------------------------------------------------------------------------------------------
{-|
Remove key value pairs from the 'Map', where the 'Set' is empty.
-}
{-# INLINE removeEmptyMapSet #-}
removeEmptyMapSet :: (Ord a) => Map a (Set b) -> Map a (Set b)
removeEmptyMapSet mapSet = Map.foldrWithKey'
  (\key is acc -> Map.update (\_ -> if Set.null is then Nothing else Just is) key acc)
  mapSet
  mapSet

{-# INLINE intRemoveEmptyMapSet #-}
intRemoveEmptyMapSet :: IntMap IntSet -> IntMap IntSet
intRemoveEmptyMapSet mapSet = IntMap.foldrWithKey'
  (\key is acc -> IntMap.update (\_ -> if IntSet.null is then Nothing else Just is) key acc)
  mapSet
  mapSet

----------------------------------------------------------------------------------------------------
{-|
Given two keys, this function will add the bidirectorially as origin ('Map' key) and target
('Set' key) to the data structure. If they already exists, they will be added to the existing
structure.
-}
{-# INLINE mapSetAddConnectionBidirectorial #-}
mapSetAddConnectionBidirectorial :: Ord a => Map a (Set a) -> (a, a) -> Map a (Set a)
mapSetAddConnectionBidirectorial mapSet (a, b) =
  let firstDirectionAdded = if a `Map.member` mapSet
        then Map.adjust (\targetSet -> Set.insert b targetSet) a mapSet
        else Map.insert a (Set.singleton b) mapSet
      secondDirectionAdded = if b `Map.member` mapSet
        then Map.adjust (\targetSet -> Set.insert a targetSet) b firstDirectionAdded
        else Map.insert b (Set.singleton a) firstDirectionAdded
  in  secondDirectionAdded

{-# INLINE intMapSetAddConnectionBidirectorial #-}
intMapSetAddConnectionBidirectorial :: IntMap IntSet -> (Int, Int) -> IntMap IntSet
intMapSetAddConnectionBidirectorial mapSet (a, b) =
  let firstDirectionAdded = if a `IntMap.member` mapSet
        then IntMap.adjust (\targetSet -> IntSet.insert b targetSet) a mapSet
        else IntMap.insert a (IntSet.singleton b) mapSet
      secondDirectionAdded = if b `IntMap.member` mapSet
        then IntMap.adjust (\targetSet -> IntSet.insert a targetSet) b firstDirectionAdded
        else IntMap.insert b (IntSet.singleton a) firstDirectionAdded
  in  secondDirectionAdded

----------------------------------------------------------------------------------------------------
{-|
Given two keys, this function will remove the connection between them. If they are not found,
nothing will be done.
-}
{-# INLINE mapSetRemoveConnectionBidirectorial #-}
mapSetRemoveConnectionBidirectorial :: Ord a => Map a (Set a) -> (a, a) -> Map a (Set a)
mapSetRemoveConnectionBidirectorial mapSet (a, b) =
  let firstDirectionRemoved = if a `Map.member` mapSet
        then Map.adjust (\targetSet -> Set.delete b targetSet) a mapSet
        else mapSet
      secondDirectionRemoved = if b `Map.member` mapSet
        then Map.adjust (\targetSet -> Set.delete a targetSet) b firstDirectionRemoved
        else firstDirectionRemoved
  in  secondDirectionRemoved

{-# INLINE intMapSetRemoveConnectionBidirectorial #-}
intMapSetRemoveConnectionBidirectorial :: IntMap IntSet -> (Int, Int) -> IntMap IntSet
intMapSetRemoveConnectionBidirectorial mapSet (a, b) =
  let firstDirectionRemoved = if a `IntMap.member` mapSet
        then IntMap.adjust (\targetSet -> IntSet.delete b targetSet) a mapSet
        else mapSet
      secondDirectionRemoved = if b `IntMap.member` mapSet
        then IntMap.adjust (\targetSet -> IntSet.delete a targetSet) b firstDirectionRemoved
        else firstDirectionRemoved
  in  secondDirectionRemoved

{-
====================================================================================================
-}
{- $bondMatrixOperations
Operations on the 'HashMap (Int, Int) Bool' type
-}
{-|
Check if the bond matrix is defined bidirectorial.
-}
isBondMatrixBidirectorial :: BondMatrix -> Bool
isBondMatrixBidirectorial bondMat = HashMap.foldlWithKey'
  (\accBool (ixO, ixT) val ->
    let mirrorVal = HashMap.lookup (ixT, ixO) bondMat
    in  case mirrorVal of
          Just mVal -> mVal == val && accBool
          Nothing   -> False
  )
  True
  bondMat

----------------------------------------------------------------------------------------------------
{-|
Check if a replacement map is complete to replace all index components in the bond matrix. Also
checks, that no values are lost because multiple old keys are mapped to a single new one.
-}
isRepMapCompleteforBondMatrix :: IntMap Int -> BondMatrix -> Bool
isRepMapCompleteforBondMatrix repMap bondMat =
  let replacementAttempts = IntMap.keysSet repMap
      replacementNewKeys  = IntMap.foldl' (flip IntSet.insert) IntSet.empty repMap
      indexComponentsToReplace =
          let bondInds    = HashMap.keys bondMat
              bondOrigins = IntSet.fromList . fmap fst $ bondInds
              bondTargets = IntSet.fromList . fmap snd $ bondInds
          in  bondOrigins <> bondTargets
      nothingLostCheck  = IntSet.size replacementNewKeys == IntSet.size replacementAttempts
      completenessCheck = IntSet.null $ indexComponentsToReplace IntSet.\\ replacementAttempts
  in  nothingLostCheck && completenessCheck

----------------------------------------------------------------------------------------------------
{-|
Replace the index components of a bond matrix with mappings from another replacement map. Index
components, that can not be found in the replacement map will not be changed.
-}
replaceBondMatrixInds :: IntMap Int -> BondMatrix -> BondMatrix
replaceBondMatrixInds repMap bondMat = HashMap.foldlWithKey'
  (\accBondMat (ixO, ixT) val ->
    let ixONew = IntMap.findWithDefault ixO ixO repMap
        ixTNew = IntMap.findWithDefault ixT ixT repMap
    in  HashMap.insert (ixONew, ixTNew) val accBondMat
  )
  HashMap.empty
  bondMat

----------------------------------------------------------------------------------------------------
{-|
Removes all bonds from a given bond matrix, that involve atoms, that are no longer present. The
'IntSet' contains the indices of the atoms, which are kept.
-}
cleanBondMatByAtomInds :: BondMatrix -> IntSet -> BondMatrix
cleanBondMatByAtomInds bondMat atomInds = HashMap.foldlWithKey'
  (\accBondMat (ixO, ixT) val -> if (ixO `IntSet.member` atomInds) && (ixT `IntSet.member` atomInds)
    then HashMap.insert (ixO, ixT) val accBondMat
    else accBondMat
  )
  HashMap.empty
  bondMat

----------------------------------------------------------------------------------------------------
{-|
Removes all bonds in which an atom is involded. The atom is specified by its index. If the atom is
involed in no bond, the bond matrix will not be changed. Multiple atoms can be specified by giving
the indices in an 'IntSet'.
-}
removeBondsByAtomIndsFromBondMat :: BondMatrix -> IntSet -> BondMatrix
removeBondsByAtomIndsFromBondMat bondMat atomInds = HashMap.foldlWithKey'
  (\accBondMat ix@(ixO, ixT) _ -> if ixO `IntSet.member` atomInds || ixT `IntSet.member` atomInds
    then HashMap.delete ix accBondMat
    else accBondMat
  )
  bondMat
  bondMat

----------------------------------------------------------------------------------------------------
{-|
Removes a bond between two atoms bidirectorially from the bond matrix. If the bond does not exist,
the bond matrix is unaltered.
-}
removeBondFromBondMat :: BondMatrix -> (Int, Int) -> BondMatrix
removeBondFromBondMat bondMat (ixO, ixT) =
  HashMap.delete (ixT, ixO) . HashMap.delete (ixO, ixT) $ bondMat

----------------------------------------------------------------------------------------------------
{-|
Adds a bond between two atoms bidirectorially. This function can add bonds between non-existing
atoms and destroy the molecule data consistency.
-}
addBondToBondMat :: BondMatrix -> (Int, Int) -> BondMatrix
addBondToBondMat bondMat (ixO, ixT) =
  HashMap.insert (ixT, ixO) True . HashMap.insert (ixO, ixT) True $ bondMat

----------------------------------------------------------------------------------------------------
{-|
Convert a bond matrix data structure to the IntMap IntSet structure as an alternative
representation.
-}
bondMat2ImIs :: BondMatrix -> IntMap IntSet
bondMat2ImIs bondMat = HashMap.foldlWithKey'
  (\accIntMap (ixO, ixT) val ->
    if val then IntMap.insertWith (<>) ixO (IntSet.singleton ixT) accIntMap else accIntMap
  )
  IntMap.empty
  bondMat

----------------------------------------------------------------------------------------------------
{-|
Makes the bond matrix unidirectorial by only taking the lower left triangular part of it. This means
that atoms only bind to those with higher index then their own.
-}
makeBondMatUnidirectorial :: BondMatrix -> BondMatrix
makeBondMatUnidirectorial bondMat =
  HashMap.filterWithKey (\(ixO, ixT) val -> ixO <= ixT && val) bondMat

{-
====================================================================================================
-}
{- $massivOperations
-}
{-|
Takewhile function for Massiv's vectors but also returns the part of the vector that has not been
consumed yet.
-}
{-# INLINE takeWhileV' #-}
takeWhileV' :: (Source r Ix1 a, Stream r Ix1 a) => (a -> Bool) -> Vector r a -> ([a], Vector r a)
takeWhileV' cond !vec = go cond vec []
 where
  go :: (Source r Ix1 a, Stream r Ix1 a) => (a -> Bool) -> Vector r a -> [a] -> ([a], Vector r a)
  go cond' !vec' foundYet' = if Massiv.snull vec'
    then (foundYet', vec')
    else
      let headOfVec = Massiv.head' vec'
          tailOfVec = Massiv.tail vec'
      in  if cond' headOfVec
            then let newFoundYet = headOfVec : foundYet' in go cond' tailOfVec newFoundYet
            else (foundYet', vec')

----------------------------------------------------------------------------------------------------
{-|
Equivalent of takeWhile for vectors.
-}
{-# INLINE takeWhileV #-}
takeWhileV :: (Source r Ix1 a, Stream r Ix1 a) => (a -> Bool) -> Vector r a -> [a]
takeWhileV cond !vec = fst $ takeWhileV' cond vec

----------------------------------------------------------------------------------------------------
{-|
Takes a sorted vector and groups its consecutive elements. Given a maximum size of elements, all
groups will be padded with a given default value.
-}
{-# INLINE vectorToVectorGroups #-}
vectorToVectorGroups
  :: (Source r1 Ix1 a, Stream r1 Ix1 a, MonadThrow m, Show a, Eq b)
  => (a -> b)              -- ^ An accessor to the values of the vector, which allows to compare on
                           --   specific constructors of the vector values.
  -> Int                   -- ^ The number of elements a vector must have.
  -> a                     -- ^ The default element to use for padding.
  -> Vector r1 a           -- ^ A vector of elements, potentially presorted, which forms groups.
  -> m (Seq (Vector B a))
vectorToVectorGroups accessorF' elemsPerRow' defElem' vec' = go accessorF'
                                                                elemsPerRow'
                                                                defElem'
                                                                vec'
                                                                Seq.empty
 where
  go
    :: (Source r1 Ix1 a, Stream r1 Ix1 a, Mutable r2 Ix1 a, MonadThrow m, Show a, Eq b)
    => (a -> b)          -- ^ An accessor to the values of the vector, which allows to compare on
                         --   specific constructors of the vector values.
    -> Int               -- ^ The number of elements a vector must have.
    -> a                 -- ^ The default value to use with padding, if a group does not have enough
                         --   elements.
    -> Vector r1 a       -- ^ A vector of elements, potentially presorted, which forms groups.
    -> Seq (Vector r2 a) -- ^ A sequence f groups, built from the vector.
    -> m (Seq (Vector r2 a))
  go accessorF elemsProRow defElem !vec !groupAcc = do
    let currentGroupVal  = Massiv.headM vec
        currentCondition = accessorF <$> currentGroupVal

    case currentCondition of
      -- Vector must have been fully consumed.
      Nothing           -> return groupAcc
      -- The start of the next group.
      Just accessorCond -> do
        let (currentGroup, restOfVec) = takeWhileV' (\a -> accessorF a == accessorCond) vec
            groupSize                 = length currentGroup

        -- Cannot continue, if more elements are in a group than the number of columns in the matrix
        -- is.
        when (groupSize > elemsProRow)
          .  throwM
          .  DataStructureException "groupedVectorToMatrix"
          $  "Taking the current group from the vector (current head: "
          <> show currentGroupVal
          <> ") and got "
          <> show groupSize
          <> "elements, but there must not be more than "
          <> show elemsProRow
          <> " elements per group."

        -- Pad the group with the default element.
        let
          numberOfPaddingElems = elemsProRow - groupSize
          paddingVec           = Massiv.sreplicate (Sz numberOfPaddingElems) defElem
          nonDefGroupVec       = Massiv.sfromListN (Sz groupSize) currentGroup
          groupVec =
            Massiv.compute . Massiv.setComp Par $ Massiv.sappend nonDefGroupVec paddingVec

        -- Add the new group vector to the sequence of groups.
        let newGroupAcc = groupVec <| groupAcc

        -- Another recursion for the next part of the vector.
        go accessorF elemsProRow defElem restOfVec newGroupAcc

----------------------------------------------------------------------------------------------------
{-|
Takes a sorted vector and groups its consecutive elements. Given a maximum size of elements, all
groups will be padded with a given default value.
-}
{-# INLINE vectorToGroups #-}
vectorToGroups
  :: (Source r1 Ix1 a, Stream r1 Ix1 a, Show a, Eq b)
  => (a -> b)    -- ^ An accessor to the values of the vector, which allows to compare on specific
                 --   constructors of the vector values.
  -> Vector r1 a -- ^ A vector of elements, potentially presorted, which forms groups.
  -> [[a]]
vectorToGroups accessorF' vec' = go accessorF' vec' []
 where
  go
    :: (Source r1 Ix1 a, Stream r1 Ix1 a, Show a, Eq b)
    => (a -> b)     -- ^ An accessor to the values of the vector, which allows to compare on
                    --   specific constructors of the vector values.
    -> Vector r1 a  -- ^ A vector of elements, potentially presorted, which forms groups.
    -> [[a]]        -- ^ A sequence f groups, built from the vector.
    -> [[a]]
  go accessorF !vec !groupAcc =
    let currentGroupVal  = Massiv.headM vec
        currentCondition = accessorF <$> currentGroupVal
    in  case currentCondition of
          -- Vector must have been fully consumed
          Nothing -> groupAcc
          -- The start of the next group.
          Just accessorCond ->
            let (currentGroup, restOfVec) = takeWhileV' (\a -> accessorF a == accessorCond) vec
                newGroupAcc               = currentGroup : groupAcc
            -- Another recursion to consume the next group in the vector.
            in  go accessorF restOfVec newGroupAcc

----------------------------------------------------------------------------------------------------
{-|
Build a matrix from a vector of grouped elements. Groups are of fixed size and will be padded with
a default argument. The function fails if one of the groups exceeds the maximum size.
-}
matrixFromGroupVector
  :: (Source r Ix1 a, Stream r Ix1 a, MonadThrow m, Eq b, Show a)
  => (a -> b)
  -> Int
  -> a
  -> Vector r a
  -> m (Matrix DL a)
matrixFromGroupVector accessorF nColumns defElem vec = do
  groups <- vectorToVectorGroups accessorF nColumns defElem vec
  let groupsExpandedToMatrix1 = fmap (Massiv.expandOuter (Sz 1) (\a _ -> a)) groups
  matrix <- Massiv.concatM 2 groupsExpandedToMatrix1
  return matrix

----------------------------------------------------------------------------------------------------
{-|
Takes up to N columns from a matrix and groups them. Behaves otherwise similiar to 'chunksOf'.
-}
chunksOfNColumns
  :: (Mutable r1 Ix2 e, Manifest r2 Ix2 e)
  => Int                         -- ^ Size of the chunks to obtain.
  -> Massiv.Array r2 Ix2 e
  -> Seq (Massiv.Array r1 Ix2 e)
chunksOfNColumns n matrix = Massiv.compute <$> go n (Massiv.toManifest matrix) Empty
 where
  go :: Int -> Matrix M a -> Seq (Matrix M a) -> Seq (Matrix M a)
  go n' restMatrix groupAcc = case Massiv.splitAtM (Dim 1) n' restMatrix of
    Nothing ->
      let (Sz (_ :. nColsRemaining)) = Massiv.size restMatrix
      in  if nColsRemaining == 0 then groupAcc else groupAcc |> restMatrix
    Just (thisChunk, restMinusThisChunk) -> go n' restMinusThisChunk (groupAcc |> thisChunk)
