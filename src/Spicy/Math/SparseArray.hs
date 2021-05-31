-- |
-- Module      : Spicy.Math.SparseArray
-- Description : Sparse arrays in the style of the standard array package.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides sparse arrays in the style of the normal array package.
module Spicy.Math.SparseArray
  ( SparseArray (..),

    -- * Construction
    -- $construction
    sparseArray,
    foldableSparseArray,

    -- * Access
    -- $access
    lookup,
    (!?),
    null,
    bounds,
    indices,
    elems,
    elemsSparse,
    assocs,
    indicesNonDefault,

    -- * Updates
    -- $updates
    update,
    (//),
    indexReplacement,

    -- * Folds
    -- $folds
    foldlWithIndex',
    foldrWithIndex',

    -- * transformations
    -- $transformations
    map,
    mapWithIndex,
    traverseWithIndex,

    -- * Rank-Specific
    -- $rankSpecific
    -- $rank1
    cons,
    snoc,

    -- * Rank 2
    -- $rank2
    isLowerTriangularMatrix,
    isUpperTriangularMatrix,
    isSymmetricMatrix,
    homogenousIndexReplacement2,
  )
where

import Data.Aeson
import Data.Ix
import RIO hiding
  ( lookup,
    map,
    null,
  )
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.List as List hiding
  ( null,
  )
import qualified RIO.Partial as RIO'

-- | Sparse array type. This works in the spirit of 'Data.Array' but uses HashMaps to store the values
-- and does not produce a bottom value for elements not present.
data SparseArray i a
  = SparseArray
      !(i, i)
      -- ^ Lower and upper bound of the array.
      !(HashMap i a)
      -- ^ The "non empty" values of a sparse array, which are given.
      !a
      -- ^ The "empty" value, which all other elements implicitly have.
  deriving (Eq, Show, Generic)

instance (Hashable i, Ix i) => Functor (SparseArray i) where
  fmap f (SparseArray ix gV dV) = SparseArray ix (fmap f gV) (f dV)

instance (Hashable i, Ix i) => Foldable (SparseArray i) where
  foldr f a (SparseArray ix gV dV) =
    let allIndices = range ix
     in foldr
          (\ix' acc -> let valAtIx' = HashMap.lookupDefault dV ix' gV in strictF valAtIx' acc)
          a
          allIndices
    where
      strictF !x !y = f x y

instance (Hashable i, Ix i) => Traversable (SparseArray i) where
  sequenceA (SparseArray ix gV dV) = SparseArray ix <$> sequenceA gV <*> dV

instance (ToJSONKey i, ToJSON i, ToJSON a) => ToJSON (SparseArray i a) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSONKey i, FromJSON i, FromJSON a, Eq i, Hashable i, Ix i) => FromJSON (SparseArray i a)

{-
####################################################################################################
-}

-- | Exception type for operations on data structures, which are not meeting necessary criteria for the
-- operation to perform.
data IndexException = IndexException
  { -- | Function which is causing the exception.
    indExcFunctionName :: String,
    -- | Description of the problem.
    indExcDescription :: String
  }

instance Show IndexException where
  show (IndexException f e) = "IndexException in function \"" ++ f ++ "\":" ++ e

instance Exception IndexException

{-
####################################################################################################
-}

-- $construction

-- | Construct a sparse array from an annotated foldable, which has mappings from array index to element.
-- A default element will be used for all indices, that were not explicitly given. Also, if default
-- elements are part of the associations, they will not be included explicitly.
--
-- An exception will be monadically thrown if elements outside the index range are given.
sparseArray ::
  (Ix i, Hashable i, MonadThrow m, Foldable f, Eq e) =>
  -- | Lower and upper bound of the index range.
  (i, i) ->
  -- | Default element. Will not appear in the map to save memory.
  e ->
  -- | An arbitary 'Foldable' structure with @(index, element)@ mappings.
  f (i, e) ->
  -- | Resulting sparse array.
  m (SparseArray i e)
sparseArray ind@(indLow, indHigh) defElement assocs'
  | not orderCheck =
    throwM $
      IndexException "sparseArray" "First given bound is not lower than second bound."
  | not boundaryCheck = throwM $ IndexException "sparseArray" "Indices are out of bound."
  | not uniqueCheck =
    throwM $
      IndexException "sparseArray" "Indices repeat and the array can not be defined uniquely."
  | otherwise = return $ SparseArray ind valueMap defElement
  where
    orderCheck = indLow < indHigh
    boundaryCheck = all (inRange ind . fst) assocs'
    uniqueCheck = length assocs' == HashMap.size valueMap
    valueMap = HashMap.fromList . List.filter ((/= defElement) . snd) . toList $ assocs'

----------------------------------------------------------------------------------------------------

-- | Creates a sparse array from a foldable structure of associations in increasing index order. The
-- behaviour of 'foldl' for the 'Foldable' determines the order of your elements!.
foldableSparseArray ::
  (Ix i, Hashable i, MonadThrow m, Foldable f, Eq e) => (i, i) -> e -> f e -> m (SparseArray i e)
foldableSparseArray indRange defElement values
  | not lengthCheck =
    throwM $
      IndexException
        "foldableSparseArray"
        "Wrong number of elements to construct the entire array."
  | otherwise = return $ SparseArray indRange valueMap defElement
  where
    lengthCheck = length values == rangeSize indRange
    assocs' = zip (range indRange) (toList values)
    valueMap = HashMap.fromList . List.filter ((/= defElement) . snd) . toList $ assocs'

{-
####################################################################################################
-}

-- $access

-- | Safe sparse array indexing.
lookup :: (Ix i, Hashable i, MonadThrow m) => SparseArray i a -> i -> m a
arr@(SparseArray ind _gV _dV) `lookup` ix
  | not boundaryCheck = throwM $ IndexException "lookup" "Indices are out of bound."
  | otherwise = return $ unsafeLookUp arr ix
  where
    boundaryCheck = inRange ind ix

unsafeLookUp :: (Ix i, Hashable i) => SparseArray i a -> i -> a
(SparseArray ind gV dV) `unsafeLookUp` ix
  | not boundaryCheck = error "Indices are out of bound."
  | otherwise = HashMap.lookupDefault dV ix gV
  where
    boundaryCheck = inRange ind ix

----------------------------------------------------------------------------------------------------

-- | Infix version of 'lookup'.
(!?) :: (Ix i, Hashable i, MonadThrow m) => SparseArray i a -> i -> m a
(!?) = lookup

(!) :: (Ix i, Hashable i) => SparseArray i a -> i -> a
(!) = unsafeLookUp

----------------------------------------------------------------------------------------------------

-- | Check if an array is empty.
null :: (Eq i) => SparseArray i a -> Bool
null (SparseArray (indLow, indHigh) _ _) = indLow == indHigh

----------------------------------------------------------------------------------------------------

-- | Lower and upper bound of a sparse array.
bounds :: SparseArray i a -> (i, i)
bounds (SparseArray ind _ _) = ind

----------------------------------------------------------------------------------------------------

-- | Indices of an array in ascending order.
indices :: Ix i => SparseArray i a -> [i]
indices (SparseArray ind _ _) = range ind

----------------------------------------------------------------------------------------------------

-- | All elements of the sparse array in index order.
--
-- __Warning: This will give **all** elements for the array, not only the non-default ones.__
elems :: (Ix i, Hashable i) => SparseArray i a -> [a]
elems (SparseArray ind gV dV) = List.map (\ix -> HashMap.lookupDefault dV ix gV) $ range ind

----------------------------------------------------------------------------------------------------

-- | Retrieve the sparse elements including their index as a 'HashMap'.
elemsSparse :: SparseArray i a -> HashMap i a
elemsSparse (SparseArray _ gV _) = gV

----------------------------------------------------------------------------------------------------

-- | Gives a list of all associations. This will
--
-- __Warning: This will give **all** elements for the array, not only the non-default ones.__
assocs :: (Ix i, Hashable i) => SparseArray i a -> [(i, a)]
assocs (SparseArray ind gV dV) = (\ix -> (ix, HashMap.lookupDefault dV ix gV)) <$> range ind

----------------------------------------------------------------------------------------------------

-- |
-- Get a list of all elements, which are non default.
indicesNonDefault :: SparseArray i a -> [i]
indicesNonDefault = HashMap.keys . elemsSparse

{-
####################################################################################################
-}

-- $updates
-- Functions for pure updates. The array will never be altered itself.

-- | This function updates a value at a given index. It will fail, if an index error is encountered.
--
-- The update function uses a 'Maybe' as a result. If a 'Nothing' is obtained, the default element of
-- the sparse array will be used. If @'Just' a@ is obtained, @a@ will be the new value. If @a@ should
-- be the default value it will be the default value.
update ::
  (Ix i, Hashable i, MonadThrow m, Eq a) =>
  (a -> Maybe a) ->
  i ->
  SparseArray i a ->
  m (SparseArray i a)
update upF ind arr@(SparseArray indRange _gV _dV)
  | not $ inRange indRange ind = throwM $ IndexException "update" "Index is out of bound."
  | otherwise = return $ unsafeUpdate upF ind arr

unsafeUpdate ::
  (Ix i, Hashable i, Eq a) => (a -> Maybe a) -> i -> SparseArray i a -> SparseArray i a
unsafeUpdate upF ind arr@(SparseArray indRange gV dV)
  | not $ inRange indRange ind =
    error "Index is out of bound."
  | otherwise =
    let valAtInd = arr ! ind
        newGivenValues = case upF valAtInd of
          Nothing -> HashMap.delete ind gV
          Just x -> if x == dV then gV else HashMap.insert ind x gV
     in SparseArray indRange newGivenValues dV

----------------------------------------------------------------------------------------------------

-- | Updates a sparse array with the given associations. The old values will be overwritten.
(//) ::
  (Ix i, Hashable i, MonadThrow m, Foldable f, Eq a) =>
  SparseArray i a ->
  f (i, a) ->
  m (SparseArray i a)
arr // updateAssocs =
  foldl'
    ( \accArrM (ind, newVal) -> do
        accArr <- accArrM
        update (\_ -> Just newVal) ind accArr
    )
    (return arr)
    updateAssocs

----------------------------------------------------------------------------------------------------

-- |
-- Resorting a sparse vector by giving exchange pairs of indices. This function fails if new keys occur
-- multiple times (and elements would therefore be lost) or if the boundaries are out of range both for
-- old as well as new indices.
indexReplacement ::
  (Ix i, Hashable i, MonadThrow m) => HashMap i i -> SparseArray i a -> m (SparseArray i a)
indexReplacement repMap (SparseArray indRange gV dV) = do
  let indsToReplace = HashMap.keys repMap
      indsAfterReplace = List.map snd . HashMap.toList $ repMap
      allIndicesInRange =
        (all (== True) . List.map (inRange indRange) $ indsToReplace)
          && (all (== True) . List.map (inRange indRange) $ indsAfterReplace)
      numberOfReplacementAttempts = HashMap.size repMap
      numberOfPossibleReplacements = HashSet.size . HashSet.fromList $ indsAfterReplace

  -- Check if the target indices are also unique and no values will be lost.
  when (numberOfPossibleReplacements /= numberOfReplacementAttempts) $
    throwM $
      IndexException
        "indexReplacement1"
        "Replacement targets occur multiple times and elements would be lost. \
        \This replacement will not be performed."

  -- Check that the array stays valid with respect to index ranges after replacement.
  unless allIndicesInRange $
    throwM $
      IndexException
        "indexReplacement1"
        "Some indices in the replacement map are out of range of the array."

  -- Perform the replacement. Iterates over all keys of the original HashMap of given values and
  -- looks up the current key in the replacement map. If a new index-to-use is found, this will be
  -- used, otherwise the index will be kept.
  let newGv =
        HashMap.foldlWithKey'
          ( \accMap ix v ->
              let newIx = HashMap.lookupDefault ix ix repMap in HashMap.insert newIx v accMap
          )
          HashMap.empty
          gV

  return $ SparseArray indRange newGv dV

{-
####################################################################################################
-}

-- $folds

-- |
-- Left fold with access to the indices of the array. The fold is strict in the accumulator.
foldlWithIndex' :: (Ix i, Hashable i) => (b -> i -> a -> b) -> b -> SparseArray i a -> b
foldlWithIndex' f start (SparseArray indRange gV dV) =
  let allIndices = range indRange
   in foldl'
        (\acc ix -> let valAtIx = HashMap.lookupDefault dV ix gV in f acc ix valAtIx)
        start
        allIndices

----------------------------------------------------------------------------------------------------
{-
Right fold with access to the indices of the array. The fold is strict in the accumulator.
-}
foldrWithIndex' :: (Ix i, Hashable i) => (i -> a -> b -> b) -> b -> SparseArray i a -> b
foldrWithIndex' f start (SparseArray indRange gV dV) =
  let allIndices = range indRange
   in foldr
        (\ix acc -> let valAtIx = HashMap.lookupDefault dV ix gV in strictF ix valAtIx acc)
        start
        allIndices
  where
    strictF ix val !acc = f ix val acc

{-
####################################################################################################
-}

-- $transformations

-- |
-- Map a function to all elements. This is exaclty like 'fmap' from the 'Functor' instance of
-- 'SparseArray', but addinitionally removes elements from the values, which became the default value
-- of the sparse array by function application.
map :: (Ix i, Hashable i, Eq b) => (a -> b) -> SparseArray i a -> SparseArray i b
map f arr@(SparseArray indRange _gV _dV) =
  let SparseArray _ newGiven newDefault = fmap f arr
      newGivenCleaned = HashMap.filter (/= newDefault) newGiven
   in SparseArray indRange newGivenCleaned newDefault

----------------------------------------------------------------------------------------------------

-- |
-- Apply a function with access to the index to all elements. As this can change the type of the
-- default element, also a index independent function must be supplied to change the default element.
-- You are completely free in the choice on how to determine the new default element. The map will be
-- sparse in the new elements, with respect to the new default element.
mapWithIndex ::
  (Ix i, Hashable i, Eq b) =>
  -- | Function, that takes an 'Ix' index and a value and constructs the new
  --   value at this index.
  (i -> a -> b) ->
  -- | Function that updates the default value to a new one, independent from its
  --   index.
  (a -> b) ->
  -- | Original array.
  SparseArray i a ->
  SparseArray i b
mapWithIndex f fDV (SparseArray indRange gV dV) =
  let newDefault = fDV dV
      allIndices = HashSet.fromList . range $ indRange
      givenIndices = HashSet.fromList . HashMap.keys $ gV
      defaultIndices = allIndices `HashSet.difference` givenIndices
      newGivenValuesFromGiven = HashMap.filter (/= newDefault) . HashMap.mapWithKey f $ gV
      newGivenValuesFromDefault =
        HashMap.filter (/= newDefault)
          . HashMap.mapWithKey (\ix _ -> f ix dV)
          . HashSet.toMap
          $ defaultIndices
      newGiven = newGivenValuesFromGiven `HashMap.union` newGivenValuesFromDefault
   in SparseArray indRange newGiven newDefault

----------------------------------------------------------------------------------------------------

-- |
-- A version of 'traverse', which has access to the keys. See 'mapWithIndex' for more details.
traverseWithIndex ::
  (Ix i, Hashable i, Applicative f, Eq (f b)) =>
  -- | Function, that takes an 'Ix' index and a value and constructs the new
  --   value at this index.
  (i -> a -> f b) ->
  -- | Function to update the default value, independent from its index.
  (a -> f b) ->
  -- | Array to update.
  SparseArray i a ->
  f (SparseArray i b)
traverseWithIndex f fDV arr = sequenceA $ mapWithIndex f fDV arr

{-
####################################################################################################
-}

-- $rankSpecific

{-
====================================================================================================
-}

-- $rank1

-- |
-- Prepends a single element to the front of a sparse array.
--
-- __Warning: This function is not safe against array mismatches.__
cons :: (Ix i, Hashable i, Enum i, Eq a) => a -> SparseArray i a -> SparseArray i a
cons a (SparseArray (lowInd, highInd) gV dV) =
  let newLow = RIO'.pred lowInd
   in if a == dV
        then SparseArray (newLow, highInd) gV dV
        else SparseArray (newLow, highInd) (HashMap.insert newLow a gV) dV

----------------------------------------------------------------------------------------------------
snoc :: (Ix i, Hashable i, Enum i, Eq a) => SparseArray i a -> a -> SparseArray i a
snoc (SparseArray (lowInd, highInd) gV dV) a =
  let newHigh = RIO'.succ highInd
   in if a == dV
        then SparseArray (lowInd, newHigh) gV dV
        else SparseArray (lowInd, newHigh) (HashMap.insert newHigh a gV) dV

{-
====================================================================================================
-}

-- $rank2

-- |
-- Checks if a matrix can be regarded as a lower triangular matrix.
isLowerTriangularMatrix :: (Ord i) => SparseArray (i, i) a -> Bool
isLowerTriangularMatrix (SparseArray _ gV _) =
  HashMap.foldlWithKey' (\boolAcc (indA, indB) _ -> (indA >= indB) && boolAcc) True gV

----------------------------------------------------------------------------------------------------

-- |
-- Checks if a matrix can be regarded as an upper triangular matrix.
isUpperTriangularMatrix :: (Ord i) => SparseArray (i, i) a -> Bool
isUpperTriangularMatrix (SparseArray _ gV _) =
  HashMap.foldlWithKey' (\boolAcc (indA, indB) _ -> (indA <= indB) && boolAcc) True gV

---------------------------------------------------------------------------------------------------

-- |
-- Check if a matrix is a square matrix.
isSquareMatrix :: (Enum i) => SparseArray (i, i) a -> Bool
isSquareMatrix (SparseArray ((indLowX, indLowY), (indHighX, indHighY)) _ _) =
  let indLowXN = fromEnum indLowX
      indLowYN = fromEnum indLowY
      indHighXN = fromEnum indHighX
      indHighYN = fromEnum indHighY
   in ((indHighXN - indLowXN) - (indHighYN - indLowYN)) == 0

---------------------------------------------------------------------------------------------------

-- |
-- Checks if a matrix is a square symmetric matrix.
isSymmetricMatrix :: (Ord i, Enum i, Eq a, Hashable i) => SparseArray (i, i) a -> Bool
isSymmetricMatrix arr@(SparseArray _ gV _) = isSquareMatrix arr && isSymmetric
  where
    isSymmetric =
      HashMap.foldlWithKey'
        ( \accBool (ixR, ixC) el ->
            if ixR >= ixC
              then
                let mirrorEl = HashMap.lookup (ixC, ixR) gV
                 in case mirrorEl of
                      Just mEl -> mEl == el && accBool
                      Nothing -> False
              else accBool
        )
        True
        gV

----------------------------------------------------------------------------------------------------

-- |
-- Index replacement for sparse matrices. Homogenouse index replacement does not distinguish between
-- rows and columns and changes the index of row or column as soon as one of both or both match a
-- replacement key from the map.
homogenousIndexReplacement2 ::
  (Ix i, Hashable i, Enum i, MonadThrow m) =>
  HashMap i i ->
  SparseArray (i, i) a ->
  m (SparseArray (i, i) a)
homogenousIndexReplacement2 repMap arr@(SparseArray indRange@((lowBoundX, _), (highBoundX, _)) gV dV) =
  do
    let indComponentToReplace = HashMap.keys repMap
        indComponentAfterReplace = List.map snd . HashMap.toList $ repMap
        allIndicesInRange =
          (all (== True) . List.map (inRange (lowBoundX, highBoundX)) $ indComponentToReplace)
            && ( all (== True)
                   . List.map (inRange (lowBoundX, highBoundX))
                   $ indComponentAfterReplace
               )
        nRepKeys = HashMap.size repMap
        nRepValues = HashSet.size . HashSet.fromList $ indComponentAfterReplace

    unless (isSquareMatrix arr) $
      throwM $
        IndexException
          "homogenousIndexReplacement"
          "Homogenous index replacement can only be applied to square matrices."

    unless allIndicesInRange $
      throwM $
        IndexException
          "homogenousIndexReplacement"
          "At least one index in the replacement map is out of range of the array to modify."

    when (nRepKeys /= nRepValues) $
      throwM $
        IndexException
          "homogenousIndexReplacement"
          "Replacement targets occur multiple times and elements would be lost.\
          \ This replacement will not be performed."

    let newGv =
          HashMap.foldlWithKey'
            ( \accMap (ixR, ixC) v ->
                let newIx =
                      (HashMap.lookupDefault ixR ixR repMap, HashMap.lookupDefault ixC ixC repMap)
                 in HashMap.insert newIx v accMap
            )
            HashMap.empty
            gV

    return $ SparseArray indRange newGv dV
