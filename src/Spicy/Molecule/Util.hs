{-|
Module      : Spicy.Molecule.Util
Description : Utilities to manipulate basic molecular data structures.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides functions to manipulate basic data structures of 'Molecule's, such as indexing.
-}
module Spicy.Molecule.Util
( checkMolecule
, reIndexMolecule
, reIndex2BaseMolecule
, groupTupleSeq
, groupBy
, makeSubMolsFromAnnoAtoms
, makeBondsUnidirectorial
, findAtomInSubMols
) where
import           Data.Foldable
import           Data.IntMap.Lazy    (IntMap)
import qualified Data.IntMap.Lazy    as IM
import           Data.IntSet         (IntSet, (\\))
import qualified Data.IntSet         as IS
import           Data.Maybe
import           Data.Sequence       (Seq (..))
import qualified Data.Sequence       as S
import qualified Data.Text.Lazy      as TL
import           Lens.Micro.Platform
import           Prelude             hiding (cycle, foldl1, foldr1, head, init,
                                      last, maximum, minimum, tail, take,
                                      takeWhile, (!!))
import           Spicy.Types
import Control.Exception.Safe

{-
"IS" = IntSet
"IM" = IntMap
"oK" = old key
"nK" = new key
"RI" = replaced index
"oS" = old set
"nS" = new set
"oA" = old atom
"nA" = new atom
"sM" = sub molecules
"nL" = next layer
"pA" = pseudo atoms
-}

{-|
Check sanity of 'Molecule', which means test the following criteria:

  - The 'IM.Key's of the '_molecule_Atoms' 'IntMap' are a superset of all 'IM.Key's and 'IS.Key's
    appearing in the 'IntMap' 'IntSet' of '_molecule_Bonds'
  - The deeper layers in 'molecule_SubMol' are proper subsets of the higher layer regarding
    non-pseudo 'Atom' indices
  - Fragments of the same layer are completelty disjoint in their atom indices
  - Bonds of a layer are bidirectorial
  - The size of '_atom_Coordinates' is strictly 3 for all atoms of this layer
-}
checkMolecule :: MonadThrow m => Molecule -> m Molecule
checkMolecule mol
  | not layerIndCheck        =
      throwM $ MolLogicException "checkMolecule" ("Bonds vs Atoms mismatch. Your bonds bind to non existing atoms." :: String)
  | not fragAtomsDisjCheck   =
      throwM $ MolLogicException "checkMolecule" ("The atoms of deeper layers are not disjoint but shared by fragments." :: String)
  | not subsetCheckAtoms     =
      throwM $ MolLogicException "checkMolecule" ("The atoms of deeper layers are not a subset of this layer." :: String)
  | not bondBidectorialCheck =
      throwM $ MolLogicException "checkMolecule" ("The bonds are not bidirectorially defined." :: String)
  | not atomCoordCheck       =
      throwM $ MolLogicException "checkMolecule" ("Not all atoms have exactly 3 coordinates." :: String)
  | otherwise                =
      if S.null (mol ^. molecule_SubMol)
        then return mol
        else do
          subMols <- traverse checkMolecule $ mol ^. molecule_SubMol
          return $ mol & molecule_SubMol .~ subMols
  where
    -- Indices of the atoms
    atomInds             = IM.keysSet $ mol ^. molecule_Atoms
    -- Indices of the bond origin atoms
    bondsOrig            = IM.keysSet $ mol ^. molecule_Bonds
    -- Indices of the bond target atoms
    bondsTarget          = IS.unions $ mol ^. molecule_Bonds
    -- Check if bond indices do not exceed atom indices.
    layerIndCheck        = IS.null $ (bondsOrig `IS.union` bondsTarget) \\ atomInds
    -- Next layer molecules
    sM                   = mol ^. molecule_SubMol
    sMSize               = S.length sM
    -- Disjointment test (no atoms and bonds shared through fragments)
    -- "bA" = Bool_A, "aA" = Atoms_A
    fragAtomsDisjCheck   =
        fst
      . foldl' (\(bA, aA) (_, aB) ->
          if aA `iMdisjoint` aB && bA
            then (True, aA `IM.union` aB)
            else (False, aA)
        ) (True, IM.empty)
      . S.zip (S.replicate sMSize True)
      . fmap (^. molecule_Atoms)
      $ sM
    -- Next Layer atoms all joined
    nLAtoms              = IM.unions . fmap (^. molecule_Atoms) $ sM
    nLAtomsInds          = IM.keysSet nLAtoms
    -- All pseudo atoms of the next layer set
    nLPseudoAtomsInds    = IM.keysSet. IM.filter (^. atom_IsPseudo) $ nLAtoms
    -- Test if the next deeper layer is a proper subset of the current layer.
    subsetCheckAtoms     = IS.null $ (nLAtomsInds \\ nLPseudoAtomsInds) \\ atomInds
    -- Check if the bonds are bidirectorial
    bondBidectorialCheck = imisBidirectorial $ mol ^. molecule_Bonds
    -- Check if all atoms have exactly 3 coodinates.
    atomCoordCheck       =
      all (\aC -> S.length aC == 3) $ mol ^.. molecule_Atoms . each . atom_Coordinates
    {-
    -- This check doesn't need to be true, as pseudobonds can break the subset property.
    -- All Bonds of the next layer joinded
    nLBonds              = IM.unions . VB.map (^. molecule_Bonds) $ sM
    -- Exclude bonds from and to pseudo atoms in deeper layers
    nLBondsOrigin        = IM.keysSet nLBonds \\ nLPseudoAtomsInds
    nLBondsTarget        = IS.unions nLBonds \\ nLPseudoAtomsInds
    subsetCheckBonds     =
      (nLBondsOrigin `IS.union` nLBondsTarget) \\ (bondsTarget `IS.union` bondsOrig)
    -}

{-|
Check wether an 'IntMap' 'IntSet' structure is bidirectorial.
-}
imisBidirectorial :: IntMap IntSet -> Bool
imisBidirectorial imis =
  IM.foldrWithKey' (\key valIS bool ->
    let -- Look for the IntSet, that can be found when looking up all values from an IntSet of Keys.
        targetIS :: Seq IntSet
        targetIS =
          IS.foldr' (\k acc ->
            case imis IM.!? k of
              Nothing  -> acc :|> IS.empty
              Just tIS -> acc :|> tIS
          ) S.empty valIS
        -- Check for all in the Seq of found IntSet, if the current key is also a member.
        keyInTargets :: Seq Bool
        keyInTargets = fmap (key `IS.member`) targetIS
    in  -- If the current key is a member of all target IntSet, we are fine. If not, we have a
        -- problem.
        all (== True) keyInTargets && bool
  ) True imis

{-|
Check if 2 'IntMap' are disjoint in their 'IM.Key's.
-}
iMdisjoint ::
     IntMap a
  -> IntMap b
  -> Bool
iMdisjoint a b = IM.null $ a `IM.intersection` b

{-|
This reindexes all structures in a 'Molecule' with predefined counting scheme. This means counting
of 'Atom's will start at 0 and be consecutive. This also influences bonds in '_molecule_Bonds' and
layers in '_molecule_SubMol'. Pseudoatoms will be taken care of.
-}
reIndex2BaseMolecule :: MonadThrow m => Molecule -> m Molecule
reIndex2BaseMolecule mol =
  let allAtomIndices = getAtomIndices mol
      repMap         =
          IM.fromAscList
        . (\old -> zip old [0 .. ])
        . IS.toList
        $ allAtomIndices
  in  reIndexMolecule repMap mol

{-|
Get the indices of all 'Atom's in a 'Molecule', including those of sublayers in '_molecule_SubMol'
and pseudoatoms therein. This assumes a sane 'Molecule' according to 'checkMolecule'.
-}
getAtomIndices :: Molecule -> IntSet
getAtomIndices mol =
  let -- The indices of all atoms of the current layer
      thisLayerIndices = IM.keysSet $ mol ^. molecule_Atoms
      -- The indices of all sublayers + this layer.
      allIndices =
          foldr' (<>) thisLayerIndices
        . fmap (getAtomIndices)
        $ mol ^. molecule_SubMol
  in allIndices

{-|
Reindex a complete 'Molecule', including all its deeper layers in '_molecule_SubMol') by mappings
from a global replacement Map, mapping old to new indices. This function assumes, that your molecule
is sane in the overall assumptions of this program. This means that the lower layers obey the
counting scheme of the atoms of the higher layers and pseudoatoms come last.
-}
reIndexMolecule :: MonadThrow m => IntMap Int -> Molecule -> m Molecule
reIndexMolecule repMap mol = do
  -- Get the submolecules
  let subMols = mol ^. molecule_SubMol
  -- Reindex the current layer only
  molRI <- reIndexMoleculeLayer repMap mol
    -- If the molecule has no submolecules:
  if S.null subMols
    -- Then we are done.
    then return molRI
    -- Else we need to reindex the deeper layers also.
    else do
      subMolsRI <- traverse (reIndexMolecule repMap) subMols
      return $ molRI & molecule_SubMol .~ subMolsRI

{-|
Reindex the '_molecule_Atoms' and '_molecule_Bonds' of a single layer of a molecule (ignoring
anything in the '_molecule_SubMol' field). While the completeness of the reindexing is checked and
incompleteness of the replacement 'IntMap' 'Int' will result in 'Left' 'String', it is not checked
if the 'Atom's indexing is sane and indices are unique.
-}
reIndexMoleculeLayer ::
     MonadThrow m
  => IntMap Int -- ^ 'IntMap' with mappings from old indices to new indices (bonds and
                --   atoms).
  -> Molecule   -- ^ Molecule to reindex.
  -> m Molecule -- ^ 'Left' 'String' in case the mapping from old keys to new keys is
                            --   incomplete.
reIndexMoleculeLayer repMap mol
  -- If old Atom indices cannot be completely replaced by the Map:
  | not $ checkRepMapCompleteIS repMap (IM.keysSet $ mol ^. molecule_Atoms) =
      throwM $ MolLogicException "reIndexMoleculeLayer" "The remapping of indices is not complete for the atom indices."
  -- If old bond type data cannot be completely replaced by the Map:
  | not $ checkRepMapCompleteIMIS repMap (mol ^. molecule_Bonds)            =
      throwM $ MolLogicException "reIndexMoleculeLayer" "The remapping of indices is not complete for the bond data."
  -- If both checks went fine:
  | otherwise                                                               =
      return $ mol
        -- Use the (%~) lens to update the atoms indices of a molecule with new indices.
        & molecule_Atoms %~ replaceIMKeys repMap
        -- Use the (%~) lens to update keys and values (IntSet) of the bond type data structure.
        & molecule_Bonds %~ replaceIMIS repMap

{-|
Check if 'IntMap' is complete to replace all values in an 'IntSet' (any old 'IS.Key' can be replaced
by a new 'IS.Key').  Gives 'True' if complete, 'False' if the replacement 'IntMap' 'Int' has holes.
-}
checkRepMapCompleteIS ::
     IntMap Int -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntSet     -- ^ 'IntSet' in which values should be replaced.
  -> Bool       -- ^ Result.
checkRepMapCompleteIS repMap is =
  let -- Keys, that can be replaced
      repKeys  = IM.keysSet repMap
      -- Keys that shall be replaced minus keys that can be replaced
      lostKeys = is \\ repKeys
  in  IS.null lostKeys

{-|
Check if 'IntMap' is complete to replace all 'IM.Key's from the old 'IntMap' by new 'IM.Key's. Gives
'True' if the 'IntMap' with replacements is complete and 'False' otherwise.
-}
checkRepMapCompleteIM ::
     IntMap Int -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntMap a   -- ^ 'IntMap' in which 'IM.Key's should be replaced.
  -> Bool       -- ^ Result.
checkRepMapCompleteIM repMap im =
  let -- Keys, that can be replaced
      repKeys = IM.keysSet repMap
      -- Keys that shall be replaced
      oldKeys = IM.keysSet im
      -- Keys that cannot be replaced
      lostKeys = oldKeys \\ repKeys
  in  IS.null lostKeys


{-|
Check if 'IntMap' is complete to replace all values in an 'IntMap' 'IntSet' type construction
(replacing both the lookup keys in the 'IntMap', as well as all values in the 'IntSet'). Gives
'True' if complete and 'False' otherwise.
-}
checkRepMapCompleteIMIS ::
     IntMap Int    -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntMap IntSet -- ^ 'IntMap' 'IntSet' in which values and keys should be replaced.
  -> Bool          -- ^ Result.
checkRepMapCompleteIMIS repMap imis =
  let -- All values, that appear in the union of all IntSet
      oldSets    = IS.unions imis
  in  checkRepMapCompleteIM repMap imis && checkRepMapCompleteIS repMap oldSets

{-|
Replace all 'IS.Key's from an 'IntSet' according to mappings from an 'IntMap' 'Int'. Entries, that
cannot be found in the 'IntMap' will no be changed.
-}
replaceIS ::
     IntMap Int -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntSet     -- ^ 'IntSet' to be modified.
  -> IntSet     -- ^ Resulting new 'IntSet'.
replaceIS repMap is =
  IS.map (\oK ->
    let nK = repMap IM.!? oK
    in  fromMaybe oK nK
  ) is

{-|
Replace all 'IM.Key's in an 'IntMap' according to mappings from an 'IntMap' 'Int'. Entries that
cannot be found in the 'IntMap' will not be changed.
-}
replaceIMKeys ::
     IntMap Int -- ^ 'IntMap' containing the mapping from old 'IM.Key's to new 'IM.Key's.
  -> IntMap a   -- ^ 'IntMap' in which 'IM.Key's shall be replaced.
  -> IntMap a   -- ^ Resulting new 'IntMap' with replaced 'IM.Key's.
replaceIMKeys repMap im =
  IM.mapKeys (\oK ->
    let nK = repMap IM.!? oK
    in  fromMaybe oK nK
  ) im

{-|
Replace all lookup-'IM.Key's of an 'IntMap' 'IntSet' and all values in the 'IntSet' by a given
mapping from an 'IntMap'. Entries that cannot be found in the 'IntMap' will not be changed.
-}
replaceIMIS ::
     IntMap Int    -- ^ 'IntMap' containing the mapping from old 'IS.Key's to new 'IS.Key's.
  -> IntMap IntSet -- ^ Original structure, which to replace both keys and values.
  -> IntMap IntSet -- ^ modified structure.
replaceIMIS repMap imis =
  -- Replace all values in all IntSet
    IM.map (replaceIS repMap)
  -- Replace all lookup keys
  . replaceIMKeys repMap
  $ imis

{-|
Group by the first tuple element and within this group build an IntSet of the the second tuple
elements.
-}
groupTupleSeq :: Seq (Int, Int) -> IntMap IntSet
groupTupleSeq a =
  let -- Build groups of tuples with same keys.
      keyValGroups :: Seq (Seq (Int, Int))
      keyValGroups  = groupBy (\x y -> fst x == fst y) . S.sortOn fst $ a
      -- Transform the grouped key value structures to a Seq (IntMap IntSet), where each IntMap has
      -- just one key.
      atomicIntMaps :: MonadThrow m => m (Seq (IntMap IntSet))
      atomicIntMaps = traverse imisFromGroupedSequence keyValGroups
      -- Fold all atom IntMap in the sequence into one.
      completeMap  = foldl' (<>) IM.empty <$> atomicIntMaps
  in  -- The only way this function can fail, is if keys would not properly be groupled. This cannot
      -- happen if 'groupBy' is called correclty before 'imisFromGroupedSequence'. Therefore default
      -- to the empty IntMap if this case, that cannot happen, happens.
      case completeMap of
        Left _   -> IM.empty
        Right im -> im

{-|
Create the IntMap IntSet structure from a group of 'IM.Key' value pairs. This means, that the first
elements of the tuple, all need to be the same 'IM.key'. If they are not the assumptions of this
function are not met and a 'Left' 'String' as error will be returned. The result will be an IntMap
with a single 'IM.Key'.
-}
imisFromGroupedSequence :: MonadThrow m => Seq (Int, Int) -> m (IntMap IntSet)
imisFromGroupedSequence group
  | S.null group = return IM.empty
  | keyCheck     =
      case headKey of
        Nothing -> return IM.empty
        Just k  -> return $ IM.fromList [(k, values)]
  | otherwise    =
      throwM $ DataStructureException "imisFromGroupedSequence" "The keys are not all the same."
  where
    headGroup = group S.!? 0
    keys      = fst <$> group
    headKey   = fst <$> headGroup
    keyCheck  = all (== headKey) (pure <$> keys)
    values    = IS.fromList . toList . fmap snd $ group

{-|
This function implements
[groupBy](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:groupBy) as in
Data.List:
"The group function takes a list and returns a list of lists such that the concatenation of the
result is equal to the argument. Moreover, each sublist in the result contains only equal elements."
-}
groupBy :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupBy _ S.Empty    = S.empty
groupBy f (x :<| xs) = (x :<| ys) :<| groupBy f zs
  where
    (ys, zs) = S.spanl (f x) xs

{-|
Group a common parser structure based on a submolecule identifier.
-}
groupAnnoAtomsAsSubMols ::
     Seq (Int, (Int, TL.Text), Atom)       -- ^ A plain 'Seq' of all 'Atom's in the 'Molecule'.
                                           --   They are annoated in a Tuple as:
                                           --   @(atomIndex, (subMolID, subMolName), atom)@
                                           --
  -> Seq (Seq (Int, (Int, TL.Text), Atom)) -- ^ Groups of annotated submolecules, which share a
                                           --   common subMolID.
groupAnnoAtomsAsSubMols annoAtoms =
    groupBy (\x y -> x ^. _2 . _1 == y ^. _2 . _1)
  . S.unstableSortOn (\(_, (subID, _), _) -> subID)
  $ annoAtoms

{-|
Take a group ('Seq') of 'Atom's as formed by 'groupAsSubMols and the bonds of the complete
'Molecule'. Then form a proper submolecule. This function relies on the group of being a proper
submolecule and will hapilly accept groups that are not.
-}
makeSubMolFromGroup ::
     Seq (Int, (Int, TL.Text), Atom) -- ^ A group of annotated atoms in the style of:
                                     --   @(atomIndex, (subMolID, subMolName), atom)@
  -> IntMap IntSet                   -- ^ Bond type data structure of the whole molecule.
  -> Molecule                        -- ^ A newly formed submolecule.
makeSubMolFromGroup group bonds =
  let atoms      = IM.fromList . toList . fmap (\(ind, _, atom) -> (ind, atom)) $ group
      label      = fromMaybe "" . (S.!? 0) . fmap (^. _2 . _2) $ group
      atomInds   = IM.keysSet atoms
      -- Remove all bonds from the IntMap, that have origin on atoms not in this set and all
      -- target atoms that are not in the set.
      bondsCleaned :: IntMap IntSet
      bondsCleaned =
          IM.map (IS.filter (`IS.member` atomInds))
        $ bonds `IM.restrictKeys` atomInds
  in  Molecule
        { _molecule_Label    = label
        , _molecule_Atoms    = atoms
        , _molecule_Bonds    = bondsCleaned
        , _molecule_SubMol   = S.empty
        , _molecule_Energy   = Nothing
        , _molecule_Gradient = Nothing
        , _molecule_Hessian  = Nothing
        }

{-|
From some common parser informations, form the sub molecules.
-}
makeSubMolsFromAnnoAtoms ::
     Seq (Int, (Int, TL.Text), Atom) -- ^ A plain 'Seq' of all 'Atom's in the 'Molecule'. They are
                                     --   annoated in a Tuple as:
                                     --   @(atomIndex, (subMolID, subMolName), atom)@
  -> IntMap IntSet                   -- ^ Bonds for the whole 'Molecule'. See '_molecule_Bonds'.
  -> Seq Molecule                    -- ^ The submolecules.
makeSubMolsFromAnnoAtoms annoAtoms bonds =
  let subMolAtomGroups = groupAnnoAtomsAsSubMols annoAtoms
  in  fmap (\g -> makeSubMolFromGroup g bonds) subMolAtomGroups

{-|
The bond structure, which is defined bidirectorially can be reduced to be defined unidirectorial. If
you imagine this structure as the bond matrix, this is the same as taking just the upper right
triangular matrix without the main diagonal.
-}
makeBondsUnidirectorial :: IntMap IntSet -> IntMap IntSet
makeBondsUnidirectorial imis =
     removeEmptyIMIS
  $ IM.foldrWithKey (\key valIS acc ->
      IM.update (\_ -> Just $ IS.filter (> key) valIS) key acc
    ) imis imis

{-|
This function takes and 'IntMap' 'IntSet' structure and a single update tuple. All values from the
'IntSet' will be looked up in the 'IntMap' as 'IM.Key', and the 'IM.Key' from the tuple will be
removed from the so obtained pairs.

Example:
@removeInverseFromIMIS map (5, IS.fromList [1,2,3])@ would remove the value 5 from the 'IntMap'
entries ('IntSet's) with the 'IM.Key's 1, 2 and 3.
-}
-- "val2Rem" = value to remove
removeInverseFromIMIS ::
     IntMap IntSet    -- ^ Original structure.
  -> (IM.Key, IntSet) -- ^ The update tuple. 'IM.Key' is the value to be removed from the 'IntSet's,
                      --   that are found, when looking up all values from the 'IntSet' in the
                      --   'IntMap'.
  -> IntMap IntSet    -- ^ Updated structure.
removeInverseFromIMIS imis (val2Rem, keys) =
  IM.foldrWithKey' (\key _ acc ->
    if key `IS.member` keys
      then IM.update (Just <$> IS.delete val2Rem) key acc
      else acc
  ) imis imis

{-|
Remove 'IM.Key' value pairs from the 'IntMap', where the 'IntSet' is empty.
-}
removeEmptyIMIS :: IntMap IntSet -> IntMap IntSet
removeEmptyIMIS imis =
  IM.foldrWithKey' (\key is acc ->
    IM.update (\_ -> if IS.null is then Nothing else Just is) key acc
  ) imis imis

{-|
Given a 'IM.Key' (representing an 'Atom'), determine in which fragment ('_molecule_SubMol') the
'Atom' is.
-}
findAtomInSubMols ::
     Int             -- ^ 'Atom' to find in the fragments.
  -> IntMap Molecule -- ^ Annotated fragments in an 'IntMap'
  -> Maybe Int       -- ^ The 'IM.Key' aka fragment number in which the atom has been found.
findAtomInSubMols atomKey annoFrags =
    fst <$>
  ( IM.lookupMin
  . IM.filter (== True)
  . IM.map (\mol -> atomKey `IM.member` (mol ^. molecule_Atoms))
  $ annoFrags
  )
