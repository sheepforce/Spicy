{-|
Module      : Spicy.Molecule.Internal.Util
Description : Utilities to manipulate basic molecular data structures.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides functions to manipulate basic data structures of 'Molecule's, such as indexing.
-}
module Spicy.Molecule.Internal.Util
  ( checkMolecule
  , reIndexMolecule
  , reIndex2BaseMolecule
  , groupTupleSeq
  , groupBy
  , makeSubMolsFromAnnoAtoms
  , findAtomInSubMols
  , groupAnnoAtomsAsSubMols
  )
where
import           Control.Exception.Safe
import           Control.Lens
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
import qualified Data.Text.Lazy                as TL
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
import           Spicy.Generic
import           Spicy.Molecule.Internal.Types

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
  | not layerIndCheck = throwM $ MolLogicException
    "checkMolecule"
    ("Bonds vs Atoms mismatch. Your bonds bind to non existing atoms." :: String)
  | not fragAtomsDisjCheck = throwM $ MolLogicException
    "checkMolecule"
    ("The atoms of deeper layers are not disjoint but shared by fragments." :: String)
  | not subsetCheckAtoms = throwM $ MolLogicException
    "checkMolecule"
    ("The atoms of deeper layers are not a subset of this layer." :: String)
  | not bondBidectorialCheck = throwM
  $ MolLogicException "checkMolecule" ("The bonds are not bidirectorially defined." :: String)
  | not atomCoordCheck = throwM
  $ MolLogicException "checkMolecule" ("Not all atoms have exactly 3 coordinates." :: String)
  | otherwise = if S.null (mol ^. molecule_SubMol)
    then return mol
    else do
      subMols <- traverse checkMolecule $ mol ^. molecule_SubMol
      return $ mol & molecule_SubMol .~ subMols
 where
  -- Indices of the atoms
  atomInds      = IM.keysSet $ mol ^. molecule_Atoms
  -- Indices of the bond origin atoms
  bondsOrig     = IM.keysSet $ mol ^. molecule_Bonds
  -- Indices of the bond target atoms
  bondsTarget   = IS.unions $ mol ^. molecule_Bonds
  -- Check if bond indices do not exceed atom indices.
  layerIndCheck = IS.null $ (bondsOrig `IS.union` bondsTarget) \\ atomInds
  -- Next layer molecules
  sM            = mol ^. molecule_SubMol
  sMSize        = S.length sM
  -- Disjointment test (no atoms and bonds shared through fragments)
  -- "bA" = Bool_A, "aA" = Atoms_A
  fragAtomsDisjCheck =
    fst
      . foldl'
          (\(bA, aA) (_, aB) ->
            if aA `iMdisjoint` aB && bA then (True, aA `IM.union` aB) else (False, aA)
          )
          (True, IM.empty)
      . S.zip (S.replicate sMSize True)
      . fmap (^. molecule_Atoms)
      $ sM
  -- Next Layer atoms all joined
  nLAtoms              = IM.unions . fmap (^. molecule_Atoms) $ sM
  nLAtomsInds          = IM.keysSet nLAtoms
  -- All pseudo atoms of the next layer set
  nLPseudoAtomsInds    = IM.keysSet . IM.filter (^. atom_IsPseudo) $ nLAtoms
  -- Test if the next deeper layer is a proper subset of the current layer.
  subsetCheckAtoms     = IS.null $ (nLAtomsInds \\ nLPseudoAtomsInds) \\ atomInds
  -- Check if the bonds are bidirectorial
  bondBidectorialCheck = imisBidirectorial $ mol ^. molecule_Bonds
  -- Check if all atoms have exactly 3 coodinates.
  atomCoordCheck = all (\aC -> S.length aC == 3) $ mol ^.. molecule_Atoms . each . atom_Coordinates
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

----------------------------------------------------------------------------------------------------
{-|
This reindexes all structures in a 'Molecule' with predefined counting scheme. This means counting
of 'Atom's will start at 0 and be consecutive. This also influences bonds in '_molecule_Bonds' and
layers in '_molecule_SubMol'. Pseudoatoms will be taken care of.
-}
reIndex2BaseMolecule :: MonadThrow m => Molecule -> m Molecule
reIndex2BaseMolecule mol =
  let allAtomIndices = getAtomIndices mol
      repMap         = IM.fromAscList . (\old -> zip old [0 ..]) . IS.toList $ allAtomIndices
  in  reIndexMolecule repMap mol

----------------------------------------------------------------------------------------------------
{-|
Get the indices of all 'Atom's in a 'Molecule', including those of sublayers in '_molecule_SubMol'
and pseudoatoms therein. This assumes a sane 'Molecule' according to 'checkMolecule'.
-}
getAtomIndices :: Molecule -> IntSet
getAtomIndices mol =
  let -- The indices of all atoms of the current layer
      thisLayerIndices = IM.keysSet $ mol ^. molecule_Atoms
      -- The indices of all sublayers + this layer.
      allIndices = foldr' (<>) thisLayerIndices . fmap (getAtomIndices) $ mol ^. molecule_SubMol
  in  allIndices

----------------------------------------------------------------------------------------------------
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

----------------------------------------------------------------------------------------------------
{-|
Reindex the '_molecule_Atoms' and '_molecule_Bonds' of a single layer of a molecule (ignoring
anything in the '_molecule_SubMol' field). While the completeness of the reindexing is checked and
incompleteness of the replacement 'IntMap' 'Int' will result in 'Left' 'String', it is not checked
if the 'Atom's indexing is sane and indices are unique.
-}
reIndexMoleculeLayer
  :: MonadThrow m
  => IntMap Int -- ^ 'IntMap' with mappings from old indices to new indices (bonds and
                --   atoms).
  -> Molecule   -- ^ Molecule to reindex.
  -> m Molecule -- ^ 'Left' 'String' in case the mapping from old keys to new keys is
                --   incomplete.
reIndexMoleculeLayer repMap mol
  |
  -- If old Atom indices cannot be completely replaced by the Map:
    not $ checkRepMapCompleteIS repMap (IM.keysSet $ mol ^. molecule_Atoms)
  = throwM $ MolLogicException "reIndexMoleculeLayer"
                               "The remapping of indices is not complete for the atom indices."
  |
  -- If old bond type data cannot be completely replaced by the Map:
    not $ checkRepMapCompleteIMIS repMap (mol ^. molecule_Bonds)
  = throwM $ MolLogicException "reIndexMoleculeLayer"
                               "The remapping of indices is not complete for the bond data."
  |
  -- If both checks went fine:
    otherwise
  = return
    $  mol
       -- Use the (%~) lens to update the atoms indices of a molecule with new indices.
    &  molecule_Atoms
    %~ replaceIMKeys repMap
       -- Use the (%~) lens to update keys and values (IntSet) of the bond type data structure.
    &  molecule_Bonds
    %~ replaceIMIS repMap

----------------------------------------------------------------------------------------------------
{-|
Take a group ('Seq') of 'Atom's as formed by 'groupAsSubMols and the bonds of the complete
'Molecule'. Then form a proper submolecule. This function relies on the group of being a proper
submolecule and will hapilly accept groups that are not.
-}
makeSubMolFromGroup
  :: Seq (Int, (Int, TL.Text), Atom) -- ^ A group of annotated atoms in the style of:
                                     --   @(atomIndex, (subMolID, subMolName), atom)@
  -> IntMap IntSet                   -- ^ Bond type data structure of the whole molecule.
  -> Molecule                        -- ^ A newly formed submolecule.
makeSubMolFromGroup group bonds =
  let atoms    = IM.fromList . toList . fmap (\(ind, _, atom) -> (ind, atom)) $ group
      label    = fromMaybe "" . (S.!? 0) . fmap (^. _2 . _2) $ group
      atomInds = IM.keysSet atoms
      -- Remove all bonds from the IntMap, that have origin on atoms not in this set and all
      -- target atoms that are not in the set.
      bondsCleaned :: IntMap IntSet
      bondsCleaned = IM.map (IS.filter (`IS.member` atomInds)) $ bonds `IM.restrictKeys` atomInds
  in  Molecule { _molecule_Label    = label
               , _molecule_Atoms    = atoms
               , _molecule_Bonds    = bondsCleaned
               , _molecule_SubMol   = S.empty
               , _molecule_Energy   = Nothing
               , _molecule_Gradient = Nothing
               , _molecule_Hessian  = Nothing
               }

----------------------------------------------------------------------------------------------------
{-|
From some common parser informations, form the sub molecules.
-}
makeSubMolsFromAnnoAtoms
  :: Seq (Int, (Int, TL.Text), Atom) -- ^ A plain 'Seq' of all 'Atom's in the 'Molecule'. They are
                                     --   annoated in a Tuple as:
                                     --   @(atomIndex, (subMolID, subMolName), atom)@
  -> IntMap IntSet                   -- ^ Bonds for the whole 'Molecule'. See '_molecule_Bonds'.
  -> Seq Molecule                    -- ^ The submolecules.
makeSubMolsFromAnnoAtoms annoAtoms bonds =
  let subMolAtomGroups = groupAnnoAtomsAsSubMols annoAtoms
  in  fmap (\g -> makeSubMolFromGroup g bonds) subMolAtomGroups

----------------------------------------------------------------------------------------------------
{-|
Given a 'IM.Key' (representing an 'Atom'), determine in which fragment ('_molecule_SubMol') the
'Atom' is.
-}
findAtomInSubMols
  :: Int             -- ^ 'Atom' to find in the fragments.
  -> IntMap Molecule -- ^ Annotated fragments in an 'IntMap'
  -> Maybe Int       -- ^ The 'IM.Key' aka fragment number in which the atom has been found.
findAtomInSubMols atomKey annoFrags =
  fst
    <$> ( IM.lookupMin
        . IM.filter (== True)
        . IM.map (\mol -> atomKey `IM.member` (mol ^. molecule_Atoms))
        $ annoFrags
        )

----------------------------------------------------------------------------------------------------
{-|
Group a common parser structure based on a submolecule identifier.
-}
groupAnnoAtomsAsSubMols
  :: Seq (Int, (Int, TL.Text), Atom)       -- ^ A plain 'Seq' of all 'Atom's in the 'Molecule'.
                                           --   They are annoated in a Tuple as:
                                           --   @(atomIndex, (subMolID, subMolName), atom)@
                                           --
  -> Seq (Seq (Int, (Int, TL.Text), Atom)) -- ^ Groups of annotated submolecules, which share a
                                           --   common subMolID.
groupAnnoAtomsAsSubMols annoAtoms =
  groupBy (\x y -> x ^. _2 . _1 == y ^. _2 . _1)
    . S.unstableSortOn (\(_, (subID, _), _) -> subID)
    $ annoAtoms
