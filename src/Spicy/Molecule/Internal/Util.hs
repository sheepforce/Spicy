{-|
Module      : Spicy.Molecule.Internal.Util
Description : Utilities to manipulate basic molecular data structures.
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides functions to manipulate basic data structures of 'Molecule's, such as indexing.
-}
{-# LANGUAGE DataKinds        #-}
module Spicy.Molecule.Internal.Util
  ( checkMolecule
  , molMap
  , molMapWithMolID
  , molTraverse
  , molTraverseWithID
  , molFoldl
  , molFoldlWithMolID
  , reIndexMolecule
  , reIndex2BaseMolecule
  , groupTupleSeq
  , groupBy
  , findAtomInSubMols
  , getNElectrons
  , getMolByID
  , molIDLensGen
  , getCalcByID
  , calcIDLensGen
  , newSubLayer
  , createPseudoAtom
  , calcPseudoCoords
  , addAtom
  , removeAtom
  , BondOperation(..)
  , changeBond
  , getCoordinatesAs3NMatrix
  , distMat
  , getDenseSparseMapping
  , getSparseDenseMapping
  , getAtomsAsVector
  , neighbourList
  , guessBondMatrixSimple
  , guessBondMatrix
  , fragmentDetectionDetached
  , getPolarisationCloudFromAbove
  , bondDistanceGroups
  , getAtomAssociationMap
  , fragmentAtomInfo2AtomsAndFragments
  )
where
import           Control.Lens            hiding ( (:>)
                                                , Empty
                                                , Index
                                                , index
                                                )
import           Control.Parallel.Strategies
import           Data.Default
import           Data.Foldable
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Data.Ix
import           Data.Massiv.Array             as Massiv
                                         hiding ( all
                                                , index
                                                , mapM
                                                , sum
                                                , toList
                                                )
import           Data.Massiv.Core.Operations    ( Numeric )
import           Data.Maybe
import           RIO                     hiding ( Vector
                                                , (^.)
                                                )
import qualified RIO.HashMap                   as HashMap
import qualified RIO.HashSet                   as HashSet
import qualified RIO.List                      as List
import qualified RIO.Map                       as Map
import           RIO.Seq                        ( Seq(..) )
import qualified RIO.Seq                       as Seq
import           Spicy.Class
import           Spicy.Data
import           Spicy.Generic
import           Spicy.Math

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

  - The 'IntMap.Key's of the '_molecule_Atoms' 'IntMap' are a superset of all 'IntMap.Key's and 'IntSet.Key's
    appearing in the 'IntMap' 'IntSet' of '_molecule_Bonds'
  - The deeper layers in 'molecule_SubMol' are proper subsets of the higher layer regarding
    non-pseudo 'Atom' indices
  - Fragments of the same layer are completelty disjoint in their atom indices
  - Bonds of a layer are bidirectorial
  - The size of '_atom_Coordinates' is strictly 3 for all atoms of this layer
-}
checkMolecule :: MonadThrow m => Molecule -> m Molecule
checkMolecule mol = do
  unless layerIndCheck . throwM $ MolLogicException
    "checkMolecule"
    "Bonds vs Atoms mismatch. Your bonds bind to non existing atoms."
  unless fragAtomsDisjCheck . throwM $ MolLogicException
    "checkMolecule"
    "The atoms of deeper layers are not disjoint but shared by fragments."
  unless subsetCheckAtoms . throwM $ MolLogicException
    "checkMolecule"
    "The atoms of deeper layers are not a subset of this layer."
  unless bondBidectorialCheck . throwM $ MolLogicException
    "checkMolecule"
    "The bonds are not bidirectorially defined."
  unless atomCoordCheck . throwM $ MolLogicException
    "checkMolecule"
    "The dimension of the coordinate vectors of the atoms is not exactly 3 for all atoms."
  unless fragmentsSelectionRangeCheck . throwM $ MolLogicException
    "checkMolecule"
    "The fragments contain indices of atoms, that do not exist in this molecule layer."
  unless fragmentCompletenessCheck . throwM $ MolLogicException
    "chechMolecule"
    "The fragments must either contain all atoms of a layer or no atoms. \
    \Sorting only a part of the atoms into fragments is not allowed."
  unless calcCheck . throwM $ MolLogicException
    "checkMolecule"
    "A calculation context has an impossible combination of charge and multiplicity."
  if IntMap.null (mol ^. molecule_SubMol)
    then return mol
    else do
      subMols <- traverse checkMolecule $ mol ^. molecule_SubMol
      return $ mol & molecule_SubMol .~ subMols
 where
  -- Indices of the atoms
  atomInds = IntMap.keysSet $ mol ^. molecule_Atoms
  -- Indices of the bonds.
  bondsInds =
    let tupleInds = HashMap.keys $ mol ^. molecule_Bonds
        origins   = IntSet.fromList . fmap fst $ tupleInds
        targets   = IntSet.fromList . fmap snd $ tupleInds
    in  origins <> targets
  -- Check if bond indices do not exceed atom indices.
  layerIndCheck = IntSet.null $ bondsInds IntSet.\\ atomInds
  -- Next layer molecules. Discard the Map structure
  sM            = mol ^. molecule_SubMol
  -- Disjointment test (no atoms and bonds shared through submolecules). This will not check
  -- pseudoatoms (they will be removed before the disjoint check), as the may have common numbers
  -- shared through the fragemnts.
  -- "bA" = Bool_A, "aA" = Atoms_A
  fragAtomsDisjCheck =
    fst
      . foldl'
          (\(bA, aA) (_, aB) ->
            if aA `intMapDisjoint` aB && bA then (True, aA `IntMap.union` aB) else (False, aA)
          )
          (True, IntMap.empty)
      . IntMap.map (\atoms -> (True, IntMap.filter (not . _atom_IsPseudo) atoms))
      . fmap (^. molecule_Atoms)
      $ sM
  -- Check if the dimension of the atom coordinate vector is exactly 3 for all atoms.
  atomCoordCheck =
    all (== 3)
      .  IntMap.map (Massiv.elemsCount . getVectorS . _atom_Coordinates)
      $  mol
      ^. molecule_Atoms
  -- Next Layer atoms all joined
  nLAtoms              = IntMap.unions . fmap (^. molecule_Atoms) $ sM
  nLAtomsInds          = IntMap.keysSet nLAtoms
  -- All pseudo atoms of the next layer set
  nLPseudoAtomsInds    = IntMap.keysSet . IntMap.filter (^. atom_IsPseudo) $ nLAtoms
  -- Test if the next deeper layer is a proper subset of the current layer.
  subsetCheckAtoms     = IntSet.null $ (nLAtomsInds IntSet.\\ nLPseudoAtomsInds) IntSet.\\ atomInds
  -- Check if the bonds are bidirectorial
  bondBidectorialCheck = isBondMatrixBidirectorial $ mol ^. molecule_Bonds
  -- Indices of all atoms assigned to fragments.
  allFragmentSelections =
    IntMap.foldl'
        (\selectionAcc fragment -> selectionAcc `IntSet.union` (fragment ^. fragment_Atoms))
        IntSet.empty
      $  mol
      ^. molecule_Fragment
  -- Check if only existing atoms are assigned to fragments.
  fragmentsSelectionRangeCheck = allFragmentSelections `IntSet.isSubsetOf` atomInds
  -- Check if either all or none of the atoms have been assigned to fragments. Sorting only a part
  -- of the atoms into fragments is not allowed.
  fragmentCompletenessCheck =
    let diffSet = atomInds IntSet.\\ allFragmentSelections
    in  diffSet == IntSet.empty || diffSet == atomInds
  -- Check if charge and multiplicity combinations of this layer are fine.
  calcCheck =
    all (== True)
      . Map.map
          (\calcContext ->
            let qmLens        = calcContext_Input . calcInput_QMMMSpec . _QM
                qmCharge      = calcContext ^? qmLens . qmContext_Charge
                qmMult        = calcContext ^? qmLens . qmContext_Mult
                nElectrons    = getNElectrons mol <$> qmCharge
                qmElectronsOK = case (nElectrons, qmMult) of
                  (Just n, Just m) ->
                    n + 1 >= m && ((even (n + 1) && odd m) || (odd (n + 1) && even m))
                  _ -> True
            in  qmElectronsOK
          )
      $ (mol ^. molecule_CalcContext)
    {-
    -- This check doesn't need to be true, as pseudobonds can break the subset property.
    -- All Bonds of the next layer joinded
    nLBonds              = IntMap.unions . VB.map (^. molecule_Bonds) $ sM
    -- Exclude bonds from and to pseudo atoms in deeper layers
    nLBondsOrigin        = IntMap.keysSet nLBonds \\ nLPseudoAtomsInds
    nLBondsTarget        = IntSet.unions nLBonds \\ nLPseudoAtomsInds
    subsetCheckBonds     =
      (nLBondsOrigin `IntSet.union` nLBondsTarget) \\ (bondsTarget `IntSet.union` bondsOrig)
    -}

----------------------------------------------------------------------------------------------------
{-|
Like a 'map' through the 'Molecule' data structure. Applies a function to each molecule. To keep
this non-mind-blowing, it is best to only use functions, which only act on the current molecule
layer and not on the deeper ones as the update function.
-}
molMap :: (Molecule -> Molecule) -> Molecule -> Molecule
molMap f mol =
  let subMols = mol ^. molecule_SubMol
  in  if IntMap.null subMols then f mol else f mol & molecule_SubMol %~ IntMap.map (molMap f)

----------------------------------------------------------------------------------------------------
{-|
Like a 'map' through the 'Molecule' data structure. Applies a function to each molecule. To keep
this non-mind-blowing, it is best to only use functions, which only act on the current molecule
layer and not on the deeper ones as the update function. The mapping function has access to the
current 'MolID'.
-}
molMapWithMolID :: (MolID -> Molecule -> Molecule) -> Molecule -> Molecule
molMapWithMolID f mol = go Empty f mol
 where
  go :: MolID -> (MolID -> Molecule -> Molecule) -> Molecule -> Molecule
  go molIdAcc func mol' =
    let subMols = mol' ^. molecule_SubMol
    in  if IntMap.null subMols
          then (func molIdAcc) mol'
          else (func molIdAcc) mol' & molecule_SubMol %~ IntMap.mapWithKey
            (\key val -> go (molIdAcc |> key) func val)

----------------------------------------------------------------------------------------------------
{-|
Like a 'mapM' through the 'Molecule' data structure. Applies a function to each molecule. To keep
this non-mind-blowing, it is best to only use functions, which only act on the current molecule
layer and not on the deeper ones as the update function.
-}
molTraverse :: Monad m => (Molecule -> m Molecule) -> Molecule -> m Molecule
molTraverse f mol = do
  topUpdated <- f mol
  let subMols = topUpdated ^. molecule_SubMol
  if IntMap.null subMols
    then return topUpdated
    else do
      newSubMols <- traverse
        (\deepMol ->
          if IntMap.null (deepMol ^. molecule_SubMol) then f deepMol else molTraverse f deepMol
        )
        subMols
      let newMol = topUpdated & molecule_SubMol .~ newSubMols
      return newMol

----------------------------------------------------------------------------------------------------
{-|
Like a 'mapM' through the 'Molecule' data structure. Applies a function to each molecule. To keep
this non-mind-blowing, it is best to only use functions, which only act on the current molecule
layer and not on the deeper ones as the update function.

This functions works top down through the molecule. The worker function has access to the MolID of
the molecule currently processed.
-}
molTraverseWithID :: Monad m => (MolID -> Molecule -> m Molecule) -> Molecule -> m Molecule
molTraverseWithID f mol = go Empty f mol
 where
  go molIDAcc func mol' = do
    let subMols = mol' ^. molecule_SubMol
    thisLayerApplied <- func molIDAcc mol'
    if IntMap.null subMols
      then return thisLayerApplied
      else do
        newSubMols <- IntMap.traverseWithKey
          (\key deepMol -> if IntMap.null (deepMol ^. molecule_SubMol)
            then (func $ molIDAcc |> key) deepMol
            else go (molIDAcc |> key) func deepMol
          )
          subMols
        let newMol = thisLayerApplied & molecule_SubMol .~ newSubMols
        return newMol

----------------------------------------------------------------------------------------------------
{-|
Like a fold through a molecule. This steps through the left-most branch of a molecule completely
before going to the more right sites.
-}
molFoldl :: (a -> Molecule -> a) -> a -> Molecule -> a
molFoldl f s mol =
  let subMols         = mol ^. molecule_SubMol
      topLayerApplied = f s mol
  in  if IntMap.null subMols
        then topLayerApplied
        else IntMap.foldl' (\acc subMol -> molFoldl f acc subMol) topLayerApplied subMols

----------------------------------------------------------------------------------------------------
{-|
Like a fold through a molecule. This steps through the left-most branch of a molecule completely
before going to the more right sites. The folding function has access to the current 'MolID'.
-}
molFoldlWithMolID :: (a -> MolID -> Molecule -> a) -> a -> Molecule -> a
molFoldlWithMolID f s mol = go Empty f s mol
 where
  -- go :: MolID -> (a -> MolID -> Molecule -> a) -> a -> Molecule -> a
  go molIdAcc func start mol' =
    let subMols          = mol' ^. molecule_SubMol
        thisLayerApplied = (\acc -> func acc molIdAcc) start mol'
    in  if IntMap.null subMols
          then thisLayerApplied
          else IntMap.foldlWithKey' (\acc key molVal -> go (molIdAcc |> key) f acc molVal)
                                    thisLayerApplied
                                    subMols

----------------------------------------------------------------------------------------------------
{-|
This reindexes all structures in a 'Molecule' with predefined counting scheme. This means counting
of 'Atom's will start at 0 and be consecutive. This also influences bonds in '_molecule_Bonds' and
layers in '_molecule_SubMol'. Pseudoatoms will be taken care of.
-}
reIndex2BaseMolecule :: MonadThrow m => Molecule -> m Molecule
reIndex2BaseMolecule mol =
  let allAtomIndices = getAtomIndices mol
      repMap = IntMap.fromAscList . (\old -> RIO.zip old [0 ..]) . IntSet.toList $ allAtomIndices
  in  reIndexMolecule repMap mol

----------------------------------------------------------------------------------------------------
{-|
Get the indices of all 'Atom's in a 'Molecule', including those of sublayers in '_molecule_SubMol'
and pseudoatoms therein. This assumes a sane 'Molecule' according to 'checkMolecule'.
-}
getAtomIndices :: Molecule -> IntSet
getAtomIndices mol =
  let -- The indices of all atoms of the current layer
      thisLayerIndices = IntMap.keysSet $ mol ^. molecule_Atoms
      -- The indices of all sublayers + this layer.
      allIndices       = foldr' (<>) thisLayerIndices . fmap getAtomIndices $ mol ^. molecule_SubMol
  in  allIndices

----------------------------------------------------------------------------------------------------
{-|
Reindex a complete 'Molecule', including all its deeper layers in '_molecule_SubMol') by mappings
from a global replacement Map, mapping old to new indices. This function assumes, that your molecule
is sane in the overall assumptions of this program. This means that the lower layers obey the
counting scheme of the atoms of the higher layers and pseudoatoms come last.
-}
reIndexMolecule :: MonadThrow m => IntMap Int -> Molecule -> m Molecule
reIndexMolecule repMap mol = molTraverse (reIndexMoleculeLayer repMap) mol

----------------------------------------------------------------------------------------------------
{-|
Reindex the '_molecule_Atoms' and '_molecule_Bonds' of a single layer of a molecule (ignoring
anything in the '_molecule_SubMol' field). While the completeness of the reindexing is checked and
incompleteness of the replacement 'IntMap' 'Int' will result in 'Left' 'String', it is not checked
if the 'Atom's indexing is sane and indices are unique.
-}
reIndexMoleculeLayer
  :: MonadThrow m
  => IntMap Int -- ^ 'IntMap' with mappings from old indices to new indices (bonds and atoms).
  -> Molecule   -- ^ 'Molecule' to reindex.
  -> m Molecule -- ^ 'Molecule' with __current__ layer reindexed.
reIndexMoleculeLayer repMap mol = do
  -- Check for completness of the replacement map for the atom keys.
  unless (intIsRepMapCompleteForSet repMap (IntMap.keysSet $ mol ^. molecule_Atoms))
    . throwM
    $ MolLogicException "reIndexMoleculeLayer"
                        "The remapping of indices is not complete for the atom indices."

  -- Check for completeness of the replacement map for the bond matrix.
  unless (isRepMapCompleteforBondMatrix repMap (mol ^. molecule_Bonds)) . throwM $ MolLogicException
    "reIndexMoleculeLayer"
    "The remapping of indices is not complete for the bond data."

  return
    $  mol
    -- Update the atoms indices of a molecule with new indices.
    &  molecule_Atoms
    %~ intReplaceMapKeys repMap
    -- Update keys and values (IntSet) of the bond type data structure.
    &  molecule_Bonds
    %~ replaceBondMatrixInds repMap
    -- Update the selection in the fragments.
    &  molecule_Fragment
    .  each
    .  fragment_Atoms
    %~ intReplaceSet repMap

----------------------------------------------------------------------------------------------------
{-|
Given a 'IntMap.Key' (representing an 'Atom'), determine in which fragment ('_molecule_SubMol') the
'Atom' is.
-}
findAtomInSubMols
  :: Int             -- ^ 'Atom' to find in the fragments.
  -> IntMap Molecule -- ^ Annotated fragments in an 'IntMap'
  -> Maybe Int       -- ^ The 'IntMap.Key' aka fragment number in which the atom has been found.
findAtomInSubMols atomKey annoFrags =
  fst
    <$> ( IntMap.lookupMin
        . IntMap.filter (== True)
        . IntMap.map (\mol -> atomKey `IntMap.member` (mol ^. molecule_Atoms))
        $ annoFrags
        )

----------------------------------------------------------------------------------------------------
{-|
Get the number of electrons for a 'Molecule' with a given charge.
-}
getNElectrons
  :: Molecule -- ^ The 'Molecule' to check.
  -> Int      -- ^ Charge of the 'Molecule'.
  -> Int      -- ^ Number of electrons of the 'Molecule' at the given charge.
getNElectrons mol charge' =
  let atoms         = mol ^. molecule_Atoms
      atomicNumbers = IntMap.map (\a -> fromEnum $ a ^. atom_Element) atoms
      nElectrons    = sum atomicNumbers - charge'
  in  nElectrons

----------------------------------------------------------------------------------------------------
{-|
From the complete data structure pf the 'Molecule', get the specific layer, you want.  This is now
the new top layer.
-}
getMolByID :: MonadThrow m => Molecule -> MolID -> m Molecule
getMolByID mol Seq.Empty = return mol
getMolByID mol (i :<| is) =
  let subMols = mol ^. molecule_SubMol
  in
    if IntMap.null subMols
      then throwM $ MolLogicException
        "getMolByID"
        "Maximum recursion depth of your molecule reached and no layers left.\
        \ Cannot go deeper into the structure."
      else case (mol ^. molecule_SubMol) IntMap.!? i of
        Just molDeeperLayer -> getMolByID molDeeperLayer is
        Nothing             -> throwM $ MolLogicException
          "getMolByID"
          "Could not find a molecule with this MolID.\
          \ Key not found in the submolecule map."

----------------------------------------------------------------------------------------------------
{-| This generator takes a 'MolID' and generates an Lens to access this molecule. Dont be confused
by the type signature, it is a normal lens to be used with '(^?)' as lookup (for setters sometimes
it helps do redefine the lens locally in the setter). Read the type signature more as:
@
    molIDLensGen :: MolID -> Lens' Molecule' Molecule'
@

 **I am a little bit proud that i figured this out.**
-}
-- molIDLensGen :: MolID -> Control.Lens.Lens' Molecule Molecule
molIDLensGen
  :: (Applicative f)
  => MolID                    -- ^ This is the MolID, for which you want a lense.
  -> (Molecule -> f Molecule)
  -> Molecule
  -> f Molecule
molIDLensGen molID =
  let stepThroughLayers = fmap (\subMolIx -> molecule_SubMol . ix subMolIx) molID
  in  foldl (.) id stepThroughLayers

----------------------------------------------------------------------------------------------------
{-|
From the complete data structure pf the 'Molecule', get the specific layer and a calculation on it.
-}
getCalcByID :: MonadThrow m => Molecule -> CalcID -> m (CalcContext, Molecule)
getCalcByID mol calcID = do
  molLayerOfInterest <- getMolByID mol (calcID ^. calcID_MolID)
  let calculations = molLayerOfInterest ^. molecule_CalcContext
  case calculations Map.!? (calcID ^. calcID_calcKey) of
    Just calc -> return (calc, molLayerOfInterest)
    Nothing ->
      throwM $ MolLogicException "getCalcByID" "Could not find a calculation with this key."

----------------------------------------------------------------------------------------------------
{-|
Generates a lens for calculation ID in a molecule. The type signature is confusing, read it more as
@
    calcIDLensGen :: CalcID -> Lens' Molecule CalcContext
@

This will create a normal lens to be used with '(^?)'.
-}
calcIDLensGen :: Applicative f => CalcID -> (CalcContext -> f CalcContext) -> Molecule -> f Molecule
calcIDLensGen (CalcID molID calcKey) = molIDLensGen molID . molecule_CalcContext . ix calcKey

----------------------------------------------------------------------------------------------------
{-|
Separates a new subsystem from the current molecule layer. Covalent bonds that were cut, can be
capped with pseudo atoms. The following behaviour is employed for the constructor fields of the
sublayer 'Molecule':

- '_molecule_Comment': Will be an empty.
- '_molecule_Atoms': The atoms that are kept will have no '_atom_Multipoles' information.
  Pseudoatoms will be added. Their 'Int' key will start at @(maximum index/key to keep) + 1@.
- '_molecule_Bonds': The ones from the top layer restricted to the indices supplied in origin and
  target.
- '_molecule_SubMol': Will be empty.
- '_molecule_EnergyDerivatives': Everything will be 'Nothing'.
- '_molecule_CalcContext': Will be empty.
-}
newSubLayer
  :: MonadThrow m
  => Molecule                           -- ^ Input molecule, for which a new layer will be inserted.
  -> IntSet                             -- ^ Selection of the atoms to keep for the sublayer.
  -> Maybe Double                       -- ^ Scaling radius $\(g\).
  -> Maybe (Element, AtomLabel, FFType) -- ^ Settings for the insertion of capping atoms.
  -> m Molecule                         -- ^ The original molecule modified by a sublayer inserted.
newSubLayer mol newLayerInds covScale capAtomInfo = do
  -- Before doing anything, make sure the molecule is sane.
  _ <- checkMolecule mol

  -- Check if the indices for the new layer are usable aka non empty
  when (IntSet.null newLayerInds) . throwM $ MolLogicException
    "newSubLayer"
    "Trying to create an empty layer. This is not possible with this function."

  -- Check if the indices for the new layer are strictly a subset of the old layer.
  let oldLayerAtomInds = IntMap.keysSet $ mol ^. molecule_Atoms
  if newLayerInds `IntSet.isSubsetOf` oldLayerAtomInds
    then return ()
    else throwM $ MolLogicException
      "newSubLayer"
      "The new layer indices are either reffering to atoms, that are not there."

  let atoms = mol ^. molecule_Atoms
      -- Convert the capping atom defaults to individual values.
      (capAtomElement, capAtomLabel, capAtomFFType) = case capAtomInfo of
        Nothing                             -> (Nothing, Nothing, Nothing)
        Just (caElement, caLabel, caFFType) -> (Just caElement, Just caLabel, Just caFFType)
      -- Determine which submolecules already exist and find the largest index of the existing
      -- submolecules.
      maxSubMolIndex   = fst <$> IntMap.lookupMax (mol ^. molecule_SubMol)
      newSubLayerIndex = fromMaybe 0 $ (+ 1) <$> maxSubMolIndex
      -- Define some default data for the new sublayer molecule.
      slComment =
        "Sublayer of "
          <> (mol ^. molecule_Comment)
          <> " keeping the indices "
          <> tShow (IntSet.toList newLayerInds)
          <> "."
      -- Just the atoms from the old layer that are kept but no pseudoatoms added yet.
      slAtomsToKeep       = (mol ^. molecule_Atoms) `IntMap.restrictKeys` newLayerInds
      -- Bonds from the old layer to keep but no bonds for pseudoatoms added yet.
      slBondsToKeep       = cleanBondMatByAtomInds (mol ^. molecule_Bonds) newLayerInds
      slSubMol            = IntMap.empty
      slFragments         = IntMap.empty
      slEnergyDerivatives = def :: EnergyDerivatives
      slCalcContext       = Map.empty

      -- Create a set of pairs of atoms, which have cut bonds due to sublayer isolation. The first
      -- part of the tuple will have the sublayer index and the second one the top layer index.
      cutAtomPairs :: HashSet (Int, Int)
      cutAtomPairs = HashMap.foldlWithKey'
        (\accPairs (ixO, ixT) val ->
          let isOinSL = ixO `IntSet.member` newLayerInds
              isTinSL = ixT `IntSet.member` newLayerInds
          in  case (val, isOinSL, isTinSL) of
                (True, True, False) -> HashSet.insert (ixO, ixT) accPairs
                _                   -> accPairs
        )
        HashSet.empty
        (mol ^. molecule_Bonds)

  -- This creates all necessary capping atoms for a new sublayer. The structure maps from the index
  -- of an atom in the new sublayer, to its capping pseudoatoms/link atoms.
  cappingPseudoAtoms <- HashSet.foldl'
    (\accPseudoAtomMap' (ixSL, ixTL) -> do
      accPseudoAtomMap <- accPseudoAtomMap'
      -- Lookup the sublayer atom, which needs a cap.
      slCappedAtom     <- case atoms IntMap.!? ixSL of
        Just a -> return a
        Nothing ->
          throwM
            .  MolLogicException "newSubLayer"
            $  "Lookup for atom with index "
            <> show ixSL
            <> " failed."
      -- Lookup the top layer atom, which has been removed and for which the cap is introduced.
      tlRemovedAtom <- case atoms IntMap.!? ixTL of
        Just a -> return a
        Nothing ->
          throwM
            .  MolLogicException "newSubLayer"
            $  "Lookup for atom with index "
            <> show ixTL
            <> " failed."
      -- Create a new pseudoatom, for the current pair.
      newPseudoAtom <-
        Seq.singleton
          <$> createPseudoAtom covScale
                               capAtomElement
                               capAtomLabel
                               capAtomFFType
                               slCappedAtom
                               tlRemovedAtom
      return . IntMap.insertWith (<>) ixSL newPseudoAtom $ accPseudoAtomMap
    )
    (return IntMap.empty)
    cutAtomPairs

  let -- Create an intermediate sublayer molecule, which does not yet have pseudoatoms but is
      -- otherwise cleaned of the information from the top layer.
      slIntermediateMol = Molecule { _molecule_Comment           = slComment
                                   , _molecule_Atoms             = slAtomsToKeep
                                   , _molecule_Bonds             = slBondsToKeep
                                   , _molecule_SubMol            = slSubMol
                                   , _molecule_Fragment          = slFragments
                                   , _molecule_EnergyDerivatives = slEnergyDerivatives
                                   , _molecule_CalcContext       = slCalcContext
                                   }

  -- Add all capping atoms to the sublayer
  subLayerWithPseudoAdded <- IntMap.foldlWithKey'
    (\accMol' slOrigin pseudoAtomList -> do
      accMol <- accMol'
      addPseudosFromList slOrigin accMol pseudoAtomList
    )
    (pure slIntermediateMol)
    cappingPseudoAtoms

  let -- Identify top level atoms, that are part of a capped bond. (Works thanks to the Foldable
      -- instance of IntMap ...)
      slAtomsCapped          = HashSet.map fst cutAtomPairs
      -- Mark all the atoms in the sublayer, that have lost a bond and got a link atom as capped.
      subLayerMarkedAsCapped = markAtomsAsCapped subLayerWithPseudoAdded slAtomsCapped
      -- Mark the top layer atoms, that are part of a capped bond as capped and add the newly
      -- constructed submolecule to this molecule.
      markedMolWithNewSublayer =
        mol & molecule_SubMol %~ IntMap.insert newSubLayerIndex subLayerMarkedAsCapped

  return markedMolWithNewSublayer
 where
  -- Add a list of capping pseudoatoms to an existing molecule and a single bond partner. Usually
  -- this will be just one pseudoatom but it could also add multiple caps to a single atom.
  addPseudosFromList :: MonadThrow m => Int -> Molecule -> Seq Atom -> m Molecule
  addPseudosFromList pseudoBondPartner mol' pseudoSeq = foldl'
    (\accMol' pseudoAtom -> do
      accMol                 <- accMol'
      (accNewInd, accNewMol) <- addAtom accMol pseudoAtom
      changeBond Add accNewMol (pseudoBondPartner, accNewInd)
    )
    (pure mol')
    pseudoSeq

  -- Mark a Set of atoms as capped in the current layer.
  markAtomsAsCapped :: Molecule -> HashSet Int -> Molecule
  markAtomsAsCapped mol' atomSelection = mol' & molecule_Atoms %~ IntMap.mapWithKey
    (\atomKey atom -> if atomKey `HashSet.member` atomSelection
      then atom & atom_IsCapped .~ True
      else atom & atom_IsCapped %~ id
    )

----------------------------------------------------------------------------------------------------
{-|
Function to create a capping pseudo atom from an atom to keep and an atom, that has been cut away.

Following special behaviours are used here:

- The scaling factor \(g\) (see below) might be given. If it is not given, a default value will be
  used
- A chemical 'Element' might be given for the new pseudoatom/link atom ('_atom_Element') and if not
  specified a hydrogen atom will be used.
- An 'AtomLabel' might be given. If not, the '_atom_Label' will be empty.
- A force-field type might be given. If not it will be 'XYZ'.

See also 'calcPseudoCoords'.

The position of the pseudoatom (commonly called link atom) is calculated as:
\[
\mathbf{R}_\mathrm{PA} =
  \mathbf{R}_\mathrm{CA} + g ( \mathbf{R}_\mathrm{RA} - \mathbf{R}_\mathrm{CA})
\]
where the scaling factor \(g\) will be calculated from a ratio of covalent radii, if not specified:
\[
g = \frac{r^\mathrm{cov}_\mathrm{CA} + \mathrm{cov}_\mathrm{PA}}{\mathrm{cov}_\mathrm{RA} + \mathrm{cov}_\mathrm{PA}}
\]
With:
- \( \mathbf{R}_\mathrm{PA} \): the coordinates of the created pseudoatom/link atom
- \( \mathbf{R}_\mathrm{RA} \): the coordinates of the atom that has been removed by creating the
  new layer
- \( \mathbf{R}_\mathrm{CA} \): the coordinates of the atom, that will be capped by the
  pseudoatom/link atom
- \( g \): the scaling factor for the position of the pseudoatom/link atom.
-}
createPseudoAtom
  :: MonadThrow m
  => Maybe Double    -- ^ Scaling factor \(g\) for the position of the pseudo atom.
  -> Maybe Element   -- ^ Chemical element for pseudoatom to create.
  -> Maybe AtomLabel -- ^ Textual label of the pseudoatom/link atom.
  -> Maybe FFType    -- ^ Force field type of the pseudoatom/link atom.
  -> Atom            -- ^ 'Atom' to cap with the pseudoatom.
  -> Atom            -- ^ 'Atom' that has been cut away and needs to be replaced by the pseudoatom,
                     --   that will be created.
  -> m Atom          -- ^ Created pseudoatom.
createPseudoAtom gScaleOption pseudoElementOption label fftype cappedAtom removedAtom = do
  let pseudoElement    = fromMaybe H pseudoElementOption
      cappedElement    = cappedAtom ^. atom_Element
      cappedCoords     = Massiv.delay . getVectorS $ cappedAtom ^. atom_Coordinates
      cappedCovRadius  = covalentRadii Map.!? cappedElement
      removedElement   = removedAtom ^. atom_Element
      removedCoords    = Massiv.delay . getVectorS $ removedAtom ^. atom_Coordinates
      removedCovRadius = covalentRadii Map.!? removedElement
      pseudoCovRadius  = covalentRadii Map.!? pseudoElement
      gFactorDefault :: Maybe Double
      gFactorDefault = do
        cappedCovRadius'  <- cappedCovRadius
        removedCovRadius' <- removedCovRadius
        pseudoCovRadius'  <- pseudoCovRadius
        return $ (cappedCovRadius' + pseudoCovRadius') / (removedCovRadius' + pseudoCovRadius')

  pseudoCoordinates <- case (gScaleOption, gFactorDefault) of
    (Just gScale, _          ) -> calcPseudoCoords cappedCoords removedCoords gScale
    (Nothing    , Just gScale) -> calcPseudoCoords cappedCoords removedCoords gScale
    (Nothing, Nothing) ->
      throwM
        .  MolLogicException "createPseudoAtom"
        $  "Can not find covalent radius of one of these elements "
        <> show (pseudoElement, cappedElement, removedElement)
        <> " and no scaling factor g has been given."

  let newPseudoAtom = Atom { _atom_Element     = pseudoElement
                           , _atom_Label       = fromMaybe "" label
                           , _atom_IsPseudo    = True
                           , _atom_IsCapped    = False
                           , _atom_IsDummy     = False
                           , _atom_FFType      = fromMaybe FFXYZ fftype
                           , _atom_Coordinates = VectorS . computeS $ pseudoCoordinates
                           , _atom_Multipoles  = def
                           }

  return newPseudoAtom

----------------------------------------------------------------------------------------------------
{-|
Calculate the new coordinates of a pseudoatom/linkatom as:
\[
\mathbf{R}_\mathrm{PA} =
  \mathbf{R}_\mathrm{CA} + g ( \mathbf{R}_\mathrm{RA} - \mathbf{R}_\mathrm{CA})
\]
With:
- \( \mathbf{R}_\mathrm{PA} \): the coordinates of the created pseudoatom/link atom
- \( \mathbf{R}_\mathrm{RA} \): the coordinates of the atom that has been removed by creating the
  new layer
- \( \mathbf{R}_\mathrm{CA} \): the coordinates of the atom, that will be capped by the
  pseudoatom/link atom
-}
-- TODO(phillip|p=40|#Unfinished) - Check if the scaling factor is correct. Results look spurious.
calcPseudoCoords
  :: (Load r Ix1 a, MonadThrow m, Numeric r a)
  => Vector r a -- ^ Coordinates of the atom being capped.
  -> Vector r a -- ^ Coordinates of the atom being removed.
  -> a          -- ^ The scaling factor g.
  -> m (Vector r a)
calcPseudoCoords cappedAtomCoords removedAtomCoords gScale = do
  diffVec <- removedAtomCoords .-. cappedAtomCoords
  let scaledDiffVec = gScale *. diffVec
  cappedAtomCoords .+. scaledDiffVec

----------------------------------------------------------------------------------------------------
{-|
Adds an 'Atom' to the current 'Molecule' layer. Its 'IntMap.Key'will be larger by 1 than the largest
atom index at the current layer. The atom will also be added to all deeper layers with the same
'IntMap.Key'. If the 'IntMap.Key', that was determined from the current layer already exists in a
deeper layer (because a pseudoatom in a deeper layer has been given this key), an error will be
thrown. It is therefore advisable to use this function only on molecules, that do not have
pseudoatoms in deeper layers yet. No bonds will be updated.

The 'Int' returned is the 'IntMap' key of the newly added atom.
-}
addAtom :: MonadThrow m => Molecule -> Atom -> m (Int, Molecule)
addAtom mol atom = do
  -- Before doing anything, make sure the molecule is sane.
  _ <- checkMolecule mol

  -- Find the largest index of this layer.
  let thisLayerMaxIndex = IntSet.findMax . IntMap.keysSet $ mol ^. molecule_Atoms
      newAtomIndex      = thisLayerMaxIndex + 1

  -- Insert the new atom recursively into all layers
  newMol <- goInsert mol newAtomIndex atom

  return (newAtomIndex, newMol)

 where
  goInsert :: MonadThrow m => Molecule -> Int -> Atom -> m Molecule
  goInsert mol' newInd' atom' = do
    let atoms = mol ^. molecule_Atoms

    -- Check if the key for the new atom would be new/unique.
    when (newInd' `IntMap.member` atoms)
      .  throwM
      .  MolLogicException "addAtom"
      $  "Cannot add atom with key "
      <> show newInd'
      <> " to the current molecule layer. The key already exists."

    -- Add the atom with the given index to the current layer.
    let thisLayerMolAtomAdded = mol' & molecule_Atoms %~ IntMap.insert newInd' atom'
        subMols               = thisLayerMolAtomAdded ^. molecule_SubMol

    -- Add recursively also to all deeper layers.
    if IntMap.null subMols
      then return thisLayerMolAtomAdded
      else do
        updatedSubMols <- traverse (\m -> goInsert m newInd' atom') subMols
        return $ thisLayerMolAtomAdded & molecule_SubMol .~ updatedSubMols

----------------------------------------------------------------------------------------------------
{-|
Removes an 'Atom' specified by its index key from the 'Molecule' and all deeper layers. If the atom
specified was a pseudoatom in the highest layer, it will remove pseudoatoms in the deeper layers,
that have the same key. If the atom to remove was not a pseudoatom in the highest layer, but a
pseudoatom with the same index is found in deeper layers, the pseudoatom will not be touched. Bonds
will be cleaned from references to this atom, if the atom is removed.
-}
removeAtom :: MonadThrow m => Molecule -> Int -> m Molecule
removeAtom mol atomInd = do
  -- Before doing anything check if the molecule is sane.
  _ <- checkMolecule mol

  -- Get information about the atom of interest.
  let atoms        = mol ^. molecule_Atoms
      atomExists   = atomInd `IntMap.member` atoms
      atomIsPseudo = case atoms IntMap.!? atomInd of
        Just a  -> a ^. atom_IsPseudo
        Nothing -> False

  when atomExists
    .  throwM
    .  MolLogicException "removeAtom"
    $  "Cannot remove atom with key "
    <> show atomInd
    <> " from the top molecule layer. The key does not exist."

  return $ goRemove mol (atomInd, atomIsPseudo)

 where
  goRemove :: Molecule -> (Int, Bool) -> Molecule
  goRemove mol' (atomInd', wasPseudo) =
    -- Get information about the atom to remove in the current layer.
    let atoms        = mol' ^. molecule_Atoms
        atomIsPseudo = case atoms IntMap.!? atomInd' of
          Just a  -> a ^. atom_IsPseudo
          Nothing -> False

        -- If it did not change if this atom is a pseudo atom, remove it from this layer.
        -- Otherwise it must be a different atom now and we dont touch it.
        atomsUpdated = if atomIsPseudo == wasPseudo then atomInd' `IntMap.delete` atoms else atoms

        -- Remove bonds involving this atom if there was no change wether this atom was a
        -- pseudoatom.
        bonds            = mol' ^. molecule_Bonds
        bondsUpdated     = removeBondsByAtomIndsFromBondMat bonds (IntSet.singleton atomInd')

        -- Update the current layer with bonds and atoms cleaned.
        thisLayerUpdated = mol' & molecule_Bonds .~ bondsUpdated & molecule_Atoms .~ atomsUpdated

        -- Get the submolecules and update them lazily.
        subMols          = thisLayerUpdated ^. molecule_SubMol
        subMolsUpdated   = IntMap.map (\m -> goRemove m (atomInd', wasPseudo)) subMols
    in  if IntMap.null subMols
          then thisLayerUpdated
          else thisLayerUpdated & molecule_SubMol .~ subMolsUpdated

----------------------------------------------------------------------------------------------------
{-|
Descriptor of what to do with bonds.
-}
data BondOperation
  = Add
  | Remove
  deriving Eq

----------------------------------------------------------------------------------------------------
{-|
Adds\/removes a bond recursively to\/from a 'Molecule' and its deeper layers. Two atom 'IntMap.Key's
are specified, to indicate between which 'Atom's a new bond shall be inserted/removed. If these
'Atom's/'IntMap.Key's do not exist in the top layer, this function will fail. If the atoms do not
exist in deeper layers or one of them became a pseudoatom, no bond will be inserted/removed in the
deeper layer. If a bond already exists/does not exist between those atoms, it will not be changed.

If one of the two atoms was a pseudoatom in the top layer and still is in the deeper layers, bonds
will be inserted/removed normally. If one the atoms was not a pseudoatom in the top layer but
becomes a pseudoatom in a deeper layer, bonds involving those atoms will not be touched.
-}
changeBond :: MonadThrow m => BondOperation -> Molecule -> (Int, Int) -> m Molecule
changeBond operation mol (at1, at2) = do
  -- Check sanity of the molecule before doin anything else.
  _ <- checkMolecule mol

  -- Check if origin and target key are present as atoms in the molecule at all.
  let atoms         = mol ^. molecule_Atoms
      atom1Exists   = at1 `IntMap.member` atoms
      atom2Exists   = at2 `IntMap.member` atoms
      -- These lookups are safe as long as they are lazy and the check if the atoms exist happens
      -- before those information are used.
      atom1IsPseudo = (atoms IntMap.! at1) ^. atom_IsPseudo
      atom2IsPseudo = (atoms IntMap.! at2) ^. atom_IsPseudo

  if atom1Exists && atom2Exists
    then return ()
    else
      throwM
      .  MolLogicException "changeBond"
      $  "Wanted to add a bond between atoms"
      <> show at1
      <> " and "
      <> show at2
      <> " but at least one of those atoms does not exist in the top layer."

  -- If checks are passed, start with updating the current layer and work all the way down.
  return $ goDeepAddRemove operation mol ((at1, atom1IsPseudo), (at2, atom2IsPseudo))

 where
  -- Update the deeper layers with a different check.
  goDeepAddRemove :: BondOperation -> Molecule -> ((Int, Bool), (Int, Bool)) -> Molecule
  goDeepAddRemove operation' mol' ((at1', wasPseudo1), (at2', wasPseudo2)) =
    -- Check if both origin and target exist and if they are pseudo atom.
    let
      atoms          = mol' ^. molecule_Atoms
      originExitst   = at1' `IntMap.member` atoms
      targetExists   = at2' `IntMap.member` atoms
      originIsPseudo = case atoms IntMap.!? at1' of
        Just a  -> a ^. atom_IsPseudo
        Nothing -> False
      targetIsPseudo = case atoms IntMap.!? at2' of
        Just a  -> a ^. atom_IsPseudo
        Nothing -> False

      -- Choose the update function depending wether to remove or add bonds.
      updateFunction = case operation' of
        Add    -> addBondToBondMat
        Remove -> removeBondFromBondMat

      -- Update the bonds of this layer if everything is fine. Keep them as is otherwise.
      thisLayerBonds = mol' ^. molecule_Bonds
      thisLayerBondsUpdated =
        if all
           (== True)
           [originExitst, targetExists, wasPseudo1 == originIsPseudo, wasPseudo2 == targetIsPseudo]
        then
          updateFunction thisLayerBonds (at1', at2')
        else
          thisLayerBonds

      -- Lazily update the submolecules recursively.
      subMols        = mol' ^. molecule_SubMol
      subMolsUpdated = IntMap.map
        (\m -> goDeepAddRemove operation' m ((at1', wasPseudo1), (at2', wasPseudo2)))
        subMols
    in
      if IntMap.null subMols -- Update deeper layers only if required to end the recursion.
        then mol' & molecule_Bonds .~ thisLayerBondsUpdated
        else mol' & molecule_Bonds .~ thisLayerBondsUpdated & molecule_SubMol .~ subMolsUpdated

----------------------------------------------------------------------------------------------------
{-|
Obtain the coordinates of the current molecule layer as nAtoms x 3 matrix. This fails if one of the
coordinate vectors is not 3 elements long.
-}
{-|
Get the coordinates of the current molecule layer as matrix with (nAtoms x 3), where the cartesian
coordinates for each atom are along the rows of the matrix. Fails if some of the coordinate vectors
have the wrong size.
-}
{-# INLINE getCoordinatesAs3NMatrix #-}
-- getCoordinatesAs3NMatrix :: (MonadThrow m) => Molecule -> m (Matrix DL Double)
getCoordinatesAs3NMatrix :: MonadThrow m => Molecule -> m (Matrix DL Double)
getCoordinatesAs3NMatrix mol = do
  let atomCoords = IntMap.map (getVectorS . _atom_Coordinates) . _molecule_Atoms $ mol
      nAtoms     = IntMap.size atomCoords
  allCoordsConcat <- Massiv.concatM 1 . fmap snd . IntMap.toAscList $ atomCoords
  Massiv.resizeM (Sz (nAtoms :. 3)) allCoordsConcat
  -- return undefined
    -- . Massiv.computeAs Massiv.S
    -- . Massiv.setComp Par

----------------------------------------------------------------------------------------------------
{-|
Calculates the distance matrix of the current molecule layer in parallel.
-}
{-|
Calculates a distance matrix from a linearised vector of cartesian coordinates.
-}
{-# INLINE distMat #-}
distMat :: (MonadThrow m) => Molecule -> m (Matrix D Double)
distMat mol = do
  -- Reshapes the 3N vector into a 3 x N matrix. The x-axis represents atom indices and y-axis has
  -- cartesian coordinates.
  n3Vec <- Massiv.computeAs Massiv.S . Massiv.setComp Par <$> getCoordinatesAs3NMatrix mol
  let
     -- Get the number of atoms.
    nAtoms :: Int
    Sz (nAtoms :. _) = Massiv.size n3Vec
    -- The x-Axis is now a repetition of the atoms on the y-Axis (which were previously
    -- the x-axis) and z now stores the 3 compotents of the coordinates.
    xVec :: Array D Ix3 Double
    xVec = Massiv.expandOuter (Sz1 nAtoms) const n3Vec
    -- Swap x and y axsis and also have numbers of atoms ox x again.
    yVec :: Array D Ix3 Double
    yVec = Massiv.transposeInner xVec
    -- The xVec is now a structure with repetition of all atom coordinates along the x-Axis, the
    -- index of the atom on the y axis and the cartesian components on the z axis. The yVec has x-
    -- and y-axes transposed. Now overlap these two matrices and for each x and y components, a
    -- pair of R3 vectors on the z-axis is obtained. Those are folded to distances between the
    -- atoms pairs represented by x and y index.
    distances :: Matrix D Double
    distances =
      Massiv.map sqrt . Massiv.foldlInner (+) 0.0 . Massiv.map (** 2) $ Massiv.zipWith (-) xVec yVec
  return distances

----------------------------------------------------------------------------------------------------
{-|
Get mapping from dense arrays as in Massiv (starting at 0) to the sparse indexing used in the
'Molecule' type with 'IntMap' and 'HashMap'.
-}
{-# INLINE getDenseSparseMapping #-}
getDenseSparseMapping :: Molecule -> Vector P Int
getDenseSparseMapping mol =
  let atomIndices = IntMap.keys . _molecule_Atoms $ mol in Massiv.fromList Par atomIndices

----------------------------------------------------------------------------------------------------
{-|
The opposite of 'getDenseSparseMapping'. Contains mapping from the original atom indices to the
dense indices.
-}
{-# INLINE getSparseDenseMapping #-}
getSparseDenseMapping :: Molecule -> IntMap Int
getSparseDenseMapping mol =
  let atomIndices  = IntMap.keys . _molecule_Atoms $ mol
      denseIndices = [0 ..]
  in  IntMap.fromList $ RIO.zip atomIndices denseIndices
----------------------------------------------------------------------------------------------------
{-|
Get the atoms of the current layer in a dense Massiv vector.
-}
{-# INLINE getAtomsAsVector #-}
getAtomsAsVector :: Molecule -> Vector B Atom
getAtomsAsVector mol = Massiv.fromList Par . fmap snd . IntMap.toAscList . _molecule_Atoms $ mol

----------------------------------------------------------------------------------------------------
{-|
Generate a neighbouhr list. This is an set of association of one atom, with all the ones, which are
within a certain distance. The neighbours are free of self-interaction.
-}
{-# INLINE neighbourList #-}
neighbourList :: (MonadThrow m, MonadIO m) => Double -> Molecule -> m (IntMap IntSet)
neighbourList maxNeighbourDist mol = do
  -- Gets the atom coordinates in Nx3 matrix representation.
  atomCoords <- Massiv.computeAs Massiv.S . Massiv.setComp Par <$> getCoordinatesAs3NMatrix mol
  -- Find maximum and minumum value in each dimension.
  xCoords    <- atomCoords <!? 0
  yCoords    <- atomCoords <!? 1
  zCoords    <- atomCoords <!? 2
  xMin       <- Massiv.minimumM xCoords
  xMax       <- Massiv.maximumM xCoords
  yMin       <- Massiv.minimumM yCoords
  yMax       <- Massiv.maximumM yCoords
  zMin       <- Massiv.minimumM zCoords
  zMax       <- Massiv.maximumM zCoords
  let
    -- Mapping from dense 0-based coordinates to the sparse indexing in the Molecule.
    atomIndexDenseToSparseMapping :: Vector P Int
    atomIndexDenseToSparseMapping = getDenseSparseMapping mol
    -- Mapping from the sparse indices as used in the mol to the dense ones as used in arrays.
    atomIndexSparseToDenseMapping :: IntMap Int
    atomIndexSparseToDenseMapping = getSparseDenseMapping mol
    -- Convert the original sparse atom IntMap to the dense representation as used in arrays.
    atomDenseMap :: IntMap Atom
    atomDenseMap = intReplaceMapKeys atomIndexSparseToDenseMapping $ mol ^. molecule_Atoms
    {-
    -- The number of atoms in the current molecule layer.
    nAtoms :: Int
    nAtoms = IntMap.size atomIndexDenseToSparseMapping
    -}
    -- Definition of the cell dimensions of an orthorhombic cell around the atoms
    _orthorhombicCellSize :: (Double, Double, Double)
    _orthorhombicCellSize@(cellSizeX, cellSizeY, cellSizeZ) =
      (xMax - xMin, yMax - yMin, zMax - zMin)
    -- Define a bin size for linear scaling neighbour search, which defines the side length of the
    -- cubic bin cells. It must be at least as big as the maximum neighbour distance but a minimum
    -- of 3 angstrom cubes will be used.
    binSize :: Double
    binSize = max maxNeighbourDist 3
    -- Calculate the maximum bin index in each direction. (0-based)
    nBinsMaxIx :: (Int, Int, Int)
    nBinsMaxIx@(nBinsMaxIxX, nBinsMaxIxY, nBinsMaxIxZ) =
      let f cellSz = floor $ cellSz / binSize in (f cellSizeX, f cellSizeY, f cellSizeZ)
    -- The number of bins per direction.
    nBinsDim :: (Int, Int, Int)
    nBinsDim@(nBinsX, nBinsY, nBinsZ) = (nBinsMaxIxX + 1, nBinsMaxIxY + 1, nBinsMaxIxZ + 1)

    -- Linearised index for the bin cells.
    ixOrigin                          = (0, 0, 0)
    binLinearIxRange                  = (ixOrigin, nBinsMaxIx)
    {-
    -- The overall number of bins.
    nBins :: Int
    nBins = Data.Ix.rangeSize binLinearIxRange
    -}
    -- Sort the atoms into bins now. This is basically the conversion from the cartesian
    -- coordinates to bin indices in R3.
    atomBinAssignment :: Matrix D Int
    atomBinAssignment = Massiv.imap
      (\((_atom :. cartComponent)) el -> case cartComponent of
        0 -> floor $ (el - xMin) / binSize
        1 -> floor $ (el - yMin) / binSize
        2 -> floor $ (el - zMin) / binSize
        _ -> -1
      )
      atomCoords
    -- Linearise the bin assignment of the atoms with respect to the bin index.
    atomBinAssignmentLinear :: Massiv.Vector D Int
    atomBinAssignmentLinear = Massiv.ifoldlInner
      (\(_atom :. cartComponent) accBinLIx el -> case cartComponent of
        0 -> accBinLIx + Data.Ix.index binLinearIxRange (el, 0, 0)
        1 -> accBinLIx + Data.Ix.index binLinearIxRange (0, el, 0)
        2 -> accBinLIx + Data.Ix.index binLinearIxRange (0, 0, el)
        _ -> Data.Ix.index binLinearIxRange nBinsDim - 1
      )
      0
      atomBinAssignment
    -- Sort the atoms into bins (bin characterised by first index) and keep their index around in a
    -- tuple as (assignedBin, atomIndex).
    atomBinAssignmentLinearSort :: Massiv.Vector U (Int, Int)
    atomBinAssignmentLinearSort =
      Massiv.quicksort
        . Massiv.computeAs Massiv.U
        . Massiv.setComp Par
        . Massiv.imap (\atomIx binLIx -> (binLIx, atomIx))
        $ atomBinAssignmentLinear
    {-
    -- A vector of (linearised bin index, number of atoms in this bin)
    atomsPerBin :: Massiv.Vector DS (Int, Int)
    atomsPerBin =
      Massiv.tally
        . Massiv.computeAs Massiv.S
        . Massiv.setComp Par
        . Massiv.map fst
        $ atomBinAssignmentLinearSort
    -- The maximum amount of atoms in a bin.
    maxAtomsPerBin :: Int
    maxAtomsPerBin =
      Massiv.maximum'
        . Massiv.map snd
        . Massiv.computeAs Massiv.U
        . Massiv.setComp Par
        $ atomsPerBin
    -}

  -- A HashMap from scalar bin index to the atoms a bin contains.
  binAtomMap <- do
    -- Create the equally sized rows of the bin (rows) to atom indices (column).
    let binGroups = vectorToGroups fst atomBinAssignmentLinearSort
    -- The bin indices (only bins that actually contain atoms)
    binIndices <-
      traverse
          (\aGroup -> do
            groupHead <- case List.headMaybe aGroup of
              Nothing -> throwM $ DataStructureException
                "neighbouhrList"
                "Found an empty atom group for a bin,\
                \ but only non-empty groups should be specified in this step."
              Just h -> return h
            let groupBinIx = fst groupHead
            return groupBinIx
          )
          binGroups
        `using` rpar
    -- The atom vectors already expanded to matrices for easier concatenation.
    let binAtomSets :: [IntSet]
        binAtomSets   = List.map (IntSet.fromList . List.map snd) binGroups
        mappingGroups = HashMap.fromList $ List.zip binIndices binAtomSets
    return mappingGroups

  let -- Create the array of ALL bins to the atoms they contain. This is a 3D array, indexed by the
      -- bins.
      binAtomMatrix :: Array B Ix3 IntSet
      binAtomMatrix = Massiv.makeArrayLinear
        Par
        (Sz (nBinsX :> nBinsY :. nBinsZ))
        (\lBinIx -> HashMap.lookupDefault IntSet.empty lBinIx binAtomMap)
      -- Create a stencil over the 3 bin dimensions, which combines all sets of the neighbouring bins
      -- with the set of this bin.
      {-# INLINE neighbourCollectionStencil #-}
      neighbourCollectionStencil :: Stencil Ix3 IntSet IntSet
      neighbourCollectionStencil =
        Massiv.makeStencilDef IntSet.empty (Sz (3 :> 3 :. 3)) (1 :> 1 :. 1) $ \get ->
          let validIndRange     = [-1, 0, 1]
              allStencilIndices = fmap
                toIx3
                [ (x, y, z) | x <- validIndRange, y <- validIndRange, z <- validIndRange ]
              allStencilGetters = fmap get allStencilIndices
          in  foldl' (<>) (pure IntSet.empty) allStencilGetters
      -- Apply the stencil to the bins and collect in each bin all the atoms, that need to be checked
      -- against each other.
      collectedAtomSetInBins :: Array DW Ix3 IntSet
      collectedAtomSetInBins =
        Massiv.mapStencil (Fill IntSet.empty) neighbourCollectionStencil binAtomMatrix
      -- Within each collected bin, calculate all distances now of all possible combinations and
      -- keep those, which fullfill the distance criterion.
      neighboursInBins :: Array D Ix3 (IntMap IntSet)
      neighboursInBins =
        Massiv.map (correlateIntSet atomDenseMap maxNeighbourDist)
          . Massiv.computeAs Massiv.B
          . Massiv.setComp Par
          $ collectedAtomSetInBins

  -- Join all neighbours from the individual bins together. Unfortunately this lives
  neighboursInDenseNumbering <- do
    let foldingF :: IntMap IntSet -> IntMap IntSet -> IntMap IntSet
        foldingF = IntMap.unionWith IntSet.union
    Massiv.foldlP foldingF IntMap.empty foldingF IntMap.empty neighboursInBins

  -- Remap the neighbour list back to the sparse mapping, that has been originally used.
  let neighboursInSparseNumbering =
        ( IntMap.map
              (\neighbourSet -> IntSet.map
                (\denseKey -> atomIndexDenseToSparseMapping Massiv.! denseKey)
                neighbourSet
              )
          . IntMap.mapKeys (\denseKey -> atomIndexDenseToSparseMapping Massiv.! denseKey)
          $ neighboursInDenseNumbering
          )
          `using` parTraversable rpar

  return neighboursInSparseNumbering
 where
  -- Calculate the distances between all atoms in the set and keep only those pairs, which have a
  -- distance less or equal than the supplied one. If an atom cannot be found in the atom map,
  -- it will no be a neighbour of anything. Therefore this function never fails.
  correlateIntSet :: IntMap Atom -> Double -> IntSet -> IntMap IntSet
  correlateIntSet denseAtomMap maxDist atomsInBin =
    let atomList = IntSet.toAscList atomsInBin
        unfilteredPotentialNeigbhours =
            IntMap.fromAscList $ List.zip atomList (List.repeat atomsInBin)
        neighbours = IntMap.filter (not . IntSet.null) $ IntMap.mapWithKey
          (\originInd targetIndSet ->
            checkDistancesFromOriginAtom denseAtomMap maxDist originInd targetIndSet
          )
          unfilteredPotentialNeigbhours
    in  neighbours
  -- Checks for a set of target atoms, if they are within a given distance of the origin atom.
  -- This never fails. If an atom that was looked up by its index is not in the IntMap of atoms,
  -- it will not be a neighbour of the origin. If the origin cannot be found, there will be no
  -- neighbours.
  checkDistancesFromOriginAtom :: IntMap Atom -> Double -> Int -> IntSet -> IntSet
  checkDistancesFromOriginAtom denseAtomMap maxDist originInd targetInds =
    let originCoords =
            Massiv.delay . getVectorS . _atom_Coordinates <$> denseAtomMap IntMap.!? originInd
    in  IntSet.filter
          (\tIx ->
            let targetCoords =
                    Massiv.delay . getVectorS . _atom_Coordinates <$> denseAtomMap IntMap.!? tIx
                dist = join $ distance <$> originCoords <*> targetCoords
            in  case dist of
                  Nothing -> False
                  Just d  -> d <= maxDist && tIx /= originInd
          )
          targetInds

----------------------------------------------------------------------------------------------------
{-|
Quadratic scaling version of bond matrix guessing. Uses full distance matrix to filter for distances
small enough. Applies only to the current layer of the molecule.

If the covalent radius of an element is unknown, no bonds for this atom will be defined.
-}
{-# INLINE guessBondMatrixSimple #-}
guessBondMatrixSimple :: (MonadThrow m, MonadIO m) => Maybe Double -> Molecule -> m BondMatrix
guessBondMatrixSimple covScaling mol = do
  let -- Mapping from the dense 0-based coordinates to the sparse indexing in the Molecule.
      atomIndexDenseToSparseMapping :: Vector P Int
      atomIndexDenseToSparseMapping = getDenseSparseMapping mol
      -- If no scaling factor for covalent radii checks has been defined, default to 1.4.
      radScaling :: Double
      radScaling = fromMaybe defCovScaling covScaling
      -- Vector of all atoms in dense indexing.
      atomsVector :: Massiv.Vector B Atom
      atomsVector = getAtomsAsVector mol
      -- Number of atoms in the molecule.
      nAtoms :: Int
      nAtoms = Massiv.elemsCount atomsVector
      -- Vector of all elements.
      elementsVector :: Massiv.Vector B Element
      elementsVector = Massiv.compute . Massiv.setComp Par . Massiv.map _atom_Element $ atomsVector
      -- Build the same strucute for element pairs as it has been done for the distance matrix.
      elementPairs :: Matrix D (Element, Element)
      elementPairs =
        let xElements :: Matrix D Element
            xElements = Massiv.expandInner (Sz nAtoms) const elementsVector
            yElements :: Matrix D Element
            yElements = Massiv.expandOuter (Sz nAtoms) const elementsVector
        in  Massiv.zip xElements yElements
      -- Calculate the sum of the covalent radii for the element pairs and then scale them.
      covRadiiSums :: Matrix D (Maybe Double)
      covRadiiSums = Massiv.map
        (\(elA, elB) -> do
          radiusA <- covalentRadii Map.!? elA
          radiusB <- covalentRadii Map.!? elB
          return $ radScaling * (radiusA + radiusB)
        )
        elementPairs
  -- Calculate the distance matrix.
  distanceMatrix <- distMat mol
  let -- Create a dense bond matrix but keep it delayed.
      bondMatrixDenseSelfeInteraction :: Matrix D Bool
      bondMatrixDenseSelfeInteraction = Massiv.zipWith
        (\dM cRM ->
          let distanceCheck = (dM <=) <$> cRM
          in  case distanceCheck of
                Just True -> True
                _         -> False
        )
        distanceMatrix
        covRadiiSums
      -- Remove the self-interaction from the bond matrix.
      bondMatrixDense :: Matrix D Bool
      bondMatrixDense = Massiv.imap (\(ixC :. ixR) val -> if ixC == ixR then False else val)
                                    bondMatrixDenseSelfeInteraction
  -- Fold the dense bond matrix to the sparse HashMap representation.
  bondMatrix <- Massiv.ifoldlP
    (\accBM (rIx :. cIx) bondBool -> if bondBool
      then
        let rIxSparse = atomIndexDenseToSparseMapping Massiv.! rIx
            cIxSparse = atomIndexDenseToSparseMapping Massiv.! cIx
        in  HashMap.insert (rIxSparse, cIxSparse) bondBool accBM
      else accBM
    )
    HashMap.empty
    (<>)
    HashMap.empty
    bondMatrixDense
  return bondMatrix

----------------------------------------------------------------------------------------------------
{-|
Linear scaling version of bond matrix guessing based on covalent radii. Constructs a neighbourlist
first and only checks within the neighbour list for potential bond partners.
-}
{-# INLINE guessBondMatrix #-}
guessBondMatrix :: (MonadThrow m, MonadIO m) => Maybe Double -> Molecule -> m BondMatrix
guessBondMatrix covScaling mol = do
  let -- Original IntMap of the atoms.
    atoms :: IntMap Atom
    atoms = mol ^. molecule_Atoms
    -- Get all chemical elements of the current molecule layer.
    atomElements :: IntMap Element
    atomElements = IntMap.map _atom_Element atoms
    -- Get the largest covalent radius.
    atomMaxCovRadius :: Maybe Double
    atomMaxCovRadius =
      fmap maximum
        . sequence
        . IntMap.filter isJust
        . IntMap.map (covalentRadii Map.!?)
        $ atomElements
    -- If no scaling factor for covalent radii checks has been defined, default to 1.4.
    radiusScale :: Double
    radiusScale = fromMaybe defCovScaling covScaling

  -- Continuing makes only sense if at least for some element the covalent radii have been found.
  maxCovAtomRadius <- case atomMaxCovRadius of
    Nothing -> throwM $ MolLogicException
      "guessBondMatrix"
      "Cannot find the largest covalent radius in the molecule. Is your molecule layer maybe empty?"
    Just r -> return r

  -- The maximum distance for neighbour search.
  let maxNeighbourDist = maxCovAtomRadius * 2 * radiusScale

  -- Build the neighbour list first.
  neighbours <- neighbourList maxNeighbourDist mol

  let -- Fold the bond matrix from the neighbour list.
      bondMatrix :: BondMatrix
      bondMatrix =
        IntMap.foldlWithKey'
            (\accBM originIx targetSet ->
              let newRow = makeScreenedBondMatrixRow radiusScale originIx targetSet atoms
              in  accBM <> newRow
            )
            HashMap.empty
            neighbours
          `using` parTraversable rpar
  return bondMatrix
 where
  -- Create a row of a bond matrix for a given origin atom and a pre-filtered set of potential
  -- target atoms. Elements, for which a covalent radius cannot be found will have no bonds.
  makeScreenedBondMatrixRow :: Double -> Int -> IntSet -> IntMap Atom -> BondMatrix
  makeScreenedBondMatrixRow covScale originInd targetInds originalAtoms = IntSet.foldl'
    (\bondMatrixRowAcc targetInd ->
      let originAtom    = originalAtoms IntMap.!? originInd
          targetAtom    = originalAtoms IntMap.!? targetInd
          originCoords  = Massiv.delay . getVectorS . _atom_Coordinates <$> originAtom
          targetCoords  = Massiv.delay . getVectorS . _atom_Coordinates <$> targetAtom
          originElement = _atom_Element <$> originAtom
          targetElement = _atom_Element <$> targetAtom
          originRadius  = (covalentRadii Map.!?) =<< originElement
          targetRadius  = (covalentRadii Map.!?) =<< targetElement
          realDistance  = join $ distance <$> originCoords <*> targetCoords
          sumCovRadii   = (+) <$> originRadius <*> targetRadius
          maxDistance   = (covScale *) <$> sumCovRadii
      in  case (<=) <$> realDistance <*> maxDistance of
            -- The radii of both elements have been found and the real distance between the atoms is
            -- smaller than the scaled sum of the covalent radii.
            Just True  -> HashMap.insert (originInd, targetInd) True bondMatrixRowAcc
            -- Both elements have been found but the distance is too large.
            Just False -> bondMatrixRowAcc
            -- One of the elements has not been found.
            Nothing    -> bondMatrixRowAcc
    )
    HashMap.empty
    targetInds

----------------------------------------------------------------------------------------------------
{-|
Fragment detection in a molecule. The fragment detection works by following the bond matrix. Sets of
atoms, which do not have any covalent connections to atoms outside of their own set are considered a
fragment.
-}
{-# INLINE fragmentDetectionDetached #-}
fragmentDetectionDetached :: MonadThrow m => Molecule -> m (IntMap IntSet)
fragmentDetectionDetached mol = do
  -- Make sure the molecule is sane, otherwise the fragments will potentially be garbage.
  _ <- checkMolecule mol

  -- Detect all fragments by following the bond matrix.
  let bonds        = mol ^. molecule_Bonds
      atoms        = mol ^. molecule_Atoms
      allFragments = findAllFragments bonds atoms IntMap.empty
  return allFragments
 where
  -- Takes a starting atom and the bond matrix of the molecule and an (initially empty) set of atoms
  -- that are already part of the fragment. Starting from this atom follow the bonds through the
  -- molecule. until no new atoms can be found anymore.
  findFromStartingAtom
    :: Int        -- ^ Index of the atom currently checked
    -> BondMatrix -- ^ The bond matrix of the molecules.
    -> IntSet     -- ^ The atom indices in the fragments. Accumulates during the recursion.
    -> IntSet     -- ^ The bond matrix after all bonds involving the current atom have been
                  --   removed and the next bound neighbours from this atom.
  findFromStartingAtom atomIndex bondMatrix fragmentAcc =
    let -- All bonding partners of the currently inspected atom.
        bondsTargetsOfThisAtoms = IntSet.fromList . fmap snd . HashMap.keys $ HashMap.filterWithKey
          (\(oIx, _) val -> oIx == atomIndex && val)
          bondMatrix
        -- Only the bond targets, that are not yet part of the fragment.
        newTargets = bondsTargetsOfThisAtoms IntSet.\\ fragmentAcc
    in  if IntSet.null newTargets
          -- If no new targets were found, terminate the recursive search here.
          then let fragmentAccNew = fragmentAcc <> IntSet.singleton atomIndex in fragmentAccNew
          -- If new targets were found, recursively also follow the new targets.
          else
            let fragmentAccNew = fragmentAcc <> newTargets <> IntSet.singleton atomIndex
            in  IntSet.foldl'
                  (\fragAcc newTarget -> findFromStartingAtom newTarget bondMatrix fragAcc)
                  fragmentAccNew
                  newTargets
  -- Move through a complete molecule until all fragments have been found.
  findAllFragments
    :: BondMatrix    -- ^ Bond matrix of the complete molecule.
    -> IntMap Atom   -- ^ An accumulator of atoms, which have not yet been assigned to a fragment.
    -> IntMap IntSet -- ^ Growing accumulator of fragments, that have been found yet.
    -> IntMap IntSet
  findAllFragments bondMat atomMapAcc fragAcc =
    let -- Use the first atom of the rest of the whole molcule, which has not yet been consumed into
        -- fragments as a starting atom for the next fragment search.
        atomHead      = fst <$> IntMap.lookupMin atomMapAcc
        newFragment   = findFromStartingAtom <$> atomHead <*> pure bondMat <*> pure IntSet.empty
        -- Remove the atoms of the new fragment from the whole system for the next iteration.
        atomMapAccNew = IntMap.withoutKeys atomMapAcc <$> newFragment
        -- Get the highest key of the fragment IntMap before insertion of the new fragment.
        highestKey    = case IntMap.lookupMax fragAcc of
          Just (k, _) -> k
          Nothing     -> 0
        -- Add the new fragment with an incremented key to the IntMap of fragments.
        newFragAcc = IntMap.insert (highestKey + 1) <$> newFragment <*> pure fragAcc
    in  case (atomMapAccNew, newFragAcc) of
          -- A new fragment was found
          (Just remainingAtoms, Just fragmentsFoundYet) ->
            findAllFragments bondMat remainingAtoms fragmentsFoundYet
          -- The atoms already have been fully consumed and therefore no new fragments could be
          -- found.
          _ -> fragAcc

----------------------------------------------------------------------------------------------------
{-|
This function adds multipole centres to a given molecule layer. In the context of ONIOM, this means,
that the multipoles of a real system are used as a polarisation cloud of the deeper layer.
Polarisation centres, that are real atoms in the model system, will be removed. A sequence of values
gives scaling factors for the multipoles in bond distances.

If the atoms of the real system above do not contain multipole information, you will have useless
dummy atoms in the model system.

Be aware of a nasty corner case:
@
  b--c--d
 /       \
A         E
@
The atoms @A@ and @E@ belong to the model system, which shall be polarised, while @b@, @c@ and @d@
belong to the real system, which provides the polarisation cloud. If scaling factors for up to three
bonds are given, atom @b@ has a distance of 1 from atom @A@ but a distance of 3 from atom @E@. This
introduces some ambiguity to the scaling. This will use the smallest distance found to select the
scaling factor. Therefore @b@ and @d@ will both be treated as being in a distance of 1 bond, not @b@
having a distance of 1 with respect to @A@ and a distance of 3 to @E@.
-}
getPolarisationCloudFromAbove
  :: (MonadThrow m)
  => Molecule   -- ^ The whole molecular system with the multipoles at least in the layer above the
                --   specified one.
  -> MolID      -- ^ Specificaiton of the molecule layer, which should get a polarisation cloud from
                --   the layer above. The layer above is also determined from the MolID by removing
                --   its last element.
  -> Seq Double -- ^ Scaling factors for the multipole moments in bond distances. The first value in
                --   the sequence would be used to scale the multipoles of atoms 1 bond away the
                --   non-pseudoatom model system atoms, the second value in the sequence multipoles
                --   2 bonds away and so on.
  -> m Molecule -- ^ The specified layer of the molecule only with polarisation centres added. All
                --   submolecules or fragments this layer may have had are removed.
getPolarisationCloudFromAbove mol layerID poleScalings = do
  -- Check for the sanity of the molecule, as otherwise the assumptions regarding bonds make no
  -- sense.
  _          <- checkMolecule mol

  -- Check if we are at least 1 layer deep into the ONIOM structure, otherwise asking for a
  -- polarisation cloud makes no sense. If we are at least one layer deep, we can also get the ID of
  -- one layer above, which provides the polarisation cloud.
  molIdAbove <- case layerID of
    Empty -> throwM $ MolLogicException
      "getPolarisationCloudFromAbove"
      "Requested to obtain a polarisation cloud for the real system, but this makes no sense."
    above :|> _ -> return above

  -- Get the layer, that shall be polarised and the layer above this one.
  layerOfInterest          <- getMolByID mol layerID
  layerOfPolarisationCloud <- getMolByID mol molIdAbove

  -- Get the atoms, that will be used for polarisation. This excludes atoms, which are also part of
  -- the model system and also atoms, that were already dummy atoms in the real system.
  let atomIndicesModelSystem = IntMap.keysSet $ layerOfInterest ^. molecule_Atoms
      polarisationCentresUnscaled =
        IntMap.withoutKeys (layerOfPolarisationCloud ^. molecule_Atoms) atomIndicesModelSystem

  -- Get polarisation centres n bonds away from the capped model-system atoms. This distance is used
  -- to scale the multipole moments. Potential problems arise, when an atom of the real system is
  -- in a different distance to capped atoms. In this case, the smallest distance will be used for
  -- scaling.
  let searchDistance = Seq.length poleScalings
      cappedOrigins =
        IntMap.keys . IntMap.filter (^. atom_IsCapped) $ layerOfInterest ^. molecule_Atoms
  distanceGroups <- mapM
    (\startAtom -> bondDistanceGroups layerOfPolarisationCloud startAtom searchDistance)
    cappedOrigins
  let distanceGroupsJoined = combineDistanceGroups distanceGroups

  -- Apply the scaling to the polarisation centres.
  let polarisationCentresScaled = IntMap.map (& atom_IsDummy .~ True) $ Seq.foldlWithIndex
        (\atomMapAcc currentDist scaleAtDist ->
          let atomsIndsAtThisDistance =
                  fromMaybe IntSet.empty $ distanceGroupsJoined Seq.!? (currentDist + 1)
              atomsWithPolesAtThisDistScaled = IntMap.mapWithKey
                (\key atom -> if key `IntSet.member` atomsIndsAtThisDistance
                  then atom & atom_Multipoles %~ scaleMultipoles scaleAtDist
                  else atom
                )
                atomMapAcc
          in  atomsWithPolesAtThisDistScaled
        )
        polarisationCentresUnscaled
        poleScalings

  -- Add the scaled polarisation centres to the model system.
  let polarisedModelSystem =
        layerOfInterest & molecule_Atoms %~ flip IntMap.union polarisationCentresScaled
  return polarisedModelSystem
 where
  -- Getting the distance of real system atoms to the capped atoms of the model system, gives
  -- multiple distances of a given real system atom to the model system atoms in bond distances.
  -- This function joins the real system atoms, which are grouped by bond distance from a given
  -- capped model system atom, in a way to ensure that a real system atom always only appears at
  -- the lowest possible distance to the model system.
  combineDistanceGroups :: [Seq IntSet] -> Seq IntSet
  combineDistanceGroups distGroups =
    let -- Join all the distance groups of different capped atoms. An atom of the real system might
        -- appear here mutliple times with different distances.
        combinationWithAmbigousAssignments = List.foldl'
          (\acc thisGroup ->
            let lengthAcc    = Seq.length acc
                lengthThis   = Seq.length thisGroup

                -- Adjust the length of the accumulator and this group, to match the longer one of both.
                lengthAdjAcc = if lengthThis > lengthAcc
                  then acc Seq.>< Seq.replicate (lengthThis - lengthAcc) IntSet.empty
                  else acc
                lengthAdjThis = if lengthAcc > lengthThis
                  then thisGroup Seq.>< Seq.replicate (lengthAcc - lengthThis) IntSet.empty
                  else thisGroup

                -- Join this group with the accumulator.
                newAcc = Seq.zipWith IntSet.union lengthAdjAcc lengthAdjThis
            in  newAcc
          )
          Seq.empty
          distGroups
        -- Make sure, that a given atom always only appears at the lower distance and remove its
        -- occurence at a higher distance, if already found at a lower distance.
        combinationUnambigous = foldl
          (\(atomsAtLowerDist, unambDistSeqAcc) thisDistanceAtoms ->
            let thisWithLowerRemoved = thisDistanceAtoms IntSet.\\ atomsAtLowerDist
                newAtomsAtLowerDist  = thisWithLowerRemoved `IntSet.union` atomsAtLowerDist
                newUnambDistSeqAcc   = unambDistSeqAcc |> thisWithLowerRemoved
            in  (newAtomsAtLowerDist, newUnambDistSeqAcc)
          )
          (IntSet.empty, Seq.empty)
          combinationWithAmbigousAssignments
    in  snd combinationUnambigous

  -- Apply a scaling factor to all multipole moments.
  scaleMultipoles :: Double -> Multipoles -> Multipoles
  scaleMultipoles scalingFactor poles =
    poles
      &  multipole_Monopole
      %~ fmap (* scalingFactor)
      &  multipole_Dipole
      %~ fmap (VectorS . massivScale scalingFactor . getVectorS)
      &  multipole_Quadrupole
      %~ fmap (MatrixS . massivScale scalingFactor . getMatrixS)
      &  mutlipole_Octopole
      %~ fmap (Array3S . massivScale scalingFactor . getArray3S)
      &  mutlipole_Hexadecapole
      %~ fmap (Array4S . massivScale scalingFactor . getArray4S)
    where massivScale a = Massiv.computeS . (.* a) . Massiv.delay


----------------------------------------------------------------------------------------------------
{-|
This function takes a starting atom in given layer and starts moving away from it along the bonds.
Atoms of the same bond distance will be grouped. Therefore all atoms 1 bond away from the start will
form a group, all bonds 2 bonds away from the starting atom will form a group and so on. Atoms will
only belong to one group always, which is relevant for cyclic structures. They will always be
assigned to the group with the lowest distance.

The function takes a maximum distance in bonds to search. The search ends, if either no new atoms
can be found or the search would extend beyond the maximum search distance.
-}
bondDistanceGroups
  :: MonadThrow m
  => Molecule       -- ^ The molecule layer which to use for the search. Sublayers are ignored.
  -> Int            -- ^ Key of the starting atom.
  -> Int            -- ^ The maximum distance for search in bonds. Must therefore be equal or
                    --   greater than 0.
  -> m (Seq IntSet) -- ^ A sequence of atoms in a given distance. At the sequence index 0 will be
                    --   the starting atom index. At sequence index n will be the set of atom
                    --   indices n bonds away from the start.
bondDistanceGroups mol startAtomInd maxBondSteps = do
  -- Check the molecule, as the bond matrix could otherwise be damaged and create strange results.
  _ <- checkMolecule mol

  let bondMat = mol ^. molecule_Bonds
      groups  = stepAndGroup bondMat maxBondSteps (Seq.singleton . IntSet.singleton $ startAtomInd)
  return groups
 where
  stepAndGroup
    :: BondMatrix -- ^ The bond matrix through which to step. During the recursion this will
                  --   shrink, as the atoms already assigned will be removed as and targets.
    -> Int        -- ^ The maximum distance in bonds.
    -> Seq IntSet -- ^ Accumulator of the groups.
    -> Seq IntSet
  stepAndGroup bondMat' maxDist groupAcc
    | currentDist > maxDist
    = groupAcc
    | otherwise
    = let -- The currently most distant atoms from the start and therefore the origin for the next
          -- search.
          searchOrigins = case groupAcc of
            Empty           -> IntSet.singleton startAtomInd
            _ :|> lastGroup -> lastGroup

          -- Make sure the group accumulator always contains the start atom at distance 0.
          groupAccAdjusted = case groupAcc of
            Empty -> Seq.singleton . IntSet.singleton $ startAtomInd
            _     -> groupAcc

          -- The next more distant sphere of atoms. Potentially, in case of a ring closure, the new
          -- sphere might contain atoms of this sphere, due to how the bond matrix is updated in the
          -- recursion. To avoid this, remove the origins from the next sphere.
          nextSphereAtoms = (`IntSet.difference` searchOrigins) . IntSet.unions $ fmap
            (getAtomBondPartners bondMat')
            (IntSet.toList searchOrigins)

          -- The bond matrix for the next iteration will have this iterations search origins removed
          -- both as origin as well as target. This should make sure, that stepping back is not
          -- possible.
          newBondMat = HashMap.filterWithKey
            (\(ixO, ixT) val ->
              not (ixO `IntSet.member` searchOrigins)
                && not (ixT `IntSet.member` searchOrigins)
                && val
            )
            bondMat'

          -- The new accumulator will contain the results as a new entry to the sequence.
          newGroupAcc = groupAccAdjusted |> nextSphereAtoms
      in  stepAndGroup newBondMat maxDist newGroupAcc
    where currentDist = Seq.length groupAcc

  -- Get all bond partners of an atom.
  getAtomBondPartners :: BondMatrix -> Int -> IntSet
  getAtomBondPartners bondMat' atom =
    IntSet.fromList
      . fmap snd
      . HashMap.keys
      . HashMap.filterWithKey (\(ixO, _) val -> ixO == atom && val)
      $ bondMat'

----------------------------------------------------------------------------------------------------
{-|
Given a molecule, build the associations of this layers atoms, with the fragments of this layers.
Therefore, if fragments represent the whole molecule, this will assign Just Fragment to each atom.
This is the fragment to which the atom belongs.
-}
getAtomAssociationMap
  :: MonadThrow m
  => Molecule                                 -- ^ The current molecule layer, for which to build
                                              --   the association map.
  -> m (IntMap (Atom, Maybe (Int, Fragment))) -- ^ The original 'IntMap' of atoms, but the atoms are
                                              --   now associated with a fragment and the fragments
                                              --   'Int' key.
getAtomAssociationMap mol = do
  let atoms     = mol ^. molecule_Atoms
      fragments = mol ^. molecule_Fragment
  atomToFragAssociations <- IntMap.traverseWithKey
    (\atomKey atom -> do
      -- Find all fragments, to which an atom is assigned.
      let matchingFragments =
            IntMap.filter (\frag -> atomKey `IntSet.member` (frag ^. fragment_Atoms)) fragments

      case IntMap.size matchingFragments of
        0 -> return (atom, Nothing)
        1 -> return (atom, IntMap.lookupMin matchingFragments)
        _ ->
          throwM
            .  MolLogicException "getAtomAssociationMap"
            $  "Assignment of atom "
            <> show atomKey
            <> " to a fragment failed, as the assignment is ambigous. \
               \This means an invalid data structure in the fragments."
    )
    atoms
  return atomToFragAssociations

----------------------------------------------------------------------------------------------------
{-|
Parser easily obtain lists of 'FragmentAtomInfo', which need to be converted to proper fragments and
atoms for the molecule. This function builds the data types for '_molecule_Atoms' and
'_molecule_Fragment'.
-}
fragmentAtomInfo2AtomsAndFragments :: [FragmentAtomInfo] -> (IntMap Atom, IntMap Fragment)
fragmentAtomInfo2AtomsAndFragments info =
  let atoms = IntMap.fromList . fmap (\i -> (faiAtomIndex i, faiAtom i)) $ info
      fragments =
          IntMap.fromListWith fragmentUnion . fmap (\i -> (faiFragmentIndex i, faiFragment i)) $ info
  in  (atoms, fragments)
 where
    -- Joins two fragments. The '_fragment_Label' and '_fragment_Chain' should be identical but this
    -- is not strictly required by this functions. Instead this functions uses a left bias for the
    -- fragments.
  fragmentUnion :: Fragment -> Fragment -> Fragment
  fragmentUnion a b =
    let atomsInB = b ^. fragment_Atoms in a & fragment_Atoms %~ IntSet.union atomsInB
