-- |
-- Module      : Spicy.Molecule.Internal.Util
-- Description : Utilities to manipulate basic molecular data structures.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides functions to manipulate basic data structures of 'Molecule's, such as indexing.
module Spicy.Molecule.Internal.Util
  ( checkMolecule,
    molMap,
    molMapWithMolID,
    molTraverse,
    molTraverseWithID,
    molFoldl,
    molFoldlWithMolID,
    isAtomLink,
    reIndexMolecule,
    reIndex2BaseMolecule,
    getMaxAtomIndex,
    groupTupleSeq,
    groupBy,
    findAtomInSubMols,
    findAtomInFragment,
    getNElectrons,
    getCappedAtoms,
    getMolByID,
    molIDLensGen,
    getCalcByID,
    calcIDLensGen,
    newSubLayer,
    createLinkAtom,
    calcLinkCoords,
    addAtom,
    removeAtom,
    BondOperation (..),
    changeBond,
    getCoordinatesAs3NMatrix,
    distMat,
    getDenseSparseMapping,
    getSparseDenseMapping,
    getAtomsAsVector,
    neighbourList,
    guessBondMatrixSimple,
    guessBondMatrix,
    fragmentDetectionDetached,
    getPolarisationCloudFromAbove,
    combineDistanceGroups,
    bondDistanceGroups,
    getAtomAssociationMap,
    fragmentAtomInfo2AtomsAndFragments,
    getJacobian,
    redistributeLinkMoments,
    redistributeLinkMoments',
    removeRealLinkTagsFromModel,
    getAllCalcIDsHierarchically,
    getAllMolIDsHierarchically,
    updatePositionsPosVec,
    updatePositions,
    shrinkNeighbourList,
    isolateMoleculeLayer,
    molID2OniomHumanID,
  )
where

import Control.Parallel.Strategies
import Data.Default
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Ix
import Data.Massiv.Array as Massiv hiding
  ( Index,
    all,
    index,
    mapM,
    new,
    sum,
    toList,
  )
import Data.Massiv.Core.Operations ()
import Data.Maybe
import Optics hiding (Empty, element, (:>))
import RIO hiding
  ( Lens',
    Vector,
    (%~),
    (.~),
    (^.),
    (^..),
    (^?),
  )
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.List as List
import qualified RIO.Map as Map
import RIO.Partial (toEnum)
import RIO.Seq (Seq (..))
import qualified RIO.Seq as Seq
import qualified RIO.Text as Text
import Spicy.Common
import Spicy.Data
import Spicy.Math
import Spicy.Molecule.Internal.Types
import System.IO.Unsafe

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
"pA" = link atoms
-}

-- | Check sanity of 'Molecule', which means test the following criteria:
--
--   - The 'IntMap.Key's of the '_#atoms' 'IntMap' are a superset of all 'IntMap.Key's and 'IntSet.Key's
--     appearing in the 'IntMap' 'IntSet' of '_#bonds'
--   - The deeper layers in '#subMol' are proper subsets of the higher layer regarding
--     non-link 'Atom' indices
--   - Fragments of the same layer are completelty disjoint in their atom indices
--   - Bonds of a layer are bidirectorial
--   - The size of '_atom_Coordinates' is strictly 3 for all atoms of this layer
checkMolecule :: MonadThrow m => Molecule -> m Molecule
checkMolecule mol = do
  unless layerIndCheck . throwM . localExc $
    "Bonds vs Atoms mismatch. Your bonds bind to non existing atoms."
  unless subMolAtomsDisjCheck . throwM . localExc $
    "The atoms of deeper layers are not disjoint but shared by fragments."
  unless subsetCheckAtoms . throwM . localExc $
    "The atoms of deeper layers are not a subset of this layer."
  unless bondBidectorialCheck . throwM . localExc $
    "The bonds are not bidirectorially defined."
  unless atomCoordCheck . throwM . localExc $
    "The dimension of the coordinate vectors of the atoms is not exactly 3 for all atoms."
  unless fragmentsSelectionRangeCheck . throwM . localExc $
    "The fragments contain indices of atoms, that do not exist in this molecule layer."
  unless fragmentCompletenessCheck . throwM . localExc $
    "The fragments must contain all atoms of a layer."
  unless calcCheck . throwM . localExc $
    "A calculation context has an impossible combination of charge and multiplicity."
  if IntMap.null (mol ^. #subMol)
    then return mol
    else do
      subMols <- traverse checkMolecule $ mol ^. #subMol
      return $ mol & #subMol .~ subMols
  where
    localExc = MolLogicException "checkMolecule"
    -- Indices of the atoms
    atomInds = IntMap.keysSet $ mol ^. #atoms

    -- Indices of the bonds.
    bondsInds =
      let tupleInds = HashMap.keys $ mol ^. #bonds
          origins = IntSet.fromList . fmap fst $ tupleInds
          targets = IntSet.fromList . fmap snd $ tupleInds
       in origins <> targets

    -- Check if bond indices do not exceed atom indices.
    layerIndCheck = IntSet.null $ bondsInds IntSet.\\ atomInds

    -- Next layer molecules. Discard the Map structure
    sM = mol ^. #subMol

    -- Disjointment test (no atoms and bonds shared through submolecules). This will not check
    -- dummy atoms (they will be removed before the disjoint check), as the may have common numbers
    -- shared through the fragemnts.
    -- "bA" = Bool_A, "aA" = Atoms_A
    subMolAtomsDisjCheck =
      fst
        . foldl'
          ( \(bA, aA) (_, aB) ->
              if aA `intMapDisjoint` aB && bA then (True, aA `IntMap.union` aB) else (False, aA)
          )
          (True, IntMap.empty)
        . IntMap.map (\atoms' -> (True, IntMap.filter (\a -> not $ a ^. #isDummy) atoms'))
        . fmap (^. #atoms)
        $ sM

    -- Check if the dimension of the atom coordinate vector is exactly 3 for all atoms.
    atomCoordCheck =
      all (== 3)
        . IntMap.map (Massiv.elemsCount . getVectorS . coordinates)
        $ mol
          ^. #atoms

    -- Next Layer atoms all joined
    nLAtoms = IntMap.unions . fmap (^. #atoms) $ sM
    nLAtomsInds = IntMap.keysSet nLAtoms

    -- All link atoms of the next layer set
    nLLinkAtomsInds = IntMap.keysSet . IntMap.filter (\a -> isAtomLink $ a ^. #isLink) $ nLAtoms

    -- Test if the next deeper layer is a proper subset of the current layer.
    subsetCheckAtoms = IntSet.null $ (nLAtomsInds IntSet.\\ nLLinkAtomsInds) IntSet.\\ atomInds

    -- Check if the bonds are bidirectorial
    bondBidectorialCheck = isBondMatrixBidirectorial $ mol ^. #bonds

    -- Indices of all atoms assigned to fragments.
    allFragmentSelections = IntSet.unions $ mol ^.. #fragment % each % #atoms

    -- Check if only existing atoms are assigned to fragments.
    fragmentsSelectionRangeCheck = allFragmentSelections `IntSet.isSubsetOf` atomInds

    -- Check if all atoms have been assigned to fragments. Sorting only a part
    -- of the atoms into fragments is not allowed.
    fragmentCompletenessCheck =
      let diffSet = atomInds IntSet.\\ allFragmentSelections
       in diffSet == IntSet.empty

    -- Check if charge and multiplicity combinations of this layer are fine.
    molNoDummies = mol & #atoms %~ IntMap.filter (\a -> not $ a ^. #isDummy)
    calcCheck =
      all (== True)
        . Map.map
          ( \calcContext' ->
              let qmLens = #input % #qMMMSpec % _QM
                  qmCharge = calcContext' ^? qmLens % #charge
                  qmMult = calcContext' ^? qmLens % #mult
                  nElectrons = getNElectrons molNoDummies <$> qmCharge
                  qmElectronsOK = case (nElectrons, qmMult) of
                    (Just n, Just m) -> n + 1 >= m && ((even n && odd m) || (odd n && even m))
                    _ -> True
               in qmElectronsOK
          )
        $ (molNoDummies ^. #calcContext)

{-
-- This check doesn't need to be true, as link bonds can break the subset property.
-- All Bonds of the next layer joinded
nLBonds              = IntMap.unions . VB.map (^. #bonds) $ sM
-- Exclude bonds from and to link atoms in deeper layers
nLBondsOrigin        = IntMap.keysSet nLBonds \\ nLLinkAtomsInds
nLBondsTarget        = IntSet.unions nLBonds \\ nLLinkAtomsInds
subsetCheckBonds     =
  (nLBondsOrigin `IntSet.union` nLBondsTarget) \\ (bondsTarget `IntSet.union` bondsOrig)
-}

----------------------------------------------------------------------------------------------------

-- |
-- Like a 'map' through the 'Molecule' data structure. Applies a function to each molecule. To keep
-- this non-mind-blowing, it is best to only use functions, which only act on the current molecule
-- layer and not on the deeper ones as the update function.
molMap :: (Molecule -> Molecule) -> Molecule -> Molecule
molMap f mol =
  let subMols = mol ^. #subMol
   in if IntMap.null subMols
        then f mol
        else f mol & #subMol %~ IntMap.map (molMap f)

----------------------------------------------------------------------------------------------------

-- |
-- Like a 'map' through the 'Molecule' data structure. Applies a function to each molecule. To keep
-- this non-mind-blowing, it is best to only use functions, which only act on the current molecule
-- layer and not on the deeper ones as the update function. The mapping function has access to the
-- current 'MolID'.
molMapWithMolID :: (MolID -> Molecule -> Molecule) -> Molecule -> Molecule
molMapWithMolID f mol = go Empty f mol
  where
    go :: MolID -> (MolID -> Molecule -> Molecule) -> Molecule -> Molecule
    go molIdAcc func mol' =
      let subMols = mol' ^. #subMol
       in if IntMap.null subMols
            then (func molIdAcc) mol'
            else
              (func molIdAcc) mol' & #subMol
                %~ IntMap.mapWithKey
                  (\key val -> go (molIdAcc |> key) func val)

----------------------------------------------------------------------------------------------------

-- |
-- Like a 'mapM' through the 'Molecule' data structure. Applies a function to each molecule. To keep
-- this non-mind-blowing, it is best to only use functions, which only act on the current molecule
-- layer and not on the deeper ones as the update function.
molTraverse :: Monad m => (Molecule -> m Molecule) -> Molecule -> m Molecule
molTraverse f mol = do
  topUpdated <- f mol
  let subMols = topUpdated ^. #subMol
  if IntMap.null subMols
    then return topUpdated
    else do
      newSubMols <-
        traverse
          ( \deepMol ->
              if IntMap.null (deepMol ^. #subMol) then f deepMol else molTraverse f deepMol
          )
          subMols
      let newMol = topUpdated & #subMol .~ newSubMols
      return newMol

----------------------------------------------------------------------------------------------------

-- |
-- Like a 'mapM' through the 'Molecule' data structure. Applies a function to each molecule. To keep
-- this non-mind-blowing, it is best to only use functions, which only act on the current molecule
-- layer and not on the deeper ones as the update function.
--
-- This functions works top down through the molecule. The worker function has access to the MolID of
-- the molecule currently processed.
molTraverseWithID :: Monad m => (MolID -> Molecule -> m Molecule) -> Molecule -> m Molecule
molTraverseWithID f mol = go Empty f mol
  where
    go molIDAcc func mol' = do
      let subMols = mol' ^. #subMol
      thisLayerApplied <- func molIDAcc mol'
      if IntMap.null subMols
        then return thisLayerApplied
        else do
          newSubMols <-
            IntMap.traverseWithKey
              ( \key deepMol ->
                  if IntMap.null (deepMol ^. #subMol)
                    then (func $ molIDAcc |> key) deepMol
                    else go (molIDAcc |> key) func deepMol
              )
              subMols
          let newMol = thisLayerApplied & #subMol .~ newSubMols
          return newMol

----------------------------------------------------------------------------------------------------

-- |
-- Like a fold through a molecule. This steps through the left-most branch of a molecule completely
-- before going to the more right sites.
molFoldl :: (a -> Molecule -> a) -> a -> Molecule -> a
molFoldl f s mol =
  let subMols = mol ^. #subMol
      topLayerApplied = f s mol
   in if IntMap.null subMols
        then topLayerApplied
        else IntMap.foldl' (\acc subMol' -> molFoldl f acc subMol') topLayerApplied subMols

----------------------------------------------------------------------------------------------------

-- |
-- Like a fold through a molecule. This steps through the left-most branch of a molecule completely
-- before going to the more right sites. The folding function has access to the current 'MolID'.
molFoldlWithMolID :: (a -> MolID -> Molecule -> a) -> a -> Molecule -> a
molFoldlWithMolID f s mol = go Empty f s mol
  where
    -- go :: MolID -> (a -> MolID -> Molecule -> a) -> a -> Molecule -> a
    go molIdAcc func start mol' =
      let subMols = mol' ^. #subMol
          thisLayerApplied = (\acc -> func acc molIdAcc) start mol'
       in if IntMap.null subMols
            then thisLayerApplied
            else
              IntMap.foldlWithKey'
                (\acc key molVal -> go (molIdAcc |> key) f acc molVal)
                thisLayerApplied
                subMols

----------------------------------------------------------------------------------------------------

-- | Translates link info to a simple boolean.
isAtomLink :: LinkInfo -> Bool
isAtomLink NotLink = False
isAtomLink IsLink {} = True

----------------------------------------------------------------------------------------------------

-- |

----------------------------------------------------------------------------------------------------

-- | This reindexes all structures in a 'Molecule' with predefined counting scheme. This means
-- counting of 'Atom's will start at 0 and be consecutive. This also influences bonds in '_#bonds'
-- and layers in '_#subMol'. Link atoms will be taken care of.
reIndex2BaseMolecule :: MonadThrow m => Molecule -> m Molecule
reIndex2BaseMolecule mol =
  let allAtomIndices = getAtomIndices mol
      repMap = IntMap.fromAscList . (\old -> RIO.zip old [0 ..]) . IntSet.toList $ allAtomIndices
   in reIndexMolecule repMap mol

----------------------------------------------------------------------------------------------------

-- | Get the indices of all 'Atom's in a 'Molecule', including those of sublayers in 'subMol'
-- and link atoms therein. This assumes a sane 'Molecule' according to 'checkMolecule'.
getAtomIndices :: Molecule -> IntSet
getAtomIndices mol =
  let -- The indices of all atoms of the current layer
      thisLayerIndices = IntMap.keysSet $ mol ^. #atoms
      -- The indices of all sublayers + this layer.
      allIndices = foldr' (<>) thisLayerIndices . fmap getAtomIndices $ mol ^. #subMol
   in allIndices

----------------------------------------------------------------------------------------------------

-- | Given the full molecular system, this function will find the maximum index respective key of an
-- atom in all layers.
getMaxAtomIndex :: MonadThrow m => Molecule -> m Int
getMaxAtomIndex mol = do
  let maybeMax = fmap fst . IntSet.maxView . getAtomIndices $ mol
  case maybeMax of
    Nothing ->
      throwM $
        MolLogicException
          "getMaxAtomIndex"
          "Cannot find the maximum index of all atoms in the molecule. The molecule seems to be empty."
    Just k -> return k

----------------------------------------------------------------------------------------------------

-- | Reindex a complete 'Molecule', including all its deeper layers in '_#subMol') by mappings
-- from a global replacement Map, mapping old to new indices. This function assumes, that your
-- molecule is sane in the overall assumptions of this program. This means that the lower layers
-- obey the counting scheme of the atoms of the higher layers and link come last.
reIndexMolecule :: MonadThrow m => IntMap Int -> Molecule -> m Molecule
reIndexMolecule repMap mol = molTraverse (reIndexMoleculeLayer repMap) mol

----------------------------------------------------------------------------------------------------

-- | Reindex the '_#atoms' and '_#bonds' of a single layer of a molecule (ignoring
-- anything in the '_#subMol' field). While the completeness of the reindexing is checked and
-- incompleteness of the replacement 'IntMap' 'Int' will result in 'Left' 'String', it is not
-- checked if the 'Atom's indexing is sane and indices are unique.
reIndexMoleculeLayer ::
  MonadThrow m =>
  -- | 'IntMap' with mappings from old indices to new indices (bonds and atoms).
  IntMap Int ->
  -- | 'Molecule' to reindex.
  Molecule ->
  -- | 'Molecule' with __current__ layer reindexed.
  m Molecule
reIndexMoleculeLayer repMap mol = do
  -- Check for completness of the replacement map for the atom keys.
  unless (intIsRepMapCompleteForSet repMap (IntMap.keysSet $ mol ^. #atoms))
    . throwM
    $ MolLogicException
      "reIndexMoleculeLayer"
      "The remapping of indices is not complete for the atom indices."

  -- Check for completeness of the replacement map for the bond matrix.
  unless (isRepMapCompleteforBondMatrix repMap (mol ^. #bonds)) . throwM $
    MolLogicException
      "reIndexMoleculeLayer"
      "The remapping of indices is not complete for the bond data."

  return $
    mol
      -- Update the atoms indices of a molecule with new indices.
      & #atoms
      %~ intReplaceMapKeys repMap
      -- Update keys and values (IntSet) of the bond type data structure.
      & #bonds
      %~ replaceBondMatrixInds repMap
      -- Update the selection in the fragments.
      & #fragment
        % each
        % #atoms
      %~ intReplaceSet repMap

----------------------------------------------------------------------------------------------------

-- | Given a 'IntMap.Key' (representing an 'Atom'), determine in which depper layer ('_#subMol') the
-- 'Atom' is.
findAtomInSubMols ::
  -- | 'Atom' to find in the fragments.
  Int ->
  -- | Annotated fragments in an 'IntMap'
  IntMap Molecule ->
  -- | The 'IntMap.Key' aka fragment number in which the atom has been found.
  Maybe Int
findAtomInSubMols atomKey annoFrags =
  fst
    <$> ( IntMap.lookupMin
            . IntMap.filter (== True)
            . IntMap.map (\mol -> atomKey `IntMap.member` (mol ^. #atoms))
            $ annoFrags
        )

----------------------------------------------------------------------------------------------------

-- | Looks to which fragment an atom is assigned. Fails if the atom is not assigned to any fragment,
-- which violates the assumptions of 'Molecule'.
findAtomInFragment :: MonadThrow m => Int -> IntMap Fragment -> m Int
findAtomInFragment ind frags = do
  let matchingFrags = IntMap.filter (\f -> ind `IntSet.member` (f ^. #atoms)) frags
  thisFrag <-
    maybe2MThrow (localExc "Atom not found in any fragment.")
      . IntMap.minViewWithKey
      $ matchingFrags
  let fragNum = thisFrag ^. _1 % _1
  return fragNum
  where
    localExc = MolLogicException "findAtomInFragment"

----------------------------------------------------------------------------------------------------

-- | Get the number of electrons for a 'Molecule' with a given charge.
getNElectrons ::
  -- | The 'Molecule' to check.
  Molecule ->
  -- | Charge of the 'Molecule'.
  Int ->
  -- | Number of electrons of the 'Molecule' at the given charge.
  Int
getNElectrons mol charge' =
  let atoms' = mol ^. #atoms
      atomicNumbers = IntMap.map (\a -> (+ 1) . fromEnum $ a ^. #element) atoms'
      nElectrons = sum atomicNumbers - charge'
   in nElectrons

----------------------------------------------------------------------------------------------------

-- | Given the atoms of a model system molecule, find all atoms @LAC@, that have been capped with a
-- link atom.
getCappedAtoms :: IntMap Atom -> IntMap Atom
getCappedAtoms atoms' =
  let linkAtoms = IntMap.filter (isAtomLink . isLink) atoms'
      cappedIndices =
        IntSet.fromList . fmap snd . IntMap.toAscList . fromMaybe IntMap.empty $
          traverse
            (\la -> la ^? #isLink % #linkModelPartner)
            linkAtoms
   in IntMap.restrictKeys atoms' cappedIndices

----------------------------------------------------------------------------------------------------

-- | From the complete data structure pf the 'Molecule', get the specific layer, you want.  This is
-- now the new top layer.
getMolByID :: MonadThrow m => Molecule -> MolID -> m Molecule
getMolByID mol Seq.Empty = return mol
getMolByID mol (i :<| is) =
  let subMols = mol ^. #subMol
   in if IntMap.null subMols
        then
          throwM $
            MolLogicException
              "getMolByID"
              "Maximum recursion depth of your molecule reached and no layers left.\
              \ Cannot go deeper into the structure."
        else case (mol ^. #subMol) IntMap.!? i of
          Just molDeeperLayer -> getMolByID molDeeperLayer is
          Nothing ->
            throwM $
              MolLogicException
                "getMolByID"
                "Could not find a molecule with this MolID.\
                \ Key not found in the submolecule map."

----------------------------------------------------------------------------------------------------

-- | This generator takes a 'MolID' and generates an Lens to access this molecule. Dont be confused
-- by the type signature, it is a normal lens to be used with '(^?)' as lookup (for setters sometimes
-- it helps do redefine the lens locally in the setter). Read the type signature more as:
--
-- @
--    molIDLensGen :: MolID -> Lens' Molecule Molecule
-- @
--
-- **I am a little bit proud that i figured this out.**
molIDLensGen :: MolID -> Optic' An_AffineTraversal NoIx Molecule Molecule
molIDLensGen molID' =
  let layerLenses = fmap (\subMolIx -> #subMol % ix subMolIx) molID'
   in -- identityMolLens = castOptic simple -- :: Lens' a Molecule
      castOptic @An_AffineTraversal $ foldl (%) (castOptic @An_AffineTraversal simple) layerLenses

{-
let stepThroughLayers = fmap (\subMolIx -> #subMol % ix subMolIx) molID'
 in foldl (.) id stepThroughLayers
 -}

----------------------------------------------------------------------------------------------------

-- | From the complete data structure pf the 'Molecule', get the specific layer and a calculation on
-- it.
getCalcByID :: MonadThrow m => Molecule -> CalcID -> m (CalcContext, Molecule)
getCalcByID mol calcID = do
  molLayerOfInterest <- getMolByID mol (calcID ^. #molID)
  let calculations = molLayerOfInterest ^. #calcContext
  case calculations Map.!? (calcID ^. #calcKey) of
    Just calc -> return (calc, molLayerOfInterest)
    Nothing ->
      throwM $ MolLogicException "getCalcByID" "Could not find a calculation with this key."

----------------------------------------------------------------------------------------------------

-- | Generates a lens for calculation ID in a molecule. The type signature is confusing, read it
-- more as
--
-- @
--     calcIDLensGen :: CalcID -> Lens' Molecule CalcContext
-- @
--
-- This will create a normal lens to be used with '(^?)'.
calcIDLensGen :: CalcID -> Optic' An_AffineTraversal NoIx Molecule CalcContext
calcIDLensGen (CalcID molID' calcKey') = castOptic @An_AffineTraversal (molIDLensGen molID') % #calcContext % ix calcKey'

----------------------------------------------------------------------------------------------------

-- | Separates a new subsystem from the current molecule layer. Covalent bonds that were cut, can be
-- capped with link atoms. The following behaviour is employed for the constructor fields of the
-- sublayer 'Molecule':
--
-- - '_molecule_Comment': Will be an empty.
-- - '_#atoms': The atoms that are kept will have no 'multipoles' information.
--   Link atoms will be added. Their 'Int' key will start at @(maximum index) + 1@.
-- - '_#bonds': The ones from the top layer restricted to the indices supplied in origin and
--   target.
-- - '_#subMol': Will be empty.
-- - '_molecule_EnergyDerivatives': Everything will be 'Nothing'.
-- - '_molecule_CalcContext': Will be empty.
newSubLayer ::
  MonadThrow m =>
  -- | The maximum index of all atoms in the full system.
  Int ->
  -- | Input molecule, for which a new layer will be inserted.
  Molecule ->
  -- | Selection of the atoms to keep for the sublayer.
  IntSet ->
  -- | Scaling radius $\(g\).
  Maybe Double ->
  -- | Settings for the insertion of capping atoms.
  Maybe (Element, Text, FFType) ->
  -- | The original molecule modified by a sublayer inserted.
  m Molecule
newSubLayer maxAtomIndex mol newLayerInds covScale capAtomInfo = do
  -- Before doing anything, make sure the molecule is sane.
  _ <- checkMolecule mol

  -- Check if the indices for the new layer are usable aka non empty
  when (IntSet.null newLayerInds) . throwM $
    MolLogicException
      "newSubLayer"
      "Trying to create an empty layer. This is not possible with this function."

  -- Check if the indices for the new layer are strictly a subset of the old layer.
  let oldLayerAtomInds = IntMap.keysSet $ mol ^. #atoms
  unless (newLayerInds `IntSet.isSubsetOf` oldLayerAtomInds) . throwM $
    MolLogicException
      "newSubLayer"
      "The new layer indices are either reffering to atoms, that are not there."

  let atoms' = mol ^. #atoms
      -- Convert the capping atom defaults to individual values.
      (capAtomElement, capAtomLabel, capAtomFFType) = case capAtomInfo of
        Nothing -> (Nothing, Nothing, Nothing)
        Just (caElement, caLabel, caFFType) -> (Just caElement, Just caLabel, Just caFFType)

      -- Determine which submolecules already exist and find the largest index of the existing
      -- submolecules.
      maxSubMolIndex = fst <$> IntMap.lookupMax (mol ^. #subMol)
      newSubLayerIndex = fromMaybe 0 $ (+ 1) <$> maxSubMolIndex

      -- Define some default data for the new sublayer molecule.
      slComment =
        "Sublayer of "
          <> (mol ^. #comment)
          <> " keeping the indices "
          <> tShow (IntSet.toList newLayerInds)
          <> "."

      -- Just the atoms from the old layer that are kept but no link atoms added yet.
      slAtomsToKeep = (mol ^. #atoms) `IntMap.restrictKeys` newLayerInds

      -- Fragments of the new layer filtered by the new atom indices and fragments that became empty
      -- removed.
      slFragments =
        let origFrags = mol ^. #fragment
            fragsRestricted = origFrags & each % #atoms %~ IntSet.filter (`IntSet.member` newLayerInds)
            nonEmptyFrags = IntMap.filter (\f -> not . IntSet.null $ f ^. #atoms) fragsRestricted
         in nonEmptyFrags

      -- Bonds from the old layer to keep but no bonds for link atoms added yet.
      slBondsToKeep = cleanBondMatByAtomInds (mol ^. #bonds) newLayerInds
      slSubMol = IntMap.empty
      slEnergyDerivatives = def :: EnergyDerivatives
      slCalcContext = Map.empty

      -- Create a set of pairs of atoms, which have cut bonds due to sublayer isolation. The first
      -- part of the tuple will have the sublayer index and the second one the top layer index.
      cutAtomPairs :: HashSet (Int, Int)
      cutAtomPairs =
        HashMap.foldlWithKey'
          ( \accPairs (ixO, ixT) val ->
              let isOinSL = ixO `IntSet.member` newLayerInds
                  isTinSL = ixT `IntSet.member` newLayerInds
               in case (val, isOinSL, isTinSL) of
                    (True, True, False) -> HashSet.insert (ixO, ixT) accPairs
                    _ -> accPairs
          )
          HashSet.empty
          (mol ^. #bonds)

  -- This creates all necessary capping atoms for a new sublayer. The structure maps from the index
  -- of an atom in the new sublayer, to its capping link atoms.
  cappingLinkAtoms <-
    HashSet.foldl'
      ( \accLinkAtomMap' (ixSL, ixTL) -> do
          accLinkAtomMap <- accLinkAtomMap'
          -- Lookup the sublayer atom, which needs a cap.
          slCappedAtom <- case atoms' IntMap.!? ixSL of
            Just a -> return a
            Nothing ->
              throwM
                . MolLogicException "newSubLayer"
                $ "Lookup for atom with index "
                  <> show ixSL
                  <> " failed."
          -- Lookup the top layer atom, which has been removed and for which the cap is introduced.
          tlRemovedAtom <- case atoms' IntMap.!? ixTL of
            Just a -> return a
            Nothing ->
              throwM
                . MolLogicException "newSubLayer"
                $ "Lookup for atom with index "
                  <> show ixTL
                  <> " failed."
          -- Create a new link atom, for the current pair.
          newLinkAtom <-
            Seq.singleton
              <$> createLinkAtom
                covScale
                capAtomElement
                capAtomLabel
                capAtomFFType
                (ixSL, slCappedAtom)
                (ixTL, tlRemovedAtom)
          return . IntMap.insertWith (<>) ixSL newLinkAtom $ accLinkAtomMap
      )
      (return IntMap.empty)
      cutAtomPairs

  let -- Create an intermediate sublayer molecule, which does not yet have link atoms but is
      -- otherwise cleaned of the information from the top layer.
      slIntermediateMol =
        Molecule
          { comment = slComment,
            atoms = slAtomsToKeep,
            bonds = slBondsToKeep,
            subMol = slSubMol,
            fragment = slFragments,
            energyDerivatives = slEnergyDerivatives,
            calcContext = slCalcContext,
            jacobian = Nothing,
            neighbourlist = fmap (`IntMap.restrictKeys` newLayerInds) $ mol ^. #neighbourlist
          }

  -- Add all capping atoms to the sublayer. Goes through all atoms that need to be capped, while the
  -- inner loop in addLinksFromList goes through all link atoms, that need to be added to a single
  -- atom that need to be capped.
  (_, subLayerWithLinkAdded) <-
    IntMap.foldlWithKey'
      ( \accIndxAndMol' slOrigin linkAtomList -> do
          (accIndx, accMol) <- accIndxAndMol'
          (newMaxIndx, newMol) <- addLinksFromList accIndx slOrigin accMol linkAtomList
          return (newMaxIndx, newMol)
      )
      (pure (maxAtomIndex, slIntermediateMol))
      cappingLinkAtoms

  -- Construction of the finaly sub molecule
  let topLayerAtoms = mol ^. #atoms
      subLayerAtoms = subLayerWithLinkAdded ^. #atoms
  slJacobianD <- getJacobian topLayerAtoms subLayerAtoms
  let slJacobian = Massiv.computeAs Massiv.S . Massiv.setComp Par $ slJacobianD

  let -- Add the Jacobian to the otherwise final sublayer and add the sublayer to the input system.
      subLayerWithJacobian = subLayerWithLinkAdded & #jacobian ?~ MatrixS slJacobian

      -- Add the sublayer with its new key as submolecule to the original input system.
      markedMolWithNewSublayer =
        mol & #subMol %~ IntMap.insert newSubLayerIndex subLayerWithJacobian

  return markedMolWithNewSublayer
  where
    -- Add a list of capping link atoms to an existing molecule and a single bond partner. Usually
    -- this will be just one link atom but it could also add multiple caps to a single atom. Returns
    -- the index of the last link atom added and the molecule layer with all the links atoms added.
    addLinksFromList :: MonadThrow m => Int -> Int -> Molecule -> Seq Atom -> m (Int, Molecule)
    addLinksFromList maxAtomIndex' linkBondPartner mol' linkSeq =
      foldl'
        ( \accIndxAndMol' linkAtom -> do
            (accIndx, accMol) <- accIndxAndMol'
            let newAtomIndex = accIndx + 1
            linkFragID <- findAtomInFragment linkBondPartner $ mol' ^. #fragment
            molAtomAdded <- addAtomWithKeyLocal accMol newAtomIndex (Just linkFragID) linkAtom
            molAtomAndBondAdded <- changeBond Add molAtomAdded (linkBondPartner, newAtomIndex)
            return (newAtomIndex, molAtomAndBondAdded)
        )
        (pure (maxAtomIndex', mol'))
        linkSeq

----------------------------------------------------------------------------------------------------

-- | Function to create a capping link atom from an atom to keep and an atom, that has been cut away.
--
-- Following special behaviours are used here:
--
-- - The scaling factor \(g\) (see below) might be given. If it is not given, a default value will be
--   used
-- - A chemical 'Element' might be given for the new link atom ('_atom_Element') and if not
--   specified a hydrogen atom will be used.
-- - An 'AtomLabel' might be given. If not, the '_atom_Label' will be empty.
-- - A force-field type might be given. If not it will be 'XYZ'.
--
-- See also 'calcLinkCoords'.
--
-- The position of the link atom is calculated as:
--
-- \[
--     \mathbf{r}^\mathrm{LA} =
--     \mathbf{r}^\mathrm{LAC} + g ( \mathbf{r}^\mathrm{LAH} - \mathbf{r}^\mathrm{LAC})
-- \]
--
-- where the scaling factor \(g\) will be calculated from a ratio of covalent radii, if not specified:
--
-- \[
--     g = \frac{r^\mathrm{cov, LAC} + r^\mathrm{cov, LA}}{r^\mathrm{cov, LAH} + r^\mathrm{cov, LAC}}
-- \]
--
-- With:
--
--   - \( \mathbf{r}^\mathrm{LA} \): the coordinates of the created link atom
--   - \( \mathbf{r}^\mathrm{LAH} \): the coordinates of the atom that has been removed by creating the
--     new layer
--   - \( \mathbf{r}^\mathrm{LAC} \): the coordinates of the atom, that will be capped by the link atom
--   - \( g \): the scaling factor for the position of the link atom.
createLinkAtom ::
  MonadThrow m =>
  -- | Scaling factor \(g\) for the position of the link atom.
  Maybe Double ->
  -- | Chemical element for link atom to create.
  Maybe Element ->
  -- | Textual label of the link atom.
  Maybe Text ->
  -- | Force field type of the link atom.
  Maybe FFType ->
  -- | 'Atom' and its key to cap with the link atom.
  (Int, Atom) ->
  -- | 'Atom' and its key that has been cut away and needs to be replaced by the
  --   link atom, that will be created.
  (Int, Atom) ->
  -- | Created link atom.
  m Atom
createLinkAtom gScaleOption linkElementOption label' fftype (cappedKey, cappedAtom) (removedKey, removedAtom) =
  do
    let linkElement = fromMaybe H linkElementOption
        cappedElement = cappedAtom ^. #element
        cappedCoords = Massiv.delay . getVectorS $ cappedAtom ^. #coordinates
        cappedCovRadius = covalentRadii Map.!? cappedElement
        removedElement = removedAtom ^. #element
        removedCoords = Massiv.delay . getVectorS $ removedAtom ^. #coordinates
        removedCovRadius = covalentRadii Map.!? removedElement
        linkCovRadius = covalentRadii Map.!? linkElement
        gFactorDefault :: Maybe Double
        gFactorDefault = do
          cappedCovRadius' <- cappedCovRadius
          removedCovRadius' <- removedCovRadius
          linkCovRadius' <- linkCovRadius
          return $ (cappedCovRadius' + linkCovRadius') / (removedCovRadius' + cappedCovRadius')

    gScaleToUse <- case (gScaleOption, gFactorDefault) of
      (Just gScale, _) -> return gScale
      (Nothing, Just gScale) -> return gScale
      (Nothing, Nothing) ->
        throwM
          . MolLogicException "createLinkAtom"
          $ "Can not find covalent radius of one of these elements "
            <> show (linkElement, cappedElement, removedElement)
            <> " and no scaling factor g has been given."

    linkCoordinates <- calcLinkCoords cappedCoords removedCoords gScaleToUse

    let newLinkAtom =
          Atom
            { element = linkElement,
              label = fromMaybe "" label',
              isLink =
                IsLink
                  { linkModelPartner = cappedKey,
                    linkRealPartner = removedKey,
                    linkGFactor = gScaleToUse
                  },
              isDummy = False,
              ffType = fromMaybe FFXYZ fftype,
              coordinates = VectorS . computeS $ linkCoordinates,
              multipoles = def
            }

    return newLinkAtom

----------------------------------------------------------------------------------------------------

-- | Calculate the new coordinates of a link atom as:
-- \[
--     \mathbf{r}^\mathrm{LA} =
--     \mathbf{r}^\mathrm{LAC} + g ( \mathbf{r}^\mathrm{LAH} - \mathbf{r}^\mathrm{LAC})
-- \]
-- With:
-- - \( \mathbf{r}^\mathrm{LA} \): the coordinates of the created link atom
-- - \( \mathbf{r}^\mathrm{LAH} \): the coordinates of the atom that has been removed by creating the
--   new layer
-- - \( \mathbf{r}^\mathrm{LAC} \): the coordinates of the atom, that will be capped by the link atom
calcLinkCoords ::
  (Load r Ix1 a, MonadThrow m, Numeric r a) =>
  -- | Coordinates of the atom being capped.
  Vector r a ->
  -- | Coordinates of the atom being removed.
  Vector r a ->
  -- | The scaling factor g.
  a ->
  m (Vector r a)
calcLinkCoords cappedAtomCoords removedAtomCoords gScale = do
  diffVec <- removedAtomCoords .-. cappedAtomCoords
  let scaledDiffVec = gScale *. diffVec
  cappedAtomCoords .+. scaledDiffVec

----------------------------------------------------------------------------------------------------

-- | Adds an 'Atom' to a specified 'Molecule' layer within the full molecular system. The
-- 'IntMap.Key' of the new atom will be larger by 1 than the largest atom index in the full system.
-- The atom will also be added to all deeper layers than the specified one with the same
-- 'IntMap.Key'. No bonds will be updated.
--
-- The 'Int' returned is the 'IntMap' key of the newly added atom.
addAtom :: MonadThrow m => Molecule -> MolID -> Atom -> m (Int, Molecule)
addAtom fullMol molID' atom = do
  -- Before doing anything, make sure the whole molecule is sane.
  _ <- checkMolecule fullMol

  -- The largest atom index in the complete molecule.
  maxAtomIndex <- case IntSet.maxView . getAtomIndices $ fullMol of
    Nothing -> throwM $ MolLogicException "addAtom" "Your molecule appears to have no atoms"
    Just (maxKey, _) -> return maxKey

  -- Get the layer from which to start adding the atom by a MolID.
  mol <- case fullMol ^? molIDLensGen molID' of
    Nothing ->
      throwM
        . MolLogicException "addAtom"
        $ ( "Requested to add an atom to layers below layer with ID "
              <> show molID'
              <> " but no layer with this id could be found."
          )
    Just m -> return m

  -- Find the largest index of this layer.
  let newAtomIndex = maxAtomIndex + 1

  -- Insert the new atom recursively into all layers below and including the selected one.
  newMol <- goInsert mol newAtomIndex atom

  -- Return the full molecular system with the sublayers updated.
  let newFullMol = fullMol & (molIDLensGen molID') .~ newMol

  return (newAtomIndex, newFullMol)
  where
    goInsert :: MonadThrow m => Molecule -> Int -> Atom -> m Molecule
    goInsert mol' newInd' atom' = do
      let atoms' = mol' ^. #atoms

      -- Check if the key for the new atom would be new/unique.
      when (newInd' `IntMap.member` atoms')
        . throwM
        . MolLogicException "addAtom"
        $ "Cannot add atom with key "
          <> show newInd'
          <> " to the current molecule layer. The key already exists."

      -- Add the atom with the given index to the current layer.
      let thisLayerMolAtomAdded = mol' & #atoms %~ IntMap.insert newInd' atom'
          subMols = thisLayerMolAtomAdded ^. #subMol

      -- Add recursively also to all deeper layers.
      if IntMap.null subMols
        then return thisLayerMolAtomAdded
        else do
          updatedSubMols <- traverse (\m -> goInsert m newInd' atom') subMols
          return $ thisLayerMolAtomAdded & #subMol .~ updatedSubMols

----------------------------------------------------------------------------------------------------

-- | This function adds an atom with a given key to all layers below the molecule which was given as
-- input. This can cause problems if the key, that is given is already present. If it is already
-- present in the  layers visible to this function (current one and below) an exception will be
-- thrown. But if a sublayer is given to this function and you specify a key to this function, which
-- is present in layers above (not visible to this function) but not in the layers visible, you will
-- end up with an inconsistent molecule.
--
-- The new atom will be added to a fragment to keep consistency with 'Molecule' data structure
-- assumptions. The fragment ID needs to be specified or a new fragment will be added.
addAtomWithKeyLocal ::
  MonadThrow m =>
  -- | Molecule to which an atom will be added.
  Molecule ->
  -- | The key the newly added atom shall have. Must not be used in any layer yet.
  Int ->
  -- | A fragment ID to which the atom should be added. If none is given, a new fragment will be
  -- created.
  Maybe Int ->
  Atom ->
  m Molecule
addAtomWithKeyLocal mol key fragID atom = do
  -- Before doing anything, make sure that the input is sane.
  _ <- checkMolecule mol

  -- Find the largest fragment ID, that is in use yet.
  let fragKeys = IntMap.keysSet $ mol ^. #fragment
      unwrapKey = maybe2MThrow (localExc "No fragments in the top layer.") . fmap fst
  fragIDTopMax <- unwrapKey . IntSet.maxView $ fragKeys
  fragIDMax <-
    molFoldl
      ( \maxFragID thisMol -> do
          thisFragIDMax <- unwrapKey . IntMap.lookupMax $ thisMol ^. #fragment
          maxFragID >>= \accID -> return $ max thisFragIDMax accID
      )
      (pure fragIDTopMax)
      mol

  -- Construct the modified molecule recursively.
  newMol <- goInsert mol key (fromMaybe fragIDMax fragID) atom
  return newMol
  where
    localExc = MolLogicException "addAtomWithKeyLocal"

    goInsert :: MonadThrow m => Molecule -> Int -> Int -> Atom -> m Molecule
    goInsert mol' newKey' fragID' atom' = do
      let atoms' = mol' ^. #atoms

      when (newKey' `IntMap.member` atoms')
        . throwM
        . MolLogicException "addAtomWithKeyLocal"
        $ "Cannot add atom with key "
          <> show newKey'
          <> " to the current molecule layer. The key already exists."

      -- Add the atom with the given index to the current layer and invalidate all data, that are
      -- not valid anymore.
      let singularFrag =
            IntMap.singleton fragID' $
              Fragment
                { label = mempty,
                  chain = Nothing,
                  atoms = IntSet.singleton newKey'
                }
          thisLayerMolAtomAdded =
            mol
              -- Invalidate data.
              & #energyDerivatives .~ def
              & #jacobian .~ Nothing
              & #neighbourlist .~ mempty
              -- Add data.
              & #atoms %~ IntMap.insert newKey' atom'
              & #fragment
                %~ IntMap.unionWith
                  (\orig new -> orig & #atoms %~ IntSet.union (new ^. #atoms))
                  singularFrag

          subMols = thisLayerMolAtomAdded ^. #subMol

      -- Add recursively also to all deeper layers.
      if IntMap.null subMols
        then return thisLayerMolAtomAdded
        else do
          updateSubMols <- traverse (\m -> goInsert m newKey' fragID' atom') subMols
          return $ thisLayerMolAtomAdded & #subMol .~ updateSubMols

----------------------------------------------------------------------------------------------------

-- | Removes an 'Atom' specified by its index key from the 'Molecule' and all deeper layers. If the
-- atom specified was a link atom in the highest layer, it will remove link atoms in the deeper
-- layers, that have the same key.
removeAtom :: MonadThrow m => Molecule -> Int -> m Molecule
removeAtom mol atomInd = do
  -- Before doing anything check if the molecule is sane.
  _ <- checkMolecule mol

  -- Get information about the atom of interest.
  let atoms' = mol ^. #atoms
      atomExists = atomInd `IntMap.member` atoms'
      atomIsLink = case atoms' IntMap.!? atomInd of
        Just a -> isAtomLink $ a ^. #isLink
        Nothing -> False

  unless atomExists
    . throwM
    . MolLogicException "removeAtom"
    $ "Cannot remove atom with key "
      <> show atomInd
      <> " from the top molecule layer. The key does not exist."

  return $ goRemove mol (atomInd, atomIsLink)
  where
    goRemove :: Molecule -> (Int, Bool) -> Molecule
    goRemove mol' (atomInd', wasLink) =
      -- Get information about the atom to remove in the current layer.
      let atoms' = mol' ^. #atoms
          atomIsLink = case atoms' IntMap.!? atomInd' of
            Just a -> isAtomLink $ a ^. #isLink
            Nothing -> False

          -- If it did not change if this atom is a link atom, remove it from this layer.
          -- Otherwise it must be a different atom now and we dont touch it.
          atomsUpdated = if atomIsLink == wasLink then atomInd' `IntMap.delete` atoms' else atoms'

          -- Remove bonds involving this atom if there was no change wether this atom was a
          -- link atom..
          bonds' = mol' ^. #bonds
          bondsUpdated = removeBondsByAtomIndsFromBondMat bonds' (IntSet.singleton atomInd')

          -- Update the current layer with bonds and atoms cleaned.
          thisLayerUpdated = mol' & #bonds .~ bondsUpdated & #atoms .~ atomsUpdated

          -- Get the submolecules and update them lazily.
          subMols = thisLayerUpdated ^. #subMol
          subMolsUpdated = IntMap.map (\m -> goRemove m (atomInd', wasLink)) subMols
       in if IntMap.null subMols
            then thisLayerUpdated
            else thisLayerUpdated & #subMol .~ subMolsUpdated

----------------------------------------------------------------------------------------------------

-- | Descriptor of what to do with bonds.
data BondOperation
  = Add
  | Remove
  deriving (Eq)

----------------------------------------------------------------------------------------------------

-- | Adds\/removes a bond recursively to\/from a 'Molecule' and its deeper layers. Two atom 'IntMap.Key's
-- are specified, to indicate between which 'Atom's a new bond shall be inserted/removed. If these
-- 'Atom's/'IntMap.Key's do not exist in the top layer, this function will fail. If the atoms do not
-- exist in deeper layers or one of them became a link atom, no bond will be inserted/removed in the
-- deeper layer. If a bond already exists/does not exist between those atoms, it will not be changed.
--
-- If one of the two atoms was a link atom in the top layer and still is in the deeper layers, bonds
-- will be inserted/removed normally. If one the atoms was not a link atom in the top layer but
-- becomes a link atom in a deeper layer, bonds involving those atoms will not be touched.
changeBond :: MonadThrow m => BondOperation -> Molecule -> (Int, Int) -> m Molecule
changeBond operation mol (at1, at2) = do
  -- Check sanity of the molecule before doin anything else.
  _ <- checkMolecule mol

  -- Check if origin and target key are present as atoms in the molecule at all.
  let atoms' = mol ^. #atoms
      atom1Exists = at1 `IntMap.member` atoms'
      atom2Exists = at2 `IntMap.member` atoms'
      -- These lookups are safe as long as they are lazy and the check if the atoms exist happens
      -- before those information are used.
      atom1IsLink = isAtomLink $ (atoms' IntMap.! at1) ^. #isLink
      atom2IsLink = isAtomLink $ (atoms' IntMap.! at2) ^. #isLink

  unless (atom1Exists && atom2Exists)
    . throwM
    . MolLogicException "changeBond"
    $ "Wanted to add a bond between atoms"
      <> show at1
      <> " and "
      <> show at2
      <> " but at least one of those atoms does not exist in the top layer."

  -- If checks are passed, start with updating the current layer and work all the way down.
  return $ goDeepAddRemove operation mol ((at1, atom1IsLink), (at2, atom2IsLink))
  where
    -- Update the deeper layers with a different check.
    goDeepAddRemove :: BondOperation -> Molecule -> ((Int, Bool), (Int, Bool)) -> Molecule
    goDeepAddRemove operation' mol' ((at1', wasLink1), (at2', wasLink2)) =
      -- Check if both origin and target exist and if they are link atom.
      let atoms' = mol' ^. #atoms
          originExitst = at1' `IntMap.member` atoms'
          targetExists = at2' `IntMap.member` atoms'
          originIsLink = case atoms' IntMap.!? at1' of
            Just a -> isAtomLink $ a ^. #isLink
            Nothing -> False
          targetIsLink = case atoms' IntMap.!? at2' of
            Just a -> isAtomLink $ a ^. #isLink
            Nothing -> False

          -- Choose the update function depending wether to remove or add bonds.
          updateFunction = case operation' of
            Add -> addBondToBondMat
            Remove -> removeBondFromBondMat

          -- Update the bonds of this layer if everything is fine. Keep them as is otherwise.
          thisLayerBonds = mol' ^. #bonds
          thisLayerBondsUpdated =
            if all
              (== True)
              [originExitst, targetExists, wasLink1 == originIsLink, wasLink2 == targetIsLink]
              then updateFunction thisLayerBonds (at1', at2')
              else thisLayerBonds

          -- Update bonds data and invalidate the ones, that make no sense after bond updates.
          molUpdated =
            mol' & #bonds .~ thisLayerBondsUpdated
              & #jacobian .~ Nothing

          -- Lazily update the submolecules recursively.
          subMols = molUpdated ^. #subMol
          subMolsUpdated =
            IntMap.map (\m -> goDeepAddRemove operation' m ((at1', wasLink1), (at2', wasLink2))) subMols
       in if IntMap.null subMols -- Update deeper layers only if required to end the recursion.
            then molUpdated
            else molUpdated & #subMol .~ subMolsUpdated

----------------------------------------------------------------------------------------------------

-- |
-- Get the coordinates of the current molecule layer as matrix with (nAtoms x 3), where the cartesian
-- coordinates for each atom are along the rows of the matrix. Fails if some of the coordinate vectors
-- have the wrong size.
{-# INLINE getCoordinatesAs3NMatrix #-}
-- getCoordinatesAs3NMatrix :: (MonadThrow m) => Molecule -> m (Matrix DL Double)
getCoordinatesAs3NMatrix :: MonadThrow m => Molecule -> m (Matrix DL Double)
getCoordinatesAs3NMatrix mol = do
  let atomCoords = IntMap.map (getVectorS . coordinates) . (^. #atoms) $ mol
      nAtoms = IntMap.size atomCoords
  allCoordsConcat <- Massiv.concatM 1 . fmap snd . IntMap.toAscList $ atomCoords
  Massiv.resizeM (Sz (nAtoms :. 3)) allCoordsConcat

----------------------------------------------------------------------------------------------------

-- | Calculates a distance matrix from a linearised vector of cartesian coordinates.
{-# INLINE distMat #-}
distMat :: (MonadThrow m) => Molecule -> m (Matrix D Double)
distMat mol = do
  -- Reshapes the 3N vector into a 3 x N matrix. The x-axis represents atom indices and y-axis has
  -- cartesian coordinates.
  n3Vec <- Massiv.computeAs Massiv.S . Massiv.setComp Par <$> getCoordinatesAs3NMatrix mol
  let -- Get the number of atoms.
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

-- | Get mapping from dense arrays as in Massiv (starting at 0) to the sparse indexing used in the
-- 'Molecule' type with 'IntMap' and 'HashMap'.
{-# INLINE getDenseSparseMapping #-}
getDenseSparseMapping :: Molecule -> Vector P Int
getDenseSparseMapping mol =
  let atomIndices = IntMap.keys . (^. #atoms) $ mol in Massiv.fromList Par atomIndices

----------------------------------------------------------------------------------------------------

-- | The opposite of 'getDenseSparseMapping'. Contains mapping from the original atom indices to the
-- dense indices.
{-# INLINE getSparseDenseMapping #-}
getSparseDenseMapping :: Molecule -> IntMap Int
getSparseDenseMapping mol =
  let atomIndices = IntMap.keys . (^. #atoms) $ mol
      denseIndices = [0 ..]
   in IntMap.fromList $ RIO.zip atomIndices denseIndices

----------------------------------------------------------------------------------------------------

-- | Get the atoms of the current layer in a dense Massiv vector.
{-# INLINE getAtomsAsVector #-}
getAtomsAsVector :: Molecule -> Vector B Atom
getAtomsAsVector mol = Massiv.fromList Par . fmap snd . IntMap.toAscList . (^. #atoms) $ mol

----------------------------------------------------------------------------------------------------

-- | Generate a neighbouhr list. This is an set of association of one atom, with all the ones, which
-- are within a certain distance. The neighbours are free of self-interaction. NOTE: contains
-- a morally pure use of unsafePerformIO in order to parallelise the operation.
{-# INLINE neighbourList #-}
neighbourList :: (MonadThrow m) => Double -> Molecule -> m (IntMap IntSet)
neighbourList maxNeighbourDist mol = do
  -- Gets the atom coordinates in Nx3 matrix representation.
  atomCoords <- Massiv.computeAs Massiv.S . Massiv.setComp Par <$> getCoordinatesAs3NMatrix mol

  -- Find maximum and minumum value in each dimension.
  xCoords <- atomCoords <!? 0
  yCoords <- atomCoords <!? 1
  zCoords <- atomCoords <!? 2
  xMin <- Massiv.minimumM xCoords
  xMax <- Massiv.maximumM xCoords
  yMin <- Massiv.minimumM yCoords
  yMax <- Massiv.maximumM yCoords
  zMin <- Massiv.minimumM zCoords
  zMax <- Massiv.maximumM zCoords

  let -- Mapping from dense 0-based coordinates to the sparse indexing in the Molecule.
      atomIndexDenseToSparseMapping :: Vector P Int
      atomIndexDenseToSparseMapping = getDenseSparseMapping mol

      -- Mapping from the sparse indices as used in the mol to the dense ones as used in arrays.
      atomIndexSparseToDenseMapping :: IntMap Int
      atomIndexSparseToDenseMapping = getSparseDenseMapping mol

      -- Convert the original sparse atom IntMap to the dense representation as used in arrays.
      atomDenseMap :: IntMap Atom
      atomDenseMap = intReplaceMapKeys atomIndexSparseToDenseMapping $ mol ^. #atoms

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
      ixOrigin = (0, 0, 0)
      binLinearIxRange = (ixOrigin, nBinsMaxIx)

      -- Sort the atoms into bins now. This is basically the conversion from the cartesian
      -- coordinates to bin indices in R3.
      atomBinAssignment :: Matrix D Int
      atomBinAssignment =
        Massiv.imap
          ( \((_atom :. cartComponent)) el -> case cartComponent of
              0 -> floor $ (el - xMin) / binSize
              1 -> floor $ (el - yMin) / binSize
              2 -> floor $ (el - zMin) / binSize
              _ -> -1
          )
          atomCoords

      -- Linearise the bin assignment of the atoms with respect to the bin index.
      atomBinAssignmentLinear :: Massiv.Vector D Int
      atomBinAssignmentLinear =
        Massiv.ifoldlInner
          ( \(_atom :. cartComponent) accBinLIx el -> case cartComponent of
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

  -- A HashMap from scalar bin index to the atoms a bin contains.
  binAtomMap <- do
    -- Create the equally sized rows of the bin (rows) to atom indices (column).
    let binGroups = vectorToGroups fst atomBinAssignmentLinearSort

    -- The bin indices (only bins that actually contain atoms)
    binIndices <-
      traverse
        ( \aGroup -> do
            groupHead <- case List.headMaybe aGroup of
              Nothing ->
                throwM $
                  DataStructureException
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
        binAtomSets = List.map (IntSet.fromList . List.map snd) binGroups
        mappingGroups = HashMap.fromList $ List.zip binIndices binAtomSets
    return mappingGroups

  let -- Create the array of ALL bins to the atoms they contain. This is a 3D array, indexed by the
      -- bins.
      binAtomMatrix :: Array B Ix3 IntSet
      binAtomMatrix =
        Massiv.makeArrayLinear
          Par
          (Sz (nBinsX :> nBinsY :. nBinsZ))
          (\lBinIx -> HashMap.lookupDefault IntSet.empty lBinIx binAtomMap)

      -- Create a stencil over the 3 bin dimensions, which combines all sets of the neighbouring bins
      -- with the set of this bin.
      {-# INLINE neighbourCollectionStencil #-}
      neighbourCollectionStencil :: Stencil Ix3 IntSet IntSet
      neighbourCollectionStencil =
        Massiv.makeStencil (Sz (3 :> 3 :. 3)) (1 :> 1 :. 1) $ \get ->
          let validIndRange = [-1, 0, 1]
              allStencilIndices =
                fmap
                  toIx3
                  [(x, y, z) | x <- validIndRange, y <- validIndRange, z <- validIndRange]
              allStencilGetters = fmap get allStencilIndices
           in foldl' (<>) IntSet.empty allStencilGetters

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

      -- Join all neighbours from the individual bins together.
      foldingF :: IntMap IntSet -> IntMap IntSet -> IntMap IntSet
      foldingF = IntMap.unionWith IntSet.union
      neighboursInDenseNumbering = unsafePerformIO $ Massiv.foldlP foldingF IntMap.empty foldingF IntMap.empty neighboursInBins

  -- Remap the neighbour list back to the sparse mapping, that has been originally used.
  let neighboursInSparseNumbering =
        ( IntMap.map
            ( \neighbourSet ->
                IntSet.map
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
          neighbours =
            IntMap.filter (not . IntSet.null) $
              IntMap.mapWithKey
                ( \originInd targetIndSet ->
                    checkDistancesFromOriginAtom denseAtomMap maxDist originInd targetIndSet
                )
                unfilteredPotentialNeigbhours
       in neighbours

    -- Checks for a set of target atoms, if they are within a given distance of the origin atom.
    -- This never fails. If an atom that was looked up by its index is not in the IntMap of atoms,
    -- it will not be a neighbour of the origin. If the origin cannot be found, there will be no
    -- neighbours.
    checkDistancesFromOriginAtom :: IntMap Atom -> Double -> Int -> IntSet -> IntSet
    checkDistancesFromOriginAtom denseAtomMap maxDist originInd targetInds =
      let originCoords =
            Massiv.delay . getVectorS . coordinates <$> denseAtomMap IntMap.!? originInd
       in IntSet.filter
            ( \tIx ->
                let targetCoords =
                      Massiv.delay . getVectorS . coordinates <$> denseAtomMap IntMap.!? tIx
                    dist = join $ distance <$> originCoords <*> targetCoords
                 in case dist of
                      Nothing -> False
                      Just d -> d <= maxDist && tIx /= originInd
            )
            targetInds

----------------------------------------------------------------------------------------------------

-- | Quadratic scaling version of bond matrix guessing. Uses full distance matrix to filter for
-- distances small enough. Applies only to the current layer of the molecule.
--
-- If the covalent radius of an element is unknown, no bonds for this atom will be defined.
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
      elementsVector = Massiv.compute . Massiv.setComp Par . Massiv.map element $ atomsVector

      -- Build the same strucute for element pairs as it has been done for the distance matrix.
      elementPairs :: Matrix D (Element, Element)
      elementPairs =
        let xElements :: Matrix D Element
            xElements = Massiv.expandInner (Sz nAtoms) const elementsVector
            yElements :: Matrix D Element
            yElements = Massiv.expandOuter (Sz nAtoms) const elementsVector
         in Massiv.zip xElements yElements

      -- Calculate the sum of the covalent radii for the element pairs and then scale them.
      covRadiiSums :: Matrix D (Maybe Double)
      covRadiiSums =
        Massiv.map
          ( \(elA, elB) -> do
              radiusA <- covalentRadii Map.!? elA
              radiusB <- covalentRadii Map.!? elB
              return $ radScaling * (radiusA + radiusB)
          )
          elementPairs

  -- Calculate the distance matrix.
  distanceMatrix <- distMat mol

  let -- Create a dense bond matrix but keep it delayed.
      bondMatrixDenseSelfeInteraction :: Matrix D Bool
      bondMatrixDenseSelfeInteraction =
        Massiv.zipWith
          ( \dM cRM ->
              let distanceCheck = (dM <=) <$> cRM
               in case distanceCheck of
                    Just True -> True
                    _ -> False
          )
          distanceMatrix
          covRadiiSums

      -- Remove the self-interaction from the bond matrix.
      bondMatrixDense :: Matrix D Bool
      bondMatrixDense =
        Massiv.imap
          (\(ixC :. ixR) val -> if ixC == ixR then False else val)
          bondMatrixDenseSelfeInteraction

  -- Fold the dense bond matrix to the sparse HashMap representation.
  bondMatrix <-
    Massiv.ifoldlP
      ( \accBM (rIx :. cIx) bondBool ->
          if bondBool
            then
              let rIxSparse = atomIndexDenseToSparseMapping Massiv.! rIx
                  cIxSparse = atomIndexDenseToSparseMapping Massiv.! cIx
               in HashMap.insert (rIxSparse, cIxSparse) bondBool accBM
            else accBM
      )
      HashMap.empty
      (<>)
      HashMap.empty
      bondMatrixDense
  return bondMatrix

----------------------------------------------------------------------------------------------------

-- | Linear scaling version of bond matrix guessing based on covalent radii. Constructs a
-- neighbourlist first and only checks within the neighbour list for potential bond partners.
{-# INLINE guessBondMatrix #-}
guessBondMatrix :: (MonadThrow m) => Maybe Double -> Molecule -> m BondMatrix
guessBondMatrix covScaling mol = do
  let -- Original IntMap of the atoms.
      atoms' :: IntMap Atom
      atoms' = mol ^. #atoms

      -- Get all chemical elements of the current molecule layer.
      atomElements :: IntMap Element
      atomElements = IntMap.map element atoms'

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
    Nothing ->
      throwM $
        MolLogicException
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
          ( \accBM originIx targetSet ->
              let newRow = makeScreenedBondMatrixRow radiusScale originIx targetSet atoms'
               in accBM <> newRow
          )
          HashMap.empty
          neighbours
          `using` parTraversable rpar
  return bondMatrix
  where
    -- Create a row of a bond matrix for a given origin atom and a pre-filtered set of potential
    -- target atoms. Elements, for which a covalent radius cannot be found will have no bonds.
    makeScreenedBondMatrixRow :: Double -> Int -> IntSet -> IntMap Atom -> BondMatrix
    makeScreenedBondMatrixRow covScale originInd targetInds originalAtoms =
      IntSet.foldl'
        ( \bondMatrixRowAcc targetInd ->
            let originAtom = originalAtoms IntMap.!? originInd
                targetAtom = originalAtoms IntMap.!? targetInd
                originCoords = Massiv.delay . getVectorS . coordinates <$> originAtom
                targetCoords = Massiv.delay . getVectorS . coordinates <$> targetAtom
                originElement = element <$> originAtom
                targetElement = element <$> targetAtom
                originRadius = (covalentRadii Map.!?) =<< originElement
                targetRadius = (covalentRadii Map.!?) =<< targetElement
                realDistance = join $ distance <$> originCoords <*> targetCoords
                sumCovRadii = (+) <$> originRadius <*> targetRadius
                maxDistance = (covScale *) <$> sumCovRadii
             in case (<=) <$> realDistance <*> maxDistance of
                  -- The radii of both elements have been found and the real distance between the atoms is
                  -- smaller than the scaled sum of the covalent radii.
                  Just True -> HashMap.insert (originInd, targetInd) True bondMatrixRowAcc
                  -- Both elements have been found but the distance is too large.
                  Just False -> bondMatrixRowAcc
                  -- One of the elements has not been found.
                  Nothing -> bondMatrixRowAcc
        )
        HashMap.empty
        targetInds

----------------------------------------------------------------------------------------------------

-- | Fragment detection in a molecule. The fragment detection works by following the bond matrix.
-- Sets of atoms, which do not have any covalent connections to atoms outside of their own set are
-- considered a fragment.
{-# INLINE fragmentDetectionDetached #-}
fragmentDetectionDetached :: MonadThrow m => Molecule -> m (IntMap IntSet)
fragmentDetectionDetached mol = do
  -- Make sure the molecule is sane, otherwise the fragments will potentially be garbage.
  _ <- checkMolecule mol

  -- Detect all fragments by following the bond matrix.
  let bonds' = mol ^. #bonds
      atoms' = mol ^. #atoms
      allFragments = findAllFragments bonds' atoms' IntMap.empty
  return allFragments
  where
    -- Takes a starting atom and the bond matrix of the molecule and an (initially empty) set of atoms
    -- that are already part of the fragment. Starting from this atom follow the bonds through the
    -- molecule. until no new atoms can be found anymore.
    findFromStartingAtom ::
      -- | Index of the atom currently checked
      Int ->
      -- | The bond matrix of the molecules.
      BondMatrix ->
      -- | The atom indices in the fragments. Accumulates during the recursion.
      IntSet ->
      -- | The bond matrix after all bonds involving the current atom have been
      --   removed and the next bound neighbours from this atom.
      IntSet
    findFromStartingAtom atomIndex bondMatrix fragmentAcc =
      let -- All bonding partners of the currently inspected atom.
          bondsTargetsOfThisAtoms =
            IntSet.fromList . fmap snd . HashMap.keys $
              HashMap.filterWithKey
                (\(oIx, _) val -> oIx == atomIndex && val)
                bondMatrix

          -- Only the bond targets, that are not yet part of the fragment.
          newTargets = bondsTargetsOfThisAtoms IntSet.\\ fragmentAcc
       in if IntSet.null newTargets
            then -- If no new targets were found, terminate the recursive search here.
              let fragmentAccNew = fragmentAcc <> IntSet.singleton atomIndex in fragmentAccNew
            else -- If new targets were found, recursively also follow the new targets.

              let fragmentAccNew = fragmentAcc <> newTargets <> IntSet.singleton atomIndex
               in IntSet.foldl'
                    (\fragAcc newTarget -> findFromStartingAtom newTarget bondMatrix fragAcc)
                    fragmentAccNew
                    newTargets

    -- Move through a complete molecule until all fragments have been found.
    findAllFragments ::
      -- | Bond matrix of the complete molecule.
      BondMatrix ->
      -- | An accumulator of atoms, which have not yet been assigned to a fragment.
      IntMap Atom ->
      -- | Growing accumulator of fragments, that have been found yet.
      IntMap IntSet ->
      IntMap IntSet
    findAllFragments bondMat atomMapAcc fragAcc =
      let -- Use the first atom of the rest of the whole molcule, which has not yet been consumed into
          -- fragments as a starting atom for the next fragment search.
          atomHead = fst <$> IntMap.lookupMin atomMapAcc
          newFragment = findFromStartingAtom <$> atomHead <*> pure bondMat <*> pure IntSet.empty

          -- Remove the atoms of the new fragment from the whole system for the next iteration.
          atomMapAccNew = IntMap.withoutKeys atomMapAcc <$> newFragment

          -- Get the highest key of the fragment IntMap before insertion of the new fragment.
          highestKey = case IntMap.lookupMax fragAcc of
            Just (k, _) -> k
            Nothing -> 0

          -- Add the new fragment with an incremented key to the IntMap of fragments.
          newFragAcc = IntMap.insert (highestKey + 1) <$> newFragment <*> pure fragAcc
       in case (atomMapAccNew, newFragAcc) of
            -- A new fragment was found
            (Just remainingAtoms, Just fragmentsFoundYet) ->
              findAllFragments bondMat remainingAtoms fragmentsFoundYet
            -- The atoms already have been fully consumed and therefore no new fragments could be
            -- found.
            _ -> fragAcc

----------------------------------------------------------------------------------------------------

-- | This function adds multipole centres to a given molecule layer. In the context of ONIOM, this
-- means, that the multipoles of a real system are used as a polarisation cloud of the deeper layer.
-- Polarisation centres, that are real atoms in the model system, will be removed. A sequence of values
-- gives scaling factors for the multipoles in bond distances.
--
-- If the atoms of the real system above do not contain multipole information, you will have useless
-- dummy atoms in the model system.
--
-- Be aware of a nasty corner case:
--
-- @
--       b--c--d
--      /       \\
--     A         E
-- @
-- The atoms @A@ and @E@ belong to the model system, which shall be polarised, while @b@, @c@ and @d@
-- belong to the real system, which provides the polarisation cloud. If scaling factors for up to three
-- bonds are given, atom @b@ has a distance of 1 from atom @A@ but a distance of 3 from atom @E@. This
-- introduces some ambiguity to the scaling. This will use the smallest distance found to select the
-- scaling factor. Therefore @b@ and @d@ will both be treated as being in a distance of 1 bond, not @b@
-- having a distance of 1 with respect to @A@ and a distance of 3 to @E@.
--
-- The function obtains a representation suitable for
-- 'Spicy.Molecule.Internal.Multipoles.molToPointCharges', where the atoms of the layer above are
-- dummy atoms but still connected in the bond matrix, which is important for definition of the
-- local axes system of the multipole point charges.
getPolarisationCloudFromAbove ::
  (MonadThrow m) =>
  -- | The whole molecular system with the multipoles at least in the layer above the
  --   specified one.
  Molecule ->
  -- | Specificaiton of the molecule layer, which should get a polarisation cloud from
  --   the layer above. The layer above is also determined from the MolID by removing
  --   its last element.
  MolID ->
  -- | Scaling factors for the multipole moments in bond distances. The first value in
  --   the sequence would be used to scale the multipoles of atoms 1 bond away from
  --   set 1 atoms, the second value in the sequence multipoles 2 bonds away and so
  --   on.
  Seq Double ->
  -- | The specified layer of the molecule only with polarisation centres added. All
  --   submolecules or fragments this layer may have had are removed.
  m Molecule
getPolarisationCloudFromAbove mol layerID poleScalings = do
  -- Check for the sanity of the molecule, as otherwise the assumptions regarding bonds make no
  -- sense.
  _ <- checkMolecule mol

  go mol layerID
  where
    go ::
      (MonadThrow m) =>
      -- | The whole molecular system with the multipoles at least in the layer above the
      --   specified one.
      Molecule ->
      -- | Specificaiton of the molecule layer, which should get a polarisation cloud from
      --   the layer above. The layer above is also determined from the MolID by removing
      --   its last element.
      MolID ->
      -- | The specified layer of the molecule only with polarisation centres added. All
      --   submolecules or fragments this layer may have had are removed.
      m Molecule
    -- The system has been reduced to a single layer and is definitely done. Return it.
    go currentMol Empty = return currentMol
    -- There are more layers above the current one which shall be polarised. Use the layer directly
    -- abpve the current model system for polarisation and join.
    go currentMol currentMolID@(polID :|> _modelID) = do
      -- Obtain the local model system and the local real system, which is being used for
      -- polarisation of the model.
      localModel <- getMolByID currentMol currentMolID
      localReal <- getMolByID currentMol polID

      -- Deconstruct the real system and do some adjustements.
      let --  - Dummy atoms are removed from the real system.
          --  - Link atoms will be changed to normal atoms.
          --  - All remaining atoms will be dummy atoms.
          realAtoms =
            IntMap.map (\a -> a & #isDummy .~ True)
              . IntMap.filter (\a -> not $ a ^. #isDummy)
              . IntMap.map (\a -> a & #isLink .~ NotLink)
              $ localReal ^. #atoms
          -- - The bond matrix will be updated only to contain the filtered atoms.
          realBonds = cleanBondMatByAtomInds (localReal ^. #bonds) (IntMap.keysSet realAtoms)
          -- - The fragments will be updated only to contain the filtered atoms.
          realFragments =
            IntMap.map
              (\f -> f & #atoms %~ IntSet.intersection (IntMap.keysSet realAtoms))
              $ localReal ^. #fragment

      -- Get the link atoms of the model system and groups of their bonding partners in the real
      -- system in a given distance of bonds. The first entry is dropped as those are distance 0
      -- atoms (therefore the capped atoms themselves).
      let modelCappedAtoms = IntMap.keys . getCappedAtoms $ localModel ^. #atoms
          searchDistance = Seq.length poleScalings
      distanceGroups <-
        combineDistanceGroups
          <$> mapM
            (\cappedStartAtom -> Seq.drop 1 <$> bondDistanceGroups localReal cappedStartAtom searchDistance)
            modelCappedAtoms

      -- Scale the polarisation centres of the real system according to their distance to the capped
      -- atoms of the model system.
      let realAtomsScaled =
            Seq.foldlWithIndex
              ( \atomsAcc bondDist atomsAtDist ->
                  let scaleFactor = fromMaybe 0 $ poleScalings Seq.!? bondDist
                   in scaleMultipoles atomsAcc atomsAtDist scaleFactor
              )
              realAtoms
              distanceGroups

      -- Combine the model system (and its already existing polarisation centres) with the modified
      -- stuff of the real layer.
      let accumulatorMol =
            localModel
              & #atoms %~ (<> realAtomsScaled)
              & #bonds %~ HashMap.unionWith (||) realBonds
              & #fragment
                %~ IntMap.unionWith
                  (\fM fR -> fM & #atoms %~ (<> fR ^. #atoms))
                  realFragments

      go accumulatorMol polID

    -- Function to scale all Multipoles of the atoms in the input set with a factor.
    scaleMultipoles :: IntMap Atom -> IntSet -> Double -> IntMap Atom
    scaleMultipoles atoms selection factor =
      IntSet.foldl'
        (\accAtoms sel -> IntMap.adjust (& #multipoles %~ modifyMultipole (* factor)) sel accAtoms)
        atoms
        selection

----------------------------------------------------------------------------------------------------

-- | Getting the distance of real system atoms to the capped atoms of the model system, gives
-- multiple distances of a given real system atom to the model system atoms in bond distances.
-- This function joins the real system atoms, which are grouped by bond distance from a given
-- capped model system atom, in a way to ensure that a real system atom always only appears at
-- the lowest possible distance to the model system.
combineDistanceGroups :: [Seq IntSet] -> Seq IntSet
combineDistanceGroups distGroups =
  let -- Join all the distance groups of different capped atoms. An atom of the real system might
      -- appear here mutliple times with different distances.
      combinationWithAmbigousAssignments =
        List.foldl'
          ( \acc thisGroup ->
              let lengthAcc = Seq.length acc
                  lengthThis = Seq.length thisGroup

                  -- Adjust the length of the accumulator and this group, to match the longer one of both.
                  lengthAdjAcc =
                    if lengthThis > lengthAcc
                      then acc Seq.>< Seq.replicate (lengthThis - lengthAcc) IntSet.empty
                      else acc
                  lengthAdjThis =
                    if lengthAcc > lengthThis
                      then thisGroup Seq.>< Seq.replicate (lengthAcc - lengthThis) IntSet.empty
                      else thisGroup

                  -- Join this group with the accumulator.
                  newAcc = Seq.zipWith IntSet.union lengthAdjAcc lengthAdjThis
               in newAcc
          )
          Seq.empty
          distGroups
      -- Make sure, that a given atom always only appears at the lower distance and remove its
      -- occurence at a higher distance, if already found at a lower distance.
      combinationUnambigous =
        foldl
          ( \(atomsAtLowerDist, unambDistSeqAcc) thisDistanceAtoms ->
              let thisWithLowerRemoved = thisDistanceAtoms IntSet.\\ atomsAtLowerDist
                  newAtomsAtLowerDist = thisWithLowerRemoved `IntSet.union` atomsAtLowerDist
                  newUnambDistSeqAcc = unambDistSeqAcc |> thisWithLowerRemoved
               in (newAtomsAtLowerDist, newUnambDistSeqAcc)
          )
          (IntSet.empty, Seq.empty)
          combinationWithAmbigousAssignments
   in snd combinationUnambigous

----------------------------------------------------------------------------------------------------

-- | This function takes a starting atom in given layer and starts moving away from it along the bonds.
-- Atoms of the same bond distance will be grouped. Therefore all atoms 1 bond away from the start will
-- form a group, all bonds 2 bonds away from the starting atom will form a group and so on. Atoms will
-- only belong to one group always, which is relevant for cyclic structures. They will always be
-- assigned to the group with the lowest distance.
--
-- The function takes a maximum distance in bonds to search. The search ends, if either no new atoms
-- can be found or the search would extend beyond the maximum search distance.
bondDistanceGroups ::
  MonadThrow m =>
  -- | The molecule layer which to use for the search. Sublayers are ignored.
  Molecule ->
  -- | Key of the starting atom.
  Int ->
  -- | The maximum distance for search in bonds. Must therefore be equal or
  --   greater than 0.
  Int ->
  -- | A sequence of atoms in a given distance. At the sequence index 0 will be
  --   the starting atom index. At sequence index n will be the set of atom
  --   indices n bonds away from the start.
  m (Seq IntSet)
bondDistanceGroups mol startAtomInd maxBondSteps = do
  -- Check the molecule, as the bond matrix could otherwise be damaged and create strange results.
  _ <- checkMolecule mol

  let bondMat = mol ^. #bonds
      groups = stepAndGroup bondMat maxBondSteps (Seq.singleton . IntSet.singleton $ startAtomInd)
  return groups
  where
    stepAndGroup ::
      -- | The bond matrix through which to step. During the recursion this will
      --   shrink, as the atoms already assigned will be removed as and targets.
      BondMatrix ->
      -- | The maximum distance in bonds.
      Int ->
      -- | Accumulator of the groups.
      Seq IntSet ->
      Seq IntSet
    stepAndGroup bondMat' maxDist groupAcc
      | currentDist > maxDist =
        groupAcc
      | otherwise =
        let -- The currently most distant atoms from the start and therefore the origin for the next
            -- search.
            searchOrigins = case groupAcc of
              Empty -> IntSet.singleton startAtomInd
              _ :|> lastGroup -> lastGroup

            -- Make sure the group accumulator always contains the start atom at distance 0.
            groupAccAdjusted = case groupAcc of
              Empty -> Seq.singleton . IntSet.singleton $ startAtomInd
              _ -> groupAcc

            -- The next more distant sphere of atoms. Potentially, in case of a ring closure, the new
            -- sphere might contain atoms of this sphere, due to how the bond matrix is updated in the
            -- recursion. To avoid this, remove the origins from the next sphere.
            nextSphereAtoms =
              (`IntSet.difference` searchOrigins) . IntSet.unions $
                fmap
                  (getAtomBondPartners bondMat')
                  (IntSet.toList searchOrigins)

            -- The bond matrix for the next iteration will have this iterations search origins removed
            -- both as origin as well as target. This should make sure, that stepping back is not
            -- possible.
            newBondMat =
              HashMap.filterWithKey
                ( \(ixO, ixT) val ->
                    not (ixO `IntSet.member` searchOrigins)
                      && not (ixT `IntSet.member` searchOrigins)
                      && val
                )
                bondMat'

            -- The new accumulator will contain the results as a new entry to the sequence.
            newGroupAcc = groupAccAdjusted |> nextSphereAtoms
         in stepAndGroup newBondMat maxDist newGroupAcc
      where
        currentDist = Seq.length groupAcc

    -- Get all bond partners of an atom.
    getAtomBondPartners :: BondMatrix -> Int -> IntSet
    getAtomBondPartners bondMat' atom =
      IntSet.fromList
        . fmap snd
        . HashMap.keys
        . HashMap.filterWithKey (\(ixO, _) val -> ixO == atom && val)
        $ bondMat'

----------------------------------------------------------------------------------------------------

-- | Given a molecule, build the associations of this layers atoms, with the fragments of this layers.
-- Therefore, if fragments represent the whole molecule, this will assign Just Fragment to each atom.
-- This is the fragment to which the atom belongs.
getAtomAssociationMap ::
  MonadThrow m =>
  -- | The current molecule layer, for which to build
  --   the association map.
  Molecule ->
  -- | The original 'IntMap' of atoms, but the atoms are
  --   now associated with a fragment and the fragments
  --   'Int' key.
  m (IntMap (Atom, Maybe (Int, Fragment)))
getAtomAssociationMap mol = do
  let atoms' = mol ^. #atoms
      fragments = mol ^. #fragment
  atomToFragAssociations <-
    IntMap.traverseWithKey
      ( \atomKey atom -> do
          -- Find all fragments, to which an atom is assigned.
          let matchingFragments =
                IntMap.filter (\frag -> atomKey `IntSet.member` (frag ^. #atoms)) fragments

          case IntMap.size matchingFragments of
            0 -> return (atom, Nothing)
            1 -> return (atom, IntMap.lookupMin matchingFragments)
            _ ->
              throwM
                . MolLogicException "getAtomAssociationMap"
                $ "Assignment of atom "
                  <> show atomKey
                  <> " to a fragment failed, as the assignment is ambigous. \
                     \This means an invalid data structure in the fragments."
      )
      atoms'
  return atomToFragAssociations

----------------------------------------------------------------------------------------------------

-- | Parser easily obtain lists of 'FragmentAtomInfo', which need to be converted to proper fragments and
-- atoms for the molecule. This function builds the data types for '_#atoms' and
-- '_#fragment'.
fragmentAtomInfo2AtomsAndFragments :: [FragmentAtomInfo] -> (IntMap Atom, IntMap Fragment)
fragmentAtomInfo2AtomsAndFragments info =
  let atoms' = IntMap.fromList . fmap (\i -> (faiAtomIndex i, faiAtom i)) $ info
      fragments =
        IntMap.fromListWith fragmentUnion . fmap (\i -> (faiFragmentIndex i, faiFragment i)) $ info
   in (atoms', fragments)
  where
    -- Joins two fragments. The @label@ and @chain@ should be identical but this
    -- is not strictly required by this functions. Instead this functions uses a left bias for the
    -- fragments.
    fragmentUnion :: Fragment -> Fragment -> Fragment
    fragmentUnion a b =
      let atomsInB = b ^. #atoms
       in a & #atoms %~ IntSet.union atomsInB

----------------------------------------------------------------------------------------------------

-- | Obtains the Jacobian matrix for the transformation from the basis of the model system to
-- the basis of a real system as used in the equation for ONIOM gradients:
--
-- \[
--     \nabla E^\mathrm{ONIOM2} = \nabla E^\mathrm{real} - \nabla E^\mathrm{model, low} \mathbf{J}
--                              + \nabla E^\mathrm{model, high} \mathbf{J}
-- \]
--
-- The gradient of the real system, which has \(N\) atoms is a row vector with \(3 N\) elements. The
-- gradient of the model system, which has \(M\) atoms is a row vector, with \(3 M\) elements. The
-- Jacobian matrix therefore has a size of \( 3 M \times 3 N\).
--
-- To transform from the basis of the model system to the basis of the real system, the Jacobian does
-- the following
--
--   - Distribute the gradient that is acting on a link atom @LA@ (which connects the model system atom
--     @LAC@ to the real system atom @LAH@) according to the factor \(g\), if the position of "LA"
--     depends on @LAH@ and @LAC@ by
--     \( \mathbf{r}^\mathrm{LA} = \mathbf{r}^\mathrm{LAC} + g (\mathbf{r}^\mathrm{LAH} - \mathbf{r}^\mathrm{LAC}) \)
--     as
--     \( \partial r_a^\mathrm{LA} / \partial r_b^\mathrm{LAH} = g \delta_{a,b} \)
--     ,
--     \(\partial r_a^\mathrm{LA} / \partial r_b^\mathrm{LAC} = (1 - g) \delta_{a,b}\)
--     , where \(a\) and \(b\) are the cartesian components \(x\), \(y\) or \(z\) of the gradient.
--   - Atoms of the real system, that are not part of the model system (set 4 atoms) get a contribution
--     of 0 from the model system.
--   - Atoms of the real system, that are part of the model system, get a full contribution from the
--     model system.
--   - To obtain an element of the Jacobian all contributions are summed up.

-- TODO (phillip|p=5|#Improvement #ExternalDependency) - As soon as Massiv supports sparse arrays, use them! https://github.com/lehins/massiv/issues/50
getJacobian ::
  MonadThrow m =>
  -- | Atoms of the real system.
  IntMap Atom ->
  -- | Atoms of the model system.
  IntMap Atom ->
  m (Matrix D Double)
getJacobian realAtoms modelAtoms = do
  let nModelAtoms = IntMap.size modelAtoms
      nRealAtoms = IntMap.size realAtoms
      realLinkAtoms = IntMap.filter (isAtomLink . isLink) realAtoms
      realLinkAtomKeys = IntMap.keysSet realLinkAtoms
      modelLinkAtoms =
        flip IntMap.withoutKeys realLinkAtomKeys
          . IntMap.filter (isAtomLink . isLink)
          $ modelAtoms
      -- Translations of different mapping schemes:
      --   - Global dense mapping means, that the real system atoms are treated as if they were dense
      --      and 0-based (array like)
      --   - Local dense mapping means, that the model system atoms are treated as if they were dense
      --     and 0-based
      modelAtomKeys = IntMap.keys modelAtoms
      allAtomKeys = IntMap.keys $ IntMap.union realAtoms modelAtoms
      sparse2DenseGlobal = IntMap.fromAscList $ RIO.zip allAtomKeys [0 ..]
      dense2SparseGlobal = Massiv.fromList Par allAtomKeys :: Vector U Int
      sparse2DenseLocal = IntMap.fromAscList $ RIO.zip modelAtomKeys [0 ..]
      -- This is the mapping from global dense indices to local dense indices. This means if you ask
      -- for a real system atom, you will get 'Just' the dense index of the same atom in the model
      -- system or 'Nothing' if this atom is not part of the model system.
      global2Local :: IntMap (Maybe Int)
      global2Local =
        let globalKeys = [0 .. nRealAtoms - 1]
            global2LocalAssoc =
              fmap
                ( \globalKey ->
                    let sparseKey = dense2SparseGlobal Massiv.!? globalKey :: Maybe Int
                        localKey = flip IntMap.lookup sparse2DenseLocal =<< sparseKey
                     in (globalKey, localKey)
                )
                globalKeys
         in IntMap.fromAscList global2LocalAssoc

  -- Create a Matrix like HashMap, similiar to the BondMat stuff, that contains for pairs of
  -- dense (local modelKey, global realKey) scaling factors g. There should be one pair per link.
  -- atom.
  -- This is close to the final jacobian but not there yet.
  ((linkToModelG, linkToRealG) :: (HashMap (Int, Int) Double, HashMap (Int, Int) Double)) <-
    IntMap.foldlWithKey'
      ( \hashMapAcc' linkKey linkAtom -> do
          (linkToModelAcc, linkToRealAcc) <- hashMapAcc'

          let denseLinkLocalKey = sparse2DenseLocal IntMap.!? linkKey
              sparseModelKey = linkAtom ^? #isLink % #linkModelPartner
              denseModelGlobalKey = (sparse2DenseGlobal IntMap.!?) =<< sparseModelKey
              sparseRealKey = linkAtom ^? #isLink % #linkRealPartner
              denseRealGlobalKey = (sparse2DenseGlobal IntMap.!?) =<< sparseRealKey
              gFactor' = linkAtom ^? #isLink % #linkGFactor

          (linkIx, modelIx, realIx) <-
            case (denseLinkLocalKey, denseModelGlobalKey, denseRealGlobalKey) of
              (Just l, Just m, Just r) -> return (l, m, r)
              _ ->
                throwM $
                  MolLogicException
                    "getJacobian"
                    "While checking for atoms, that are influenced by link atoms, either a model system\
                    \ atom or a real system atom, that must be associated to the link atom, could not be\
                    \ found."

          gFactor <- case gFactor' of
            Nothing ->
              throwM $
                MolLogicException
                  "getJacobian"
                  "The scaling factor g for this link atom could not be found."
            Just g -> return g

          return
            ( HashMap.insert (linkIx, modelIx) (1 - gFactor) linkToModelAcc,
              HashMap.insert (linkIx, realIx) gFactor linkToRealAcc
            )
      )
      (return (HashMap.empty, HashMap.empty))
      modelLinkAtoms

  let jacobian' :: Matrix D Double
      jacobian' =
        Massiv.makeArray
          Par
          (Sz (3 * nModelAtoms :. 3 * nRealAtoms))
          ( \(modelCartIx :. realCartIx) ->
              let -- The indices of the matrix are the expanded atom indices, meaning that actually 3
                  -- values per atom (x, y, z) are present. These are the atom indices again now.
                  modelIx = modelCartIx `div` 3
                  realIx = realCartIx `div` 3

                  -- This is the cartesian component of the gradient of an atom. 0:x, 1:y, 2:z
                  modelComponent = modelCartIx `mod` 3
                  realComponent = realCartIx `mod` 3

                  -- Check wether the current real atom is also in the model system.
                  realInModel = join $ global2Local IntMap.!? realIx

                  -- If the real atom is present in the model system, the gradient of the model system
                  -- for this atom is used instead. Only use the gradient for the appropriate cartesian
                  -- components (x component provides x component, but not x provided y).
                  defaultValue = case realInModel of
                    Nothing -> 0
                    Just localModelKey ->
                      if modelIx == localModelKey && modelComponent == realComponent then 1 else 0

                  -- Check if the current pair belogns to a link atom in the model system. Keep it only
                  -- if the cartesian components match.
                  (link2ModelGValue, link2RealGValue) =
                    let link2ModelG = fromMaybe 0 $ HashMap.lookup (modelIx, realIx) linkToModelG
                        link2RealG = fromMaybe 0 $ HashMap.lookup (modelIx, realIx) linkToRealG
                     in if modelComponent == realComponent then (link2ModelG, link2RealG) else (0, 0)
               in defaultValue + link2ModelGValue + link2RealGValue
          )
  return jacobian'

----------------------------------------------------------------------------------------------------

-- | Applies a function that somehow combines the two set of multipoles.
combineMultipoles :: (Double -> Double -> Double) -> Multipoles -> Multipoles -> Multipoles
combineMultipoles f a b =
  let newMonopole = do
        mA <- a ^. #monopole
        mB <- b ^. #monopole
        return $
          Monopole
            { q00 = f (mA ^. #q00) (mB ^. #q00)
            }
      newDipole = do
        dA <- a ^. #dipole
        dB <- b ^. #dipole
        return $
          Dipole
            { q10 = f (dA ^. #q10) (dB ^. #q10),
              q11c = f (dA ^. #q11c) (dB ^. #q11c),
              q11s = f (dA ^. #q11s) (dB ^. #q11s)
            }
      newQuadrupole = do
        qA <- a ^. #quadrupole
        qB <- b ^. #quadrupole
        return $
          Quadrupole
            { q20 = f (qA ^. #q20) (qB ^. #q20),
              q21c = f (qA ^. #q21c) (qB ^. #q21c),
              q21s = f (qA ^. #q21s) (qB ^. #q21s),
              q22c = f (qA ^. #q22c) (qB ^. #q22c),
              q22s = f (qA ^. #q22s) (qB ^. #q22s)
            }
      newOctopole = do
        oA <- a ^. #octopole
        oB <- b ^. #octopole
        return $
          Octopole
            { q30 = f (oA ^. #q30) (oB ^. #q30),
              q31c = f (oA ^. #q31c) (oB ^. #q31c),
              q31s = f (oA ^. #q31s) (oB ^. #q31s),
              q32c = f (oA ^. #q32c) (oB ^. #q32c),
              q32s = f (oA ^. #q32s) (oB ^. #q32s),
              q33c = f (oA ^. #q33c) (oB ^. #q33c),
              q33s = f (oA ^. #q33s) (oB ^. #q33s)
            }
      newHexadecapole = do
        hA <- a ^. #hexadecapole
        hB <- b ^. #hexadecapole
        return $
          Hexadecapole
            { q40 = f (hA ^. #q40) (hB ^. #q40),
              q41c = f (hA ^. #q41c) (hB ^. #q41c),
              q41s = f (hA ^. #q41s) (hB ^. #q41s),
              q42c = f (hA ^. #q42c) (hB ^. #q42c),
              q42s = f (hA ^. #q42s) (hB ^. #q42s),
              q43c = f (hA ^. #q43c) (hB ^. #q43c),
              q43s = f (hA ^. #q43s) (hB ^. #q43s),
              q44c = f (hA ^. #q44c) (hB ^. #q44c),
              q44s = f (hA ^. #q44s) (hB ^. #q44s)
            }
   in Multipoles
        { monopole = newMonopole,
          dipole = newDipole,
          quadrupole = newQuadrupole,
          octopole = newOctopole,
          hexadecapole = newHexadecapole
        }

----------------------------------------------------------------------------------------------------

-- | Applies a function to multipoles.
modifyMultipole :: (Double -> Double) -> Multipoles -> Multipoles
modifyMultipole f a =
  let newMonopole = do
        mA <- a ^. #monopole
        return $
          Monopole
            { q00 = f (mA ^. #q00)
            }
      newDipole = do
        dA <- a ^. #dipole
        return $
          Dipole
            { q10 = f (dA ^. #q10),
              q11c = f (dA ^. #q11c),
              q11s = f (dA ^. #q11s)
            }
      newQuadrupole = do
        qA <- a ^. #quadrupole
        return $
          Quadrupole
            { q20 = f (qA ^. #q20),
              q21c = f (qA ^. #q21c),
              q21s = f (qA ^. #q21s),
              q22c = f (qA ^. #q22c),
              q22s = f (qA ^. #q22s)
            }
      newOctopole = do
        oA <- a ^. #octopole
        return $
          Octopole
            { q30 = f (oA ^. #q30),
              q31c = f (oA ^. #q31c),
              q31s = f (oA ^. #q31s),
              q32c = f (oA ^. #q32c),
              q32s = f (oA ^. #q32s),
              q33c = f (oA ^. #q33c),
              q33s = f (oA ^. #q33s)
            }
      newHexadecapole = do
        hA <- a ^. #hexadecapole
        return $
          Hexadecapole
            { q40 = f (hA ^. #q40),
              q41c = f (hA ^. #q41c),
              q41s = f (hA ^. #q41s),
              q42c = f (hA ^. #q42c),
              q42s = f (hA ^. #q42s),
              q43c = f (hA ^. #q43c),
              q43s = f (hA ^. #q43s),
              q44c = f (hA ^. #q44c),
              q44s = f (hA ^. #q44s)
            }
   in Multipoles
        { monopole = newMonopole,
          dipole = newDipole,
          quadrupole = newQuadrupole,
          octopole = newOctopole,
          hexadecapole = newHexadecapole
        }

----------------------------------------------------------------------------------------------------

-- | Redistributes the multipole moments of the link atoms of a given molecule layer (not its
-- sublayers) homogenously among all other atoms of the the layer.
redistributeLinkMoments :: Molecule -> Molecule
redistributeLinkMoments = Optics.over #atoms redistributeLinkMoments'

-- | Redistributes the multipole moments of the link atoms of a given set of atoms
-- homogenously among all other atoms in the set.
redistributeLinkMoments' :: IntMap Atom -> IntMap Atom
redistributeLinkMoments' allAtoms =
  let zeroMoment =
        Multipoles
          { monopole = Just $ Monopole 0,
            dipole = Just $ Dipole 0 0 0,
            quadrupole = Just $ Quadrupole 0 0 0 0 0,
            octopole = Just $ Octopole 0 0 0 0 0 0 0,
            hexadecapole = Just $ Hexadecapole 0 0 0 0 0 0 0 0 0
          }

      -- Link atoms of this layer.
      linkAtoms = IntMap.filter (isAtomLink . isLink) allAtoms

      -- The model atoms without the link atoms.
      set1Atoms = IntMap.filter (not . isAtomLink . isLink) allAtoms
      nSet1Atoms = IntMap.size set1Atoms
      sumOfLinkMoments = IntMap.foldl' (combineMultipoles (+)) zeroMoment . fmap (^. #multipoles) $ linkAtoms
      linkMomentsScaled = modifyMultipole (/ (fromIntegral nSet1Atoms)) sumOfLinkMoments

      -- Distribute the link atom multipoles homogenously over the set1 atoms.
      newSet1Atoms = fmap (\a -> a & #multipoles %~ (combineMultipoles (+)) linkMomentsScaled) set1Atoms

      -- Remove the multipole information from the link atoms.
      newLinkAtoms = fmap (\a -> a & #multipoles .~ def) linkAtoms

      -- Recombine the set 1 and 2 atoms again to give the layer with redistributed multipoles.
      newAtoms = IntMap.union newSet1Atoms newLinkAtoms
   in newAtoms

----------------------------------------------------------------------------------------------------

-- | This function takes a local MC-ONIOM2 setup and removes link tag of atoms from the model
-- systems, if they were already link atoms in the real system.
removeRealLinkTagsFromModel ::
  -- | The real system.
  Molecule ->
  -- | A model system directly below the real system.
  Molecule ->
  Molecule
removeRealLinkTagsFromModel realMol modelCentre =
  let realLinkAtoms =
        IntMap.keysSet . IntMap.filter (isAtomLink . isLink) $ realMol ^. #atoms

      -- The model centres, but all atoms, that were already a link atom in the real system, do not
      -- longer have the link tag.
      newModel =
        let modelAtoms = modelCentre ^. #atoms
            modelAtomsClean =
              IntMap.mapWithKey
                ( \atomKey atom ->
                    if atomKey `IntSet.member` realLinkAtoms
                      then atom & #isLink .~ NotLink
                      else atom
                )
                modelAtoms
         in modelCentre & #atoms .~ modelAtomsClean
   in newModel

----------------------------------------------------------------------------------------------------

-- | Obtain all calculations that need to be performed on a molecule in hierarchical order
-- (top to down and left to right)
getAllCalcIDsHierarchically :: Molecule -> Seq CalcID
getAllCalcIDsHierarchically mol =
  molFoldlWithMolID
    ( \idAcc currentMolID currentMol ->
        let calcIDsOfCurrentMol = getCalcIDsOfMolLayer currentMolID currentMol
         in idAcc <> calcIDsOfCurrentMol
    )
    Empty
    mol
  where
    getCalcIDsOfMolLayer :: MolID -> Molecule -> Seq CalcID
    getCalcIDsOfMolLayer molID molLayer =
      Map.foldlWithKey'
        (\idsAcc calcKey _ -> idsAcc |> CalcID {molID = molID, calcKey = calcKey})
        Empty
        (molLayer ^. #calcContext)

----------------------------------------------------------------------------------------------------

-- | Get all layer IDs of a molecule in ONIOM hierarchical order (top to bottom, left to right).
getAllMolIDsHierarchically :: Molecule -> Seq MolID
getAllMolIDsHierarchically mol =
  molFoldlWithMolID
    ( \idAcc currentMolID _ -> idAcc |> currentMolID
    )
    Empty
    mol

----------------------------------------------------------------------------------------------------

-- | Generic update of some atomic positions. The updated atoms need to be specified by an 'IntSet'
-- and the position vector needs to match the number of atoms (for \(n\) atoms a position vector of
-- length \(3 n\) is required). The coordinates in the vector need to be in the same (strictly
-- ascending) order as the atoms in the 'IntSet' (always strictly ascending).
updatePositionsPosVec :: MonadThrow m => Vector S Double ->  IntSet -> Molecule -> m Molecule
updatePositionsPosVec pos sel mol = do
  -- Associate new positions with individual atoms.
  pos3 <- Massiv.map (VectorS . compute @S). outerSlices <$> resizeM (Sz $ IntSet.size sel :. 3) pos
  atomPosMap <- associate pos3 sel mempty

  -- Update all atoms with new positions.
  updatePositions atomPosMap mol
  where
    -- A zipper for vectors with sets to construct an IntMap
    associate :: (Load r Ix1 e, Source r Ix1 e, MonadThrow m) => Vector r e -> IntSet -> IntMap e -> m (IntMap e)
    associate vec selSet acc
      | sizeV /= sizeS = throwM $ MolLogicException "updatePositionsGeneric" "mismatch between number of atoms and coordinates"
      | sizeV == 0 && sizeS == 0 = pure mempty
      | sizeV == 1 && sizeS == 1 = IntMap.insert <$> headSet <*> headVec <*> pure acc
      | otherwise =
        let newAcc = IntMap.insert <$> headSet <*> headVec <*> pure acc
        in join $ associate tailVec <$> tailSet <*> newAcc
      where
        localExc = SpicyIndirectionException "updatePositionsGeneric"
        Sz sizeV = Massiv.size vec
        sizeS = IntSet.size selSet
        splitSet = maybe2MThrow (localExc "no atoms to update left") $ IntSet.minView selSet
        headSet = fst <$> splitSet
        tailSet = snd <$> splitSet
        headVec = headM vec
        tailVec = Massiv.tail vec


----------------------------------------------------------------------------------------------------

-- | Recursively update the coordinates of all real and link atoms in all layers. Link atoms that
-- were found get new coordinates on this layer and then their new coordinates are added to the
-- 'IntMap' of new atom coordinates. This allows to safely update link atom coordinates in
-- deeper layers, that depend on link atoms of higher layers. Dummy atoms in the layer will be
-- removed. But this is what we would want anyway as also the layers above are probably now
-- displaced and the dummies need to be regenerated by another function.
updatePositions :: MonadThrow m => IntMap (VectorS Double) -> Molecule -> m Molecule
updatePositions newCoords molLayer = do
  -- Update all real atoms of this layer excluding Dummy atoms.
  let atoms = molLayer ^. #atoms
      linkAtoms = IntMap.filter (isAtomLink . isLink) atoms
      updatedRealAtoms =
        IntMap.intersectionWith
          ( \newC oldA -> oldA & #coordinates .~ newC
          )
          newCoords
          atoms
  updateLinkAtoms <-
    maybe2MThrow (localExc "atom key not found for coordinate update") $
      traverse
        ( \lA -> case isLink lA of
            IsLink {linkModelPartner, linkRealPartner, linkGFactor} -> do
              coordsMP <- getVectorS <$> newCoords IntMap.!? linkModelPartner
              coordsRP <- getVectorS <$> newCoords IntMap.!? linkRealPartner
              newLinkCoords <- VectorS <$> calcLinkCoords coordsMP coordsRP linkGFactor
              return $ lA & #coordinates .~ newLinkCoords
            NotLink -> return lA
        )
        linkAtoms

  -- Updates of this layers data and inputs for next recursion steps.
  let updateNonDummies = updateLinkAtoms <> updatedRealAtoms
      thisLayerUpdated = molLayer & #atoms .~ updateNonDummies
      newCoordsUpdated = newCoords <> fmap (^. #coordinates) updateLinkAtoms

  -- Recursively update all sublayers.
  let subMols = thisLayerUpdated ^. #subMol
  subMolsUpdated <- traverse (updatePositions newCoordsUpdated) subMols

  return $ thisLayerUpdated & #subMol .~ subMolsUpdated
  where
    localExc = MolLogicException "updatePositions"

----------------------------------------------------------------------------------------------------

-- | Reduces the search radius of a neighbour list to a smaller distance of neighbours. This should
-- be more efficient than building a new neighbourlist with a different radius.
shrinkNeighbourList ::
  MonadThrow m =>
  IntMap Atom ->
  Map Double NeighbourList ->
  Double ->
  m (Map Double NeighbourList)
shrinkNeighbourList atoms oldNLs newDist
  | isNothing maxDist = throwM . localExc $ "No neighbourlists available."
  | maxDist < Just newDist = throwM . localExc $ "New distance of the neighbour list must be smaller than the old one"
  | otherwise = do
    -- Obtain the neighbourlist which has a larger search distance but is closest to the new one.
    -- This saves time in computing distances.
    bestMatchNL <-
      maybe2MThrow
        ( localExc
            "no matching neighbourlist with a distance larger\
            \ than the new search distance could be found"
        )
        . fmap fst
        . Map.minView
        . Map.filterWithKey (\k _ -> k > newDist)
        $ oldNLs

    -- Check all distances and remove those atoms, which have a higher distance than the search
    -- distance
    newNL <- IntMap.traverseWithKey (\oA tA -> filterNeigbours atoms newDist oA tA) bestMatchNL

    -- Return the old neighbourlists together with the new shrinked one.
    return $ oldNLs <> Map.singleton newDist newNL
  where
    localExc = MolLogicException "shrinkNeighbourList"
    maxDist = fmap (fst . fst) . Map.maxViewWithKey $ oldNLs
    filterNeigbours :: MonadThrow m => IntMap Atom -> Double -> Int -> IntSet -> m IntSet
    filterNeigbours atoms' dist origin targets = do
      originCoord <-
        maybe2MThrow (localExc "Origin atom does not exist in the set of atoms.")
          . fmap getVectorS
          $ (atoms' IntMap.!? origin ^? _Just % #coordinates)
      let filteredTargets =
            IntSet.filter
              ( \t ->
                  let targetCoord = getVectorS <$> (atoms' IntMap.!? t ^? _Just % #coordinates)
                   in case targetCoord of
                        Nothing -> False
                        Just coord -> case distance originCoord coord of
                          Nothing -> False
                          Just d -> d <= dist
              )
              targets
      return filteredTargets

----------------------------------------------------------------------------------------------------

-- | Isolate a molecule layer, usually in preparation for printing. Cleaning involves:
--  * Remove all dummy atoms
--  * Removing all sub-molecules
--  * Update the bond matrix and the fragments
isolateMoleculeLayer :: Molecule -> Molecule
isolateMoleculeLayer mol =
  mol
    & #atoms %~ flip IntMap.restrictKeys realAtomInds
    & #fragment % each % #atoms %~ IntSet.intersection realAtomInds
    & #bonds %~ flip cleanBondMatByAtomInds realAtomInds
    & #subMol .~ mempty
  where
    realAtomInds = IntMap.keysSet . IntMap.filter (\a -> not $ a ^. #isDummy) $ mol ^. #atoms

----------------------------------------------------------------------------------------------------

-- | Spicy tree notation string for identifying an ONIOM node in the tree.
molID2OniomHumanID :: MolID -> Text
molID2OniomHumanID Seq.Empty = "0"
molID2OniomHumanID molID =
  let depth = Seq.length molID
      idTree =
        foldr'
          ( \currentID textAcc ->
              let offSet = fromEnum 'A'
                  idLetter = toEnum $ currentID + offSet
               in textAcc `Text.snoc` idLetter
          )
          (tShow depth)
          molID
   in idTree
