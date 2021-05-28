-- |
-- Module      : Spicy.Molecule.Internal.Writer
-- Description : Writing chemical data formats
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- A module which converts the internal Molecule representation to a text, which is a common chemical
-- file format, that can be read by Avogadro, VMD, OpenBabel etc.. The writers are not fool proof with
-- respect to force field types, which should always be remembered when usings its results.
module Spicy.Molecule.Internal.Writer
  ( writeXYZ,
    writeTXYZ,
    writeMOL2,
    writePDB,
    writeSpicy,
    writeONIOM,
  )
where

import Data.Aeson.Encode.Pretty
import Data.Attoparsec.Text (isEndOfLine)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.Split (chunksOf)
import Data.Massiv.Array as Massiv hiding
  ( all,
    map,
    mapM_,
    toList,
    zip,
  )
import Data.Maybe
import qualified Data.Text.Lazy.Builder as TextBuilder
import Formatting hiding ((%))
import qualified Formatting as F
import Optics
import RIO hiding
  ( view,
    (.~),
    (^.),
    (^?),
  )
import qualified RIO.ByteString as ByteString
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List
import qualified RIO.Map as Map
import RIO.Partial (toEnum)
import qualified RIO.Seq as Seq
import qualified RIO.Text as Text
import qualified RIO.Text.Lazy as TextLazy
import RIO.Writer
import Spicy.Common
import Spicy.Molecule.Internal.Multipoles
import Spicy.Molecule.Internal.Types
import Spicy.Molecule.Internal.Util

-- | Write a Molden XYZ file from a molecule. This format ignores all deep level layers of a molecule.
writeXYZ :: MonadThrow m => Molecule -> m Text
writeXYZ mol = do
  _ <- checkMolecule mol
  toXYZ mol
  where
    -- Assemble a XYZ file from a molecule.
    toXYZ :: MonadThrow m => Molecule -> m Text
    toXYZ m = do
      let -- Line 1 of the header contains the number of atoms
          nAtoms = bprint (int F.% "\n") . IntMap.size $ (m ^. #atoms)
          -- Line 2 of the header contains 1 line of commenText. Remove all linebreaks by filtering.
          commentLine =
            bprint (stext F.% "\n") . Text.filter (not . isEndOfLine) $ m ^. #comment

      -- Every atom printed as a single line.
      atomLines <- fmap (fmap snd . IntMap.toAscList) . traverse atomLineWriter $ m ^. #atoms

      let xyzFileBuilder = foldl' (<>) "" $ nAtoms : commentLine : atomLines
          xyzFile = TextLazy.toStrict . TextBuilder.toLazyText $ xyzFileBuilder

      return xyzFile
      where
        atomLineWriter :: MonadThrow m => Atom -> m TextBuilder.Builder
        atomLineWriter a = do
          let elementOfAtom = a ^. #element
              isDummy' = a ^. #isDummy
              symbolToShow = if isDummy' then "Xx" else show elementOfAtom
          xCoord <- getVectorS (a ^. #coordinates) Massiv.!? 0
          yCoord <- getVectorS (a ^. #coordinates) Massiv.!? 1
          zCoord <- getVectorS (a ^. #coordinates) Massiv.!? 2

          let coordF = left 20 ' ' %. fixed 12
              atomLine =
                bprint
                  ((right 5 ' ' %. string) F.% coordF F.% coordF F.% coordF F.% "\n")
                  symbolToShow
                  xCoord
                  yCoord
                  zCoord

          return atomLine

----------------------------------------------------------------------------------------------------

-- |
-- Write a Tinker XYZ from a 'Molecule'. The writer trusts the '_atom_FFType' to be
-- correct (if set) and will simply write them ouText. Therefore it is possible, that wrong atom types can
-- be written. If they are not set, the writer will simply equalise all atom types to 0, which is OK
-- for visualisation but obviously not for MM.
--
-- This format ingores all deeper level layers of a molecule.
writeTXYZ :: MonadThrow m => Molecule -> m Text
writeTXYZ mol = do
  _ <- checkMolecule mol
  toTXYZ mol
  where
    -- Write the molecule as a TXYZ formatted file.
    toTXYZ :: MonadThrow m => Molecule -> m Text
    toTXYZ m = do
      let nAtoms = IntMap.size $ mol ^. #atoms
          comment' = mol ^. #comment
          headerLine = bprint ((left 6 ' ' %. int) F.% " " F.% stext F.% "\n") nAtoms comment'

      atomLines <-
        fmap (fmap snd . IntMap.toAscList)
          . IntMap.traverseWithKey (\key atom -> atomLineWriter key atom m)
          $ m
            ^. #atoms

      let txyzFileBuilder = foldl' (<>) "" $ headerLine : atomLines
          txyzFile = TextLazy.toStrict . TextBuilder.toLazyText $ txyzFileBuilder

      return txyzFile

    -- Writes the first part of an atom line in a TXYZ file (without bonds).
    atomLineWriter :: MonadThrow m => Int -> Atom -> Molecule -> m TextBuilder.Builder
    atomLineWriter k a m = do
      xCoord <- getVectorS (a ^. #coordinates) Massiv.!? 0
      yCoord <- getVectorS (a ^. #coordinates) Massiv.!? 1
      zCoord <- getVectorS (a ^. #coordinates) Massiv.!? 2
      ffNum <- case a ^. #ffType of
        FFTXYZ i -> return i
        _ ->
          throwM
            . MolLogicException "writeTXYZ"
            $ "Writing a Tinker XYZ file requires all atoms to have a force field type assigned, but atom "
              <> show k
              <> " has another type."

      let atomElement = a ^. #element
          -- Defining the formatter for the first part.
          nFmt = left 6 ' ' %. int
          elmFmt = right 2 ' ' %. string
          coordFmt = left 10 ' ' %. fixed 6
          -- Build the first part (index, element, coordinates, force field type).
          firstPart =
            bprint
              (nFmt F.% "  " F.% elmFmt F.% "  " F.% coordFmt F.% "  " F.% coordFmt F.% "  " F.% coordFmt F.% "  " F.% nFmt)
              k
              (show atomElement)
              xCoord
              yCoord
              zCoord
              ffNum
          -- Build the second part (bond targets).
          secondPart = atomSecondPart k m

      return $ firstPart <> secondPart <> "\n"
      where
        -- Writes the second part of an atom line in a TXYZ file (bond targets).
        atomSecondPart :: Int -> Molecule -> TextBuilder.Builder
        atomSecondPart k' m' =
          let targets =
                IntSet.fromList
                  . fmap snd
                  . HashMap.keys
                  . HashMap.filterWithKey (\(ixO, _) val -> ixO == k' && val)
                  $ m'
                    ^. #bonds
              targetFmt = left 6 ' ' %. int
           in IntSet.foldr' (\x xs -> bprint targetFmt x <> xs) "" targets

----------------------------------------------------------------------------------------------------

-- |
-- Write a simplified .mol2 file (Tripos SYBYL) from a 'Molecule', containing the atoms, connectivities
-- (single bonds only) and partial charges. This format writes atoms and bonds __only__ from the the
-- first sublayer of the 'Molecule', which includes the fragment definitions.
writeMOL2 :: MonadThrow m => Molecule -> m Text
writeMOL2 mol = do
  _ <- checkMolecule mol
  toMOL2 mol
  where
    -- Write a MOL2 formatted text from the current molecule.
    toMOL2 :: MonadThrow m => Molecule -> m Text
    toMOL2 m = do
      let moleculeBlock = toMOLECULE m
          bondBlock = toBOND m
      atomBlock <- toATOM m
      return . TextLazy.toStrict . TextBuilder.toLazyText $ moleculeBlock <> atomBlock <> bondBlock

    -- Write the "@<TRIPOS>MOLECULE" block.
    toMOLECULE :: Molecule -> TextBuilder.Builder
    toMOLECULE m =
      let nAtoms = IntMap.size $ m ^. #atoms
          -- Make the bonds unidirectorial first.
          bonds' = makeBondMatUnidirectorial $ m ^. #bonds
          -- Then get the overall number of bonds.
          nBonds = HashMap.size bonds'
          -- Comment line without any potential line breaks.
          comment' = TextBuilder.fromText . Text.filter (not . isEndOfLine) $ m ^. #comment
       in "@<TRIPOS>MOLECULE\n"
            <> (comment' <> "\n")
            <> bprint (int F.% " " F.% int F.% " 0 0 0\n") nAtoms nBonds
            <> "SMALL\n"
            <> "USER_CHARGES\n"
            <> "\n"

    -- Write the @<TRIPOS>ATOM block.
    toATOM :: MonadThrow m => Molecule -> m TextBuilder.Builder
    toATOM m = do
      -- Associations of the atom key to its fragment label and fragment number, if assigned to a
      -- fragment at all.
      keyToFragAssoc <- getAtomAssociationMap m

      -- Atom line text builders.
      atomLines <-
        fmap (fmap snd . IntMap.toAscList)
          . IntMap.traverseWithKey (\atomKey atomFragAssoc -> atomLineWriter atomKey atomFragAssoc)
          $ keyToFragAssoc

      return . foldl' (<>) "" $ "@<TRIPOS>ATOM\n" : atomLines
      where
        atomLineWriter ::
          MonadThrow m =>
          -- | Int key of the atom from the original IntMap.
          Int ->
          -- | Association of the atom with its fragment id and label,
          --   if the atom was assigned to a fragment.
          (Atom, Maybe (Int, Fragment)) ->
          m TextBuilder.Builder
        atomLineWriter atomKey (atom, fragNumAndLabel) = do
          let -- Define the formatters.
              keyFmt = left 6 ' ' %. int
              labelFmt = right 6 ' ' %. stext
              coordFmt = left 10 ' ' %. fixed 4
              ffFmt = right 6 ' ' %. stext
              fragnumFmt = left 6 ' ' %. int
              fragFmt = right 10 ' ' %. stext
              chrgFmt = coordFmt
              fullFormatter =
                (keyFmt F.% " ")
                  F.% (labelFmt F.% " ")
                  F.% (coordFmt F.% " ")
                  F.% (coordFmt F.% " ")
                  F.% (coordFmt F.% " ")
                  F.% (ffFmt F.% " ")
                  F.% (fragnumFmt F.% " ")
                  F.% (fragFmt F.% " ")
                  F.% chrgFmt
                  F.% "\n"

          -- Get the coordinates.
          xCoord <- getVectorS (atom ^. #coordinates) Massiv.!? 0
          yCoord <- getVectorS (atom ^. #coordinates) Massiv.!? 1
          zCoord <- getVectorS (atom ^. #coordinates) Massiv.!? 2

          -- Get the force field type for this atom.
          atomFFString <- case atom ^. #ffType of
            FFMol2 label' -> return label'
            _ ->
              throwM $
                MolLogicException
                  "writeMOL2"
                  "Writing an atom to a mol2 file requires that the atom has a force field type of FFMol2."

          -- Obtain other information for the atom, which are required for printing.
          let pCharge = fromMaybe 0 $ atom ^? #multipoles % #monopole % _Just % #q00
              atomLabel = case atom ^. #label of
                "" -> "X"
                l -> l
              fragmentNum = fromMaybe 0 . fmap fst $ fragNumAndLabel
              fragmentLabel = fromMaybe "UNK" . fmap ((^. #label) . snd) $ fragNumAndLabel

          let atomLine =
                bprint
                  fullFormatter
                  atomKey
                  atomLabel
                  xCoord
                  yCoord
                  zCoord
                  atomFFString
                  fragmentNum
                  fragmentLabel
                  pCharge
          return atomLine

    -- Write the "@<TRIPOS>BOND" block. All bonds are defined as single bonds for convenience.
    toBOND :: Molecule -> TextBuilder.Builder
    toBOND m =
      let -- Define the formatters.
          nFmt = left 6 ' ' %. int
          sndPartFmt = " " F.% nFmt F.% " " F.% nFmt F.% "     1\n"
          -- Print bond lines unidirectorial.
          uniBonds = makeBondMatUnidirectorial $ m ^. #bonds
          bondLinesSndPart =
            fmap snd
              . HashMap.toList
              . HashMap.mapWithKey (\(ixO, ixT) val -> if val then bprint sndPartFmt ixO ixT else "")
              $ uniBonds
          bondLines =
            List.map (\(n, sndPart) -> bprint (nFmt F.% builder) n sndPart)
              . List.zip [1 ..]
              $ bondLinesSndPart
       in foldl' (<>) "" $ "@<TRIPOS>BOND\n" : bondLines

----------------------------------------------------------------------------------------------------

-- |
-- Writes a 'Molecule' to a PDB file. The PDB format is simplified and recounts atom, discards
-- different chains, sets dummy values for the temperature and occupation factors and cannot write
-- charges, as PDB expects integers.
writePDB :: MonadThrow m => Molecule -> m Text
writePDB mol = do
  _ <- checkMolecule mol
  toPDB mol
  where
    toPDB :: MonadThrow m => Molecule -> m Text
    toPDB m = do
      let label' = Text.filter (not . isEndOfLine) $ m ^. #comment
          -- (Maybe it is necessary to strip whitespace at the end of the line.)
          header = bprint ("HEADER    " F.% (right 70 ' ' %. stext) F.% "\n") label'
          conects = toCONECT m
      hetatoms <- toHETATM m
      return . TextLazy.toStrict . TextBuilder.toLazyText $ header <> hetatoms <> conects
    -- Writes all atoms as HETATM records to the PDB.
    toHETATM :: MonadThrow m => Molecule -> m TextBuilder.Builder
    toHETATM m = do
      atomFragAssocs <- getAtomAssociationMap m
      atomLines <-
        fmap (fmap snd . IntMap.toAscList)
          . IntMap.traverseWithKey
            (\atomKey (atom, fragNumAndLabel) -> writeAtomLine atomKey atom fragNumAndLabel)
          $ atomFragAssocs
      return . foldl' (<>) "" $ atomLines
      where
        writeAtomLine :: MonadThrow m => Int -> Atom -> Maybe (Int, Fragment) -> m TextBuilder.Builder
        writeAtomLine atomID atom fragNumAndLabel = do
          -- Define the formatters.
          let hetatmFmt = "HETATM" -- 1-6 -> 6
              serialFmt = (left 5 ' ' %. int) F.% " " -- 7-11 (+12w) -> 5
              atomNameFmt = right 4 ' ' %. stext -- 13-16- -> 4 - Modify label depending in length before
              altLocFmt = " " -- 17 -> 1
              resNameFmt = (right 3 ' ' %. stext) F.% " " -- 18-20 (+19w) -> 3
              chainIDFmt = char -- 22 -> 1
              resSeqFmt = left 4 ' ' %. int -- 23-26 -> 4
              icodeFmt = " " F.% "   " -- 27 (+28-30w) -> 1
              coordFmt = left 8 ' ' %. fixed 3 -- 31-38 + 39-46 + 47-54 -> 3*8
              occFmt = left 6 ' ' %. fixed 2 -- 55-60 -> 6
              tempfacFmt = (left 6 ' ' %. fixed 2) F.% "          " -- 61-66 (+67-76w) -> 6
              elementFmt = left 2 ' ' %. stext -- 77-78 -> 2
              chgFmt = left 2 ' ' %. stext -- 79-80 -> 2

          -- Get the coordinates of the atom.
          xCoord <- getVectorS (atom ^. #coordinates) Massiv.!? 0
          yCoord <- getVectorS (atom ^. #coordinates) Massiv.!? 1
          zCoord <- getVectorS (atom ^. #coordinates) Massiv.!? 2

          -- Get other values for printing.
          let -- The label of the atom. Must not be longer than 4 and 4 indicates, that it contains an
              -- element symbol with 2 characters. Labels of 3 or shorter start 1 character later than
              -- labels with 4 characters.
              pdbSaneAtomName =
                let label4OrShorter = Text.take 4 $ atom ^. #label
                 in if Text.length label4OrShorter <= 3 then " " <> label4OrShorter else label4OrShorter
          -- Get the fragment ID and fragment data. They are necessary for writing a PDB and not having
          -- them can go very wrong.
          (fragID, fragmentInfo) <- case fragNumAndLabel of
            Nothing -> throwM . MolLogicException "writePDB" $ ("Writing a PDB requires all atoms to be associated with a fragment, but atom " <> show atomID <> " does not seem to be assigned with any fragment.")
            Just f -> return f

          let -- The residue name of the PDB can be not longer than 3 characters.
              pdbSaneResName = Text.take 3 . (^. #label) $ fragmentInfo
              -- The chain ID of this atom.
              pdbChainID = fromMaybe 'A' . chain $ fragmentInfo
              -- The residue/fragment ID, that atom belongs to. This number has been made unique by the
              -- PDB parser. Using the chainID of the fragment, convert this number back to the
              -- non-unique PDB representation (unique only within a chain).
              pdbSaneResSeq =
                let numA = fromEnum 'A'
                    numID = fromEnum pdbChainID
                    maxResiduesPerChain = 10000
                    offSet = (numID - numA) * maxResiduesPerChain
                 in fragID - offSet
              -- The element symbol limited to 2 characters and capitalised.
              pdbSaneElement = Text.toUpper . Text.take 2 . tShow $ atom ^. #element

          -- Format an atom line.
          let atomLine =
                bprint
                  ( hetatmFmt
                      F.% serialFmt
                      F.% atomNameFmt
                      F.% altLocFmt
                      F.% resNameFmt
                      F.% chainIDFmt
                      F.% resSeqFmt
                      F.% icodeFmt
                      F.% coordFmt
                      F.% coordFmt
                      F.% coordFmt
                      F.% occFmt
                      F.% tempfacFmt
                      F.% elementFmt
                      F.% chgFmt
                      F.% "\n"
                  )
                  atomID
                  pdbSaneAtomName
                  pdbSaneResName
                  pdbChainID
                  pdbSaneResSeq
                  xCoord
                  yCoord
                  zCoord
                  (1.0 :: Double) -- Occupancy -- irrelevant for Spicy, therefore simply 1
                  (0.0 :: Double) -- Temperature factor -- irrelevant for Spicy, therefore simply 0
                  pdbSaneElement
                  "" -- Charge as string. Idk. Normally not set.
          return atomLine

    -- Write all bonds to CONNECT records. They are bidirectorial, but the format is complicated, as a
    -- maximum of 4 bond targets are allowed per line for an origin.
    toCONECT :: Molecule -> TextBuilder.Builder
    toCONECT m =
      let bondMat = m ^. #bonds
          allBondOrigins = fmap fst . HashMap.keys $ bondMat
          -- Construct groupings from every possible origin to all targets.
          originTargetGroups =
            fmap
              ( \origin ->
                  let targets =
                        IntSet.fromList
                          . fmap snd
                          . HashMap.keys
                          . HashMap.filterWithKey (\(ixO, _) val -> ixO == origin && val)
                          $ bondMat
                   in (origin, targets)
              )
              allBondOrigins
          -- Translate all groupings into the corresponding CONECT lines.
          connctLines = foldl' (<>) "" . fmap (uncurry writeOriginLines) $ originTargetGroups
       in connctLines
      where
        -- Write all connect lines, that are necessary for a given origin (in most cases that will be
        -- just one).
        writeOriginLines :: Int -> IntSet -> TextBuilder.Builder
        writeOriginLines origin targets =
          let -- Create groups of 4 targets per origin.
              targetGroups = chunksOf 4 . IntSet.toList $ targets
              conectGroups = zip (List.repeat origin) targetGroups

              -- Create the formatters.
              conectFmt = "CONECT" -- 1-6 -> 6
              nFmt = left 5 ' ' %. int

              -- Construct all necessary connect lines.
              conectLines =
                fmap
                  ( \(origin', targetSet) ->
                      let firstPart = bprint (conectFmt F.% nFmt) origin'
                          secondPart =
                            foldl'
                              (\targetAcc currentTarget -> targetAcc <> bprint nFmt currentTarget)
                              ""
                              targetSet
                       in firstPart <> secondPart <> "\n"
                  )
                  conectGroups
           in foldl' (<>) "" conectLines

----------------------------------------------------------------------------------------------------

-- |
-- Write Spicy format, which is an AESON generated JSON document, directly representing the data type
-- of 'Molecule'.
writeSpicy :: Molecule -> Text
writeSpicy mol =
  Text.decodeUtf8With lenientDecode . ByteString.concat . ByteStringL.toChunks . encodePretty $ mol

{-
####################################################################################################
-}

-- $layoutWriter

-- | An ONIOM tree can be fully represented in a MOL2 file with proper labels on the atoms and
-- fragments.
writeONIOM :: (MonadThrow m) => Molecule -> m Text
writeONIOM mol = do
  --  Check sanity.
  _ <- checkMolecule mol

  let -- Association from atoms to the deepest layer it is found in.
      atomAssocs =
        molFoldlWithMolID
          ( \acc lid layer ->
              let afAssocs = atomFragAssoc layer
                  assignements = IntMap.map (\(frag, atom) -> (lid, frag, atom)) afAssocs
               in assignements <> acc
          )
          mempty
          mol

      -- The bond matrix of all layers joined
      fullBondMat = molFoldl (\acc m -> m ^. #bonds <> acc) mempty mol

  -- Point charges as per layer. Converts all atoms to dummies first, so that all get a point charge.
  layerChargeModel <-
    molFoldlWithMolID
      ( \acc' lid layer -> do
          acc <- acc'
          chargeMat <- molToPointCharges $ layer & #atoms % each % #isDummy .~ True
          return $ Map.insert lid chargeMat acc
      )
      (pure mempty)
      mol

  (nAtoms, atomBlock) <- runWriterT $ do
    -- Real atoms
    tell "@<TRIPOS>ATOM\n"
    atomLineBuilder <-
      IntMap.foldlWithKey'
        ( \acc' ak (mid, fragId, a) -> do
            acc <- acc'
            xCoord <- getVectorS (a ^. #coordinates) Massiv.!? 0
            yCoord <- getVectorS (a ^. #coordinates) Massiv.!? 1
            zCoord <- getVectorS (a ^. #coordinates) Massiv.!? 2

            let aType = case a ^. #ffType of
                  FFMol2 l -> l
                  _ -> tshow $ a ^. #element
                aLabel
                  | isAtomLink $ a ^. #isLink = "LA"
                  | a ^. #label == mempty = "X"
                  | otherwise = a ^. #label
                aLine =
                  bprint
                    fullFormatter
                    ak
                    aLabel
                    xCoord
                    yCoord
                    zCoord
                    aType
                    fragId
                    ("L" <> molID2OniomHumandID mid)
                    0

            return $ acc <> aLine
        )
        (pure mempty)
        atomAssocs
    tell atomLineBuilder

    -- Point charges in the atoms
    maxAtomKey <- getMaxAtomIndex mol
    (nAtomLines, pointChargeBuilder) <-
      Map.foldlWithKey'
        ( \acc' lid pcMat -> do
            (maxKey, lineAcc) <- acc'
            let Sz (_ :. nPts) = Massiv.size pcMat
                aLines = pcLines maxKey lid pcMat
            return (maxKey + nPts, lineAcc <> aLines)
        )
        (pure (maxAtomKey, mempty))
        layerChargeModel
    tell pointChargeBuilder
    return nAtomLines

  (nBonds, bondBlock) <- runWriterT $ do
    -- Bonds
    tell "@<TRIPOS>BOND\n"
    let (nb, bl) = bondLines fullBondMat
    tell bl
    return nb

  (_, header) <- runWriterT $ do
    -- Header block
    tell "@<TRIPOS>MOLECULE\n"
    tell "ONIOM\n"
    tell $ bprint (int F.% " " F.% int F.% " 0 0 0\n") nAtoms nBonds
    tell "SMALL\n"
    tell "USER_CHARGES\n\n"

  return . sformat builder $ header <> atomBlock <> bondBlock
  where
    keyFmt = left 6 ' ' %. int
    labelFmt = right 6 ' ' %. stext
    coordFmt = left 10 ' ' %. fixed 4
    ffFmt = right 6 ' ' %. stext
    fragnumFmt = left 6 ' ' %. int
    fragFmt = right 10 ' ' %. stext
    chrgFmt = coordFmt
    fullFormatter =
      (keyFmt F.% " ")
        F.% (labelFmt F.% " ")
        F.% (coordFmt F.% " ")
        F.% (coordFmt F.% " ")
        F.% (coordFmt F.% " ")
        F.% (ffFmt F.% " ")
        F.% (fragnumFmt F.% " ")
        F.% (fragFmt F.% " ")
        F.% chrgFmt
        F.% "\n"

    pcLines :: Int -> MolID -> Matrix S Double -> TextBuilder.Builder
    pcLines maxKey mid pcMat =
      let slices = innerSlices pcMat
          aLines =
            ifoldMono
              ( \i pc ->
                  let x = pc Massiv.! 0
                      y = pc Massiv.! 1
                      z = pc Massiv.! 2
                      c = pc Massiv.! 3
                   in bprint
                        fullFormatter
                        (maxKey + 1 + i)
                        "Bq"
                        x
                        y
                        z
                        "H"
                        0
                        ("L" <> molID2OniomHumandID mid <> "_C")
                        c
              )
              slices
       in aLines

    bondLines :: BondMatrix -> (Int, TextBuilder.Builder)
    bondLines bm =
      let uniBonds = makeBondMatUnidirectorial bm
          nFmt = left 6 ' ' %. int
          sndPartFmt = " " F.% nFmt F.% " " F.% nFmt F.% "     1\n"
          bondLinesSndPart =
            fmap snd
              . HashMap.toList
              . HashMap.mapWithKey (\(ixO, ixT) val -> if val then bprint sndPartFmt ixO ixT else "")
              $ uniBonds
          bLines =
            List.map (\(n, sndPart) -> bprint (nFmt F.% builder) n sndPart)
              . List.zip [1 ..]
              $ bondLinesSndPart
          nBonds = HashMap.size uniBonds
       in (nBonds, foldl' (<>) mempty bLines)

    -- Find the first matching set in an IntMap, that contains the search key.
    firstMatch :: Int -> IntMap Fragment -> Maybe Int
    firstMatch s m =
      IntMap.foldlWithKey'
        ( \acc fragId frag -> case acc of
            Just x -> Just x
            Nothing ->
              if s `IntSet.member` (frag ^. #atoms)
                then Just fragId
                else Nothing
        )
        Nothing
        m

    -- Associates every atom with its fragment in this layer.
    atomFragAssoc :: Molecule -> IntMap (Int, Atom)
    atomFragAssoc m =
      let frags = m ^. #fragment
          atoms = IntMap.filter (\a -> not $ a ^. #isDummy) $ m ^. #atoms
       in IntMap.mapWithKey (\ak a -> (fromMaybe 0 $ firstMatch ak frags, a)) atoms

    --
    molID2OniomHumandID :: MolID -> Text
    molID2OniomHumandID Seq.Empty = "0"
    molID2OniomHumandID molID =
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
