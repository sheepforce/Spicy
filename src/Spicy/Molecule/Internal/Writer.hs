{-|
Module      : Spicy.Molecule.Internal.Writer
Description : Writing chemical data formats
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

A module which converts the internal Molecule representation to a text, which is a common chemical
file format, that can be read by Avogadro, VMD, OpenBabel etc.. The writers are not fool proof with
respect to force field types, which should always be remembered when usings its results.
-}
module Spicy.Molecule.Internal.Writer
  ( writeXYZ
  , writeTXYZ
  , writeMOL2
  , writePDB
  , writeSpicy
  , writeLayout
  )
where
import           Control.Lens
import           Data.Aeson.Encode.Pretty
import           Data.Attoparsec.Text           ( isEndOfLine )
import qualified Data.ByteString.Lazy          as ByteStringL
import           Data.Foldable
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Data.List.Split                ( chunksOf )
import           Data.Massiv.Array             as Massiv
                                         hiding ( all
                                                , map
                                                , mapM_
                                                , toList
                                                , zip
                                                )
import           Data.Maybe
import           RIO                     hiding ( view
                                                , (^.)
                                                )
import qualified RIO.ByteString                as ByteString
import qualified RIO.HashMap                   as HashMap
import qualified RIO.List                      as List
import qualified RIO.Seq                       as Seq
import qualified RIO.Text                      as Text
import           Spicy.Class
import           Spicy.Generic
import           Spicy.Molecule.Internal.Util
import           System.Path                   as Path
import qualified System.Path.Directory         as Dir
import           Text.Printf

{-|
Write a Molden XYZ file from a molecule. This format ignores all deep level layers of a molecule.
-}
writeXYZ :: MonadThrow m => Molecule -> m Text
writeXYZ mol = toXYZ <$> checkMolecule mol
 where
  -- Assemble a XYZ file from a moleculeimport           Control.Monad
  toXYZ :: Molecule -> Text
  toXYZ m =
    let -- Line 1 of the header contains the number of atoms
        headL1 = Text.pack . show . IntMap.size $ m ^. molecule_Atoms
        -- Line 2 of the header contains 1 line of commenText. Remove all linebreaks by filtering.
        headL2 = Text.filter (not . isEndOfLine) $ m ^. molecule_Comment
        atomLs = IntMap.foldr' (\atom acc -> atomLineWriter atom <> acc) "" $ m ^. molecule_Atoms
    in  Text.unlines [headL1, headL2] <> atomLs
   where
    atomLineWriter :: Atom -> Text
    atomLineWriter a = Text.pack $ printf
      "%-4s    %12.8F    %12.8F    %12.8F\n"
      (show $ a ^. atom_Element)
      (fromMaybe 0.0 $ getVectorS (a ^. atom_Coordinates) Massiv.!? 0)
      (fromMaybe 0.0 $ getVectorS (a ^. atom_Coordinates) Massiv.!? 1)
      (fromMaybe 0.0 $ getVectorS (a ^. atom_Coordinates) Massiv.!? 2)

----------------------------------------------------------------------------------------------------
{-|
Write a Tinker XYZ from a 'Molecule'. The writer trusts the '_atom_FFType' to be
correct (if set) and will simply write them ouText. Therefore it is possible, that wrong atom types can
be written. If they are not set, the writer will simply equalise all atom types to 0, which is OK
for visualisation but obviously not for MM.

This format ingores all deeper level layers of a molecule.
-}
writeTXYZ :: MonadThrow m => Molecule -> m Text
writeTXYZ mol
  | ffTypeCheck
  = toTXYZ <$> (checkMolecule =<< reIndex2BaseMolecule (mol & molecule_SubMol .~ IntMap.empty))
  | otherwise
  = throwM
    $ MolLogicException "writeMOL2" "writeTXYZ: Not all atoms have Tinker XYZ style atom types."
 where
  -- Check if all atoms have TXYZ atom types.
  ffTypeCheck = all (== FFTXYZ 0) . IntMap.map (^. atom_FFType) $ mol ^. molecule_Atoms
  --
  -- Write the molecule as a TXYZ formatted file.
  toTXYZ :: Molecule -> Text
  toTXYZ m =
    let -- The header line contains the number of atoms and separated by a space a commenText.
        headL1 =
            (Text.pack . show . IntMap.size $ mol ^. molecule_Atoms)
              <> "  "
              <> Text.filter (not . isEndOfLine) (mol ^. molecule_Comment)
              <> "\n"
        -- The atom lines contain:
        --   - serial number of the atom
        --   - element symbol of the atom
        --   - XYZ coordinates in angstrom
        --   - The integer number of the atom type
        --   - The indices of all atoms, where to bind to
        atomLs = IntMap.foldrWithKey' (\key atom acc -> atomLineWriter key atom m <> acc)
                                      ""
                                      (m ^. molecule_Atoms)
    in  headL1 <> atomLs
  --
  -- Writes the first part of an atom line in a TXYZ file (without bonds).
  atomFirstPart :: Int -> Atom -> Text
  atomFirstPart k a =
    let ffNum = case a ^. atom_FFType of
          FFTXYZ i -> i
          _        -> 0
    in  Text.pack $ printf "%6d    %2s    %12.8F    %12.8F    %12.8F    %6d"
                           (k + 1)
                           (show $ a ^. atom_Element)
                           (fromMaybe 0.0 $ getVectorS (a ^. atom_Coordinates) Massiv.!? 0)
                           (fromMaybe 0.0 $ getVectorS (a ^. atom_Coordinates) Massiv.!? 1)
                           (fromMaybe 0.0 $ getVectorS (a ^. atom_Coordinates) Massiv.!? 2)
                           ffNum
  --
  -- Writes the second part of an atom line in a TXYZ file (bond targets).
  atomSecondPart :: Int -> Molecule -> Text
  atomSecondPart k m =
    let targets =
            IntSet.fromList
              .  fmap snd
              .  HashMap.keys
              .  HashMap.filterWithKey (\(ixO, _) val -> ixO == k && val)
              $  m
              ^. molecule_Bonds
    in  Text.stripEnd
            (IntSet.foldr' (\x xs -> Text.pack (printf "    %6d" (x + 1)) <> xs) "" targets)
          <> "\n"
  --
  -- Writes an complete atom line.
  atomLineWriter :: Int -> Atom -> Molecule -> Text
  atomLineWriter k a m = atomFirstPart k a <> atomSecondPart k m

----------------------------------------------------------------------------------------------------
{-|
Write a simplified .mol2 file (Tripos SYBYL) from a 'Molecule', containing the atoms, connectivities
(single bonds only) and partial charges. This format writes atoms and bonds __only__ from the the
first sublayer of the 'Molecule', which includes the fragment definitions.
-}
-- TODO (phillip|p=75|#Broken) - writeMol2 is broken now, as fragments live in _molecule_Fragment now. Fix while porting to text formatters.
writeMOL2 :: MonadThrow m => Molecule -> m Text
writeMOL2 mol
  | ffTypeCheck = toMOL2 <$> (checkMolecule =<< reIndex2BaseMolecule mol)
  | otherwise   = throwM $ MolLogicException "writeMOL2" "Not all atoms have MOL2 style atom types."
 where
  ffTypeCheck = all (== FFMol2 "") . IntMap.map (^. atom_FFType) $ mol ^. molecule_Atoms
  --
  -- Write a MOL2 formatted text from the current molecule.
  toMOL2 :: Molecule -> Text
  toMOL2 m =
    let -- First sublayer of the molecule.
        subMols = m ^. molecule_SubMol
    in  toMOLECULE m <> toATOM m subMols <> toBOND m
  --
  -- Write the "@<TRIPOS>MOLECULE" block.
  toMOLECULE :: Molecule -> Text
  toMOLECULE m =
    let nAtoms = IntMap.size $ m ^. molecule_Atoms
        -- Make the bonds unidirectorial firsText.
        bonds  = makeBondMatUnidirectorial $ m ^. molecule_Bonds
        -- Then get the overall number of bonds.
        nBonds = HashMap.size bonds
    in  Text.unlines
          [ "@<TRIPOS>MOLECULE"
          , Text.filter (not . isEndOfLine) $ m ^. molecule_Comment
          , (Text.pack . show $ nAtoms) <> " " <> (Text.pack . show $ nBonds) <> " 0 0 0"
          , "SMALL"
          , "USER_CHARGES"
          , ""
          ]
  --
  -- Write the @<TRIPOS>ATOM block.
  toATOM :: Molecule -> IntMap Molecule -> Text
  toATOM m sM =
    let
      annoFrags = sM
      atomLines = IntMap.foldrWithKey'
        (\key atom acc ->
          let -- The index of the submol/fragment, in which the current atom can be found
            fragmentNum   = findAtomInSubMols key annoFrags
            fragment      = fragmentNum >>= (`IntMap.lookup` annoFrags)
            fragmentLabel = _molecule_Comment <$> fragment
            thisAtomLine  = Text.pack $ printf
              "%7d %-6s %12.8F %12.8F %12.8F %-8s %4d  %10s %8.4F\n"
              (key + 1)                                                                -- Index
              (Text.unpack $ atom ^. atom_Label)                                       -- Atom label
              (fromMaybe 0.0 $ getVectorS (atom ^. atom_Coordinates) Massiv.!? 0) -- X
              (fromMaybe 0.0 $ getVectorS (atom ^. atom_Coordinates) Massiv.!? 1) -- Y
              (fromMaybe 0.0 $ getVectorS (atom ^. atom_Coordinates) Massiv.!? 2) -- Z
              (case atom ^. atom_FFType of                                             -- Atom label
                FFMol2 t -> Text.unpack t
                _        -> show $ atom ^. atom_Element
              )
              (fromMaybe 0 fragmentNum)                          -- Fragment number
              (fromMaybe "UNL1" $ Text.unpack <$> fragmentLabel) -- Fragment name
              ((Data.Maybe.fromMaybe 0 $ atom ^. atom_Multipoles . multipole_Monopole) :: Double)
          in
            thisAtomLine <> acc
        )
        ""
        (m ^. molecule_Atoms)
    in
      "@<TRIPOS>ATOM\n" <> atomLines
  --
  -- Write the "@<TRIPOS>BOND" block. All bonds are defined as single bonds for convenience.
  toBOND :: Molecule -> Text
  toBOND m =
    let -- Make the bonds unidirectorial.
        bonds     = makeBondMatUnidirectorial $ m ^. molecule_Bonds
        -- Write a single line for each bond.
        -- Fold the keys (origins) in the IntMap to bond lines. Each origin key might give
        -- multiple lines (one per target), and the targets are folded in an inner fold.
        bondLines = snd $ IntMap.foldrWithKey'
          (\origin targets (nthBond, prevBLines) ->

            let -- For each target, write a new line. This is the inner loop, processing all
                -- targets for the current origin in the outer fold.
                targetLines = snd $ IntSet.foldr'
                  (\target (nthTarget, prevTLines) ->
                    let targetLine = Text.pack $ printf "%6d %6d %6d %4s\n"
                                                        (nthTarget + nthBond)
                                                        (origin + 1)
                                                        (target + 1)
                                                        ("1" :: String)
                    in  (nthTarget + 1, prevTLines <> targetLine)
                  )
                  (0, "")
                  targets
            in  (nthBond + IntSet.size targets, prevBLines <> targetLines)
          )
          (1, "")
          (bondMat2ImIs bonds)
    in  "@<TRIPOS>BOND\n" <> bondLines

----------------------------------------------------------------------------------------------------
{-|
Writes a 'Molecule' to a PDB file. The PDB format is simplified and recounts atom, discards
different chains, sets dummy values for the temperature and occupation factors and cannot write
charges (as PDB) expects integers.
-}
-- TODO (phillip|p=75|#Broken) - writePDB is broken now, as fragments live in _molecule_Fragment now. Fix while porting to text formatters.
writePDB :: MonadThrow m => Molecule -> m Text
writePDB mol
  | ffTypeCheck = toPDB <$> (checkMolecule =<< reIndex2BaseMolecule mol)
  | otherwise   = throwM $ MolLogicException "writePDB" "Not all atoms have PDB style atom types."
 where
  ffTypeCheck = all (== FFPDB "") . IntMap.map (^. atom_FFType) $ mol ^. molecule_Atoms
  toPDB :: Molecule -> Text
  toPDB m =
    let label = Text.filter (not . isEndOfLine) $ m ^. molecule_Comment
        -- Strange construct to avoid conflict with the parser with trailing white space in the
        -- "HEADER" records.
        header =
            (Text.stripEnd . Text.pack $ printf "%-6s    %-70s" ("HEADER" :: Text) label) <> "\n"
    in  header <> toHETATM m <> toCONECT m
  toHETATM :: Molecule -> Text
  toHETATM m =
    let
      subMols   = m ^. molecule_SubMol
      annoFrags = subMols
      atomLines = IntMap.foldrWithKey'
        (\key atom acc ->
          let
            fragmentNum   = findAtomInSubMols key annoFrags
            fragment      = fragmentNum >>= (`IntMap.lookup` annoFrags)
            fragmentLabel = _molecule_Comment <$> fragment
            thisAtomLine =
              Text.concat
                . fmap Text.pack
                $ [ printf "%-6s" ("HETATM" :: Text)                                              -- 1-6: Record type
                  , printf "%5d " (key + 1)                                                       -- 7-11: Atom serial number
                  , printf
                    "%-4s "
                    (if Text.length (atom ^. atom_Label) <= 3
                      then " " <> Text.unpack (atom ^. atom_Label)
                      else Text.unpack $ atom ^. atom_Label
                    )
                  , printf "%3s " (fromMaybe "UNL" $ Text.unpack . Text.take 3 <$> fragmentLabel) -- 18-20: Residue name
                  , printf "%1s"     ("A" :: Text)                                        -- 22: Chain identifier
                  , printf "%4d    " (fromMaybe 0 fragmentNum)                            -- 23-26: Residue sequence number
                  , printf "%8.3F"
                           (fromMaybe 0.0 $ getVectorS (atom ^. atom_Coordinates) Massiv.!? 0)          -- 31-38: X
                  , printf "%8.3F"
                           (fromMaybe 0.0 $ getVectorS (atom ^. atom_Coordinates) Massiv.!? 1)          -- 39-46: Y
                  , printf "%8.3F"
                           (fromMaybe 0.0 $ getVectorS (atom ^. atom_Coordinates) Massiv.!? 2)          -- 47-54: Z
                  , printf "%6.2F"           (1.0 :: Double)                                      -- 55-60: Occupancy
                  , printf "%6.2F          " (0.0 :: Double)                                      -- 61-66: Temperature factor
                  , printf "%2s" (Text.toUpper . Text.pack . show $ atom ^. atom_Element)         -- 77-78: Element symbol
                  , printf "%2s\n"           ("" :: Text)                                         -- 79-80: Charge of the atom.
                  ]
          in
            thisAtomLine <> acc
        )
        ""
        (m ^. molecule_Atoms)
    in
      atomLines
  toCONECT :: Molecule -> Text
  toCONECT m =
    IntMap.foldrWithKey'
        (\o ts acc -> let originGroupLines = writeOriginLines o ts in originGroupLines <> acc)
        ""
      .  bondMat2ImIs
      $  m
      ^. molecule_Bonds
   where
    writeOriginLines :: Int -> IntSet -> Text
    writeOriginLines origin targets =
      let targetGroups = chunksOf 4 . IntSet.toList $ targets
          conectGroups = zip (List.repeat origin) targetGroups
      in  Text.concat
            . fmap
                (\(o, ts) ->
                  "CONECT"
                    <> Text.pack (printf "%5d" (o + 1))
                    <> (Text.concat . fmap (Text.pack . printf "%5d" . (+ 1)) $ ts)
                    <> "\n"
                )
            $ conectGroups

----------------------------------------------------------------------------------------------------
{-|
Write Spicy format, which is an AESON generated JSON document, directly representing the data type
of 'Molecule'.
-}
writeSpicy :: Molecule -> Text
writeSpicy mol =
  Text.decodeUtf8With lenientDecode . ByteString.concat . ByteStringL.toChunks . encodePretty $ mol

{-
####################################################################################################
-}
{- $layoutWriter
-}
{-|
This function will write a complete molecule layout to the disk in a calculation context. This is
extremely useful for debugging.
-}
writeLayout
  :: (HasInputFile env, HasMolecule env, HasLogFunc env)
  => (Molecule -> Either SomeException Text)
  -> RIO env ()
writeLayout writer = do
  let molIDStart = Seq.empty
  goLayers molIDStart
 where
  -- Removes all submolecules in preparation for a writer.
  removeSubMols :: Molecule -> Molecule
  removeSubMols mol = mol & molecule_SubMol .~ IntMap.empty

  -- Recursively write all layers of a molecule.
  goLayers :: (HasInputFile env, HasMolecule env, HasLogFunc env) => MolID -> RIO env ()
  goLayers molID = do
    topMol <- view moleculeL
    writeMolFromID molID
    thisMol <- getMolByID topMol molID
    let nextMolIDs = fmap (molID |>) . Seq.fromList . IntMap.keys $ thisMol ^. molecule_SubMol
    mapM_ goLayers nextMolIDs

  -- Write a single layer specified by its MolID to a file in a unique directory.
  writeMolFromID :: (HasInputFile env, HasMolecule env, HasLogFunc env) => MolID -> RIO env ()
  writeMolFromID molID = do
    molTop    <- view moleculeL
    inputFile <- view inputFileL
    let
      -- Create a directory pattern for molecules given by the hierarchy.
      permaDir            = getDirPath $ inputFile ^. permanent
      layoutDir           = permaDir </> Path.dirPath "MoleculeLayout"
      (layoutLayerDir, _) = foldl'
        (\(dirAcc, recursionDepth) layerKey ->
          let molDir =
                  dirAcc </> Path.relDir ("Layer" <> show recursionDepth <> "-Mol" <> show layerKey)
              nextDepthCounter = recursionDepth + 1
          in  (molDir, nextDepthCounter)
        )
        (layoutDir, 0 :: Int)
        molID
      molLayerOfInterest = getMolByID molTop molID
      molWriterText      = writer =<< removeSubMols <$> molLayerOfInterest
      molFilePath        = layoutLayerDir </> Path.relFile "Molecule.dat"

    -- Try writing the current layer to a file.
    case molWriterText of
      Left exception ->
        logWarnS "writeLayout"
          $  "Writing molecule with MolID "
          <> (text2Utf8Builder . tShow . toList $ molID)
          <> " failed with: "
          <> (text2Utf8Builder . tShow $ exception)
      Right molText -> do
        liftIO $ Dir.createDirectoryIfMissing True layoutLayerDir
        hasMolFileAlready <- liftIO $ Dir.doesFileExist molFilePath
        if hasMolFileAlready
          then do
            logWarnS "writeLayout"
              $  "Molecule file for MolID "
              <> (text2Utf8Builder . tShow . toList $ molID)
              <> " does already exist at "
              <> path2Utf8Builder molFilePath
              <> ". Overwriting it."
            writeFileUTF8 molFilePath molText
          else writeFileUTF8 molFilePath molText
