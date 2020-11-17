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
import qualified Data.Text.Lazy.Builder        as TextBuilder
import           Formatting
import           RIO                     hiding ( view
                                                , (^.)
                                                , (.~)
                                                )
import qualified RIO.ByteString                as ByteString
import qualified RIO.HashMap                   as HashMap
import qualified RIO.List                      as List
import qualified RIO.Seq                       as Seq
import qualified RIO.Text                      as Text
import qualified RIO.Text.Lazy                 as TextLazy
import           Spicy.Class
import           Spicy.Generic
import           Spicy.Molecule.Internal.Util
import           System.Path                   as Path
import qualified System.Path.Directory         as Dir
import           Spicy.Logger

{-|
Write a Molden XYZ file from a molecule. This format ignores all deep level layers of a molecule.
-}
writeXYZ :: MonadThrow m => Molecule -> m Text
writeXYZ mol = do
  _ <- checkMolecule mol
  toXYZ mol
 where
  -- Assemble a XYZ file from a molecule.
  toXYZ :: MonadThrow m => Molecule -> m Text
  toXYZ m = do
    let -- Line 1 of the header contains the number of atoms
        nAtoms = bprint (int % "\n") . IntMap.size $ (m ^. molecule_Atoms)
        -- Line 2 of the header contains 1 line of commenText. Remove all linebreaks by filtering.
        commentLine =
          bprint (stext % "\n") . Text.filter (not . isEndOfLine) $ m ^. molecule_Comment

    -- Every atom printed as a single line.
    atomLines <- fmap (fmap snd . IntMap.toAscList) . traverse atomLineWriter $ m ^. molecule_Atoms

    let xyzFileBuilder = foldl' (<>) "" $ nAtoms : commentLine : atomLines
        xyzFile        = TextLazy.toStrict . TextBuilder.toLazyText $ xyzFileBuilder

    return xyzFile
   where
    atomLineWriter :: MonadThrow m => Atom -> m TextBuilder.Builder
    atomLineWriter a = do
      let elementOfAtom = a ^. atom_Element
          isDummy       = a ^. atom_IsDummy
          symbolToShow  = if isDummy then "Xx" else show elementOfAtom
      xCoord <- getVectorS (a ^. atom_Coordinates) Massiv.!? 0
      yCoord <- getVectorS (a ^. atom_Coordinates) Massiv.!? 1
      zCoord <- getVectorS (a ^. atom_Coordinates) Massiv.!? 2

      let coordF   = left 20 ' ' %. fixed 12
          atomLine = bprint ((right 5 ' ' %. string) % coordF % coordF % coordF % "\n")
                            symbolToShow
                            xCoord
                            yCoord
                            zCoord

      return atomLine

----------------------------------------------------------------------------------------------------
{-|
Write a Tinker XYZ from a 'Molecule'. The writer trusts the '_atom_FFType' to be
correct (if set) and will simply write them ouText. Therefore it is possible, that wrong atom types can
be written. If they are not set, the writer will simply equalise all atom types to 0, which is OK
for visualisation but obviously not for MM.

This format ingores all deeper level layers of a molecule.
-}
writeTXYZ :: MonadThrow m => Molecule -> m Text
writeTXYZ mol = do
  _ <- checkMolecule mol
  toTXYZ mol
 where
  -- Write the molecule as a TXYZ formatted file.
  toTXYZ :: MonadThrow m => Molecule -> m Text
  toTXYZ m = do
    let nAtoms     = IntMap.size $ mol ^. molecule_Atoms
        comment    = mol ^. molecule_Comment
        headerLine = bprint ((left 6 ' ' %. int) % " " % stext % "\n") nAtoms comment

    atomLines <-
      fmap (fmap snd . IntMap.toAscList)
      .  IntMap.traverseWithKey (\key atom -> atomLineWriter key atom m)
      $  m
      ^. molecule_Atoms

    let txyzFileBuilder = foldl' (<>) "" $ headerLine : atomLines
        txyzFile        = TextLazy.toStrict . TextBuilder.toLazyText $ txyzFileBuilder

    return txyzFile

  -- Writes the first part of an atom line in a TXYZ file (without bonds).
  atomLineWriter :: MonadThrow m => Int -> Atom -> Molecule -> m TextBuilder.Builder
  atomLineWriter k a m = do
    xCoord <- getVectorS (a ^. atom_Coordinates) Massiv.!? 0
    yCoord <- getVectorS (a ^. atom_Coordinates) Massiv.!? 1
    zCoord <- getVectorS (a ^. atom_Coordinates) Massiv.!? 2
    ffNum  <- case a ^. atom_FFType of
      FFTXYZ i -> return i
      _ ->
        throwM
          .  MolLogicException "writeTXYZ"
          $ "Writing a Tinker XYZ file requires all atoms to have a force field type assigned, but atom "
          <> show k
          <> " has another type."

    let atomElement = a ^. atom_Element
        -- Defining the formatter for the first part.
        nFmt        = left 6 ' ' %. int
        elmFmt      = right 2 ' ' %. string
        coordFmt    = left 10 ' ' %. fixed 6
        -- Build the first part (index, element, coordinates, force field type).
        firstPart   = bprint
          (nFmt % "  " % elmFmt % "  " % coordFmt % "  " % coordFmt % "  " % coordFmt % "  " % nFmt)
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
                .  fmap snd
                .  HashMap.keys
                .  HashMap.filterWithKey (\(ixO, _) val -> ixO == k' && val)
                $  m'
                ^. molecule_Bonds
          targetFmt = left 6 ' ' %. int
      in  IntSet.foldr' (\x xs -> bprint targetFmt x <> xs) "" targets

----------------------------------------------------------------------------------------------------
{-|
Write a simplified .mol2 file (Tripos SYBYL) from a 'Molecule', containing the atoms, connectivities
(single bonds only) and partial charges. This format writes atoms and bonds __only__ from the the
first sublayer of the 'Molecule', which includes the fragment definitions.
-}
writeMOL2 :: MonadThrow m => Molecule -> m Text
writeMOL2 mol = do
  _ <- checkMolecule mol
  toMOL2 mol
 where
  -- Write a MOL2 formatted text from the current molecule.
  toMOL2 :: MonadThrow m => Molecule -> m Text
  toMOL2 m = do
    let moleculeBlock = toMOLECULE m
        bondBlock     = toBOND m
    atomBlock <- toATOM m
    return . TextLazy.toStrict . TextBuilder.toLazyText $ moleculeBlock <> atomBlock <> bondBlock

  -- Write the "@<TRIPOS>MOLECULE" block.
  toMOLECULE :: Molecule -> TextBuilder.Builder
  toMOLECULE m =
    let nAtoms  = IntMap.size $ m ^. molecule_Atoms
        -- Make the bonds unidirectorial first.
        bonds   = makeBondMatUnidirectorial $ m ^. molecule_Bonds
        -- Then get the overall number of bonds.
        nBonds  = HashMap.size bonds
        -- Comment line without any potential line breaks.
        comment = TextBuilder.fromText . Text.filter (not . isEndOfLine) $ m ^. molecule_Comment
    in  "@<TRIPOS>MOLECULE\n"
          <> (comment <> "\n")
          <> bprint (int % " " % int % " 0 0 0\n") nAtoms nBonds
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
    atomLines      <-
      fmap (fmap snd . IntMap.toAscList)
      . IntMap.traverseWithKey (\atomKey atomFragAssoc -> atomLineWriter atomKey atomFragAssoc)
      $ keyToFragAssoc

    return . foldl' (<>) "" $ "@<TRIPOS>ATOM\n" : atomLines
   where
    atomLineWriter
      :: MonadThrow m
      => Int                           -- ^ Int key of the atom from the original IntMap.
      -> (Atom, Maybe (Int, Fragment)) -- ^ Association of the atom with its fragment id and label,
                                       --   if the atom was assigned to a fragment.
      -> m TextBuilder.Builder
    atomLineWriter atomKey (atom, fragNumAndLabel) = do
      let -- Define the formatters.
          keyFmt     = left 6 ' ' %. int
          labelFmt   = right 6 ' ' %. stext
          coordFmt   = left 10 ' ' %. fixed 4
          ffFmt      = right 6 ' ' %. stext
          fragnumFmt = left 6 ' ' %. int
          fragFmt    = right 10 ' ' %. stext
          chrgFmt    = coordFmt
          fullFormatter =
            (keyFmt % " ")
              % (labelFmt % " ")
              % (coordFmt % " ")
              % (coordFmt % " ")
              % (coordFmt % " ")
              % (ffFmt % " ")
              % (fragnumFmt % " ")
              % (fragFmt % " ")
              % chrgFmt
              % "\n"

      -- Get the coordinates.
      xCoord       <- getVectorS (atom ^. atom_Coordinates) Massiv.!? 0
      yCoord       <- getVectorS (atom ^. atom_Coordinates) Massiv.!? 1
      zCoord       <- getVectorS (atom ^. atom_Coordinates) Massiv.!? 2

      -- Get the force field type for this atom.
      atomFFString <- case atom ^. atom_FFType of
        FFMol2 label -> return label
        _            -> throwM $ MolLogicException
          "writeMOL2"
          "Writing an atom to a mol2 file requires that the atom has a force field type of FFMol2."

      -- Obtain other information for the atom, which are required for printing.
      let pCharge   = fromMaybe 0 $ atom ^. atom_Multipoles . multipole_Monopole
          atomLabel = case atom ^. atom_Label of
            "" -> "X"
            l  -> l
          fragmentNum   = fromMaybe 0 . fmap fst $ fragNumAndLabel
          fragmentLabel = fromMaybe "UNK" . fmap (_fragment_Label . snd) $ fragNumAndLabel

      let atomLine = bprint fullFormatter
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
    let
      -- Define the formatters.
      nFmt       = left 6 ' ' %. int
      sndPartFmt = " " % nFmt % " " % nFmt % "     1\n"
      -- Print bond lines unidirectorial.
      uniBonds   = makeBondMatUnidirectorial $ m ^. molecule_Bonds
      bondLinesSndPart =
        fmap snd
          . HashMap.toList
          . HashMap.mapWithKey (\(ixO, ixT) val -> if val then bprint sndPartFmt ixO ixT else "")
          $ uniBonds
      bondLines =
        List.map (\(n, sndPart) -> bprint (nFmt % builder) n sndPart)
          . List.zip [1 ..]
          $ bondLinesSndPart
    in
      foldl' (<>) "" $ "@<TRIPOS>BOND\n" : bondLines

----------------------------------------------------------------------------------------------------
{-|
Writes a 'Molecule' to a PDB file. The PDB format is simplified and recounts atom, discards
different chains, sets dummy values for the temperature and occupation factors and cannot write
charges, as PDB expects integers.
-}
writePDB :: MonadThrow m => Molecule -> m Text
writePDB mol = do
  _ <- checkMolecule mol
  toPDB mol
 where
  toPDB :: MonadThrow m => Molecule -> m Text
  toPDB m = do
    let label   = Text.filter (not . isEndOfLine) $ m ^. molecule_Comment
        -- (Maybe it is necessary to strip whitespace at the end of the line.)
        header  = bprint ("HEADER    " % (right 70 ' ' %. stext) % "\n") label
        conects = toCONECT m
    hetatoms <- toHETATM m
    return . TextLazy.toStrict . TextBuilder.toLazyText $ header <> hetatoms <> conects
  -- Writes all atoms as HETATM records to the PDB.
  toHETATM :: MonadThrow m => Molecule -> m TextBuilder.Builder
  toHETATM m = do
    atomFragAssocs <- getAtomAssociationMap m
    atomLines      <-
      fmap (fmap snd . IntMap.toAscList)
      . IntMap.traverseWithKey
          (\atomKey (atom, fragNumAndLabel) -> writeAtomLine atomKey atom fragNumAndLabel)
      $ atomFragAssocs
    return . foldl' (<>) "" $ atomLines
   where
    writeAtomLine :: MonadThrow m => Int -> Atom -> Maybe (Int, Fragment) -> m TextBuilder.Builder
    writeAtomLine atomID atom fragNumAndLabel = do
      -- Define the formatters.
      let hetatmFmt   = "HETATM"                               -- 1-6 -> 6
          serialFmt   = (left 5 ' ' %. int) % " "              -- 7-11 (+12w) -> 5
          atomNameFmt = right 4 ' ' %. stext                   -- 13-16- -> 4 - Modify label depending in length before
          altLocFmt   = " "                                    -- 17 -> 1
          resNameFmt  = (right 3 ' ' %. stext) % " "           -- 18-20 (+19w) -> 3
          chainIDFmt  = char                                   -- 22 -> 1
          resSeqFmt   = left 4 ' ' %. int                      -- 23-26 -> 4
          icodeFmt    = " " % "   "                            -- 27 (+28-30w) -> 1
          coordFmt    = left 8 ' ' %. fixed 3                  -- 31-38 + 39-46 + 47-54 -> 3*8
          occFmt      = left 6 ' ' %. fixed 2                  -- 55-60 -> 6
          tempfacFmt  = (left 6 ' ' %. fixed 2) % "          " -- 61-66 (+67-76w) -> 6
          elementFmt  = left 2 ' ' %. stext                    -- 77-78 -> 2
          chgFmt      = left 2 ' ' %. stext                    -- 79-80 -> 2

      -- Get the coordinates of the atom.
      xCoord <- getVectorS (atom ^. atom_Coordinates) Massiv.!? 0
      yCoord <- getVectorS (atom ^. atom_Coordinates) Massiv.!? 1
      zCoord <- getVectorS (atom ^. atom_Coordinates) Massiv.!? 2

      -- Get other values for printing.
      let -- The label of the atom. Must not be longer than 4 and 4 indicates, that it contains an
          -- element symbol with 2 characters. Labels of 3 or shorter start 1 character later than
          -- labels with 4 characters.
          pdbSaneAtomName =
            let label4OrShorter = Text.take 4 $ atom ^. atom_Label
            in  if Text.length label4OrShorter <= 3 then " " <> label4OrShorter else label4OrShorter
      -- Get the fragment ID and fragment data. They are necessary for writing a PDB and not having
      -- them can go very wrong.
      (fragID, fragmentInfo) <- case fragNumAndLabel of
        Nothing -> throwM . MolLogicException "writePDB" $ ("Writing a PDB requires all atoms to be associated with a fragment, but atom " <> show atomID <> " does not seem to be assigned with any fragment.")
        Just f -> return f

      let -- The residue name of the PDB can be not longer than 3 characters.
          pdbSaneResName = Text.take 3 . _fragment_Label $ fragmentInfo
          -- The chain ID of this atom.
          pdbChainID     = fromMaybe 'A' . _fragment_Chain $ fragmentInfo
          -- The residue/fragment ID, that atom belongs to. This number has been made unique by the
          -- PDB parser. Using the chainID of the fragment, convert this number back to the
          -- non-unique PDB representation (unique only within a chain).
          pdbSaneResSeq =
            let numA                = fromEnum 'A'
                numID               = fromEnum pdbChainID
                maxResiduesPerChain = 10000
                offSet              = (numID - numA) * maxResiduesPerChain
            in  fragID - offSet
          -- The element symbol limited to 2 characters and capitalised.
          pdbSaneElement = Text.toUpper . Text.take 2 . tShow $ atom ^. atom_Element

      -- Format an atom line.
      let atomLine = bprint
            ( hetatmFmt
            % serialFmt
            % atomNameFmt
            % altLocFmt
            % resNameFmt
            % chainIDFmt
            % resSeqFmt
            % icodeFmt
            % coordFmt
            % coordFmt
            % coordFmt
            % occFmt
            % tempfacFmt
            % elementFmt
            % chgFmt
            % "\n"
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
    let bondMat            = m ^. molecule_Bonds
        allBondOrigins     = fmap fst . HashMap.keys $ bondMat
        -- Construct groupings from every possible origin to all targets.
        originTargetGroups = fmap
          (\origin ->
            let targets =
                    IntSet.fromList
                      . fmap snd
                      . HashMap.keys
                      . HashMap.filterWithKey (\(ixO, _) val -> ixO == origin && val)
                      $ bondMat
            in  (origin, targets)
          )
          allBondOrigins
        -- Translate all groupings into the corresponding CONECT lines.
        connctLines = foldl' (<>) "" . fmap (uncurry writeOriginLines) $ originTargetGroups
    in  connctLines
   where
    -- Write all connect lines, that are necessary for a given origin (in most cases that will be
    -- just one).
    writeOriginLines :: Int -> IntSet -> TextBuilder.Builder
    writeOriginLines origin targets =
      let -- Create groups of 4 targets per origin.
          targetGroups = chunksOf 4 . IntSet.toList $ targets
          conectGroups = zip (List.repeat origin) targetGroups

          -- Create the formatters.
          conectFmt    = "CONECT" -- 1-6 -> 6
          nFmt         = left 5 ' ' %. int

          -- Construct all necessary connect lines.
          conectLines  = fmap
            (\(origin', targetSet) ->
              let firstPart  = bprint (conectFmt % nFmt) origin'
                  secondPart = foldl'
                    (\targetAcc currentTarget -> targetAcc <> bprint nFmt currentTarget)
                    ""
                    targetSet
              in  firstPart <> secondPart <> "\n"
            )
            conectGroups
      in  foldl' (<>) "" conectLines

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
  inputFile <- view inputFileL
  let writerDir = getDirPath (inputFile ^. permanent) </> Path.dirPath "MoleculeLayout"

  -- Create the directory for the molecule files.
  liftIO $ Dir.createDirectoryIfMissing True writerDir

  goLayers writerDir Seq.Empty
 where
  -- Removes all submolecules in preparation for a writer.
  removeSubMols :: Molecule -> Molecule
  removeSubMols mol = mol & molecule_SubMol .~ IntMap.empty

  -- Recursively write all layers of a molecule.
  goLayers :: (HasMolecule env, HasLogFunc env) => Path.AbsRelDir -> MolID -> RIO env ()
  goLayers writerDir molID = do
    topMol <- view moleculeL
    writeMolFromID writerDir molID
    thisMol <- getMolByID topMol molID
    let nextMolIDs = fmap (molID |>) . Seq.fromList . IntMap.keys $ thisMol ^. molecule_SubMol
    mapM_ (goLayers writerDir) nextMolIDs

  -- Write a single layer specified by its MolID to a file in a unique directory.
  writeMolFromID :: (HasMolecule env, HasLogFunc env) => Path.AbsRelDir -> MolID -> RIO env ()
  writeMolFromID writerDir molID = do
    molTop <- view moleculeL
    let molLayerOfInterest = getMolByID molTop molID
        layerONIOMHumanID =
          Text.unpack . removeWhiteSpace . textDisplay . molID2OniomHumandID $ molID
        molWriterText = writer =<< removeSubMols <$> molLayerOfInterest
        molFilePath   = writerDir </> Path.relFile layerONIOMHumanID

    -- Try writing the current layer to a file.
    case molWriterText of
      Left exception ->
        logWarnS "writeLayout"
          $  "Writing molecule "
          <> molID2OniomHumandID molID
          <> " (MolID "
          <> displayShow molID
          <> ") failed with: "
          <> (text2Utf8Builder . tShow $ exception)
      Right molText -> do
        hasMolFileAlready <- liftIO $ Dir.doesFileExist molFilePath
        if hasMolFileAlready
          then do
            logWarnS "writeLayout"
              $  "Molecule file for "
              <> molID2OniomHumandID molID
              <> " (MolID "
              <> displayShow molID
              <> ") does already exist at "
              <> path2Utf8Builder molFilePath
              <> ". Overwriting it."
            writeFileUTF8 molFilePath molText
          else writeFileUTF8 molFilePath molText
