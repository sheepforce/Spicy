{-|
Module      : Spicy.Molecule.Internal.Writer
Description : Writing chemical data formats
Copyright   : Phillip Seeber, 2019
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
  )
where
import           Control.Exception.Safe
import           Data.Aeson.Encode.Pretty
import           Data.Attoparsec.Text.Lazy      ( isEndOfLine )
import           Data.Foldable
import qualified Data.IntMap.Lazy              as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.List.Split                ( chunksOf )
import           Data.Maybe
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as T
import           Lens.Micro.Platform
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
import           Spicy.Molecule.Internal.Util
import           Text.Printf

{-|
Write a Molden XYZ file from a molecule. This format ignores all deep level layers of a molecule.
-}
writeXYZ :: MonadThrow m => Molecule -> m Text
writeXYZ mol = toXYZ <$> checkMolecule mol
 where
  -- Assemble a XYZ file from a molecule
  toXYZ :: Molecule -> Text
  toXYZ m =
    let -- Line 1 of the header contains the number of atoms
      headL1 = T.pack . show . IM.size $ m ^. molecule_Atoms
      -- Line 2 of the header contains 1 line of comment. Remove all linebreaks by filtering.
      headL2 = T.filter (not . isEndOfLine) $ m ^. molecule_Label
      atomLs = IM.foldr' (\atom acc -> atomLineWriter atom `T.append` acc) "" $ m ^. molecule_Atoms
    in
      T.unlines [headL1, headL2] `T.append` atomLs
   where
    atomLineWriter :: Atom -> Text
    atomLineWriter a = T.pack $ printf "%-4s    %12.8F    %12.8F    %12.8F\n"
                                       (show $ a ^. atom_Element)
                                       (fromMaybe 0.0 $ (a ^. atom_Coordinates) S.!? 0)
                                       (fromMaybe 0.0 $ (a ^. atom_Coordinates) S.!? 1)
                                       (fromMaybe 0.0 $ (a ^. atom_Coordinates) S.!? 2)

----------------------------------------------------------------------------------------------------
{-|
Write a Tinker XYZ from a 'Molecule'. The writer trusts the '_atom_FFType' to be
correct (if set) and will simply write them out. Therefore it is possible, that wrong atom types can
be written. If they are not set, the writer will simply equalise all atom types to 0, which is OK
for visualisation but obviously not for MM.

This format ingores all deeper level layers of a molecule.
-}
writeTXYZ :: MonadThrow m => Molecule -> m Text
writeTXYZ mol
  | ffTypeCheck
  = toTXYZ <$> (checkMolecule =<< reIndex2BaseMolecule (mol & molecule_SubMol .~ S.empty))
  | otherwise
  = throwM
    $ MolLogicException "writeMOL2" "writeTXYZ: Not all atoms have Tinker XYZ style atom types."
 where
    -- Check if all atoms have TXYZ atom types.
  ffTypeCheck = all (== TXYZ 0) . IM.map (\a -> a ^. atom_FFType) $ mol ^. molecule_Atoms
  --
  -- Write the molecule as a TXYZ formatted file.
  toTXYZ :: Molecule -> Text
  toTXYZ m =
    let -- The header line contains the number of atoms and separated by a space a comment.
        headL1 =
            (T.pack . show . IM.size $ mol ^. molecule_Atoms)
              `T.append` "  "
              `T.append` (T.filter (not . isEndOfLine) $ mol ^. molecule_Label)
              `T.append` "\n"
        -- The atom lines contain:
        --   - serial number of the atom
        --   - element symbol of the atom
        --   - XYZ coordinates in angstrom
        --   - The integer number of the atom type
        --   - The indices of all atoms, where to bind to
        atomLs = IM.foldrWithKey' (\key atom acc -> (atomLineWriter key atom m) `T.append` acc)
                                  ""
                                  (m ^. molecule_Atoms)
    in  headL1 `T.append` atomLs
  --
  -- Writes the first part of an atom line in a TXYZ file (without bonds).
  atomFirstPart :: Int -> Atom -> Text
  atomFirstPart k a =
    let ffNum = case a ^. atom_FFType of
          TXYZ i -> i
          _      -> 0
    in  T.pack $ printf "%6d    %2s    %12.8F    %12.8F    %12.8F    %6d"
                        (k + 1)
                        (show $ a ^. atom_Element)
                        (fromMaybe 0.0 $ (a ^. atom_Coordinates) S.!? 0)
                        (fromMaybe 0.0 $ (a ^. atom_Coordinates) S.!? 1)
                        (fromMaybe 0.0 $ (a ^. atom_Coordinates) S.!? 2)
                        ffNum
  --
  -- Writes the second part of an atom line in a TXYZ file (bond targets).
  atomSecondPart :: Int -> Molecule -> Text
  atomSecondPart k m =
    let targets = fromMaybe IS.empty ((m ^. molecule_Bonds) IM.!? k)
    in  T.stripEnd
            (IS.foldr' (\x xs -> (T.pack $ printf "    %6d" (x + 1)) `T.append` xs) "" $ targets)
          `T.append` "\n"
  --
  -- Writes an complete atom line.
  atomLineWriter :: Int -> Atom -> Molecule -> Text
  atomLineWriter k a m = (atomFirstPart k a) `T.append` (atomSecondPart k m)

----------------------------------------------------------------------------------------------------
{-|
Write a simplified .mol2 file (Tripos SYBYL) from a 'Molecule', containing the atoms, connectivities
(single bonds only) and partial charges. This format writes atoms and bonds __only__ from the the
first sublayer of the 'Molecule', which includes the fragment definitions.
-}
writeMOL2 :: MonadThrow m => Molecule -> m Text
writeMOL2 mol
  | ffTypeCheck = toMOL2 <$> (checkMolecule =<< reIndex2BaseMolecule mol)
  | otherwise   = throwM $ MolLogicException "writeMOL2" "Not all atoms have MOL2 style atom types."
 where
  ffTypeCheck = all (== Mol2 "") . IM.map (\a -> a ^. atom_FFType) $ mol ^. molecule_Atoms
  --
  -- Write a MOL2 formatted text from the current molecule.
  toMOL2 :: Molecule -> Text
  toMOL2 m =
    let -- First sublayer of the molecule.
        subMols = m ^. molecule_SubMol
    in  (toMOLECULE m) `T.append` (toATOM m subMols) `T.append` (toBOND m)
  --
  -- Write the "@<TRIPOS>MOLECULE" block.
  toMOLECULE :: Molecule -> Text
  toMOLECULE m =
    let nAtoms = IM.size $ m ^. molecule_Atoms
        -- Make the bonds unidirectorial first.
        bonds  = makeIMISUnidirectorial $ m ^. molecule_Bonds
        -- Then get the overall number of bonds.
        nBonds = IM.foldr' (+) 0 . IM.map IS.size $ bonds
    in  T.unlines
          [ "@<TRIPOS>MOLECULE"
          , T.filter (not . isEndOfLine) $ m ^. molecule_Label
          , (T.pack . show $ nAtoms)
          `T.append` " "
          `T.append` (T.pack . show $ nBonds)
          `T.append` " 0 0 0"
          , "SMALL"
          , "USER_CHARGES"
          , ""
          ]
  --
  -- Write the @<TRIPOS>ATOM block.
  toATOM :: Molecule -> Seq Molecule -> Text
  toATOM m sM =
    let annoFrags = IM.fromAscList . toList . S.zip (S.fromList [1 .. S.length sM]) $ sM
        atomLines = IM.foldrWithKey'
          (\key atom acc ->
            let -- The index of the submol/fragment, in which the current atom can be found
                fragmentNum   = findAtomInSubMols key annoFrags
                fragment      = fragmentNum >>= (\fragKey -> IM.lookup fragKey annoFrags)
                fragmentLabel = _molecule_Label <$> fragment
                thisAtomLine  = T.pack $ printf
                  "%7d %-6s %12.8F %12.8F %12.8F %-8s %4d  %10s %8.4F\n"
                  (key + 1)                                           -- Index
                  (T.unpack $ atom ^. atom_Label)                     -- Atom label
                  (fromMaybe 0.0 $ (atom ^. atom_Coordinates) S.!? 0) -- X
                  (fromMaybe 0.0 $ (atom ^. atom_Coordinates) S.!? 1) -- Y
                  (fromMaybe 0.0 $ (atom ^. atom_Coordinates) S.!? 2) -- Z
                  (case atom ^. atom_FFType of                        -- Atom label
                    Mol2 t -> T.unpack t
                    _      -> show $ atom ^. atom_Element
                  )
                  (fromMaybe 0 fragmentNum)                           -- Fragment number
                  (fromMaybe "UNL1" $ T.unpack <$> fragmentLabel)     -- Fragment name
                  ((case atom ^. atom_PCharge of                      -- Partial charge
                     Nothing -> 0
                     Just c  -> c
                   ) :: Double
                  )
            in  thisAtomLine `T.append` acc
          )
          ""
          (m ^. molecule_Atoms)
    in  "@<TRIPOS>ATOM\n" `T.append` atomLines
  --
  -- Write the "@<TRIPOS>BOND" block. All bonds are defined as single bonds for convenience.
  toBOND :: Molecule -> Text
  toBOND m =
    let -- Make the bonds unidirectorial first.
        bonds     = makeIMISUnidirectorial $ m ^. molecule_Bonds
        -- Write a single line for each bond.
        -- Fold the keys (origins) in the IntMap to bond lines. Each origin key might give
        -- multiple lines (one per target), and the targets are folded in an inner fold.
        bondLines = snd $ IM.foldrWithKey'
          (\origin targets (nthBond, prevBLines) ->

            let -- For each target, write a new line. This is the inner loop, processing all
                -- targets for the current origin in the outer fold.
                targetLines = snd $ IS.foldr'
                  (\target (nthTarget, prevTLines) ->
                    let targetLine = T.pack $ printf "%6d %6d %6d %4s\n"
                                                     (nthTarget + nthBond)
                                                     (origin + 1)
                                                     (target + 1)
                                                     ("1" :: String)
                    in  (nthTarget + 1, prevTLines `T.append` targetLine)
                  )
                  (0, "")
                  targets
            in  (nthBond + IS.size targets, prevBLines `T.append` targetLines)
          )
          (1, "")
          bonds
    in  "@<TRIPOS>BOND\n" `T.append` bondLines

----------------------------------------------------------------------------------------------------
{-|
Writes a 'Molecule' to a PDB file. The PDB format is simplified and recounts atom, discards
different chains, sets dummy values for the temperature and occupation factors and cannot write
charges (as PDB) expects integers.
-}
writePDB :: MonadThrow m => Molecule -> m Text
writePDB mol
  | ffTypeCheck = toPDB <$> (checkMolecule =<< reIndex2BaseMolecule mol)
  | otherwise   = throwM $ MolLogicException "writePDB" "Not all atoms have PDB style atom types."
 where
  ffTypeCheck = all (== PDB "") . IM.map (\a -> a ^. atom_FFType) $ mol ^. molecule_Atoms
  toPDB :: Molecule -> Text
  toPDB m =
    let label = T.filter (not . isEndOfLine) $ m ^. molecule_Label
        -- Strange construct to avoid conflict with the parser with trailing white space in the
        -- "HEADER" records.
        header =
            (T.stripEnd . T.pack $ printf "%-6s    %-70s" ("HEADER" :: Text) label) `T.append` "\n"
    in  header `T.append` (toHETATM m) `T.append` (toCONECT m)
  toHETATM :: Molecule -> Text
  toHETATM m =
    let subMols   = m ^. molecule_SubMol
        annoFrags = IM.fromAscList . toList . S.zip (S.fromList [1 .. S.length subMols]) $ subMols
        atomLines = IM.foldrWithKey'
          (\key atom acc ->
            let fragmentNum   = findAtomInSubMols key annoFrags
                fragment      = fragmentNum >>= (\fragKey -> IM.lookup fragKey annoFrags)
                fragmentLabel = _molecule_Label <$> fragment
                thisAtomLine =
                    T.concat
                      . map T.pack
                      $ [ printf "%-6s" ("HETATM" :: Text)                                        -- 1-6: Record type
                        , printf "%5d " (key + 1)                                                 -- 7-11: Atom serial number
                        , printf
                          "%-4s "
                          (if T.length (atom ^. atom_Label) <= 3
                            then " " ++ (T.unpack $ atom ^. atom_Label)
                            else T.unpack $ atom ^. atom_Label
                          )
                        , printf "%3s " (fromMaybe "UNL" $ T.unpack . T.take 3 <$> fragmentLabel) -- 18-20: Residue name
                        , printf "%1s"             ("A" :: Text)                                  -- 22: Chain identifier
                        , printf "%4d    "         (fromMaybe 0 fragmentNum)                      -- 23-26: Residue sequence number
                        , printf "%8.3F" (fromMaybe 0.0 $ (atom ^. atom_Coordinates) S.!? 0)      -- 31-38: X
                        , printf "%8.3F" (fromMaybe 0.0 $ (atom ^. atom_Coordinates) S.!? 1)      -- 39-46: Y
                        , printf "%8.3F" (fromMaybe 0.0 $ (atom ^. atom_Coordinates) S.!? 2)      -- 47-54: Z
                        , printf "%6.2F"           (1.0 :: Double)                                -- 55-60: Occupancy
                        , printf "%6.2F          " (0.0 :: Double)                                -- 61-66: Temperature factor
                        , printf "%2s" (T.toUpper . T.pack . show $ atom ^. atom_Element)         -- 77-78: Element symbol
                        , printf "%2s\n"           ("" :: Text)                                   -- 79-80: Charge of the atom.
                        ]
            in  thisAtomLine `T.append` acc
          )
          ""
          (m ^. molecule_Atoms)
    in  atomLines
  toCONECT :: Molecule -> Text
  toCONECT m =
    IM.foldrWithKey'
        (\o ts acc ->
          let originGroupLines = writeOriginLines o ts in originGroupLines `T.append` acc
        )
        ""
      $  m
      ^. molecule_Bonds
   where
    writeOriginLines :: Int -> IntSet -> Text
    writeOriginLines origin targets =
      let targetGroups = chunksOf 4 . IS.toList $ targets
          conectGroups = zip (repeat origin) targetGroups
      in  T.concat
            . map
                (\(o, ts) ->
                  "CONECT"
                    `T.append` T.pack (printf "%5d" (o + 1))
                    `T.append` (T.concat . map (T.pack . (printf "%5d") . (+ 1)) $ ts)
                    `T.append` "\n"
                )
            $ conectGroups

----------------------------------------------------------------------------------------------------
{-|
Write Spicy format, which is an AESON generated JSON document, directly representing the data type
of 'Molecule'.
-}
writeSpicy :: Molecule -> Text
writeSpicy mol = T.decodeUtf8 . encodePretty $ mol
