-- |
-- Module      : Spicy.Molecule.Internal.Parser
-- Description : Parsers for chemical data formats and computational chemistry output fileSeq.
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides parsers for chemical file formatSeq. For the internal representation no parser is
-- provided, as this is a JSON structured file, which should be parsed by
-- [aeson](http://hackage.haskell.org/package/aeson)'s @decodeEither@.
module Spicy.Molecule.Internal.Parser
  ( -- * Chemical Data Formats
    xyz,
    txyz,
    mol2,
    pdb,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Default
import Data.Either
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Massiv.Array as Massiv hiding
  ( index,
    map,
    swap,
    take,
    takeWhile,
    zip,
  )
import Data.Maybe
import Data.Tuple
import Optics
import RIO hiding
  ( take,
    takeWhile,
    (.~),
    (^.),
  )
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import Spicy.Common
import Spicy.Molecule.Internal.Types
import Spicy.Molecule.Internal.Util

-- | Parse a .xyz file (has no connectivity, atom types or partioal charges). Atom counting starts
-- at 1 for XYZ files, which makes it more convenient to use with most visualisation software.
xyz :: Parser Molecule
xyz = do
  nAtoms <- skipHorizontalSpace *> decimal <* skipHorizontalSpace <* endOfLine
  label' <- skipHorizontalSpace *> takeWhile (not . isEndOfLine) <* endOfLine
  atoms' <- count nAtoms xyzLineParser
  let atoms = IntMap.fromList $ zip [1 ..] atoms'
      fragment = IntMap.singleton 0
        Fragment
          { label = "all",
            chain = Nothing,
            atoms = IntMap.keysSet atoms
          }
  return
    Molecule
      { comment = label',
        atoms = atoms,
        bonds = HashMap.empty,
        subMol = IntMap.empty,
        fragment = fragment,
        energyDerivatives = def,
        calcContext = Map.empty,
        jacobian = Nothing
      }
  where
    xyzLineParser :: Parser Atom
    xyzLineParser = do
      cElement <- skipHorizontalSpace *> many1 letter
      x <- skipHorizontalSpace *> double
      y <- skipHorizontalSpace *> double
      z <- skipHorizontalSpace *> double
      skipHorizontalSpace
      endOfLine
      return
        Atom
          { element = fromMaybe H . readMaybe $ cElement,
            label = "",
            isLink = NotLink,
            isDummy = False,
            ffType = FFXYZ,
            multipoles = def,
            coordinates = VectorS . Massiv.fromList Seq $ [x, y, z]
          }

----------------------------------------------------------------------------------------------------

-- | Parse a Tinker XYZ formatted file. It has coordinates and might have connectivity and atom
-- types. This format and therefore parser are not using any submolecules or fragments. The atom
-- counting is directly taken from the file.
txyz :: Parser Molecule
txyz = do
  nAtoms <- skipHorizontalSpace *> (decimal :: Parser Int)
  label' <- skipHorizontalSpace *> takeWhile (not . isEndOfLine) <* skipSpace
  conAndAtoms <- count nAtoms txyzLineParser
  let atoms' = IntMap.fromAscList . fmap (\info -> (txyzIndex info, txyzAtom info)) $ conAndAtoms
      bonds' =
        HashMap.fromList
          . concatMap
            ( \info ->
                let originReplicated = List.repeat . txyzIndex $ info
                    bondedAtomPairs = List.zip originReplicated $ txyzBondTargets info
                    bondsWithSameOrigin = List.zip bondedAtomPairs (List.repeat True)
                 in bondsWithSameOrigin
            )
          $ conAndAtoms
      fragment =
        Fragment
          { label = "all",
            chain = Nothing,
            atoms = IntMap.keysSet atoms'
          }
  return
    Molecule
      { comment = label',
        atoms = atoms',
        bonds = bonds',
        subMol = IntMap.empty,
        fragment = IntMap.singleton 0 fragment,
        energyDerivatives = def,
        calcContext = Map.empty,
        jacobian = Nothing
      }
  where
    -- Parsing a single line of atomSeq. Tinker's format keeps bonds associated with atomSeq. So a tuple
    -- suitable to construct the 'IntMap' is returned additional to the pure atomSeq.
    txyzLineParser :: Parser TXYZAtomInfo
    txyzLineParser = do
      index <- skipHorizontalSpace *> decimal
      cElement <- skipHorizontalSpace *> many1 letter
      x <- skipHorizontalSpace *> double
      y <- skipHorizontalSpace *> double
      z <- skipHorizontalSpace *> double
      mFFType <- skipHorizontalSpace *> maybeOption (decimal :: Parser Int)
      connectivityRaw <- many' (skipHorizontalSpace *> decimal <* skipHorizontalSpace)
      endOfLine
      let atom =
            Atom
              { element = fromMaybe H . readMaybe $ cElement,
                label = "",
                isLink = NotLink,
                isDummy = True,
                ffType = case mFFType of
                  Nothing -> FFTXYZ 0
                  Just a -> FFTXYZ a,
                coordinates = VectorS . Massiv.fromList Seq $ [x, y, z],
                multipoles = def
              }
      return
        TXYZAtomInfo
          { txyzIndex = index,
            txyzAtom = atom,
            txyzBondTargets = connectivityRaw
          }

----------------------------------------------------------------------------------------------------

-- | Parse the "interesting" fields of a MOL2 file. This contains partial charges as well as
-- connectivity. There is no special understanding for the atom types, that are available in MOL2
-- fileSeq. They will simply be treated as the force field string. See
-- <http://chemyang.ccnu.edu.cn/ccb/server/AIMMS/mol2.pdf>.
mol2 :: Parser Molecule
mol2 = do
  (label', nAtoms, nBonds) <- moleculeParser
  atomAndFragmentInfo <- atomParser nAtoms
  bonds' <- bondParser nBonds
  let -- Construct the atoms and fragments of the top layer from the atom block.
      (atoms', fragments) = fragmentAtomInfo2AtomsAndFragments atomAndFragmentInfo
  return
    Molecule
      { comment = label',
        atoms = atoms',
        bonds = bonds',
        subMol = IntMap.empty,
        fragment = fragments,
        energyDerivatives = def,
        calcContext = Map.empty,
        jacobian = Nothing
      }
  where
    -- Parse the @<TRIPOS>MOLECULE block of MOL2.
    moleculeParser :: Parser (Text, Int, Maybe Int)
    moleculeParser = do
      _header <- manyTill anyChar (string "@<TRIPOS>MOLECULE") <* endOfLine
      -- Line 1 -> "mol_name"
      label' <- takeWhile (not . isEndOfLine) <* endOfLine
      -- Line 2 -> "num_atoms [num_bonds [num_subst [num_feat [num_sets]]]]"
      nAtoms <- skipHorizontalSpace *> decimal
      nBonds <- maybeOption $ skipHorizontalSpace *> decimal
      _nSubMols <- maybeOption $ skipHorizontalSpace *> (decimal :: Parser Int)
      _nFeatures <- maybeOption $ skipHorizontalSpace *> (decimal :: Parser Int)
      _nSets <-
        maybeOption $
          skipHorizontalSpace
            *> (decimal :: Parser Int)
            <* skipHorizontalSpace
            <* endOfLine
      -- Line 3 -> "mol_type"
      _molType <-
        skipHorizontalSpace
          *> string "SMALL"
          <|> string "BIOPOLYMER"
          <|> string "PROTEIN"
          <|> string "NUCLEIC_ACID"
          <|> string "SACCHARIDE"
          <* skipSpace
      -- Line 4 -> "charge_type"
      _chargeType <-
        skipSpace
          *> ( string "NO_CHARGES"
                 <|> string "DEL_RE"
                 <|> string "GASTEIGER"
                 <|> string "GAST_HUCK"
                 <|> string "HUCKEL"
                 <|> string "PULLMAN"
                 <|> string "GAUSS80_CHARGES"
                 <|> string "AMPAC_CHARGES"
                 <|> string "MULLIKEN_CHARGES"
                 <|> string "DICT_CHARGES"
                 <|> string "MMFF94_CHARGES"
                 <|> string "USER_CHARGES"
             )
          <* skipHorizontalSpace
          <* endOfLine
      -- Line 5 -> "[status_bits"
      _statusBit <- maybeOption $ skipHorizontalSpace *> many1 letter <* skipSpace
      -- Line 6 -> "[mol_comment]]"
      _comment <- maybeOption $ skipHorizontalSpace *> takeWhile (not . isEndOfLine) <* skipSpace
      return (label', nAtoms, nBonds)
    -- Parse the @<TRIPOS>ATOM block of MOL2.
    atomParser :: Int -> Parser [FragmentAtomInfo]
    atomParser nAtoms = do
      _header <- manyTill anyChar (string "@<TRIPOS>ATOM") <* endOfLine
      -- Parse multiple lines of ATOM data.
      atomAndFragmentInfo <- count nAtoms $ do
        index <- skipHorizontalSpace *> decimal
        -- Most often this will be the element symbol.
        label' <- skipHorizontalSpace *> takeWhile (not . isHorizontalSpace)
        -- x, y and z coordinates
        x <- skipHorizontalSpace *> double
        y <- skipHorizontalSpace *> double
        z <- skipHorizontalSpace *> double
        -- Parse the chemical element, which is actually the first part of the SYBYL atom type.
        cElem <- skipHorizontalSpace *> many1 letter
        -- A dot often separates the element from the type of this element.
        ffdot <- maybeOption $ char '.'
        -- And after the dot the rest of the SYBYL atom type might come.
        ffType' <- maybeOption $ takeWhile (not . isHorizontalSpace)
        -- The substructure ID. This is the identifier to identify sub moleculeSeq.
        subID <- skipHorizontalSpace *> (decimal :: Parser Int)
        -- The substructure Name. This should be used as label for the sub molecule.
        subName <- skipHorizontalSpace *> takeWhile (not . isHorizontalSpace)
        -- The partial charge
        pCharge <- skipHorizontalSpace *> double <* skipSpace
        -- Construct the force field string for the 'MOL2' field.
        let mol2FFText =
              Text.pack cElem
                <> ( case ffdot of
                       Nothing -> ""
                       Just _ -> "."
                   )
                <> fromMaybe "" ffType'
            atom =
              Atom
                { element = fromMaybe H . readMaybe $ cElem,
                  label = label',
                  isLink = NotLink,
                  isDummy = False,
                  ffType = FFMol2 mol2FFText,
                  coordinates = VectorS . Massiv.fromList Seq $ [x, y, z],
                  multipoles = def & #monopole % _Just % #q00 .~ pCharge
                }
            thisAtomsFragment =
              Fragment
                { label = subName,
                  chain = Nothing,
                  atoms = IntSet.singleton index
                }
        return
          FragmentAtomInfo
            { faiAtomIndex = index,
              faiFragmentIndex = subID,
              faiAtom = atom,
              faiFragment = thisAtomsFragment
            }
      return atomAndFragmentInfo
    --
    -- Parse the @<TRIPOS>BOND part. Unfortunately, the bonds in the MOL2 format are unidirectiorial
    -- and need to be flipped to.
    bondParser :: Maybe Int -> Parser (HashMap (Int, Int) Bool)
    bondParser nBonds = do
      let -- How often to parse bond fields depends on if the number of bonds has been specified.
          nParser = case nBonds of
            Nothing -> many'
            Just n -> count n
      _header <- manyTill anyChar (string "@<TRIPOS>BOND") <* endOfLine
      uniBonds <- nParser $ do
        -- Bond id, which does not matter.
        _id <- skipHorizontalSpace *> (decimal :: Parser Int)
        -- Origin atom index
        origin <- skipHorizontalSpace *> (decimal :: Parser Int)
        -- Target atom index
        target <- skipHorizontalSpace *> (decimal :: Parser Int)
        -- Bond type, which we don't care about.
        _type <- skipHorizontalSpace *> takeWhile (not . isSpace) <* skipSpace
        return (origin, target)
      let -- Make the bonds bidirectorial
          bondTuplesForth = uniBonds
          bondTuplesBack = swap <$> bondTuplesForth
          bondTuplesBoth = zip (bondTuplesForth <> bondTuplesBack) (List.repeat True)
          bonds' = HashMap.fromList bondTuplesBoth
      return bonds'

----------------------------------------------------------------------------------------------------

-- | Parse a PDB file as described in
-- <ftp://ftp.wwpdb.org/pub/pdb/doc/format_descriptions/Format_v33_A4.pdf>. If parsing of a single ATOM
-- or CONETC line fails, the parser will stop there and ignore all the other records of same type,
-- directly coming after the failed one.
--
-- /This is not an entirely valid PDB parser. You will run into problems for very large structures,
-- /where atom indices exist multiple times./
pdb :: Parser Molecule
pdb = do
  -- Parse the HEADER field as a label. Only the first line of COMPND will be used.
  label' <- maybeOption $ do
    _ <- manyTill anyChar (string "HEADER")
    compoundLabel <- skipHorizontalSpace *> (Text.strip <$> takeWhile (not . isEndOfLine))
    return compoundLabel
  -- Parse atoms only and ignore other fiels
  atomsAndFragmentInfo <- many1 atomParser
  -- Parse the bonds to the tuple structure.
  bondTuples <- many' connectParser
  let -- Transform the information from the parsers.
      bonds' = HashMap.fromList $ zip (List.concat bondTuples) (List.repeat True)
      (atoms', fragments) = fragmentAtomInfo2AtomsAndFragments atomsAndFragmentInfo
  return
    Molecule
      { comment = fromMaybe "" label',
        atoms = atoms',
        bonds = bonds',
        subMol = IntMap.empty,
        fragment = fragments,
        energyDerivatives = def,
        calcContext = Map.empty,
        jacobian = Nothing
      }
  where
    -- Parser fot HETATM and ATOM records.
    atomParser :: Parser FragmentAtomInfo
    atomParser = do
      -- First check, that this line really starts with an ATOM or HETATM record (6 characters).
      -- Columns 1-6: record type.
      _recordStart <- Text.pack <$> manyTill anyChar (string "\nATOM  " <|> string "\nHETATM")
      -- Columns 7-11: serial key/number of the atom.
      atomKey <- (Text.strip <$> take 5) >>= nextParse decimal
      -- Column 12: space
      _ <- char ' '
      -- Column 13-16: atom label
      atomLabel <- Text.strip <$> take 4
      -- Column 17: alternate location index. Usually not set and not of interest to Spicy.
      _altLoc <- anyChar
      -- Column 18-20: fragment name.
      fragmentLabel <- Text.strip <$> take 3
      -- Column 21: space
      _ <- char ' '
      -- Column 22: chain identifier
      chainID <- letter
      -- Column 23-26: residue sequence ID. This is not unique for in a whole PDB but only unique
      -- within a chain. A fragment ID therefore needs to be constructed from both together. Also the
      -- writer needs to take care of this conversion.
      fragmentInChain <- (Text.strip <$> take 4) >>= nextParse decimal
      -- Column 27: "Code for insertion of residues". Does not matter for Spicy.
      _ <- anyChar
      -- Column 29-30: space
      _ <- count 3 $ char ' '
      -- Column 31-38: x coordinates in angstrom.
      xCoord <- (Text.strip <$> take 8) >>= nextParse double
      -- Column 39-46: y coordinates in angstrom.
      yCoord <- (Text.strip <$> take 8) >>= nextParse double
      -- Column 47-54: z coordinates in angstom.
      zCoord <- (Text.strip <$> take 8) >>= nextParse double
      -- Column 55-60: occupancy. Does not matter for Spicy.
      _occupancy <- (Text.strip <$> take 6) >>= nextParse double
      -- Column 61-66: temperature factor. Does not matter for Spicy.
      _tempFac <- (Text.strip <$> take 6) >>= nextParse double
      -- Column 67-76: space:
      _ <- count 10 $ char ' '
      -- Column 77-78: Chemical element in all upper case.
      element' <-
        maybeToParserMonad
          =<< ( (readMaybe :: String -> Maybe Element)
                  . Text.unpack
                  . Text.toTitle
                  . Text.strip
                  <$> take 2
              )
      -- Column 79-80: Charge on the atom as string. Its strange. IDK. Often not set. Ignored
      _charge <- take 2
      -- Do not parse an EOL character. This happens at the start of a record.

      let atom =
            Atom
              { element = element',
                label = atomLabel,
                isLink = NotLink,
                isDummy = False,
                ffType = FFPDB atomLabel,
                coordinates = VectorS . Massiv.fromList Massiv.Seq $ [xCoord, yCoord, zCoord],
                multipoles = def
              }
          fragment' =
            Fragment
              { label = fragmentLabel,
                chain = Just chainID,
                atoms = IntSet.singleton atomKey
              }
          -- The residue id is only unique per chain. To become completely unique, enumerate chain
          -- IDs, multiply with the maximum number of residues per chain possible (10000) and add the
          -- local chain ID. When writing the PDB again, this conversion has to be reversed.
          uniqueFragmentKey =
            let numA = fromEnum 'A'
                numID = fromEnum chainID
                maxResiduesPerChain = 10000
             in (numID - numA) * maxResiduesPerChain + fragmentInChain
      return
        FragmentAtomInfo
          { faiAtomIndex = atomKey,
            faiFragmentIndex = uniqueFragmentKey,
            faiAtom = atom,
            faiFragment = fragment'
          }

    -- Parse CONECT fields of the PDB. PDB bonds are bidirectorial, so no swapping required.
    connectParser :: Parser [(Int, Int)]
    connectParser = do
      -- First check, that this line really starts with a CONECT record (6 characters).
      -- Columns 1-6: record type.
      _recordStart <- Text.pack <$> manyTill anyChar (string "\nCONECT")
      -- Column 7-11: Bond origin
      bondOrigin <- take 5 >>= nextParse decimal
      -- Column 12-16, 17-21, 22-26 and 27-31: Bond targets
      bondTargets <- many1 $ (Text.strip <$> take 5) >>= nextParse decimal
      -- Do no parse EOL. The EOL is parsed in the beginning as the start of an record.
      return $ List.zip (List.repeat bondOrigin) bondTargets

    -- A helper function to feed the result of one parser into another.
    nextParse :: Parser a -> Text -> Parser a
    nextParse nextParser text = case parseOnly nextParser text of
      Left err -> fail err
      Right res -> return res

    -- Converts a 'Maybe' result to a 'Parser' result.
    maybeToParserMonad :: Maybe a -> Parser a
    maybeToParserMonad a = case a of
      Nothing -> fail "Could not parse the element symbol."
      Just x -> return x
