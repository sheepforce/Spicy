{-|
Module      : Spicy.Molecule.Internal.Parser
Description : Parsers for chemical data formats and computational chemistry output fileSeq.
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for chemical file formatSeq. For the internal representation no parser is
provided, as this is a JSON structured file, which should be parsed by
[aeson](http://hackage.haskell.org/package/aeson)'s @decodeEither@.
-}
module Spicy.Molecule.Internal.Parser
  ( -- * Chemical Data Formats
    parseXYZ
  , parseTXYZ
  , parseMOL2
  , parsePDB
  )
where

import           Control.Applicative

import           Control.Lens            hiding ( index )
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Either
import           Data.Foldable
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Data.Maybe
import qualified Data.Text.Read                as Text
import           Data.Tuple
import           RIO                     hiding ( takeWhile
                                                , (^.)
                                                )
import qualified RIO.List                      as List
import qualified RIO.Seq                       as Seq
import qualified RIO.Text                      as Text

import           Data.Default
import           Data.Massiv.Array             as Massiv
                                         hiding ( index
                                                , map
                                                , swap
                                                , zip
                                                )
import qualified RIO.HashMap                   as HashMap
import qualified RIO.Map                       as Map
import           Spicy.Class
import           Spicy.Generic
import           Spicy.Molecule.Internal.Util

{-|
Parse a .xyz file (has no connectivity, atom types or partioal charges). Atom counting starts at 1
for XYZ files, which makes it more convenient to use with most visualisation software.
-}
parseXYZ :: Parser Molecule
parseXYZ = do
  nAtoms <- skipHorizontalSpace *> decimal <* skipHorizontalSpace <* endOfLine
  label  <- skipHorizontalSpace *> takeWhile (not . isEndOfLine) <* endOfLine
  atoms  <- count nAtoms xyzLineParser
  return Molecule { _molecule_Comment           = label
                  , _molecule_Atoms             = IntMap.fromList $ zip [1 ..] atoms
                  , _molecule_Bonds             = HashMap.empty
                  , _molecule_SubMol            = IntMap.empty
                  , _molecule_Fragment          = IntMap.empty
                  , _molecule_EnergyDerivatives = def
                  , _molecule_CalcContext       = Map.empty
                  }
 where
  xyzLineParser :: Parser Atom
  xyzLineParser = do
    cElement <- skipHorizontalSpace *> many1 letter
    x        <- skipHorizontalSpace *> double
    y        <- skipHorizontalSpace *> double
    z        <- skipHorizontalSpace *> double
    skipSpace
    return Atom { _atom_Element     = fromMaybe H . readMaybe $ cElement
                , _atom_Label       = ""
                , _atom_IsPseudo    = False
                , _atom_IsCapped    = False
                , _atom_IsDummy     = False
                , _atom_FFType      = FFXYZ
                , _atom_Multipoles  = def
                , _atom_Coordinates = VectorS . Massiv.fromList Seq $ [x, y, z]
                }

----------------------------------------------------------------------------------------------------
{-|
Parse a Tinker XYZ formatted file. It has coordinates and might have connectivity and atom typeSeq.
This format and therefore parser are not using any layers (recursions of 'Molecule').
-}
parseTXYZ :: Parser Molecule
parseTXYZ = do
  _nAtoms     <- skipHorizontalSpace *> (decimal :: Parser Int)
  label       <- skipHorizontalSpace *> takeWhile (not . isEndOfLine) <* skipSpace
  conAndAtoms <- many1 txyzLineParser
  let atoms = IntMap.fromList . map (\((atomNumber, _), atom) -> (atomNumber, atom)) $ conAndAtoms
      bonds =
        HashMap.fromList
          . concatMap
              ( (\(origin, targets) ->
                  zip (zip (List.repeat origin) (IntSet.toList targets)) (List.repeat True)
                )
              . fst
              )
          $ conAndAtoms
  return Molecule { _molecule_Comment           = label
                  , _molecule_Atoms             = atoms
                  , _molecule_Bonds             = bonds
                  , _molecule_SubMol            = IntMap.empty
                  , _molecule_Fragment          = IntMap.empty
                  , _molecule_EnergyDerivatives = def
                  , _molecule_CalcContext       = Map.empty
                  }
 where
  -- Parsing a single line of atomSeq. Tinker's format keeps bonds associated with atomSeq. So a tuple
  -- suitable to construct the 'IntMap' is returned additional to the pure atomSeq.
  txyzLineParser :: Parser ((Int, IntSet), Atom)
  txyzLineParser = do
    index           <- skipHorizontalSpace *> decimal
    cElement        <- skipHorizontalSpace *> many1 letter
    x               <- skipHorizontalSpace *> double
    y               <- skipHorizontalSpace *> double
    z               <- skipHorizontalSpace *> double
    mFFType         <- skipHorizontalSpace *> maybeOption (decimal :: Parser Int)
    connectivityRaw <- skipHorizontalSpace *> many' columnDecimal
    endOfLine
    return
      ( (index, IntSet.fromList connectivityRaw)
      , Atom
        { _atom_Element     = fromMaybe H . readMaybe $ cElement
        , _atom_Label       = ""
        , _atom_IsPseudo    = False
        , _atom_IsCapped    = False
        , _atom_IsDummy     = True
        , _atom_FFType      = case mFFType of
                                Nothing -> FFTXYZ 0
                                Just a  -> FFTXYZ a
        , _atom_Coordinates = VectorS . Massiv.fromList Seq $ [x, y, z]
        , _atom_Multipoles  = def
        }
      )
  -- Parse multiple non-line-breaking whitespace separated decimalSeq.
  columnDecimal :: Parser Int
  columnDecimal = skipHorizontalSpace *> decimal <* skipHorizontalSpace

----------------------------------------------------------------------------------------------------
{-|
Parse the "interesting" fields of a MOL2 file. This contains partial charges as well as
connectivity. There is no special understanding for the atom types, that are available in MOL2
fileSeq. They will simply be treated as the force field string. See
<http://chemyang.ccnu.edu.cn/ccb/server/AIMMS/mol2.pdf>.
-}
parseMOL2 :: Parser Molecule
parseMOL2 = do
  (label, nAtoms, nBonds) <- moleculeParser
  atomsLabeled            <- atomParser nAtoms
  bonds                   <- bondParser nBonds
  let
    -- Construct the top layer of the molecule.
      atoms = IntMap.fromList . RIO.toList . fmap (\(ind, _, atom) -> (ind, atom)) $ atomsLabeled
      -- From the groups of same substructure ID fragments, build fragments.
      fragments = makeFragmentsFromAnnoAtoms atomsLabeled
  return Molecule { _molecule_Comment           = label
                  , _molecule_Atoms             = atoms
                  , _molecule_Bonds             = bonds
                  , _molecule_SubMol            = IntMap.empty
                  , _molecule_Fragment          = fragments
                  , _molecule_EnergyDerivatives = def
                  , _molecule_CalcContext       = Map.empty
                  }
 where
   -- Parse the @<TRIPOS>MOLECULE block of MOL2.
  moleculeParser :: Parser (Text, Int, Maybe Int)
  moleculeParser = do
    _header    <- manyTill anyChar (string "@<TRIPOS>MOLECULE") <* endOfLine
    -- Line 1 -> "mol_name"
    label      <- takeWhile (not . isEndOfLine) <* endOfLine
    -- Line 2 -> "num_atoms [num_bonds [num_subst [num_feat [num_sets]]]]"
    nAtoms     <- skipHorizontalSpace *> decimal
    nBonds     <- maybeOption $ skipHorizontalSpace *> decimal
    _nSubMols  <- maybeOption $ skipHorizontalSpace *> (decimal :: Parser Int)
    _nFeatures <- maybeOption $ skipHorizontalSpace *> (decimal :: Parser Int)
    _nSets     <-
      maybeOption
      $  skipHorizontalSpace
      *> (decimal :: Parser Int)
      <* skipHorizontalSpace
      <* endOfLine
    -- Line 3 -> "mol_type"
    _molType <-
      skipHorizontalSpace
      *>  string "SMALL"
      <|> string "BIOPOLYMER"
      <|> string "PROTEIN"
      <|> string "NUCLEIC_ACID"
      <|> string "SACCHARIDE"
      <*  skipSpace
    -- Line 4 -> "charge_type"
    _chargeType <-
      skipSpace
      *> (   string "NO_CHARGES"
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
    _comment   <- maybeOption $ skipHorizontalSpace *> takeWhile (not . isEndOfLine) <* skipSpace
    return (label, nAtoms, nBonds)
  --
  -- Parse the @<TRIPOS>ATOM block of MOL2. This will give:
  -- (index of the atom, (substructure ID, substructure name), atom).
  atomParser :: Int -> Parser (Seq (Int, (Int, Text), Atom))
  atomParser nAtoms = do
    _header <- manyTill anyChar (string "@<TRIPOS>ATOM") <* endOfLine
    -- Parse multiple lines of ATOM data.
    atoms   <- count nAtoms $    -- atomLineParser
                              do
      index   <- skipHorizontalSpace *> decimal
      -- Most often this will be the element symbol.
      label   <- skipHorizontalSpace *> takeWhile (not . isHorizontalSpace)
      -- x, y and z coordinates
      x       <- skipHorizontalSpace *> double
      y       <- skipHorizontalSpace *> double
      z       <- skipHorizontalSpace *> double
      -- Parse the chemical element, which is actually the first part of the SYBYL atom type.
      cElem   <- skipHorizontalSpace *> many1 letter
      -- A dot often separates the element from the type of this element.
      ffdot   <- maybeOption $ char '.'
      -- And after the dot the rest of the SYBYL atom type might come.
      ffType  <- maybeOption $ takeWhile (not . isHorizontalSpace)
      -- The substructure ID. This is the identifier to identify sub moleculeSeq.
      subID   <- skipHorizontalSpace *> (decimal :: Parser Int)
      -- The substructure Name. This should be used as label for the sub molecule.
      subName <- skipHorizontalSpace *> takeWhile (not . isHorizontalSpace)
      -- The partial charge
      pCharge <- skipHorizontalSpace *> double <* skipSpace
      -- Construct the force field string for the 'MOL2' field.
      let mol2FFText =
            Text.pack cElem
              <> (case ffdot of
                   Nothing -> ""
                   Just _  -> "."
                 )
              <> fromMaybe "" ffType
      return
        ( index
        , (subID, subName)
        , Atom { _atom_Element     = fromMaybe H . readMaybe $ cElem
               , _atom_Label       = label
               , _atom_IsPseudo    = False
               , _atom_IsCapped    = False
               , _atom_IsDummy     = False
               , _atom_FFType      = FFMol2 mol2FFText
               , _atom_Coordinates = VectorS . Massiv.fromList Seq $ [x, y, z]
               , _atom_Multipoles  = def & multipole_Monopole ?~ pCharge
               }
        )
    return $ Seq.fromList atoms
  --
  -- Parse the @<TRIPOS>BOND part. Unfortunately, the bonds in the MOL2 format are unidirectiorial
  -- and need to be flipped to.
  bondParser :: Maybe Int -> Parser (HashMap (Int, Int) Bool)
  bondParser nBonds = do
    let
        -- How often to parse bond fields depends on if the number of bonds has been specified.
        nParser = case nBonds of
          Nothing -> many'
          Just n  -> count n
    _header  <- manyTill anyChar (string "@<TRIPOS>BOND") <* endOfLine
    uniBonds <- nParser $ do
      -- Bond id, which does not matter.
      _id    <- skipHorizontalSpace *> (decimal :: Parser Int)
      -- Origin atom index
      origin <- skipHorizontalSpace *> (decimal :: Parser Int)
      -- Target atom index
      target <- skipHorizontalSpace *> (decimal :: Parser Int)
      -- Bond type, which we don't care about.
      _type  <- skipHorizontalSpace *> takeWhile (not . isSpace) <* skipSpace
      return (origin, target)
    let
      -- Make the bonds bidirectorial
        bondTuplesForth = uniBonds
        bondTuplesBack  = swap <$> bondTuplesForth
        bondTuplesBoth  = zip (bondTuplesForth <> bondTuplesBack) (List.repeat True)
        bonds           = HashMap.fromList bondTuplesBoth
    return bonds

----------------------------------------------------------------------------------------------------
{-|
Parse a PDB file as described in
<ftp://ftp.wwpdb.org/pub/pdb/doc/format_descriptions/Format_v33_A4.pdf>. If parsing of a single ATOM
or CONETC line fails, the parser will stop there and ignore all the other records of same type,
directly coming after the failed one.

/This is not an entirely valid PDB parser. You will run into problems for very large structures,
/where atom indices exist multiple timeSeq./
-}
parsePDB :: Parser Molecule
parsePDB = do
  -- Parse the COMPND field as a label. Only the first line of COMPND will be used.
  label <- maybeOption $ do
    _             <- manyTill anyChar (string "HEADER")
    compoundLabel <- skipHorizontalSpace *> takeWhile (not . isEndOfLine)
    return compoundLabel
  -- Parse atoms only and ignore other fiels
  atomsLabeled <- Seq.fromList <$> many1 atomParser
  -- Parse the bonds to the tuple structure.
  bondTuples   <- many' connectParser
  -- links <- undefined --many' linkParser
  let
      -- Transform the informations from the parsers.
      bonds = HashMap.fromList $ zip (List.concat bondTuples) (List.repeat True)
      atomsIM = IntMap.fromList . RIO.toList . fmap (\(ind, _, atom) -> (ind, atom)) $ atomsLabeled
      fragments = makeFragmentsFromAnnoAtoms atomsLabeled
  return Molecule { _molecule_Comment           = fromMaybe "" label
                  , _molecule_Atoms             = atomsIM
                  , _molecule_Bonds             = bonds
                  , _molecule_SubMol            = IntMap.empty
                  , _molecule_Fragment          = fragments
                  , _molecule_EnergyDerivatives = def
                  , _molecule_CalcContext       = Map.empty
                  }
 where
   -- This parser works a little bit different than the others, as this is a fixed columnd witdth
   -- format and we can't rely on white spaces or fields really containing a value. A tuple of
   -- following structure is returned: (Index of atom, (subMolID, subMolName), atom)
  atomParser :: Parser (Int, (Int, Text), Atom)
  atomParser = do
    -- First check, that this line really starts with an ATOM or HETATM record (6 characters).
    -- Columns 1-6: record type.
    _recordStart <- Text.pack <$> manyTill anyChar (string "\nATOM  " <|> string "\nHETATM")
    -- Then take the rest of the line, till the end of line is reached.
    recordRest   <- takeWhile (not . isEndOfLine)
    let
      -- Recombine the line and split it according to the PDB format specifier.
      atomLine                = "ATOM  " <> recordRest
      -- Now according to the PDB specification. Fields exaclty named as in the PDF with "c" as
      -- prefix for chunk.
      -- Columns 1-6: record type.
      (_cAtom      , rest1  ) = Text.splitAt 6 atomLine
      -- Column 7-11: atom serial number
      (cSerial     , rest2  ) = Text.splitAt 5 rest1
      -- Column 13-16: atom name
      (cName       , rest3  ) = Text.splitAt 4 . Text.drop 1 $ rest2
      -- Column 17: alternate location indicator
      (_cAltLoc    , rest4  ) = Text.splitAt 1 rest3
      -- Columnt 18-20: residue name (use this as submolecule label)
      (cResName    , rest5  ) = Text.splitAt 3 rest4
      -- Column 22: chain identifier
      (cChainID    , rest6  ) = Text.splitAt 1 . Text.drop 1 $ rest5
      -- Column 23-26: residue sequence number
      (cResSeq     , rest7  ) = Text.splitAt 4 rest6
      -- Column 27: Code for insertion of residue
      (_cICode     , rest8  ) = Text.splitAt 1 rest7
      -- Column 31-38: Orthogonal coordinates for x in Angstrom
      (cX          , rest9  ) = Text.splitAt 8 . Text.drop 3 $ rest8
      -- Column 39-46: Orthogonal coordinates for y in Angstrom
      (cY          , rest10 ) = Text.splitAt 8 rest9
      -- Column 47-54: Orthogonal coordinates for z in Angstrom
      (cZ          , rest11 ) = Text.splitAt 8 rest10
      -- Column 55-60: Occupancy
      (_cOccupancy , rest12 ) = Text.splitAt 6 rest11
      -- Column 61-66: temperature factor
      (_cTempFactor, rest13 ) = Text.splitAt 6 . Text.drop 10 $ rest12
      -- Column 77-78: element
      (cElement    , rest14 ) = Text.splitAt 2 rest13
      -- Columnt 79-80: charge on the atom
      (cCharge     , _rest15) = Text.splitAt 2 rest14
      --
      -- Now parse all fields of interest using text readerSeq.
      aSerial                 = fst <$> (Text.decimal . Text.strip $ cSerial)
      aResSeq                 = fst <$> (Text.decimal . Text.strip $ cResSeq)
      aChainID =
        let cID = map ((\x -> x - 65) . fromEnum) . Text.unpack . Text.toUpper $ cChainID
        in  case cID of
              []      -> 0
              (x : _) -> x
      -- Modify the residue sequence ID with the chainID, to avoid combining fragments with same
      -- ID but different chainSeq. Add 10000 per letter (A=0, B=10000, C=20000, ...) to avoid
      -- wrong fragmentation. The ResSeq is not unique and can occur multiple times in different
      -- chainSeq.
      aChainResSeq = (\x -> x + aChainID * 10000) <$> aResSeq
      aElement =
        let elemMaybe =
                (readMaybe :: String -> Maybe Element)
                  . Text.unpack
                  . Text.toTitle
                  . Text.strip
                  $ cElement
        in  case elemMaybe of
              Nothing -> Left "parsePDB: Could not read the element symbol."
              Just e  -> Right e
      aLabel  = Text.strip cName
      aFFType = FFPDB aLabel
      aPCharge =
        let pChargeMaybe = fst <$> (Text.double . Text.strip $ cCharge)
        in  case pChargeMaybe of
              Left  _  -> Nothing
              Right pC -> Just pC
      aCoordinates = VectorS . Massiv.fromList Seq <$> traverse
        (fmap fst . Text.double . Text.strip)
        [cX, cY, cZ]
      -- Use the Atom constructor applicatively to get Either String Atom. This relies on the
      -- order of the atom argumentSeq.
      eitherAtom =
        Atom
          <$> aElement                                    -- _atom_Element
          <*> pure aLabel                                 -- _atom_Label
          <*> pure False                                  -- _atom_IsPseudo
          <*> pure False                                  -- _atom_IsCapped
          <*> pure False                                  -- _atom_IsDummy
          <*> pure aFFType                                -- _atom_FFType
          <*> aCoordinates                                -- _atom_Coordinates
          <*> pure (def & multipole_Monopole .~ aPCharge) -- _atom_Multipoles
    -- If all the non-Attoparsec parse actions succeeded, return an Attoparsec result or fail with
    -- an Attoparsec error.
    case (aSerial, (aChainResSeq, cResName), eitherAtom) of
      (Right ind   , (Right sInd, _)  , Right a  ) -> return (ind, (sInd, cResName), a)
      (Left  indErr, _                , _        ) -> fail indErr
      (_           , (Left sIndErr, _), _        ) -> fail sIndErr
      (_           , _                , Left aErr) -> fail aErr
  --
  -- Parse CONECT fields of the PDB. PDB bonds are bidirectorial, so no swapping required.
  connectParser :: Parser [(Int, Int)]
  connectParser = do
    -- First check, that this line really starts with a CONECT record (6 characters).
    -- Columns 1-6: record type.
    _recordStart <- Text.pack <$> manyTill anyChar (string "CONECT")
    -- Then take the rest of the line, till the end of line is reached.
    recordRest   <- takeWhile (not . isEndOfLine) <* endOfLine
    let
        -- Recombine the line and split them according to PDB standard.
        conectLine         = "CONECT" <> recordRest
        -- Columns 1-6: "CONECT"
        (_cConect, rest1 ) = Text.splitAt 6 conectLine
        -- Columns 7-11: serial of origin atom.
        (cOrigin , rest2 ) = Text.splitAt 5 rest1
        -- Columns 12-16, 17-21, 22-26,27-31: serial of a target atomSeq.
        (cTarget1, rest3 ) = Text.splitAt 5 rest2
        (cTarget2, rest4 ) = Text.splitAt 5 rest3
        (cTarget3, rest5 ) = Text.splitAt 5 rest4
        (cTarget4, _rest6) = Text.splitAt 5 rest5
        --
        -- Now parse all fields of interest using text readerSeq.
        pOrigin            = fst <$> (Text.decimal . Text.strip $ cOrigin)
        pTargets :: Either String [Int]
        pTargets =
          sequence
            . filter isRight
            . map (fmap fst . Text.decimal . Text.strip)
            $ [cTarget1, cTarget2, cTarget3, cTarget4]
    case (pOrigin, pTargets) of
      (Right o   , Right t  ) -> return $ zip (List.repeat o) t
      (Left  oErr, _        ) -> fail oErr
      (_         , Left tErr) -> fail tErr
