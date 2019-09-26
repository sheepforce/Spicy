{-|
Module      : Spicy.Molecule.Internal.Parser
Description : Parsers for chemical data formats and computational chemistry output files.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides parsers for chemical file formats. For the internal representation no parser is
provided, as this is a JSON structured file, which should be parsed by
[aeson](http://hackage.haskell.org/package/aeson)'s @decodeEither@.
-}
{-# LANGUAGE OverloadedStrings #-}

module Spicy.Molecule.Internal.Parser
  ( -- * Chemical Data Formats
    -- $chemicalFormats
    parseXYZ
  , parseTXYZ
  , parseMOL2
  , parsePDB
  )
where

import           Control.Applicative

import           Control.Lens            hiding ( index )
import           Data.Attoparsec.Text.Lazy
import           Data.Char
import           Data.Either
import           Data.Foldable
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import qualified Data.List                     as L
import           Data.Maybe
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as S
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Read           as TL
import           Data.Tuple
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
import           Text.Read

import           Spicy.Generic
import           Spicy.Molecule.Internal.Types
import           Spicy.Molecule.Internal.Util

{-|
Parse a .xyz file (has no connectivity, atom types or partioal charges).
-}
parseXYZ :: Parser Molecule
parseXYZ = do
  nAtoms <- skipHorizontalSpace *> decimal <* skipHorizontalSpace <* endOfLine
  label  <- skipHorizontalSpace *> takeWhile (not . isEndOfLine) <* endOfLine
  atoms  <- count nAtoms xyzLineParser
  return Molecule { _molecule_Label    = textS2L label
                  , _molecule_Atoms    = IM.fromList $ zip [1 ..] atoms
                  , _molecule_Bonds    = IM.empty
                  , _molecule_SubMol   = S.empty
                  , _molecule_Energy   = Nothing
                  , _molecule_Gradient = Nothing
                  , _molecule_Hessian  = Nothing
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
                , _atom_FFType      = XYZ
                , _atom_PCharge     = Nothing
                , _atom_Coordinates = S.fromList [x, y, z]
                }

----------------------------------------------------------------------------------------------------
{-|
Parse a Tinker XYZ formatted file. It has coordinates and might have connectivity and atom types.
This format and therefore parser are not using any layers (recursions of 'Molecule').
-}
parseTXYZ :: Parser Molecule
parseTXYZ = do
  _nAtoms     <- skipHorizontalSpace *> (decimal :: Parser Int)
  label       <- skipHorizontalSpace *> takeWhile (not . isEndOfLine) <* skipSpace
  conAndAtoms <- many1 txyzLineParser
  return Molecule
    { _molecule_Label    = textS2L label
    , _molecule_Atoms    = IM.fromList . map (\a -> (a ^. _1 . _1, a ^. _2)) $ conAndAtoms
    , _molecule_Bonds    = IM.fromList . map fst $ conAndAtoms
    , _molecule_SubMol   = S.empty
    , _molecule_Energy   = Nothing
    , _molecule_Gradient = Nothing
    , _molecule_Hessian  = Nothing
    }
 where
  -- Parsing a single line of atoms. Tinker's format keeps bonds associated with atoms. So a tuple
  -- suitable to construct the 'IntMap' is returned additional to the pure atoms.
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
      ( (index, IS.fromList connectivityRaw)
      , Atom
        { _atom_Element     = fromMaybe H . readMaybe $ cElement
        , _atom_Label       = ""
        , _atom_IsPseudo    = False
        , _atom_FFType      = case mFFType of
                                Nothing -> TXYZ 0
                                Just a  -> TXYZ a
        , _atom_PCharge     = Nothing
        , _atom_Coordinates = S.fromList [x, y, z]
        }
      )
  -- Parse multiple non-line-breaking whitespace separated decimals.
  columnDecimal :: Parser Int
  columnDecimal = skipHorizontalSpace *> decimal <* skipHorizontalSpace

----------------------------------------------------------------------------------------------------
{-|
Parse the "interesting" fields of a MOL2 file. This contains partial charges as well as
connectivity. There is no special understanding for the atom types, that are available in MOL2
files. They will simply be treated as the force field string. See
<http://chemyang.ccnu.edu.cn/ccb/server/AIMMS/mol2.pdf>.
-}
parseMOL2 :: Parser Molecule
parseMOL2 = do
  (label, nAtoms, nBonds) <- moleculeParser
  atomsLabeled            <- atomParser nAtoms
  bonds                   <- bondParser nBonds
  let
    -- Construct the top layer of the molecule.
      atoms   = IM.fromList . toList . fmap (\(ind, _, atom) -> (ind, atom)) $ atomsLabeled
      -- From the groups of same substructure ID fragments, build molecules.
      subMols = makeSubMolsFromAnnoAtoms atomsLabeled bonds
  return Molecule { _molecule_Label    = label
                  , _molecule_Atoms    = atoms
                  , _molecule_Bonds    = bonds
                  , _molecule_SubMol   = subMols
                  , _molecule_Energy   = Nothing
                  , _molecule_Gradient = Nothing
                  , _molecule_Hessian  = Nothing
                  }
 where
   -- Parse the @<TRIPOS>MOLECULE block of MOL2.
  moleculeParser :: Parser (TL.Text, Int, Maybe Int)
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
    return (textS2L label, nAtoms, nBonds)
  --
  -- Parse the @<TRIPOS>ATOM block of MOL2. This will give:
  -- (index of the atom, (substructure ID, substructure name), atom).
  atomParser :: Int -> Parser (Seq (Int, (Int, TL.Text), Atom))
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
      -- The substructure ID. This is the identifier to identify sub molecules.
      subID   <- skipHorizontalSpace *> (decimal :: Parser Int)
      -- The substructure Name. This should be used as label for the sub molecule.
      subName <- skipHorizontalSpace *> takeWhile (not . isHorizontalSpace)
      -- The partial charge
      pCharge <- skipHorizontalSpace *> double <* skipSpace
      -- Construct the force field string for the 'MOL2' field.
      let mol2FFText =
            (TL.pack cElem)
              `TL.append` (case ffdot of
                            Nothing -> ""
                            Just _  -> "."
                          )
              `TL.append` (fromMaybe "" $ textS2L <$> ffType)
      return
        ( index
        , (subID, textS2L subName)
        , Atom { _atom_Element     = fromMaybe H . readMaybe $ cElem
               , _atom_Label       = textS2L label
               , _atom_IsPseudo    = False
               , _atom_FFType      = Mol2 mol2FFText
               , _atom_PCharge     = Just pCharge
               , _atom_Coordinates = S.fromList [x, y, z]
               }
        )
    return $ S.fromList atoms
  --
  -- Parse the @<TRIPOS>BOND part. Unfortunately, the bonds in the MOL2 format are unidirectiorial
  -- and need to be flipped to.
  bondParser :: Maybe Int -> Parser (IntMap IntSet)
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
        bondTupleSeq    = S.fromList uniBonds
        bondTuplesForth = bondTupleSeq
        bondTuplesBack  = swap <$> bondTuplesForth
        bondTuplesBoth  = bondTuplesForth <> bondTuplesBack
        bonds           = groupTupleSeq bondTuplesBoth
    return bonds

----------------------------------------------------------------------------------------------------
{-|
Parse a PDB file as described in
<ftp://ftp.wwpdb.org/pub/pdb/doc/format_descriptions/Format_v33_A4.pdf>. If parsing of a single ATOM
or CONETC line fails, the parser will stop there and ignore all the other records of same type,
directly coming after the failed one.

/This is not an entirely valid PDB parser. You will run into problems for very large structures,
/where atom indices exist multiple times./
-}
parsePDB :: Parser Molecule
parsePDB = do
  -- Parse the COMPND field as a label. Only the first line of COMPND will be used.
  label <- maybeOption $ do
    _             <- manyTill anyChar (string "HEADER")
    compoundLabel <- skipHorizontalSpace *> takeWhile (not . isEndOfLine)
    return $ textS2L compoundLabel
  -- Parse atoms only and ignore other fiels
  atomsLabeled  <- S.fromList <$> many1 atomParser
  -- Parse the bonds to the tuple structure. Need to be joinded to get more than 4 bonds for strange
  -- cases, before constructing the IMIS.
  bondRedundant <- many' connectParser
  -- links <- undefined --many' linkParser
  let
    -- Transform the informations from the parsers.
      atomsIM = IM.fromList . toList . fmap (\(ind, _, atom) -> (ind, atom)) $ atomsLabeled
      subMols            = makeSubMolsFromAnnoAtoms atomsLabeled bonds
      -- Join bond tuples with same origin and then build the IntMap IntSet structure from it. This
      -- avoids losing different targets, for origins, that are multiply defined, as PDB allows
      -- maximum 4 bond targets per line.
      bondsSortOnOrigin  = L.sortOn fst $ bondRedundant
      bondsGroupByOrigin = L.groupBy (\a b -> fst a == fst b) bondsSortOnOrigin
      bondsOriginsCombined =
        map (foldr' (\(o, t) (_, tA) -> (o, t <> tA)) (0, IS.empty)) bondsGroupByOrigin
      bonds = IM.fromList bondsOriginsCombined
  return Molecule { _molecule_Label    = fromMaybe "" label
                  , _molecule_Atoms    = atomsIM
                  , _molecule_Bonds    = bonds
                  , _molecule_SubMol   = subMols
                  , _molecule_Energy   = Nothing
                  , _molecule_Gradient = Nothing
                  , _molecule_Hessian  = Nothing
                  }
 where
   -- This parser works a little bit different than the others, as this is a fixed columnd witdth
   -- format and we can't rely on white spaces or fields really containing a value. A tuple of
   -- following structure is returned: (Index of atom, (subMolID, subMolName), atom)
  atomParser :: Parser (Int, (Int, TL.Text), Atom)
  atomParser = do
    -- First check, that this line really starts with an ATOM or HETATM record (6 characters).
    -- Columns 1-6: record type.
    _recordStart <- TL.pack <$> manyTill anyChar (string "\nATOM  " <|> string "\nHETATM")
    -- Then take the rest of the line, till the end of line is reached.
    recordRest   <- textS2L <$> takeWhile (not . isEndOfLine)
    let
      -- Recombine the line and split it according to the PDB format specifier.
        atomLine                = "ATOM  " `TL.append` recordRest
        -- Now according to the PDB specification. Fields exaclty named as in the PDF with "c" as
        -- prefix for chunk.
        -- Columns 1-6: record type.
        (_cAtom      , rest1  ) = TL.splitAt 6 atomLine
        -- Column 7-11: atom serial number
        (cSerial     , rest2  ) = TL.splitAt 5 rest1
        -- Column 13-16: atom name
        (cName       , rest3  ) = TL.splitAt 4 . TL.drop 1 $ rest2
        -- Column 17: alternate location indicator
        (_cAltLoc    , rest4  ) = TL.splitAt 1 rest3
        -- Columnt 18-20: residue name (use this as submolecule label)
        (cResName    , rest5  ) = TL.splitAt 3 rest4
        -- Column 22: chain identifier
        (cChainID    , rest6  ) = TL.splitAt 1 . TL.drop 1 $ rest5
        -- Column 23-26: residue sequence number
        (cResSeq     , rest7  ) = TL.splitAt 4 rest6
        -- Column 27: Code for insertion of residue
        (_cICode     , rest8  ) = TL.splitAt 1 rest7
        -- Column 31-38: Orthogonal coordinates for x in Angstrom
        (cX          , rest9  ) = TL.splitAt 8 . TL.drop 3 $ rest8
        -- Column 39-46: Orthogonal coordinates for y in Angstrom
        (cY          , rest10 ) = TL.splitAt 8 rest9
        -- Column 47-54: Orthogonal coordinates for z in Angstrom
        (cZ          , rest11 ) = TL.splitAt 8 rest10
        -- Column 55-60: Occupancy
        (_cOccupancy , rest12 ) = TL.splitAt 6 rest11
        -- Column 61-66: temperature factor
        (_cTempFactor, rest13 ) = TL.splitAt 6 . TL.drop 10 $ rest12
        -- Column 77-78: element
        (cElement    , rest14 ) = TL.splitAt 2 rest13
        -- Columnt 79-80: charge on the atom
        (cCharge     , _rest15) = TL.splitAt 2 rest14
        --
        -- Now parse all fields of interest using text readers.
        aSerial                 = fst <$> (TL.decimal . TL.strip $ cSerial)
        aResSeq                 = fst <$> (TL.decimal . TL.strip $ cResSeq)
        aChainID =
          let cID = map ((\x -> x - 65) . fromEnum) . TL.unpack . TL.toUpper $ cChainID
          in  case cID of
                []      -> 0
                (x : _) -> x
        -- Modify the residue sequence ID with the chainID, to avoid combining fragments with same
        -- ID but different chains. Add 10000 per letter (A=0, B=10000, C=20000, ...) to avoid
        -- wrong fragmentation. The ResSeq is not unique and can occur multiple times in different
        -- chains.
        aChainResSeq = (\x -> x + aChainID * 10000) <$> aResSeq
        aElement =
          let
            elemMaybe =
              (readMaybe :: String -> Maybe Element) . TL.unpack . TL.toTitle . TL.strip $ cElement
          in  case elemMaybe of
                Nothing -> Left "parsePDB: Could not read the element symbol."
                Just e  -> Right e
        aLabel  = TL.strip cName
        aFFType = PDB aLabel
        aPCharge =
          let pChargeMaybe = fst <$> (TL.double . TL.strip $ cCharge)
          in  case pChargeMaybe of
                Left  _  -> Nothing
                Right pC -> Just pC
        aCoordinates = traverse (fmap fst . TL.double . TL.strip) . S.fromList $ [cX, cY, cZ]
        -- Use the Atom constructor applicatively to get Either String Atom. This relies on the
        -- order of the atom arguments.
        eitherAtom =
          Atom
            <$> aElement            -- _atom_Element
            <*> pure aLabel         -- _atom_Label
            <*> pure False          -- _atom_IsPseudo
            <*> pure aFFType        -- _atom_FFType
            <*> pure aPCharge       -- _atom_FFType
            <*> aCoordinates        -- _atom_Coordinates
    -- If all the non-Attoparsec parse actions succeeded, return an Attoparsec result or fail with
    -- an Attoparsec error.
    case (aSerial, (aChainResSeq, cResName), eitherAtom) of
      (Right ind   , (Right sInd, _)  , Right a  ) -> return (ind, (sInd, cResName), a)
      (Left  indErr, _                , _        ) -> fail indErr
      (_           , (Left sIndErr, _), _        ) -> fail sIndErr
      (_           , _                , Left aErr) -> fail aErr
  --
  -- Parse CONECT fields of the PDB. PDB bonds are bidirectorial, so no swapping required.
  connectParser :: Parser (Int, IntSet)
  connectParser = do
    -- First check, that this line really starts with a CONECT record (6 characters).
    -- Columns 1-6: record type.
    _recordStart <- TL.pack <$> manyTill anyChar (string "CONECT")
    -- Then take the rest of the line, till the end of line is reached.
    recordRest   <- textS2L <$> takeWhile (not . isEndOfLine) <* endOfLine
    let
        -- Recombine the line and split them according to PDB standard.
        conectLine         = "CONECT" `TL.append` recordRest
        -- Columns 1-6: "CONECT"
        (_cConect, rest1 ) = TL.splitAt 6 conectLine
        -- Columns 7-11: serial of origin atom.
        (cOrigin , rest2 ) = TL.splitAt 5 rest1
        -- Columns 12-16, 17-21, 22-26,27-31: serial of a target atoms.
        (cTarget1, rest3 ) = TL.splitAt 5 rest2
        (cTarget2, rest4 ) = TL.splitAt 5 rest3
        (cTarget3, rest5 ) = TL.splitAt 5 rest4
        (cTarget4, _rest6) = TL.splitAt 5 rest5
        --
        -- Now parse all fields of interest using text readers.
        pOrigin            = fst <$> (TL.decimal . TL.strip $ cOrigin)
        pTargets :: Either String IntSet
        pTargets =
          fmap IS.fromList
            . sequence
            . filter isRight
            . map (fmap fst . TL.decimal . TL.strip)
            $ [cTarget1, cTarget2, cTarget3, cTarget4]
    case (pOrigin, pTargets) of
      (Right o   , Right t  ) -> return (o, t)
      (Left  oErr, _        ) -> fail oErr
      (_         , Left tErr) -> fail tErr
