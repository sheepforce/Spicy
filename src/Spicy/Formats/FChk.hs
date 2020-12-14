-- |
-- Module      : Spicy.Formats.FChk
-- Description : Functions to work with FChk files
-- Copyright   : Phillip Seeber, 2019
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides functions to read, manipulate and write FChk files.
module Spicy.Formats.FChk
  ( -- * Types
    -- $types
    FChk (..),

    -- * Parsers/Analysers
    -- $analysers
    getResultsFromFChk,
    fChk,

    -- * Manipulation
    -- $manipulation
    relabelLinkAtoms,

    -- * Writer
    -- $writer
    writeFChk,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Default
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Massiv.Array ()
import Data.Massiv.Array as Massiv hiding
  ( take,
    takeWhile,
  )
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Optics
import RIO hiding
  ( Builder,
    Vector,
    lens,
    take,
    takeWhile,
    (.~),
    (^.),
    (^?),
  )
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified RIO.Text.Lazy as TL
import Spicy.Common
import Spicy.Math
import Spicy.Molecule

-- $types

-- | The contents of an FChk.
data FChk = FChk
  { -- | Short title from the header of the FChk.
    title :: !Text,
    -- | Calculation type from the header.
    calcType :: !CalcType,
    -- | Primary orbital basis from the FChk header.
    basis :: !Text,
    -- | Calculation method (such as CCSD, HF, MP2) from the header.
    method :: !Text,
    -- | Labeled of 'Content's.
    blocks :: !(Map Text Content)
  }
  deriving (Eq, Show)

-- Lenses
instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "title" k FChk FChk a b where
  labelOptic = lens (\s -> title s) $ \s b -> s {title = b}

instance (k ~ A_Lens, a ~ CalcType, b ~ a) => LabelOptic "calcType" k FChk FChk a b where
  labelOptic = lens (\s -> calcType s) $ \s b -> s {calcType = b}

instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "basis" k FChk FChk a b where
  labelOptic = lens (\s -> basis s) $ \s b -> s {basis = b}

instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "method" k FChk FChk a b where
  labelOptic = lens (\s -> method s) $ \s b -> s {method = b}

instance (k ~ A_Lens, a ~ Map Text Content, b ~ a) => LabelOptic "blocks" k FChk FChk a b where
  labelOptic = lens (\s -> blocks s) $ \s b -> s {blocks = b}

----------------------------------------------------------------------------------------------------

-- | In the FChk format after 2 lines of header an arbitrary amount of contents will follow. These are
-- either scalar values or arrays of values. The arrays are always printed as vectors but might
-- actually refer to matrices. The meaning of the content blocks must be obtained by the label fields.
data Content
  = Scalar !ScalarVal
  | Array !ArrayVal
  deriving (Eq, Show)

-- Prisms
_Scalar :: Prism' Content ScalarVal
_Scalar = prism (\b -> Scalar b) $ \s -> case s of
  Scalar b -> Right b
  a -> Left a

_Array :: Prism' Content ArrayVal
_Array = prism (\b -> Array b) $ \s -> case s of
  Array b -> Right b
  a -> Left a

----------------------------------------------------------------------------------------------------

-- | Data types an array can hold.

-- The text value is very strange. A normal text is split into chunks of 12 or 8 characters and
-- print without separator. Therefore it appears as a normal text but for Fortran it is actually an
-- array of text fragments. I don't care about the crazy Fortran internals and treat it as a single
-- text, which it actually is.
data ArrayVal
  = ArrayInt !(Vector S Int)
  | ArrayDouble !(Vector S Double)
  | ArrayText !Text
  | ArrayLogical !(Vector S Bool)
  deriving (Eq, Show)

-- Prisms
_ArrayInt :: Prism' ArrayVal (Vector S Int)
_ArrayInt = prism (\b -> ArrayInt b) $ \s -> case s of
  ArrayInt b -> Right b
  a -> Left a

_ArrayDouble :: Prism' ArrayVal (Vector S Double)
_ArrayDouble = prism (\b -> ArrayDouble b) $ \s -> case s of
  ArrayDouble b -> Right b
  a -> Left a

_ArrayText :: Prism' ArrayVal Text
_ArrayText = prism (\b -> ArrayText b) $ \s -> case s of
  ArrayText b -> Right b
  a -> Left a

_ArrayLogical :: Prism' ArrayVal (Vector S Bool)
_ArrayLogical = prism (\b -> ArrayLogical b) $ \s -> case s of
  ArrayLogical b -> Right b
  a -> Left a

----------------------------------------------------------------------------------------------------

-- | Possible scalar values in an FChk.
data ScalarVal
  = ScalarInt !Int
  | ScalarDouble !Double
  | ScalarText !Text
  | ScalarLogical !Bool
  deriving (Eq, Show)

-- Prisms
_ScalarInt :: Prism' ScalarVal Int
_ScalarInt = prism' (\b -> ScalarInt b) $ \s -> case s of
  ScalarInt b -> Just b
  _ -> Nothing

_ScalarDouble :: Prism' ScalarVal Double
_ScalarDouble = prism' (\b -> ScalarDouble b) $ \s -> case s of
  ScalarDouble b -> Just b
  _ -> Nothing

_ScalarText :: Prism' ScalarVal Text
_ScalarText = prism' (\b -> ScalarText b) $ \s -> case s of
  ScalarText b -> Just b
  _ -> Nothing

_ScalarLogical :: Prism' ScalarVal Bool
_ScalarLogical = prism' (\b -> ScalarLogical b) $ \s -> case s of
  ScalarLogical b -> Just b
  _ -> Nothing

----------------------------------------------------------------------------------------------------

-- | Possible types of calculation types from an FChk file.
data CalcType
  = -- | Single point
    SP
  | -- | Full optimisation to minimum
    FOPT
  | -- | Partial optimisation to an minimum
    POPT
  | -- | Full optimisation to a transition state
    FTS
  | -- | Partial optimisation to a transition state
    PTS
  | -- | Full optimisation to a saddle point of higher order
    FSADDLE
  | -- | Partial optimisation to a saddle point of higher order
    PSADDLE
  | -- | Energy + gradient calculation
    FORCE
  | -- | Frequency calculation (2nd derivative)
    FREQ
  | -- | Potential energy surface scan
    SCAN
  | -- | Generates molecular orbitals only, also orbital localisation
    GUESS
  | -- | Linear synchronous transit calculation
    LST
  | -- | SCF stability analysis
    STABILITY
  | -- | Generate archive information from checkpoint file
    REARCHIVE
  | -- | Generate archive information from checkpoint file
    MSRESTART
  | -- | Compound models such as G2, G3, ...
    MIXED
  deriving (Eq, Show)

{-
####################################################################################################
-}

-- $analysers

-- | A function that uses information from FChk files to obtain 'CalcOutput'. The function will not
-- fill in a value ('Just') if it cannot be found in the FChk and fail if the FChk cannot be parsed
-- or a block occurs multiple times.
getResultsFromFChk :: MonadThrow m => Text -> m CalcOutput
getResultsFromFChk content = do
  -- Parse the complete FChk. The Hessian is obtained in row major order as a vector representing
  -- the lower triangular matrix and needs to be expanded to the full square matrix here.
  fchk <- parse' fChk content
  let fchkBlocks = fchk ^. #blocks
      energy' = fchkBlocks Map.!? "Total Energy"
      gradient' = fchkBlocks Map.!? "Cartesian Gradient"
      hessianContent = fchkBlocks Map.!? "Cartesian Force Constants"

  let hessianLTVec = hessianContent ^? _Just % _Array % _ArrayDouble
  hessian' <- case hessianLTVec of
    Just vec -> Just . MatrixS <$> ltMat2Square vec
    Nothing -> return Nothing

  return
    CalcOutput
      { multipoles = def,
        energyDerivatives =
          EnergyDerivatives
            { energy = energy' ^? _Just % _Scalar % _ScalarDouble,
              gradient = VectorS <$> gradient' ^? _Just % _Array % _ArrayDouble,
              hessian = hessian'
            }
      }

----------------------------------------------------------------------------------------------------

-- | Parser for Gaussian Formatted Checkpoint files version 3. See
-- <http://wild.life.nctu.edu.tw/~jsyu/compchem/g09/g09ur/f_formchk.htm> for details.
fChk :: Parser FChk
fChk = do
  -- Line 1: "Initial 72 characters of the title section"
  initTitle <- takeWhile (not <$> isEndOfLine) <* endOfLine

  -- Line2: "Type, Method, Basis"
  -- Calculation type (format: A10)
  typeString <- Text.toUpper . Text.strip <$> take 10
  readType <- case typeString of
    "SP" -> return SP
    "FOPT" -> return FOPT
    "POPT" -> return POPT
    "FTS" -> return FTS
    "PTS" -> return PTS
    "FSADDLE" -> return FSADDLE
    "PSADDLE" -> return PSADDLE
    "FORCE" -> return FORCE
    "FREQ" -> return FREQ
    "SCAN" -> return SCAN
    "GUESS=ONLY" -> return GUESS
    "LST" -> return LST
    "STABILITY" -> return STABILITY
    "REARCHIVE" -> return REARCHIVE
    "MS-RESTART" -> return MSRESTART
    "REARCHIVE/MS-RESTART" -> return REARCHIVE
    "MIXED" -> return MIXED
    _ -> fail "Could not assign calculation string to calculation type."
  -- Calculation method such as MP2 (format: A30)
  methodString <- Text.strip <$> take 30
  -- Basis set (format: A30)
  basisString <- Text.strip <$> (takeWhile (not <$> isEndOfLine) <* endOfLine)
  -- An arbitrary amount of scalar fields or array blocks with labels
  content <- many1 (scalar <|> array)
  return
    FChk
      { title = initTitle,
        calcType = readType,
        basis = basisString,
        method = methodString,
        blocks = Map.fromList content
      }

----------------------------------------------------------------------------------------------------

-- |
-- Parser for 'Scalar' fields in FChk files.
scalar :: Parser (Text, Content)
scalar = do
  label <- Text.strip <$> take 40 <* count 3 (char ' ')
  typeChar <- take 1 <* count 5 (char ' ')
  value <- case typeChar of
    "I" -> ScalarInt <$> (skipHorizontalSpace *> decimal)
    "R" -> ScalarDouble <$> (skipHorizontalSpace *> double)
    "C" -> ScalarText <$> (skipHorizontalSpace *> takeWhile (not <$> isEndOfLine))
    "L" -> do
      textBool <- skipHorizontalSpace *> (string "T" <|> string "F")
      case textBool of
        "T" -> return $ ScalarLogical True
        "F" -> return $ ScalarLogical False
        _ -> fail "Could not parse boolean scalar expression."
    _ -> fail "Unknown identifier for scalar expression."
  skipHorizontalSpace
  endOfLine
  return (label, Scalar value)

----------------------------------------------------------------------------------------------------

-- | Parser for 'Array' fields in FChk files.

-- Text arrays appear in no way to be actual arrays of strings. Nevertheles for Fortran they are, as
-- the format for those is 5A12 per line (C) or 9A8 (H). In the FChk they appear as a single normal
-- string, but need to be parsed in those chunks to comply with the strange Fortran string handling.
-- This format makes me wanna cry ...
array :: Parser (Text, Content)
array = do
  label <- Text.strip <$> take 40 <* count 3 (char ' ')
  typeChar <- (char 'I' <|> char 'R' <|> char 'C' <|> char 'H' <|> char 'L') <* count 3 (char ' ')
  _ <- string "N="
  nElements <- skipHorizontalSpace *> decimal <* endOfLine
  values <- case typeChar of
    'I' -> do
      -- Integer arrays, safely separated by spaces.
      intVals <- count nElements $ skipSpace *> signed decimal
      endOfLine
      return . ArrayInt . Massiv.fromList Par $ intVals
    'R' -> do
      -- Real arrays, safely separated by spaces.
      doubleVals <- count nElements $ skipSpace *> double
      endOfLine
      return . ArrayDouble . Massiv.fromList Par $ doubleVals
    'C' -> do
      -- The horror of Fortran string parsing begins ...
      -- let linesToExcpect = nElements `div` 5 + if nElements `mod` 5 /= 0 then 1 else 0
      --     elementsInLastLine = nElements `mod` 5
      textChunks <- count nElements (take 12 <* option () endOfLine)
      return . ArrayText . Text.concat $ textChunks
    'H' -> do
      -- Another strange fortran format for chunks of text.
      textChunks <- count nElements (take 9 <* option () endOfLine)
      return . ArrayText . Text.concat $ textChunks
    'L' -> do
      -- Logical arrays. Single characters and no separation.
      boolChars <- count nElements ((char 'T' <|> char 'F') <* option () endOfLine)
      bools <-
        traverse
          ( \c -> case c of
              'T' -> return True
              'F' -> return False
              other -> fail $ "Character \"" <> [other] <> "\" cannot be parsed as Boolean."
          )
          boolChars
      return . ArrayLogical . Massiv.fromList Par $ bools
    _ -> fail $ "Character \"" <> [typeChar] <> " \" is not a valid type character. Cannot parse."
  return (label, Array values)

{-
####################################################################################################
-}

-- $manipulation

-- | This function replaces the element of all link atoms by a different one (one that is not in the
-- fchk). This is dirty but
-- FChks cannot contain labels, that would allow to delete
relabelLinkAtoms ::
  MonadThrow m =>
  -- | Element to use as the link atoms in the FChks.
  Element ->
  -- | All atoms that are in the FChk from the original structure.
  -- Atoms that are labels there will be renamed in the FChk.
  IntMap Atom ->
  -- | Original FChk
  FChk ->
  m FChk
relabelLinkAtoms newLinkElem atoms fchk = do
  -- Common bindings.
  let blocks = fchk ^. #blocks
      atomicNumbersKey = "Atomic numbers"
      sparse2Dense = getSparse2Dense atoms
      linkAtomsSparse = IntMap.keysSet . IntMap.filter (isAtomLink . isLink) $ atoms
      linkAtomsDense = intReplaceSet sparse2Dense linkAtomsSparse
      newLinkElemNumber = fromEnum newLinkElem + 1

  -- Obtain initial values.
  let nAtomsMol = IntMap.size atoms

  nAtomsFChk <-
    maybe2MThrow (localExcp "Could not find number of Atoms in the FChk.") $
      (blocks Map.!? "Number of atoms") >>= (^? _Scalar % _ScalarInt)

  atomicNumbersFChk <-
    maybe2MThrow (localExcp "Could not find atomic numbers in the FChk.") $
      (blocks Map.!? atomicNumbersKey) >>= (^? _Array % _ArrayInt)

  -- Sanity checks
  unless (nAtomsMol == nAtomsFChk) . throwM . localExcp $
    "Number of atoms in the molecule and FChk do not match."

  -- Construct a new Atomic Numbers block for the FChk.
  let newAtomicNumbers =
        Massiv.computeP
          . Massiv.imap
            ( \i e ->
                if i `IntSet.member` linkAtomsDense
                  then newLinkElemNumber
                  else e
            )
          $ atomicNumbersFChk

  -- Build a new FChk and return it.
  return $ fchk & #blocks % ix atomicNumbersKey .~ (Array . ArrayInt $ newAtomicNumbers)
  where
    localExcp = MolLogicException "relabelLinkAtoms"

    -- Conversion from sparse (IntMap) indices to 0 based dense indices.
    getSparse2Dense :: IntMap a -> IntMap Int
    getSparse2Dense origMap =
      let keys = IntMap.keys origMap
       in IntMap.fromAscList $ List.zip keys [0 ..]

{-
####################################################################################################
-}

-- $writer

-- | A function to construct the FChk as a textual object from its internal representation.
-- Unfortunately, the order of the content blocks seems to be important and it is not entirely clear
-- what the correct order is.
writeFChk :: FChk -> Text
writeFChk fchk =
  let --Header line by line
      titleB = (fA 72 $ fchk ^. #title) <> nL

      infoLineB =
        (fA 10 . tShow $ fchk ^. #calcType)
          <> (fA 30 $ fchk ^. #method)
          <> (fA 30 $ fchk ^. #basis)
          <> nL

      -- CONTENT BLOCKS
      cntntBlks = fchk ^. #blocks

      -- Some blocks are expected in a given order apparently. The order of the hardcoded blocks is
      -- given here.
      blockOrder :: [Text]
      blockOrder =
        [ "Number of atoms",
          "Charge",
          "Multiplicity",
          "Number of electrons",
          "Number of alpha electrons",
          "Number of beta electrons",
          "Number of basis functions",
          "Number of independent functions",
          "Atomic numbers",
          "Nuclear charges",
          "Current cartesian coordinates"
        ]

      orderedBlocks =
        let localMap = Map.restrictKeys cntntBlks $ Set.fromList blockOrder
            keyVec = Massiv.fromList Par blockOrder :: Vector B Text
         in Massiv.foldMono
              (\k -> fromMaybe mempty . fmap (blockWriter k) $ localMap Map.!? k)
              keyVec

      unorderedOtherBlocks =
        let localMap = Map.withoutKeys cntntBlks $ Set.fromList blockOrder
         in Map.foldlWithKey' (\acc k v -> acc <> blockWriter k v) mempty localMap

      fchkBuilder =
        titleB
          <> infoLineB
          <> orderedBlocks
          <> unorderedOtherBlocks
   in TL.toStrict . TB.toLazyText $ fchkBuilder
  where
    nL = TB.singleton '\n'

----------------------------------------------------------------------------------------------------

-- | A writer for FChk content blocks.
blockWriter :: Text -> Content -> Builder
blockWriter label value = case value of
  Scalar a -> scalarWriter label a
  Array a -> arrayWriter label a

----------------------------------------------------------------------------------------------------

-- | A writer for FChk Scalars.
scalarWriter :: Text -> ScalarVal -> Builder
scalarWriter label value = case value of
  ScalarInt a -> (fA 40 label) <> (fX 3) <> (fA 1 "I") <> (fX 5) <> (fI 12 a) <> "\n"
  ScalarDouble a -> (fA 40 label) <> (fX 3) <> (fA 1 "R") <> (fX 5) <> (fE 22 15 a) <> "\n"
  ScalarText a -> (fA 40 label) <> (fX 3) <> (fA 1 "C") <> (fX 5) <> (fA 12 a) <> "\n"
  ScalarLogical a -> (fA 40 label) <> (fX 3) <> (fA 1 "L") <> (fX 5) <> (fA 12 (if a then "T" else "F")) <> "\n"

----------------------------------------------------------------------------------------------------

-- | A writer for FChk Arrays. Text arrays are always written in @C@ type instead of @H@ type.
arrayWriter :: Text -> ArrayVal -> Builder
arrayWriter label value = case value of
  ArrayInt a ->
    let nElems = Massiv.elemsCount a
        elemChunks =
          Massiv.ifoldMono
            (\i e -> (fI 12 e) <> if (i + 1) `mod` 6 == 0 || i == nElems - 1 then "\n" else mempty)
            a
     in (fA 40 label) <> (fX 3) <> (fA 1 "I") <> (fX 3) <> "N=" <> (fI 12 nElems) <> "\n"
          <> elemChunks
  ArrayDouble a ->
    let nElems = Massiv.elemsCount a
        elemsChunks =
          Massiv.ifoldMono
            (\i e -> (fE 16 8 e) <> if (i + 1) `mod` 5 == 0 || i == nElems - 1 then "\n" else mempty)
            a
     in (fA 40 label) <> (fX 3) <> (fA 1 "R") <> (fX 3) <> "N=" <> (fI 12 nElems) <> "\n"
          <> elemsChunks
  ArrayText a ->
    let vectors :: Vector B Text
        vectors = Massiv.fromList Par . Text.chunksOf 12 $ a
        elemsChunks =
          Massiv.ifoldMono
            (\i e -> fA 12 e <> if (i + 1) `mod` 5 == 0 || i == nElems - 1 then "\n" else mempty)
            vectors
        nElems = Text.length a `div` 12 + 1
     in (fA 40 label) <> (fX 3) <> (fA 1 "C") <> (fX 3) <> "N=" <> (fI 12 nElems) <> "\n"
          <> elemsChunks
  ArrayLogical a ->
    let nElems = Massiv.elemsCount a
        elemsChunks =
          Massiv.ifoldMono
            (\i e -> (fL 1 e) <> if (i + 1) `mod` 72 == 0 || i == nElems - 1 then "\n" else mempty)
            a
     in (fA 40 label) <> (fX 3) <> (fA 1 "L") <> (fX 3) <> "N=" <> (fI 12 nElems) <> "\n"
          <> elemsChunks
