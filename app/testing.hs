{-
Unit testing and golden testing (unit testing based on files) for Spicy. Test all the difficult
parts of Spicy. This is especially Spicy.MolecularSystem and Spicy.Parser.
All tests are required to pass. There is no gray zone!!
-}
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Attoparsec.Text.Lazy
import qualified Data.ByteString.Lazy          as B
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Spicy.Generic
import           Spicy.Math
import           Spicy.Molecule
import           System.FilePath         hiding ( (<.>) )
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [testParser, testMath, testWriter]

{-
####################################################################################################
-}
{- $math
-}
{-|
These test are unit tests for Math parallel and serial computations. Parallel accelerate
calculations are embedded by TemplateHaskell as compiled LLVM code by means of 'A.runQ'. This serves
also as a test for a correct build, as the system is somewhat sensitive with all its dependencies.
-}
testMath :: TestTree
testMath = testGroup "Math" [testDotProduct, testVLength, testVDistance, testVAngle, testVCross]

{-
====================================================================================================
-}
{-|
Dot product of 2 simple vectors.
-}
testDotProduct :: TestTree
testDotProduct =
  let vecA       = S.fromList [1, 2, 3] :: Seq Double
      vecB       = S.fromList [-7, 8, 9] :: Seq Double
      dotProduct = 36
  in  testCase "Vector Dot Product" $ vecA <.> vecB @?= dotProduct

----------------------------------------------------------------------------------------------------
{-|
Length of a vector.
-}
testVLength :: TestTree
testVLength =
  let vecA = S.fromList [2, 4, 4] :: Seq Double
      len  = 6
  in  testCase "Vector Length" $ vLength vecA @?= len

----------------------------------------------------------------------------------------------------
{-|
Distance between 2 points.
-}
testVDistance :: TestTree
testVDistance =
  let vecA = S.fromList [0, 0, 10] :: Seq Double
      vecB = S.fromList [0, 0, 0] :: Seq Double
      dist = 10
  in  testCase "Vectors Distance" $ vDistance vecA vecB @?= dist

----------------------------------------------------------------------------------------------------
{-|
Anlge between two vectors.
-}
testVAngle :: TestTree
testVAngle =
  let vecA  = S.fromList [0, 0, 1] :: Seq Double
      vecB  = S.fromList [0, 1, 0] :: Seq Double
      angle = 0.5 * pi
  in  testCase "Vectors Angle" $ vAngle vecA vecB @?= angle

----------------------------------------------------------------------------------------------------
{-|
Cross product between two R3 vectors.
-}
testVCross :: TestTree
testVCross =
  let vecA = S.fromList [0, 0, 1] :: Seq Double
      vecB = S.fromList [0, 1, 0] :: Seq Double
      vecC = S.fromList [-1, 0, 0] :: Seq Double
  in  testCase "Vectors Cross Product" $ (vCross vecA vecB :: Maybe (Seq Double)) @?= Just vecC


{-
####################################################################################################
-}
{- $molecule
These tests are golden tests within the Tasty framework. If one of these tests fail, you are in
trouble, as all the others will rely on working parsers and are golden tests, too.
-}

{-
====================================================================================================
-}
{- $moleculeParser
-}
{-|
Data type to define common parameters for processing the 'Spicy.Parser' test cases.
-}
data ParserEnv = ParserEnv
  { peTestName   :: String
  , peGoldenFile :: FilePath
  , peInputFile  :: FilePath
  , peOutputFile :: FilePath
  , peParser     :: Parser Molecule
  }

----------------------------------------------------------------------------------------------------
{-|
This provided the IO action for 'goldenVsFile' for testing of the parsers.
-}
peParseAndWrite :: ReaderT ParserEnv IO ()
peParseAndWrite = do
  -- Get the environment informations.
  env <- ask
  -- Read the input file.
  raw <- liftIO $ T.readFile (peInputFile env)
  -- Parse the input file.
  mol <- parse' (peParser env) raw
  -- Write the internal representation of the parser result to JSON file.
  liftIO . T.writeFile (peOutputFile env) . writeSpicy $ mol

----------------------------------------------------------------------------------------------------
{-|
Wrapper for 'goldenVsFile' for the 'Spicy.Parser' test cases.
-}
peGoldenVsFile :: ParserEnv -> TestTree
peGoldenVsFile env = goldenVsFile (peTestName env)
                                  (peGoldenFile env)
                                  (peOutputFile env)
                                  (runReaderT peParseAndWrite env)

----------------------------------------------------------------------------------------------------
testParser :: TestTree
testParser = testGroup
  "Parser"
  [ testParserTXYZ1
  , testParserXYZ1
  , testParserXYZ2
  , testParserXYZ3
  , testParserPDB1
  , testParserPDB2
  , testParserPDB3
  , testParserMOL21
  , testParserSpicy1
  ]

----------------------------------------------------------------------------------------------------
testParserTXYZ1 :: TestTree
testParserTXYZ1 =
  let testEnv = ParserEnv
        { peTestName   = "Tinker TXYZ (1)"
        , peGoldenFile = "goldentests"
                         </> "goldenfiles"
                         </> "RuKomplex__testParserTXYZ1.json.golden"
        , peInputFile  = "goldentests" </> "input" </> "RuKomplex.txyz"
        , peOutputFile = "goldentests" </> "output" </> "RuKomplex__testParserTXYZ1.json"
        , peParser     = parseTXYZ
        }
  in  peGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testParserXYZ1 :: TestTree
testParserXYZ1 =
  let testEnv = ParserEnv
        { peTestName   = "Molden XYZ (1)"
        , peGoldenFile = "goldentests"
                         </> "goldenfiles"
                         </> "FePorphyrine__testParserXYZ1.json.golden"
        , peInputFile  = "goldentests" </> "input" </> "FePorphyrine.xyz"
        , peOutputFile = "goldentests" </> "output" </> "FePorphyrine__testParserXYZ1.json"
        , peParser     = parseXYZ
        }
  in  peGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testParserXYZ2 :: TestTree
testParserXYZ2 =
  let testName      = "Molden XYZ Trajectory (1)"
      goldenFile    = "goldentests" </> "goldenfiles" </> "HeteroTraj__testParserXYZ2.json.golden"
      inputFile     = "goldentests" </> "input" </> "HeteroTraj.xyz"
      outputFile    = "goldentests" </> "output" </> "HeteroTraj__testParserXYZ2.json"
      parseAndWrite = do
        raw <- T.readFile inputFile
        case parse (many1 parseXYZ) raw of
          Done _ mol -> T.writeFile outputFile . T.concat . map writeSpicy $ mol
          Fail _ _ e -> T.writeFile outputFile . T.pack $ e
  in  goldenVsFile testName goldenFile outputFile parseAndWrite

----------------------------------------------------------------------------------------------------
testParserXYZ3 :: TestTree
testParserXYZ3 =
  let testEnv = ParserEnv
        { peTestName   = "Molden XYZ (3)"
        , peGoldenFile = "goldentests" </> "goldenfiles" </> "Ferrocene__testParserXYZ3.json.golden"
        , peInputFile  = "goldentests" </> "input" </> "Ferrocene.xyz"
        , peOutputFile = "goldentests" </> "output" </> "Ferrocene__testParserXYZ3.json"
        , peParser     = parseXYZ
        }
  in  peGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testParserPDB1 :: TestTree
testParserPDB1 =
  let testEnv = ParserEnv
        { peTestName   = "PDB 1HFE Hydrogenase (1)"
        , peGoldenFile = "goldentests" </> "goldenfiles" </> "1hfe__testParserPDB1.json.golden"
        , peInputFile  = "goldentests" </> "input" </> "1hfe.pdb"
        , peOutputFile = "goldentests" </> "output" </> "1hfe__testParserPDB1.json"
        , peParser     = parsePDB
        }
  in  peGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testParserPDB2 :: TestTree
testParserPDB2 =
  let testEnv = ParserEnv
        { peTestName   = "PDB 6CVR Aprataxin (2)"
        , peGoldenFile = "goldentests" </> "goldenfiles" </> "6cvr__testParserPDB2.json.golden"
        , peInputFile  = "goldentests" </> "input" </> "6cvr.pdb"
        , peOutputFile = "goldentests" </> "output" </> "6cvr__testParserPDB2.json"
        , peParser     = parsePDB
        }
  in  peGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testParserPDB3 :: TestTree
testParserPDB3 =
  let testEnv = ParserEnv
        { peTestName   = "PDB 4NDG Aprataxin (3)"
        , peGoldenFile = "goldentests" </> "goldenfiles" </> "4ndg__testParserPDB3.json.golden"
        , peInputFile  = "goldentests" </> "input" </> "4ndg.pdb"
        , peOutputFile = "goldentests" </> "output" </> "4ndg__testParserPDB3.json"
        , peParser     = parsePDB
        }
  in  peGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testParserMOL21 :: TestTree
testParserMOL21 =
  let testEnv = ParserEnv
        { peTestName   = "SyByl MOL2 (1)"
        , peGoldenFile = "goldentests" </> "goldenfiles" </> "Peptid__testParserMOL21.json.golden"
        , peInputFile  = "goldentests" </> "input" </> "Peptid.mol2"
        , peOutputFile = "goldentests" </> "output" </> "Peptid__testParserMOL21.json"
        , peParser     = parseMOL2
        }
  in  peGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testParserSpicy1 :: TestTree
testParserSpicy1 =
  let testName      = "Spicy JSON Parser (1)"
      goldenFile    = "goldentests" </> "goldenfiles" </> "6cvr__testParserSpicy1.json.golden"
      inputFile     = "goldentests" </> "input" </> "6cvr.json"
      outputFile    = "goldentests" </> "output" </> "6cvr__testParserSpicy1.json"
      parseAndWrite = do
        raw <- B.readFile inputFile
        case eitherDecode raw of
          Left  err   -> T.writeFile outputFile . T.pack $ err
          Right spicy -> T.writeFile outputFile . writeSpicy $ spicy
  in  goldenVsFile testName goldenFile outputFile parseAndWrite


{-
====================================================================================================
-}
{- $moleculeWriter
These tests are meant to check if the writers produce parsable formats. Parsing an "original" file
(from Babel, PDB, ...), writing it again and parsing the written result, should produce the same
'Molecule's.

The test scheme works as follows:

  - Read and parse an "original" file.
  - Write the so obtained representation to the Spicy JSON format (golden file).
  - Write the corresponding representation of the original file with the writer to test.
  - Read and parse the Spicy-written representation.
  - Write the result of the Spicy parsed-Spicy written orientation to the Spicy JSON format and
-}
{-
"mwe" = Spicy.Writer.Molecule environment
-}
testWriter :: TestTree
testWriter = testGroup "Writer" [testWriterMolecule]

----------------------------------------------------------------------------------------------------
{-|
Data type to define common parameters for processing the 'Molecule.Writer' test cases.
-}
data MolWriterEnv = MolWriterEnv
  { mweTestName   :: String                         -- ^ Name of the test case.
  , mweOrigFile   :: FilePath                       -- ^ Original input file of the current format,
                                                    --   created by an external program, such as
                                                    --   Babel.
  , mweGoldenJSON :: FilePath                       -- ^ The file storing the internal
                                                    --   representation of the original input file
                                                    --   after parsing with 'parser'.
  , mweWriterFile :: FilePath                       -- ^ The file with the representation, suitable
                                                    --   for external programs and the writer
                                                    --   representation of 'origInput' after
                                                    --   parsing.
  , mweWriterJSON :: FilePath                       -- ^ The internal representation of the
                                                    --   'Molecule' after parsing 'writerFile'
                                                    --   again.
  , mweParser     :: Parser Molecule                -- ^ A 'Parser' for the 'Molecule'.
  , mweWriter     :: Molecule -> Either SomeException Text -- ^ A writer for 'Molecule'
  }

----------------------------------------------------------------------------------------------------
{-|
Provides the IO action for 'goldenVsFile', writing the original representation in JSON and the
writer format, and the writer representation as JSON again. Compares the writer JSON representation
against the original JSON representation.
-}
mweParseWriteParseWrite :: ReaderT MolWriterEnv IO ()
mweParseWriteParseWrite = do
  -- Get the environment.
  env     <- ask
  -- Read and parse the original (external) file
  origRaw <- liftIO $ T.readFile (mweOrigFile env)
  -- Parse the original file.
  origMol <- parse' (mweParser env) origRaw
  -- Get the text representation of the original file.
  let origText = (mweWriter env) origMol
  -- Write the Spicy JSON representation of the original molecule (Golden File) and the writer (to
  -- test) representation of the original molecule.
  liftIO . T.writeFile (mweGoldenJSON env) . writeSpicy $ origMol
  case origText of
    Left  e -> liftIO . T.writeFile (mweWriterFile env) . T.pack . show $ e
    Right t -> liftIO . T.writeFile (mweWriterFile env) $ t
  -- Parse the writer result again with the supplied parser.
  writerRaw <- liftIO $ T.readFile (mweWriterFile env)
  -- Parse the result from the writer.
  writerMol <- parse' (mweParser env) writerRaw
  -- Get the text representation of the writer molecule and write to the Spicy JSON representation
  -- (Output file).
  liftIO . T.writeFile (mweWriterJSON env) . writeSpicy $ writerMol

----------------------------------------------------------------------------------------------------
{-|
This function provides a wrapper around 'goldenVsFile', tuned for the writer tests.
-}
mweGoldenVsFile :: MolWriterEnv -> TestTree
mweGoldenVsFile env = goldenVsFile (mweTestName env)
                                   (mweGoldenJSON env)
                                   (mweWriterJSON env)
                                   (runReaderT mweParseWriteParseWrite env)

----------------------------------------------------------------------------------------------------
testWriterMolecule :: TestTree
testWriterMolecule =
  testGroup "Molecule Formats" [testWriterXYZ1, testWriterTXYZ1, testWriterMOL21, testWriterPDB1]

----------------------------------------------------------------------------------------------------
testWriterXYZ1 :: TestTree
testWriterXYZ1 =
  let
    testEnv = MolWriterEnv
      { mweTestName   = "Molden XYZ (1)"
      , mweOrigFile   = "goldentests" </> "input" </> "FePorphyrine.xyz"
      , mweGoldenJSON = "goldentests"
                        </> "goldenfiles"
                        </> "FePorphyrine__testWriterXYZ1_Orig.json.golden"
      , mweWriterFile = "goldentests" </> "output" </> "FePorphyrine__testWriterXYZ1_Writer.xyz"
      , mweWriterJSON = "goldentests" </> "output" </> "FePorphyrine__testWriterXYZ1_Writer.json"
      , mweParser     = parseXYZ
      , mweWriter     = writeXYZ
      }
  in  mweGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testWriterTXYZ1 :: TestTree
testWriterTXYZ1 =
  let
    testEnv = MolWriterEnv
      { mweTestName   = "Tinker TXYZ (1)"
      , mweOrigFile   = "goldentests" </> "input" </> "SulfateInSolution.txyz"
      , mweGoldenJSON = "goldentests"
                        </> "goldenfiles"
                        </> "SulfateInSolution__testWriterTXYZ1_Orig.json.golden"
      , mweWriterFile = "goldentests"
                        </> "output"
                        </> "SulfateInSolution__testWriterTXYZ1_Writer.txyz"
      , mweWriterJSON = "goldentests"
                        </> "output"
                        </> "SulfateInSolution__testWriterTXYZ1_Writer.json"
      , mweParser     = parseTXYZ
      , mweWriter     = writeTXYZ
      }
  in  mweGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
testWriterMOL21 :: TestTree
testWriterMOL21 =
  let
    testEnv = MolWriterEnv
      { mweTestName   = "SyByl MOL2 (1)"
      , mweOrigFile   = "goldentests" </> "input" </> "FePorphyrine.mol2"
      , mweGoldenJSON = "goldentests"
                        </> "goldenfiles"
                        </> "FePorphyrine__testWriterMOL21_Orig.json.golden"
      , mweWriterFile = "goldentests" </> "output" </> "FePorphyrine__testWriterMOL21_Writer.mol2"
      , mweWriterJSON = "goldentests" </> "output" </> "FePorphyrine__testWriterMOL21_Writer.json"
      , mweParser     = parseMOL2
      , mweWriter     = writeMOL2
      }
  in  mweGoldenVsFile testEnv

----------------------------------------------------------------------------------------------------
{-|
Note, that changed atom indices can occur in strange PDBs. This is normal and this is wanted. This
test case therefore uses a "correctly" counting PDB, as 'writePDB' will reindex. Therefore you will
obtain an offset, if this PDB would not start counting from one continously.
-}
testWriterPDB1 :: TestTree
testWriterPDB1 =
  let testEnv = MolWriterEnv
        { mweTestName   = "PDB 4NDG Aprataxin (1)"
        , mweOrigFile   = "goldentests" </> "input" </> "Peptid.pdb"
        , mweGoldenJSON = "goldentests"
                          </> "goldenfiles"
                          </> "Peptid__testWriterPDB1_Orig.json.golden"
        , mweWriterFile = "goldentests" </> "output" </> "Peptid__testWriterPDB1_Writer.pdb"
        , mweWriterJSON = "goldentests" </> "output" </> "Peptid__testWriterPDB1_Writer.json"
        , mweParser     = parsePDB
        , mweWriter     = writePDB
        }
  in  mweGoldenVsFile testEnv
