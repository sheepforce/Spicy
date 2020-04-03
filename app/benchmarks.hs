{-
Benchmarks of Spicy functions
-}
import           Criterion.Main
import           RIO

main :: IO ()
main = defaultMain []

{-
main = defaultMain
  [ benchmarkMath
  , benchmarkParser
  , benchmarkMolecularSystem
  ]
----------------------------------------------------------------------------------------------------
-- Generate test data
{-|
Generate a very simple repetitive trajectory in XYZ format.
-}
generateTrajectoryXYZ ::
     Int  -- ^ Number of atoms to generate.
  -> Int  -- ^ Number of trajectory frames to generate.
  -> Text -- ^ Trajectory in XYZ format.
generateTrajectoryXYZ nAtoms nFrames = T.pack frames
  where
    atomLine = "Rb   1.00000  -15.031654 0.00354\n"
    frame =
      (show nAtoms) ++ "\n" ++
      "Comment\n" ++
      ( concat $
        replicate nAtoms atomLine
      )
    frames =
      concat $
      replicate nFrames frame

{-|
Replicate phosphinin n times in space, so that a set of molecules in space is formed, which is in
the shape of a rectangle. The molecules will be nonbonded.
-}
generateNonbondedMolecules :: Int -> Molecule
generateNonbondedMolecules nMols = superMolecule
  where
    molecule = Molecule
      { _molecule_Label = "Phosphinin"
      , _molecule_Atoms = atomsCarbons ++ atomsHydrogen ++ atomsPhosphor
      , _molecule_Bonds = I.empty
      , _molecule_Energy = Nothing
      , _molecule_Gradient = Nothing
      , _molecule_Hessian = Nothing
      }
    templateAtom = Atom
        { _atom_Element = undefined
        , _atom_Label = ""
        , _atom_IsPseudo = False
        , _atom_FFType = ""
        , _atom_PCharge = Nothing
        , _atom_Coordinates = undefined
        }
    atomsCarbons = zipWith (\a c -> a & atom_Coordinates .~ c & atom_Element .~ C)
      (replicate 5 templateAtom)
      [ ( 1.18770,  0.66347,  2.50000)
      , ( 1.20847,  2.06088,  2.50000)
      , (-1.18770,  0.66348,  2.50000)
      , ( 0.00000,  2.76139,  2.50000)
      , (-1.20847,  2.06088,  2.50000)
      ]
    atomsHydrogen = zipWith (\a c -> a & atom_Coordinates .~ c & atom_Element .~ H)
      (replicate 5 templateAtom)
      [ (-2.15024,  2.59423,  2.50000)
      , ( 0.00000,  3.84372,  2.50000)
      , (-2.11521,  0.10669,  2.50000)
      , ( 2.11521,  0.10669,  2.50000)
      , ( 2.15024,  2.59423,  2.50000)
      ]
    atomsPhosphor = zipWith (\a c -> a & atom_Coordinates .~ c & atom_Element .~ P)
      (replicate (1 :: Int) templateAtom)
      [ (0.00000,  0.00000,  2.50000)
      ]
    shiftVec =
      [ (\(x, y, z) -> (6.0 * fromIntegral x, 6.0 * fromIntegral y, 3.0 *fromIntegral z)) i
      | i <- (\x y z -> (x, y, z)) <$>
          [1 .. l] <*>
          [1 .. l] <*>
          [1 .. l]
      ]
      where
        l = ceiling . (**(1/3)) . fromIntegral $ nMols
    superMolecule =
      molecule & molecule_Atoms .~
      ( concat . map _molecule_Atoms
      $ zipWith (shiftFragment) (take nMols shiftVec) (repeat molecule)
      )

{-|
As 'generateNonbondedMolecules' but the result will be bonded 'Molecule's.
-}
generateBondedMolecules :: Int -> Molecule
generateBondedMolecules nMols = guessBonds (Just 1.1) $ generateNonbondedMolecules nMols

{-|
{-|
As 'generateNonbondedMolecules' but the result will be split into fragments as a 'SuperMolecule'.
-}
-}
generateFragmentedMolecules :: Int -> Maybe SuperMolecule
generateFragmentedMolecules nMols = fragmentMolecule RemoveAll $ generateBondedMolecules nMols
-}
----------------------------------------------------------------------------------------------------
{-
-- Benchmark for math
benchmarkMath :: Benchmark
-}

----------------------------------------------------------------------------------------------------
{-
-- Benchmarks for the parsers
benchmarkParser :: Benchmark
benchmarkParser = bgroup
  "Parser"
  [ benchmarkParserXYZ
  ]

benchmarkParserXYZ :: Benchmark
benchmarkParserXYZ = bgroup
  "XYZ Trajectory"
  [ bench "100 atoms, 100 frames" $ nf (testCase 100) 100
  , bench "100 atoms, 1000 frames" $ nf (testCase 100) 1000
  , bench "1000 atoms, 100 frames" $ nf (testCase 1000) 100
  ]
  where
    testCase nAtoms nFrames = (parse (many1 parseXYZ)) (generateTrajectoryXYZ nAtoms nFrames)


----------------------------------------------------------------------------------------------------
-- Benchmarks MolecularSystem
benchmarkMolecularSystem :: Benchmark
benchmarkMolecularSystem = bgroup
  "Molecular system"
  [ benchmarkGuessBonds
  , benchmarkIsolateLayer
  , benchmarkFragmentMolecule
  , benchmarkWrapFragmentsToBox
  , benchmarkReplicateSystemAlongAxis
  , benchmarkFilterByCriteria
  ]
----------------------------------------------------------------------------------------------------
benchmarkGuessBonds :: Benchmark
benchmarkGuessBonds = bgroup
  "Guess bonds"
  [ bench "Small (10 molecules)" $ nf testCase 10
  , bench "Medium (50 molecules)" $ nf testCase 50
  , bench "Large (100 molecules)" $ nf testCase 100
  ]
  where
    testCase nMols = (guessBonds (Just 1.2)) (generateNonbondedMolecules nMols)
----------------------------------------------------------------------------------------------------
benchmarkIsolateLayer :: Benchmark
benchmarkIsolateLayer = bgroup
  "Isolate ONIOM layer"
  [ bench "Small (10 molecules)" $ nf (testCase 30) 10
  , bench "Medium (50 molecules)" $ nf (testCase 150) 50
  , bench "Large (100 molecules)" $ nf (testCase 300) 100
  ]
  where
    testCase nAtoms nMols =
      (isolateLayer [0 .. nAtoms] Nothing Nothing) (generateNonbondedMolecules nMols)
----------------------------------------------------------------------------------------------------
benchmarkFragmentMolecule :: Benchmark
benchmarkFragmentMolecule = bgroup
  "Fragment molecule"
  [ bench "Small (10 molecules)" $ nf testCase 10
  , bench "Medium (50 molecules)" $ nf testCase 50
  , bench "Medium (100 molecules)" $ nf testCase 100
  ]
  where
    testCase nMols =
      (fragmentMolecule RemoveAll) (generateBondedMolecules 10)
----------------------------------------------------------------------------------------------------
benchmarkWrapFragmentsToBox :: Benchmark
benchmarkWrapFragmentsToBox = bgroup
  "Wrap fragments"
  [ bench "Small (10 molecules)" $ nf testCase 10
  , bench "Medium (50 molecules)" $ nf testCase 50
  , bench "Large (100 molecules)" $ nf testCase 100
  ]
  where
    testCase nMols =
      (wrapFragmentsToBox (10.0, 10.0, 10.0) <$>) (generateFragmentedMolecules nMols)
----------------------------------------------------------------------------------------------------
benchmarkReplicateSystemAlongAxis :: Benchmark
benchmarkReplicateSystemAlongAxis = bgroup
  "Replicate system"
  [ bench "Small (10 molecules)" $ nf testCase 10
  , bench "Medium (50 molecules)" $ nf testCase 50
  , bench "Large (100 molecules)" $ nf testCase 100
  ]
  where
    testCase nMols =
      (replicateSystemAlongAxis (10.0, 10.0, 10.0) AxisX) (generateNonbondedMolecules nMols)
----------------------------------------------------------------------------------------------------
benchmarkFilterByCriteria :: Benchmark
benchmarkFilterByCriteria = bgroup
  "Filter by criteria"
  [ bench "10 frames, 10 molecules" $ nf (testCase 10) 10
  , bench "10 frames, 50 molecules" $ nf (testCase 10) 50
  , bench "10 frames, 100 molecules" $ nf (testCase 10) 100
  , bench "50 frames, 10 molecules" $ nf (testCase 50) 10
  , bench "50 frames, 50 molecules" $ nf (testCase 50) 50
  , bench "50 frames, 100 molecules" $ nf (testCase 50) 100
  , bench "100 frames, 10 molecules" $ nf (testCase 100) 10
  , bench "100 frames, 50 molecules" $ nf (testCase 100) 50
  , bench "100 frames, 100 molecules" $ nf (testCase 100) 100
  ]
  where
    testCase nFrames nMols =
      filterByCriteria
      [ fromMaybe False <$> (criterionDistance (2, 15) (< 5.0)) ]
      (replicate nFrames (generateNonbondedMolecules nMols))
-}
