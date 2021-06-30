-- |
-- Module      : Spicy.Wrapper.Internal.Executor
-- Description : System calls to the quantum chemistry software.
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Provides the callers to quantum chemistry software.
module Spicy.Wrapper.Internal.Executor
  ( runCalculation,
  )
where

import qualified Data.ByteString.Lazy.Char8 as ByteStringLazy8
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Optics hiding (view)
import RIO hiding
  ( view,
    (.~),
    (^.),
    (^?),
  )
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as List
import RIO.Process
import Spicy.Common
import Spicy.Formats.FChk
import Spicy.Molecule
import Spicy.RuntimeEnv
import Spicy.Wrapper.Internal.Input.IO
import Spicy.Wrapper.Internal.Output.GDMA
import Spicy.Wrapper.Internal.Output.Generic
import Spicy.Wrapper.Internal.Output.XTB
import System.Path
  ( (<++>),
    (<.>),
    (</>),
  )
import qualified System.Path as Path
import qualified System.Path.Directory as Dir

logSource :: LogSource
logSource = "Wraper Executor"

----------------------------------------------------------------------------------------------------

-- | Run a given calculation of a molecule, given by a 'CalcID'. This updates the 'CalcOutput' of
-- this 'CalcID'.
runCalculation ::
  ( HasWrapperConfigs env,
    HasLogFunc env,
    HasMolecule env,
    HasProcessContext env
  ) =>
  CalcID ->
  RIO env Molecule
runCalculation calcID = do
  -- Create the lenses for accessing calculation and molecule.
  let calcLens = calcIDLensGen calcID

  -- Gather the information for this calculation.
  mol <- view moleculeL >>= readTVarIO
  (calcContext, _) <- maybe2MThrow (localExc "Requested to perform a calculation, which does not exist") $ getCalcByID mol calcID
  unless (isNothing $ calcContext ^. #output) . throwM . localExc $ "Requested to perform a calculation, which has already been performed."

  let permanentDir = getDirPathAbs $ calcContext ^. #input % #permaDir
      scratchDir = getDirPathAbs $ calcContext ^. #input % #scratchDir
      software = calcContext ^. #input % #software

  -- Create permanent and scratch directory
  liftIO $ Dir.createDirectoryIfMissing True permanentDir
  liftIO $ Dir.createDirectoryIfMissing True scratchDir

  -- Execute the wrapper on the input file.
  case software of
    Psi4 _ -> executePsi4 calcID
    XTB _ -> executeXTB calcID
    Turbomole _ -> executeTurbomole calcID

  -- Parse the output, that has been produced by the wrapper.
  calcOutput <- case software of
    Psi4 _ -> analysePsi4 calcID
    XTB _ -> analyseXTB calcID
    Turbomole _ -> analyseTurbomole calcID

  -- Insert the output, that has been obtained for this calculation into the corresponding CalcID.
  let molUpdated = mol & calcLens % #output ?~ calcOutput

  -- Write the updated molecule to the shared variable.
  return molUpdated
  where
    localExc = WrapperGenericException "runCalculation"

{-
####################################################################################################
-}

-- $executors
-- Functions to call computational chemistry software with the appropiate arguments. These functions
-- are not responsible for processing of the output.

-- | Run Psi4 on a given calculation. A Psi4 run will behave as follows:
--
-- - All permanent files will be in the 'calcInput_PermaDir'.
-- - The 'calcInput_ScratchDir' will be used for scratch files and keep temporary files after
--   execution.
-- - All files will be prefixed with the 'calcInput_PrefixName'.
-- - The output will be kept for analysis.
executePsi4 ::
  (HasWrapperConfigs env, HasMolecule env, HasLogFunc env, HasProcessContext env) =>
  -- | ID of the calculation to perform.
  CalcID ->
  RIO env ()
executePsi4 calcID = do
  -- Obtain the Psi4 wrapper.
  psi4Wrapper <-
    view (wrapperConfigsL % #psi4) >>= \wrapper -> case wrapper of
      Just w -> return . getFilePath $ w
      Nothing -> throwM $ WrapperGenericException "executePsi4" "Psi4 wrapper is not configured. Cannot execute."

  -- Create the calculation context lens.
  let calcLens = calcIDLensGen calcID

  -- Gather information for the execution of Psi4
  mol <- view moleculeL >>= readTVarIO
  let calcContextM = mol ^? calcLens
  calcContext <- case calcContextM of
    Nothing ->
      throwM $
        MolLogicException
          "runCalculation"
          "Requested to perform a calculation, which does not exist."
    Just cntxt -> return cntxt

  let permanentDir = getDirPathAbs $ calcContext ^. #input % #permaDir
      scratchDir = getDirPathAbs $ calcContext ^. #input % #scratchDir
      software = calcContext ^. #input % #software

  -- Check if this function is appropiate to execute the calculation at all.
  unless (software & isPsi4) $ do
    logErrorS
      "psi4"
      "A calculation should be done with the Psi4 driver function,\
      \ but the calculation is not a Psi4 calculation."
    throwM $
      SpicyIndirectionException
        "executePsi4"
        ( "Requested to execute Psi4 on calculation with CalcID "
            <> show calcID
            <> "but this is not a Psi4 calculation."
        )

  -- Write the Psi4 input.
  inputFilePath <- writeInputs calcID
  let outputFilePath = Path.replaceExtension inputFilePath ".out"

  -- Prepare the command line arguments to Psi4.
  let psi4CmdArgs =
        [ "--input=" <> Path.toString inputFilePath,
          "--output=" <> Path.toString outputFilePath,
          "--nthread=" <> show (calcContext ^. #input % #nThreads),
          "--scratch=" <> Path.toString scratchDir,
          "--prefix=" <> (calcContext ^. #input % #prefixName),
          "--messy"
        ]

  -- Debug logging before execution of Psi4.
  logDebugS "psi4" $ "Starting Psi4 with command line arguments: " <> displayShow psi4CmdArgs

  -- Launch the Psi4 process and read its stdout and stderr.
  (exitCode, psi4Out, psi4Err) <-
    withWorkingDir (Path.toString permanentDir) $
      proc
        (Path.toString psi4Wrapper)
        psi4CmdArgs
        readProcess

  -- Provide some information if something went wrong.
  unless (exitCode == ExitSuccess) $ do
    logErrorS "psi4" $ "Execution terminated abnormally. Got exit code: " <> displayShow exitCode
    logErrorS "psi4" $ "Error messages:\n" <> (displayBytesUtf8 . toStrictBytes $ psi4Err)
    logErrorS "psi4" $ "Stdout messsages:\n" <> (displayBytesUtf8 . toStrictBytes $ psi4Out)
    throwM $ WrapperGenericException "executePsi4" "Psi4 execution terminated abnormally."

-- | Run a given XTB calculation. This function will write its own input files.
--
-- Unlike other software, xtb does not seem to support the use of a dedicated
-- scratch directory. Hence, all files are kept in the permanent directory.
executeXTB ::
  (HasWrapperConfigs env, HasMolecule env, HasLogFunc env, HasProcessContext env) =>
  -- | ID of the calculation to perform.
  CalcID ->
  RIO env ()
executeXTB calcID = do
  -- Obtain the XTB wrapper.
  xtbWrapper <-
    view (wrapperConfigsL % #xtb) >>= \wrapper -> case wrapper of
      Just w -> return . getFilePath $ w
      Nothing -> throwM $ WrapperGenericException "executeXTB" "XTB wrapper is not configured. Cannot execute."

  -- Create the calculation context lens.
  let calcLens = calcIDLensGen calcID

  -- Gather information for the execution of XTB
  mol <- view moleculeL >>= readTVarIO

  calcContext <-
    maybe2MThrow
      (MolLogicException "runCalculation" "Requested to perform a calculation which does not exist.")
      (mol ^? calcLens)

  let thisSoftware = calcContext ^. #input % #software

  -- Check if this function is appropiate to execute the calculation at all.
  unless (thisSoftware & isXTB) $ do
    logErrorS
      "xtb"
      "A calculation should be done with the XTB driver function,\
      \ but the calculation is not a XTB calculation."
    throwM $
      SpicyIndirectionException
        "executeXTB"
        ( "Requested to execute XTB on calculation with CalcID "
            <> show calcID
            <> "but this is not a XTB calculation."
        )

  -- Write the input files for XTB
  inputFilePath <- writeInputs calcID
  let geomFilePath = Path.replaceExtension inputFilePath ".xyz"

  -- Prepare the command line arguments to XTB.
  let cmdTask = case calcContext ^. #input % #task of
        WTEnergy -> "--sp"
        WTGradient -> "--grad"
        WTHessian -> "--hess"
      xtbCmdArgs =
        [ "--json",
          cmdTask,
          Path.toString geomFilePath,
          "--input", -- This...
          Path.toString inputFilePath -- And this need to be separate, for reasons unknown to mankind.
          --"--parallel " <> show (calcContext ^. #input % #nProc),
        ]

  -- Debug logging before execution of XTB.
  logDebugS "xtb" $ "Starting XTB with command line arguments: " <> displayShow xtbCmdArgs

  -- Launch the XTB process and read its stdout and stderr.
  let permanentDir = getDirPathAbs $ calcContext ^. #input % #permaDir
  let nThreads = calcContext ^. #input % #nThreads
  (exitCode, xtbOut, xtbErr) <-
    withModifyEnvVars (Map.insert "OMP_NUM_THREADS" (tShow nThreads <> ",1"))
      . withWorkingDir (Path.toString permanentDir)
      $ proc
        (Path.toString xtbWrapper)
        xtbCmdArgs
        readProcess

  -- Writing an output file of the XTB run. 
  writeFileBinary (Path.toString $ permanentDir </> Path.relFile "xtb.log") . toStrictBytes $ xtbOut

  -- Provide some information if something went wrong.
  unless (exitCode == ExitSuccess) $ do
    logErrorS "xtb" $ "Execution terminated abnormally. Got exit code: " <> displayShow exitCode
    logErrorS "xtb" $ "Error messages:\n" <> (displayBytesUtf8 . toStrictBytes $ xtbErr)
    logErrorS "xtb" $ "Stdout messsages:\n" <> (displayBytesUtf8 . toStrictBytes $ xtbOut)
    throwM $ WrapperGenericException "executeXTB" "XTB execution terminated abnormally."

----------------------------------------------------------------------------------------------------

-- | Executes a given Turbomole calculation. Based on the associated QC Hamiltonian it will make
-- best attempts to figure out which of the various Turbomole executables need to be called.
executeTurbomole ::
  (HasWrapperConfigs env, HasMolecule env, HasLogFunc env, HasProcessContext env) =>
  CalcID ->
  RIO env ()
executeTurbomole calcID = do
  -- Obtain the turbomole installation bin directory, in which the executables are expected.
  turbomoleBinDir <-
    view (wrapperConfigsL % #turbomole) >>= \wrapper -> case wrapper of
      Just w -> return . getDirPath $ w
      Nothing -> throwM (localExc "Turbomole wrapper is not configured. Cannot execute.")
  turbomoleBindDirAbs <- liftIO . Path.genericMakeAbsoluteFromCwd $ turbomoleBinDir

  -- Look for all executables, that we need. If any cannot be found we must assume that the
  -- turbomole installation is broken and crash.
  let exes = ["dscf", "escf", "ridft", "ricc2", "rigrad", "egrad", "grad", "pnoccsd", "actual"]
  exesPaths <- fmap Map.fromList . forM exes $ \e -> do
    let exeAbsFile = turbomoleBindDirAbs </> Path.relFile e
    hasExe <- liftIO . Dir.doesFileExist $ turbomoleBinDir </> Path.relFile e
    unless hasExe $ do
      logErrorS logSourceS $ "Could not find Turbomole executable: " <> displayShow e
      throwM . localExc $ "Could not find executable " <> show exes
    return (e, exeAbsFile)

  -- Create the calculation context lens.
  let calcLens = calcIDLensGen calcID

  -- Gather information for a turbomole run.
  mol <- view moleculeL >>= readTVarIO
  calcContext <-
    maybe2MThrow (MolLogicException "runCalculation" "Requested to perform a calculation, which does not exist.") $
      mol ^? calcLens

  let permanentDir = getDirPathAbs $ calcContext ^. #input % #permaDir
      scratchDir = getDirPathAbs $ calcContext ^. #input % #scratchDir
      software = calcContext ^. #input % #software
      task = calcContext ^. #input % #task

  -- Check suitability and obtain the hamiltonian. The Hamiltonian is required
  hamiltonian <- case software of
    Turbomole h -> return h
    _ -> do
      logErrorS
        logSourceS
        "A calculation should be done with the Turbomole driver function,\
        \ but the calculation is not a Turbomole calculation."
      throwM . localExc $
        "Requested to execute Turbomole on a calculation with CalcID "
          <> show calcID
          <> " but this is not a Turbomole calculation."

  -- Write the Turbomole input file "control". Make a backup, which does not get modified.
  inputFilePath <- writeInputs calcID
  liftIO $ Dir.copyFile inputFilePath (Path.takeDirectory inputFilePath </> Path.relFile "control.bak")

  -- Step 1: Calculate reference wavefunctions.
  --   Figure out which turbomole executables to call. dscf and grad for HF and DFT without RI,
  --   ridft and rigrad otherwise
  case hamiltonian ^. #ri of
    Nothing -> do
      commonCaller permanentDir exesPaths "dscf"
      when (task /= WTEnergy) $ commonCaller permanentDir exesPaths "grad"
    Just RIJ -> do
      commonCaller permanentDir exesPaths "ridft"
      when (task /= WTEnergy) $ commonCaller permanentDir exesPaths "rigrad"
    Just RIJK -> do
      commonCaller permanentDir exesPaths "ridft"
      when (task /= WTEnergy) $ commonCaller permanentDir exesPaths "rigrad"
    Just (OtherRI _) -> do
      logErrorS logSourceS "No other RI methods availabel in Turbomole."
      throwM . localExc $ "Unknown RI settings for Turbomole."

  -- Step 2: Calculate correlated wavefunctions if requested.
  case hamiltonian ^. #corr of
    Nothing -> return ()
    Just Correlation {corrModule} -> case corrModule of
      Nothing -> do
        logErrorS logSourceS "No correlation module specified for Turbomole, but is required."
        throwM . localExc $ "Correlation module for Turbomole not specified."
      Just "pnoccsd" -> commonCaller permanentDir exesPaths "pnoccsd"
      Just "ricc" -> commonCaller permanentDir exesPaths "ricc"
      Just e -> do
        logErrorS logSourceS $ "Unknown correlation module " <> display e <> " for Turbomole."
        throwM . localExc $ "Unknown correlation module " <> show e <> " for Turbomole."

  -- Step 3: If excitations were requested and not already calculated by the correlation module,
  -- calculate them now.
  case (hamiltonian ^. #exc, hamiltonian ^. #corr) of
    (Just _, Nothing) -> commonCaller permanentDir exesPaths "escf"
    _ -> return ()
  where
    localExc = WrapperGenericException "executeTurbomole"
    logSourceS = "Turbomole"
    commonCaller permanentDir exeMap exeName = do
      logDebugS logSourceS $ "Calculating wavefunction with " <> displayShow exeName <> " ..."
      (exitCode, out, err) <-
        withWorkingDir (Path.toString permanentDir) $
          proc
            (Path.toString $ exeMap Map.! exeName)
            mempty
            readProcess
      unless (exitCode == ExitSuccess) $ do
        logErrorS logSourceS $ "Execution of " <> displayShow exeName <> " ended abnormally. Got exit code: " <> displayShow exitCode
        logErrorS logSourceS $ "Error messages:\n" <> (displayBytesUtf8 . toStrictBytes $ err)
        logErrorS logSourceS $ "Stdout messages:\n" <> (displayBytesUtf8 . toStrictBytes $ out)
        throwM . localExc $ "Turbomole dscf execution ended abnormally."
      writeFileBinary (Path.toString $ permanentDir </> Path.relFile (exeName <> ".out")) . toStrictBytes $ out

----------------------------------------------------------------------------------------------------

-- | Run GDMA on a given FChk file. The function takes also a set of link atom indices (in dense mapping,
-- suitable for GDMA), that are to be ignored in the multipole expansion. This replaces the charge
-- redistribution.
--
-- By the way GDMA expects its input file, to delete link atoms, the need to be named @L@ in the FChk
-- file. This conversion of the FChk happens within this function.
--
-- The function will return multipole moments in spherical tensor representation up to given order.
gdmaAnalysis ::
  (HasWrapperConfigs env, HasLogFunc env, HasProcessContext env) =>
  -- | The absolute path to the FChk file, which is the GDMA input.
  Path.AbsFile ->
  -- | The 'Atom's of the current molecule layer, which is being analysed.
  IntMap Atom ->
  -- | The order of the multipole expansion. >= 0 && <= 10
  Int ->
  RIO env (IntMap Multipoles)
gdmaAnalysis fchkPath atoms expOrder = do
  -- Obtain the GDMA wrapper.
  gdmaWrapper <-
    view (wrapperConfigsL % #gdma) >>= \wrapper -> case wrapper of
      Just w -> return . getFilePath $ w
      Nothing -> throwM $ WrapperGenericException "executeGDMA" "The GDMA wrapper is not configured."

  -- Sanity Checks.
  unless (expOrder <= 10 && expOrder >= 0) . throwM $
    WrapperGenericException
      "executeGDMA"
      "The multipole expansion order in GDMA must be >= 0 && <= 10."
  fchkExists <- liftIO . Dir.doesFileExist $ fchkPath
  unless fchkExists . throwM $
    WrapperGenericException "executeGDMA" "The formatted checkpoint file does not exist."

  -- Read the FChk and reconstruct it with link atoms replaced by an uncommon element.
  let linkReplacement = Xe -- Xenon ist the heaviest element supported by GDMA.
      gdmaFChkPath = Path.takeDirectory fchkPath </> (Path.takeBaseName fchkPath <++> "_gdma" <++> ".fchk")
  fchkOrig <- readFileUTF8 (Path.toAbsRel fchkPath) >>= parse' fChk
  fchkLink <- relabelLinkAtoms linkReplacement atoms fchkOrig
  writeFileUTF8 (Path.toAbsRel gdmaFChkPath) . writeFChk $ fchkLink

  -- Construct the GDMA input.
  let gdmaInput =
        ByteStringLazy8.unlines
          [ "file " <> (ByteStringLazy8.pack . Path.toString $ gdmaFChkPath),
            "angstrom",
            "multipoles",
            "limit " <> (ByteStringLazy8.pack . show $ expOrder),
            "delete " <> (ByteStringLazy8.pack . show $ linkReplacement),
            "start"
          ]

  -- Execute GDMA on the input file now and pipe the input file into it.
  (exitCode, gdmaOut, gdmaErr) <-
    proc
      (Path.toString gdmaWrapper)
      mempty
      (readProcess . setStdin (byteStringInput gdmaInput))

  unless (exitCode == ExitSuccess) $ do
    logErrorS "gdma" $ "Execution terminated abnormally. Got exit code: " <> displayShow exitCode
    logErrorS "gdma" "Error messages:"
    logErrorS "gdma" . displayShow $ gdmaErr
    logErrorS "gdma" "Stdout messages:"
    logErrorS "gdma" . displayShow $ gdmaOut
    throwM . localExcp $ "Run uncessfull"

  -- Parse the GDMA output and rejoin them with the model atoms.
  modelMultipoleList <-
    getResOrErr
      . parse' gdmaMultipoles
      . decodeUtf8Lenient
      . BL.toStrict
      $ gdmaOut
  let modelAtomKeys =
        IntSet.toAscList
          . IntMap.keysSet
          . IntMap.filter (not . isAtomLink . isLink)
          . IntMap.filter (\a -> not $ a ^. #isDummy)
          $ atoms
      multipoleMap = IntMap.fromAscList $ zip modelAtomKeys modelMultipoleList

  -- Message if something is wrong with the number of poles expected and obtained.
  unless (List.length modelMultipoleList == List.length modelAtomKeys) $ do
    logErrorS
      "gdma"
      "Number of model atoms and number of multipole expansions centres obtained from GDMA\
      \ do not match."
    throwM . localExcp $ "Problematic behaviour in GDMA multipole analysis."

  return multipoleMap
  where
    localExcp = WrapperGenericException "gdmaAnalysis"

{-
####################################################################################################
-}

-- $analysers
-- Functions to process the outputs of computational chemistry software. This relies on consistent file
-- structure in the file system.

-- | Parse output produced by Psi4. The Psi4 analyser relies on a formatted checkpoint file for most
-- information, which must be at @permanentDir </> Path.relFile prefixName <.> ".fchk"@. If a hessian
-- calculation was requested, also a plain text hessian file (numpy style) is required, which must be
-- at @permanentDir </> Path.relFile prefixName <.> ".hess"@.
--
-- GDMA multipoles are obtained from this FCHK by an additional call to GDMA.
analysePsi4 ::
  (HasLogFunc env, HasMolecule env, HasProcessContext env, HasWrapperConfigs env) =>
  CalcID ->
  RIO env CalcOutput
analysePsi4 calcID = do
  -- Create the calcID lens and the mol lens.
  let molLens = molIDLensGen (calcID ^. #molID)
      calcLens = calcIDLensGen calcID

  -- Gather information about the run which to analyse.
  mol <- view moleculeL >>= readTVarIO
  localMol <- maybe2MThrow (localExcp "Specified molecule not found in hierarchy") $ mol ^? molLens
  calcContext <- case mol ^? calcLens of
    Just x -> return x
    Nothing ->
      throwM $
        MolLogicException
          "runCalculation"
          "Requested to perform a cdata CalcOutalculation, which does not exist."

  let permanentDir = getDirPathAbs $ calcContext ^. #input % #permaDir
      prefixName = calcContext ^. #input % #prefixName
      fchkPath = permanentDir </> Path.relFile prefixName <.> ".fchk"
      hessianPath = permanentDir </> Path.relFile prefixName <.> ".hess"
      task' = calcContext ^. #input % #task

  logDebugS "psi4" $ "Reading the formatted checkpoint file: " <> path2Utf8Builder fchkPath

  -- Read the formatted checkpoint file from the permanent directory.
  fChkOutput <- getResultsFromFChk =<< readFileUTF8 (Path.toAbsRel fchkPath)

  -- If the task was a hessian calculation, also parse the numpy array with the hessian.
  hessianOutput <- case task' of
    WTHessian -> do
      logDebugS "psi4" $ "Reading the hessian file: " <> path2Utf8Builder hessianPath
      hessianContent <- readFileUTF8 (Path.toAbsRel hessianPath)
      hessian <- parse' doubleSquareMatrix hessianContent
      return . Just $ hessian
    _ -> return Nothing

  -- Perform the GDMA calculation on the FChk file and obtain its results.
  multipoles <- gdmaAnalysis fchkPath (localMol ^. #atoms) 4

  -- Combine the Hessian and multipole information into the main output from the FChk.
  let calcOutput =
        fChkOutput
          & #energyDerivatives % #hessian .~ hessianOutput
          & #multipoles .~ multipoles

  return calcOutput
  where
    localExcp = WrapperGenericException "analysePsi4"

----------------------------------------------------------------------------------------------------

-- | Analyse the output of an xtb calculation. Unlike Psi4, xtb output file names
-- are independent of the input (unless explicitly specified). This function
-- is not reliant on gdma either.
analyseXTB ::
  (HasLogFunc env, HasMolecule env) =>
  CalcID ->
  RIO env CalcOutput
analyseXTB calcID = do
  -- Create the calcID lens and the mol lens.
  let molLens = molIDLensGen (calcID ^. #molID)
      calcLens = calcIDLensGen calcID

  -- Gather information about the run which to analyse.
  mol <- view moleculeL >>= readTVarIO
  localMol <- maybe2MThrow (localExcp "Specified molecule not found in hierarchy") $ mol ^? molLens
  calcContext <-
    maybe2MThrow
      (MolLogicException "analyseXTB" "Requested to analyze a Calculation, which does not exist.")
      (mol ^? calcLens)

  let permanentDir = getDirPathAbs $ calcContext ^. #input % #permaDir
      jsonPath = permanentDir </> Path.relFile "xtbout" <.> ".json"
      gradientPath = permanentDir </> Path.relFile "gradient"
      hessianPath = permanentDir </> Path.relFile "hessian"
      task' = calcContext ^. #input % #task

  -- If required, get the gradient from the respective output file.
  gradientOutput <- case task' of
    WTEnergy -> return Nothing
    _ -> do
      logDebugS "xtb" $ "Reading gradient file " <> path2Utf8Builder gradientPath
      gradientContent <- readFileUTF8 $ Path.toAbsRel gradientPath
      gradient <- parse' parseXTBgradient gradientContent
      return . Just $ gradient

  -- If the task was a hessian calculation, also parse the array with the hessian.
  hessianOutput <- case task' of
    WTHessian -> do
      logDebugS "xtb" $ "Reading the hessian file: " <> path2Utf8Builder hessianPath
      hessianContent <- readFileUTF8 (Path.toAbsRel hessianPath)
      hessian <- parse' parseXTBhessian hessianContent
      return . Just $ hessian
    _ -> return Nothing

  -- Get multipoles from the output json.
  logDebugS "xtb" $ "Reading the XTB json file: " <> path2Utf8Builder jsonPath
  (energy, modelMultipoleList) <- fromXTBout =<< readFileBinary (Path.toString jsonPath)

  -- Assign multipoles to the correct keys
  let realAtoms = IntMap.filter (\a -> not $ a ^. #isDummy) $ localMol ^. #atoms
      realAtomsKeys = IntMap.keys realAtoms
      multipoleMap = IntMap.fromAscList $ zip realAtomsKeys modelMultipoleList

  -- Finalize the output
  let enDeriv =
        EnergyDerivatives
          (pure energy)
          gradientOutput
          hessianOutput
      calcOutput =
        CalcOutput
          enDeriv
          multipoleMap
  return calcOutput
  where
    localExcp = WrapperGenericException "analyseXTB"

----------------------------------------------------------------------------------------------------

-- | Analyse the output of an xtb calculation. Unlike Psi4, xtb output file names
-- are independent of the input (unless explicitly specified). This function
-- is not reliant on gdma either.
analyseTurbomole ::
  (HasLogFunc env, HasMolecule env) =>
  CalcID ->
  RIO env CalcOutput
analyseTurbomole calcID = undefined
