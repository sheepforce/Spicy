{-|
Module      : Spicy.Wrapper.Internal.Executor
Description : System calls to the quantum chemistry software.
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

Provides the callers to quantum chemistry software.
-}
module Spicy.Wrapper.Internal.Executor
  ( runAllCalculations
  , runCalculation
  )
where
import           Control.Lens            hiding ( (<.>) )
import qualified Data.ByteString.Lazy.Char8    as ByteStringLazy8
import           Data.Foldable
import           RIO                     hiding ( view
                                                , (^.)
                                                , (^?)
                                                , (.~)
                                                )
import qualified RIO.Map                       as Map
import           RIO.Process
import           Spicy.Class
import           Spicy.Generic
import           Spicy.Logger
import           Spicy.Molecule
import           Spicy.Wrapper.Internal.Input.Shallow
import           Spicy.Wrapper.Internal.Output.FChk
import           Spicy.Wrapper.Internal.Output.Generic
import           System.Path                    ( (<.>)
                                                , (</>)
                                                )
import qualified System.Path                   as Path
import qualified System.Path.Directory         as Dir

{-|
Run all calculations of a complete molecular system on all layers. Moves top down the leftmost tree
first.
-}
runAllCalculations
  :: (HasPreStartUpEnv env, HasLogFunc env, HasLogFile env, HasMolecule env, HasProcessContext env)
  => RIO env Molecule
runAllCalculations = do
  -- Get the molecule to process.
  mol <- view moleculeL

  -- Obtain all CalcIDs of this molecule.
  let allCalcIDs = molFoldlWithMolID
        (\idAcc currentMolID currentMol ->
          let calcIDsOfCurrentMol = getCalcIDsOfMolLayer currentMolID currentMol
          in  idAcc <> calcIDsOfCurrentMol
        )
        Empty
        mol

  -- Fold over all CalcIDs of the molecule to resolve all calculations of the molecule.
  molCalculated <- foldl
    (\molCalcAcc' calcID -> do
      molCalcAcc           <- molCalcAcc'
      molWithThisIdUpdated <- local (& moleculeL .~ molCalcAcc) $ runCalculation calcID
      return molWithThisIdUpdated
    )
    (return mol)
    allCalcIDs

  return molCalculated
 where
  getCalcIDsOfMolLayer :: MolID -> Molecule -> Seq CalcID
  getCalcIDsOfMolLayer molID molLayer = Map.foldlWithKey'
    (\idsAcc calcKey _ -> idsAcc |> CalcID { _calcID_MolID = molID, _calcID_calcKey = calcKey })
    Empty
    (molLayer ^. molecule_CalcContext)

----------------------------------------------------------------------------------------------------
{-|
Run a given calculation of a molecule, given by a 'CalcID'. This updates the 'CalcOutput' of this
'CalcID'.
-}
runCalculation
  :: (HasPreStartUpEnv env, HasLogFunc env, HasLogFile env, HasMolecule env, HasProcessContext env)
  => CalcID
  -> RIO env Molecule
runCalculation calcID = do
  -- Initial logging.
  logInfoF
    $  "Running calculation with ID \""
    <> displayShow (calcID ^. calcID_calcKey)
    <> "\" on "
    <> molID2OniomHumandID (calcID ^. calcID_MolID)
    <> "."

  -- Create the lenses for accessing calculation and molecule.
  let calcLens = calcIDLensGen calcID

  -- Gather the information for this calculation.
  mol <- view moleculeL
  let calcContextM = mol ^? calcLens
  calcContext <- case calcContextM of
    Nothing -> throwM $ MolLogicException
      "runCalculation"
      "Requested to perform a calculation, which does not exist."
    Just cntxt -> if isJust (cntxt ^. calcContext_Output)
      then throwM $ MolLogicException
        " runCalculation"
        " Requested to perform a calculation, which has already been performed."
      else return cntxt

  let
    permanentDir = getDirPathAbs $ calcContext ^. calcContext_Input . calcInput_PermaDir
    scratchDir   = getDirPathAbs $ calcContext ^. calcContext_Input . calcInput_ScratchDir
    inputFilePrefix =
      Path.relFile
        .  replaceProblematicChars
        $  calcContext
        ^. calcContext_Input
        .  calcInput_PrefixName
    inputFileName = inputFilePrefix <.> ".inp"
    inputFilePath = permanentDir </> inputFileName
    software      = calcContext ^. calcContext_Input . calcInput_Software
    calcContextL  = calcIDLensGen calcID

  -- Create permanent and scratch directory
  liftIO $ Dir.createDirectoryIfMissing True permanentDir
  liftIO $ Dir.createDirectoryIfMissing True scratchDir

  -- Write the input file for the calculation to its permanent directory.
  wrapperInputFile <- translate2Input mol calcID
  writeFileUTF8 (Path.toAbsRel inputFilePath) wrapperInputFile

  -- Logging about the Wrapper execution.
  mapM_ (logInfo . ("   " <>)) . hShow $ calcContext ^. calcContext_Input

  -- Execute the wrapper on the input file.
  case software of
    Psi4   -> executePsi4 calcID inputFilePath
    Nwchem -> undefined

  -- Parse the output, that has been produced by the wrapper.
  calcOutput <- case software of
    Psi4   -> analysePsi4 calcID
    Nwchem -> undefined

  -- Print logging information about the output obtained.
  mapM_ logInfoF . hShow $ calcOutput

  -- Insert the output, that has been obtained for this calculation into the corresponding CalcID.
  let molUpdated = mol & calcContextL . calcContext_Output ?~ calcOutput

  return molUpdated

{-
####################################################################################################
-}
{- $executors
Functions to call computational chemistry software with the appropiate arguments. These functions
are not responsible for processing of the output.
-}
{-|
Run Psi4 on a given input file. A Psi4 run will behave as follows:

- All permanent files will be in the 'calcInput_PermaDir'.
- The 'calcInput_ScratchDir' will be used for scratch files and keep temporary files after
  execution.
- All files will be prefixed with the 'calcInput_PrefixName'.
- The output will be kept for
-}
executePsi4
  :: (HasPreStartUpEnv env, HasMolecule env, HasLogFunc env, HasLogFile env, HasProcessContext env)
  => CalcID       -- ^ ID of the calculation to perform.
  -> Path.AbsFile -- ^ Path to the input file of Psi4.
  -> RIO env ()
executePsi4 calcID inputFilePath = do
  -- Create the calculation context lens.
  let calcLens = calcIDLensGen calcID

  -- Gather information for the execution of Psi4
  preStartUpConf <- view preStartupEnvL
  mol            <- view moleculeL
  let calcContextM = mol ^? calcLens
  calcContext <- case calcContextM of
    Nothing -> throwM $ MolLogicException
      "runCalculation"
      "Requested to perform a calculation, which does not exist."
    Just cntxt -> return cntxt

  let permanentDir   = getDirPathAbs $ calcContext ^. calcContext_Input . calcInput_PermaDir
      scratchDir     = getDirPathAbs $ calcContext ^. calcContext_Input . calcInput_ScratchDir
      software       = calcContext ^. calcContext_Input . calcInput_Software
      outputFilePath = Path.replaceExtension inputFilePath ".out"

  -- Check if this function is appropiate to execute the calculation at all.
  unless (software == Psi4) $ do
    logError
      "A calculation should be done with the Psi4 driver function,\
      \ but the calculation is not a Psi4 calculation."
    throwM $ SpicyIndirectionException
      "executePsi4"
      (  "Requested to execute Psi4 on calculation with CalcID "
      <> show calcID
      <> "but this is not a Psi4 calculation."
      )

  -- Use the environment in which spicy was launched to launch Psi4.
  defaultEnv  <- view envVarsL

  -- Create a new process context from the environment variables.
  psi4Context <- mkProcessContext defaultEnv
  -- Prepare the command line arguments to Psi4.
  let psi4CmdArgs =
        [ "--input=" <> Path.toString inputFilePath
        , "--output=" <> Path.toString outputFilePath
        , "--nthread=" <> show (calcContext ^. calcContext_Input . calcInput_NThreads)
        , "--scratch=" <> Path.toString scratchDir
        , "--prefix=" <> (calcContext ^. calcContext_Input . calcInput_PrefixName)
        , "--messy"
        ]
      psi4Wrapper = getFilePathAbs $ preStartUpConf ^. psEnvPsi4
  logInfoF $ "Using Psi4 wrapper at: " <> displayShow psi4Wrapper

  -- Debug logging before execution of Psi4.
  logDebug $ "Starting Psi4 with command line arguments: " <> displayShow psi4CmdArgs

  -- Launch the Psi4 process and read its stdout and stderr.
  (exitCode, psi4Out, psi4Err) <-
    local (& processContextL .~ psi4Context) . withWorkingDir (Path.toString permanentDir) $ proc
      (Path.toString psi4Wrapper)
      psi4CmdArgs
      readProcess

  -- Provide some information if something went wrong.
  unless (exitCode == ExitSuccess) $ do
    logError $ "Psi4 execution terminated abnormally. Got exit code: " <> displayShow exitCode
    logError "Psi4 error messages:"
    mapM_ (logError . ("  " <>) . displayShow) . ByteStringLazy8.lines $ psi4Err
    logError "Psi4 stdout messsages:"
    mapM_ (logError . ("  " <>) . displayShow) . ByteStringLazy8.lines $ psi4Out
    throwM $ WrapperGenericException "executePsi4" "Psi4 execution terminated abnormally."

{-|
Run GDMA on a given FChk file. The function takes also a set of link atom indices (in dense mapping,
suitable for GDMA), that are to be ignored in the multipole expansion. This replaces the charge
redistribution.

By the way GDMA expects its input file, to delete link atoms, the need to be named @L@ in the FChk
file. This conversion of the FChk happens within this function.

The function will return multipole moments in spherical tensor representation up to given order.
-}
executeGDMA
  :: (HasPreStartUpEnv env, HasLogFunc env, HasProcessContext env)
  => Path.AbsFile -- ^ The absolute path to the FChk file, which is the GDMA input.
  -> IntSet       -- ^ The dense set of link atoms, which shall **not** be expansion centres.
  -> Int          -- ^ The order of the multipole expansion. >= 0 && <= 10
  -> RIO env (IntMap Multipoles)
executeGDMA fchkPath linkAtoms expOrder = do
  -- Sanity checks.
  unless (expOrder <= 10 && expOrder >= 0) . throwM $
    WrapperGenericException
    "executeGDMA"
    "The multipole expansion order in GDMA must be >= 0 && <= 10."
  fchkExists <- liftIO . Dir.doesFileExist $ fchkPath
  unless fchkExists
    . throwM $ WrapperGenericException "executeGDMA" "The formatted checkpoint file does not exist."

  -- TODO - Reconstruct FChk file for suitable link atom names


  -- Look for the GDMA wrapper script.
  gdmaWrapper <- getFilePathAbs <$> view (preStartupEnvL . psEnvGDMA)

  -- Construct the GDMA input.
  let gdmaInput = ByteStringLazy8.unlines
        [ "file " <> (ByteStringLazy8.pack . Path.toString $ fchkPath)
        , "multipoles"
        , "limit " <> (ByteStringLazy8.pack . show $ expOrder)
        , "delete L"
        , "start"
        ]

  -- Create the process context for GDMA. Simple default context as we expect the wrapper to take
  -- care of providing good GDMA environment.
  context                      <- mkDefaultProcessContext

  -- Execute GDMA on the input file now and pipe the input file into it.
  (exitCode, gdmaOut, gdmaErr) <- local (& processContextL .~ context)
    $ proc (Path.toString gdmaWrapper) mempty (readProcess . setStdin (byteStringInput gdmaInput))

  -- Parse the GDMA output.

  unless (exitCode == ExitSuccess) $ do
    logError $ "GDMA execution terminated abnormally. Got exit code: " <> displayShow exitCode
    logError "GDMA error messages:"
    logError . displayShow $ gdmaErr
    logError "GDMA stdout messages:"
    logError . displayShow $ gdmaOut



  return undefined


{-
####################################################################################################
-}
{- $analysers
Functions to process the outputs of computational chemistry software. This relies on consistent file
structure in the file system.
-}
{-|
Parse output produced by Psi4. The Psi4 analyser relies on a formatted checkpoint file for most
information, which must be at @permanentDir </> Path.relFile prefixName <.> ".fchk"@. If a hessian
calculation was requested, also a plain text hessian file (numpy style) is required, which must be
at @permanentDir </> Path.relFile prefixName <.> ".hess"@.
-}
analysePsi4 :: (HasLogFunc env, HasMolecule env) => CalcID -> RIO env CalcOutput
analysePsi4 calcID = do
  -- Create the calcID lens.
  let calcLens = calcIDLensGen calcID

  -- Gather information about the run which to analyse.
  mol <- view moleculeL
  let calcContextM = mol ^? calcLens
  calcContext <- case calcContextM of
    Nothing -> throwM $ MolLogicException
      "runCalculation"
      "Requested to perform a calculation, which does not exist."
    Just cntxt -> return cntxt

  let permanentDir = getDirPathAbs $ calcContext ^. calcContext_Input . calcInput_PermaDir
      prefixName   = calcContext ^. calcContext_Input . calcInput_PrefixName
      fchkPath     = permanentDir </> Path.relFile prefixName <.> ".fchk"
      hessianPath  = permanentDir </> Path.relFile prefixName <.> ".hess"
      task'        = calcContext ^. calcContext_Input . calcInput_Task

  logDebug $ "Reading the formatted checkpoint file: " <> path2Utf8Builder fchkPath

  -- Read the formatted checkpoint file from the permanent directory.
  fChkOutput    <- getResultsFromFChk =<< readFileUTF8 (Path.toAbsRel fchkPath)

  -- If the task was a hessian calculation, also parse the numpy array with the hessian.
  hessianOutput <- case task' of
    WTHessian -> do
      logDebug $ "Reading the hessian file: " <> path2Utf8Builder hessianPath
      hessianContent <- readFileUTF8 (Path.toAbsRel hessianPath)
      hessian        <- parse' doubleSquareMatrix hessianContent
      return . Just $ hessian
    _ -> return Nothing

  -- Perform the GDMA calculation on the FChk file and obtain its results.
  multipoles <- undefined -- gdmaAnalysis fchkPath linkAtoms

  -- Combine the Hessian information into the main output from the FChk.
  let calcOutput =
        fChkOutput & calcOutput_EnergyDerivatives . energyDerivatives_Hessian .~ hessianOutput

  return calcOutput
