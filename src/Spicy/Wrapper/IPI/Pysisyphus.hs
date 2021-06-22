-- |
-- Module      : Spicy.Wrapper.IPI.Pysisyphus
-- Description : Interactions with Pysisyphus
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Interaction with Pysisyphus.
module Spicy.Wrapper.IPI.Pysisyphus
  ( providePysis,
    providePysisAbstract,
    runPysisServer,
    runPysisServerAbstract,
  )
where

import Data.Aeson
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Yaml.Pretty
import Optics hiding (view)
import RIO hiding (lens, view, (^.), (^?))
import RIO.Process
import Spicy.Common
import Spicy.Molecule hiding (Minimum, SaddlePoint)
import qualified Spicy.Molecule as Mol
import Spicy.RuntimeEnv
import Spicy.Wrapper.IPI.Protocol
import System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path

-- | The log source for RIO logger.
logSource :: LogSource
logSource = "pysisyphus"

-- | Exceptions thrown by Pysisphus callers.
newtype PysisyphusExc = PysisyphusExc String deriving (Eq, Show)

instance Exception PysisyphusExc

----------------------------------------------------------------------------------------------------

-- | Provides a companion thread, that starts a pysisyphus server with the necessary input in the
-- background and then listens for data to send to this pysisyphus server with the i-PI protocol.
-- Returns the 'Async' value of the background thread, that runs the Pysisyphus server. When this
-- function returns the job on spicy side should be done but Pysis might continue to run. Give it a
-- reasonable time to finish and then use the 'Async' value to shut this thread down.
-- This function cannot be used for micro-cycle driven optimisations.
providePysis ::
  ( HasWrapperConfigs env,
    HasLogFunc env,
    HasProcessContext env,
    HasMolecule env
  ) =>
  RIO env (Async (), Async ())
providePysis = do
  logInfoS logSource "Starting companion threads ..."
  mol <- view moleculeL >>= readTVarIO
  optSettings <-
    let maybeOpt = mol ^? #calcContext % ix (ONIOMKey Original) % #input % #optimisation
     in maybe2MThrow (molExc "Optimisation settings missing") maybeOpt
  let atoms = mol ^. #atoms

  providePysisAbstract False atoms optSettings
  where
    molExc = MolLogicException "providePysis"

----------------------------------------------------------------------------------------------------

-- | Provides a companion thread, that starts a pysisyphus server with the necessary input in the
-- background and then listens for data to send to this pysisyphus server with the i-PI protocol.
-- Returns the 'Async' value of the background thread, that runs the Pysisyphus server. When this
-- function returns the job on spicy side should be done but Pysis might continue to run. Give it a
-- reasonable time to finish and then use the 'Async' value to shut this thread down.
providePysisAbstract ::
  ( HasWrapperConfigs env,
    HasLogFunc env,
    HasProcessContext env
  ) =>
  -- | Use micro-cycle driven optimisations.
  Bool ->
  IntMap Atom ->
  Optimisation ->
  RIO env (Async (), Async ())
providePysisAbstract doMicro atoms optSettings = do
  logDebugS logSource "Starting pysisyphus companion threads ..."

  serverThread <- async $ runPysisServerAbstract doMicro atoms optSettings
  link serverThread

  clientThread <- async $ ipiClient (optSettings ^. #pysisyphus)

  return (serverThread, clientThread)

----------------------------------------------------------------------------------------------------

-- | Launches a Pysishus i-PI server in the background with the required input and waits for it to
-- finish. Pysisyphus will respect the optimisation settings, that are given on the top level of the
-- visible molecule.
-- This will always launch a pysisyphus instance with normal/no micro-cycle behaviour.
runPysisServer ::
  ( HasWrapperConfigs env,
    HasProcessContext env,
    HasLogFunc env,
    HasMolecule env
  ) =>
  RIO env ()
runPysisServer = do
  mol <- view moleculeL >>= readTVarIO
  let atoms = mol ^. #atoms
  optSettings <-
    maybe2MThrow (localExc "calculation context with settings for optimiser missing") $
      mol ^? #calcContext % ix (ONIOMKey Original) % #input % #optimisation
  runPysisServerAbstract False atoms optSettings
  where
    localExc = MolLogicException "runPysisServer"

----------------------------------------------------------------------------------------------------

-- | Launches a pysisyphus server for an abstract optimisation, suitable for optimisations of parts
-- of molecules or multiple molecules. The main thread has to make sure to send proper gradients to
-- the corresponding iPI client.
runPysisServerAbstract ::
  ( HasWrapperConfigs env,
    HasProcessContext env,
    HasLogFunc env
  ) =>
  -- | Use optimisers with behaviour appropriate for micro-cycles.
  Bool ->
  IntMap Atom ->
  Optimisation ->
  RIO env ()
runPysisServerAbstract doMicro atoms optSettings = do
  -- Obtain initial information.
  socketPath <- unixSocket2Path $ optSettings ^. #pysisyphus % #socketAddr
  pysisWrapper <-
    view wrapperConfigsL >>= \wc -> case wc ^. #pysisyphus of
      Nothing -> throwM . localExc $ "Pysisyphus executable could not be located."
      Just (JFilePath path) -> return path

  -- Create the work directory for pysisyphus.
  liftIO $ Path.createDirectoryIfMissing True pysisWorkDir
  let scratchDir = Path.takeDirectory socketPath
  liftIO $ Path.createDirectoryIfMissing True scratchDir

  -- LOG
  logDebugS logSource $
    "Pysisyphus-server companion thread. Preparing to start pysisyphus:\n"
      <> ("  UNIX socket      : " <> displayShow socketPath <> "\n")
      <> ("  Pysisphus wrapper: " <> displayShow (Path.toString pysisWrapper) <> "\n")
      <> ("  Working directory: " <> path2Utf8Builder pysisWorkDir)

  -- Write initial coordinates to the pysisyphus file.
  initCoordFileAbs <- liftIO . Path.genericMakeAbsoluteFromCwd $ initCoordFile
  writeXYZSimple atoms >>= writeFileUTF8 (Path.toAbsRel initCoordFile)

  logDebugS logSource $
    "Wrote initial coordinates for Pysisyphus to " <> path2Utf8Builder initCoordFileAbs

  -- Construct the input file for pysisyphus.
  pysisInput <- opt2Pysis doMicro atoms initCoordFileAbs optSettings
  let pysisYamlPath = pysisWorkDir </> Path.relFile "pysis_servers_spicy.yml"
      pysisYaml = decodeUtf8Lenient . encodePretty defConfig $ pysisInput
  writeFileUTF8 pysisYamlPath pysisYaml

  -- LOG
  logDebugS logSource $ "Wrote Pysisyphus YAML input to " <> path2Utf8Builder pysisYamlPath

  -- Build Pysis command line arguments.
  let pysisCmdArgs = [Path.toString . Path.takeFileName $ pysisYamlPath]

  -- Mark the Pysis server as ready immediately before starting it.
  logDebugS logSource "Pysisyphus server starts and becomes ready ..."
  (exitCode, pysisOut, pysisErr) <-
    withWorkingDir (Path.toString pysisWorkDir) $
      proc (Path.toString pysisWrapper) pysisCmdArgs readProcess
  logDebugS logSource "Pysisyphus sever terminated."

  -- Pysisyphus output.
  writeFileBinary
    (Path.toString $ pysisWorkDir </> Path.relFile "pysisyphus.out")
    . toStrictBytes
    $ pysisOut

  -- Final check if everything went well. Then return.
  unless (exitCode == ExitSuccess) $ do
    logErrorS logSource $
      "Pysisyphus terminated abnormally with error messages:\n"
        <> (displayBytesUtf8 . toStrictBytes $ pysisErr)
    throwM . PysisException $ "Pysisyphus terminated abnormally with errors."
  where
    localExc = PysisyphusExc
    pysisWorkDir = optSettings ^. #pysisyphus % #workDir
    initCoordFile = optSettings ^. #pysisyphus % #initCoords

{-
####################################################################################################
-}
-- The JSON instances need to be written by hand unfortunately. The type keyword causes too many
-- problems.

-- | Pysisyphus input file data structure.
data PysisInput = PysisInput
  { geom :: Geom,
    optimiser :: Optimiser,
    calc :: Calc
  }

instance ToJSON PysisInput where
  toJSON PysisInput {..} =
    object $
      [ "geom" .= geom,
        "calc" .= calc
      ]
        <> case optimiser of
          Minimum opt -> ["opt" .= opt]
          SaddlePoint tsopt -> ["tsopt" .= tsopt]
          Micro microOpt -> ["opt" .= microOpt]

----------------------------------------------------------------------------------------------------

-- | Types of optimisers.
data Optimiser
  = -- | Normal pysisyphus optimisers for minima.
    Minimum MinOpt
  | -- | Normal optimisers for transition state search (local optimisers, not COS).
    SaddlePoint TSOpt
  | -- | Special optimisers, that implement appropriate behaviour for micro-cycle driven
    -- optimisation.
    Micro MicroOpt

----------------------------------------------------------------------------------------------------

-- | @opt@ block of a pysisyphus input. All values are explicit here and will be written always.
data MinOpt = MinOpt
  { optType :: MinOptAlg,
    maxCycles :: Int,
    recalcHessian :: Maybe Int,
    updateHessian :: HessianUpdate,
    lineSearch :: Bool,
    minTrust :: Double,
    maxTrust :: Double,
    trustRadius :: Double,
    thresh :: ConvThresh,
    reparamWhen :: Maybe ReparamEvent
  }

instance ToJSON MinOpt where
  toJSON MinOpt {..} =
    object
      [ "type" .= optType,
        "max_cycles" .= maxCycles,
        "hessian_recalc" .= recalcHessian,
        "hessian_update" .= updateHessian,
        "line_search" .= lineSearch,
        "trust_radius" .= trustRadius,
        "trust_min" .= minTrust,
        "trust_max" .= maxTrust,
        "thresh" .= thresh,
        "reparam_when" .= reparamWhen
      ]

----------------------------------------------------------------------------------------------------

-- | @tsopt@ block of a pysisyphus input.
data TSOpt = TSOpt
  { optType :: TSOptAlg,
    maxCycles :: Int,
    recalcHessian :: Maybe Int,
    minTrust :: Double,
    maxTrust :: Double,
    trustRadius :: Double,
    thresh :: ConvThresh,
    reparamWhen :: Maybe ReparamEvent
  }

instance ToJSON TSOpt where
  toJSON TSOpt {..} =
    object
      [ "type" .= optType,
        "max_cycles" .= maxCycles,
        "hessian_recalc" .= recalcHessian,
        "trust_radius" .= trustRadius,
        "trust_max" .= maxTrust,
        "trust_min" .= minTrust,
        "thresh" .= thresh,
        "reparam_when" .= reparamWhen
      ]

----------------------------------------------------------------------------------------------------

-- | Possible settings for micro optimisation.
data MicroOpt = MicroOpt
  { -- | Always must indicate micro-driven cycle optimisation.
    optType :: MicroOptType,
    -- | The geometry update type to use.
    step :: MicroStep,
    -- | Maximum number of geometry optimisation steps.
    maxCycles :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON MicroOpt where
  toJSON MicroOpt {..} =
    object
      ["type" .= optType, "step" .= step, "max_cycles" .= maxCycles]

----------------------------------------------------------------------------------------------------

-- | For micro optimisation the value of @type@ always needs to be @micro@.
data MicroOptType = MicroOptType deriving (Eq, Show, Generic)

instance ToJSON MicroOptType where
  toJSON MicroOptType = toJSON @Text "micro"

----------------------------------------------------------------------------------------------------

-- | Convergence threshold for Pysisyphus. Server is meant to never converge. Convergence is checked
-- by Spicy.
data ConvThresh = Never

instance ToJSON ConvThresh where
  toJSON Never = toJSON @Text "never"

----------------------------------------------------------------------------------------------------

-- | @geom@ input block of Pysisyphus
data Geom = Geom
  { fn :: JFilePathAbs,
    coordType :: CoordType,
    freeze_atoms :: IntSet
  }

instance ToJSON Geom where
  toJSON Geom {..} =
    object
      [ "fn" .= fn,
        "type" .= coordType,
        "freeze_atoms" .= freeze_atoms
      ]

----------------------------------------------------------------------------------------------------

-- | @calc@ input block of Pysisyphus.
data Calc = Calc
  { calcType :: CalcType,
    address :: Maybe String,
    verbose :: Bool
  }

instance ToJSON Calc where
  toJSON Calc {..} =
    object
      [ "type" .= calcType,
        "address" .= address,
        "verbose" .= verbose
      ]

----------------------------------------------------------------------------------------------------

-- | Calculation types that the combination of Pysisyphus and Spicy supports.
data CalcType = IPIServer

instance ToJSON CalcType where
  toJSON IPIServer = toJSON @Text "ipiserver"

----------------------------------------------------------------------------------------------------

-- | When client coordinate update shall occur. Only one of the cases is coded for Spicy, as the
-- other makes no sense here.
data ReparamEvent
  = Before
  | After

instance ToJSON ReparamEvent where
  toJSON Before = toJSON @Text "before"
  toJSON After = toJSON @Text "after"

----------------------------------------------------------------------------------------------------

-- | Conversion of Optimisation settings to a Pysisyphus input.
opt2Pysis :: MonadThrow m => Bool -> IntMap Atom -> Path.AbsFile -> Optimisation -> m PysisInput
opt2Pysis
  doMicro
  atoms
  initCoordFile
  Optimisation
    { ..
    } = do
    scktAddr <- unixSocket2Path $ pysisyphus ^. #socketAddr
    return
      PysisInput
        { geom = geom,
          optimiser = optimiser,
          calc = calc . Path.toString $ scktAddr
        }
    where
      geom = Geom {fn = JFilePathAbs initCoordFile, coordType, freeze_atoms = denseFreeze}
      optimiser =
        if doMicro
          then Micro $ MicroOpt {optType = MicroOptType, step = microStep, maxCycles = 1000000000}
          else case optType of
            Mol.SaddlePoint alg -> SaddlePoint (tsOpt {optType = alg} :: TSOpt)
            Mol.Minimum alg ->
              Minimum $
                MinOpt
                  { optType = alg,
                    maxCycles,
                    recalcHessian = hessianRecalc,
                    updateHessian = hessianUpdate,
                    lineSearch,
                    minTrust,
                    maxTrust,
                    trustRadius,
                    thresh = Never,
                    reparamWhen = Nothing
                  }
      tsOpt =
        TSOpt
          { optType = RS_I_RFO,
            maxCycles,
            recalcHessian = hessianRecalc,
            minTrust,
            maxTrust,
            trustRadius,
            thresh = Never,
            reparamWhen = Nothing
          }
      calc addr =
        Calc
          { calcType = IPIServer,
            address = Just addr,
            verbose = True
          }

      denseFreeze =
        let atomKeys = IntMap.keys atoms
            sparse2Dense = IntMap.fromAscList $ RIO.zip atomKeys [0 :: Int ..]
         in IntSet.fromAscList
              . fmap snd
              . IntMap.toAscList
              $ sparse2Dense `IntMap.restrictKeys` freezes
