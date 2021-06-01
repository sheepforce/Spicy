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
    runPysisServer,
  )
where

import Data.Aeson
import Data.Yaml.Pretty
import Optics hiding (view)
import RIO hiding (lens, view, (^.), (^?))
import RIO.Process
import Spicy.Common
import Spicy.Molecule
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
providePysis ::
  ( HasWrapperConfigs env,
    HasLogFunc env,
    HasProcessContext env,
    HasMolecule env
  ) =>
  RIO env (Async (), Async ())
providePysis = do
  logInfoS logSource "Starting companion threads ..."
  molT <- view moleculeL
  pysisIPI <-
    readTVarIO molT >>= \mol ->
      let maybeIPI = mol ^? #calcContext % ix (ONIOMKey Original) % #input % #optimisation % #pysisyphus
       in case maybeIPI of
            Nothing -> throwM . PysisException $ "IPI connection settings missing."
            Just ipi -> return ipi

  -- Start another thread in the backgroud, that runs Pysisyphus.
  -- logDebugS logSource "Starting pysisyphus i-PI server ..."
  serverThread <- async runPysisServer
  link serverThread

  -- Start another thread that runs the client
  -- logDebugS logSource "Starting i-PI client loop ..."
  clientThread <- async $ ipiClient pysisIPI
  link clientThread

  return (serverThread, clientThread)

----------------------------------------------------------------------------------------------------

-- | Launches a Pysishus i-PI server in the background with the required input and waits for it to
-- finish. Pysisyphus will respect the optimisation settings, that are given on the top level of the
-- visible molecule.
runPysisServer ::
  ( HasWrapperConfigs env,
    HasProcessContext env,
    HasLogFunc env,
    HasMolecule env
  ) =>
  RIO env ()
runPysisServer = do
  -- Obtain initial information
  mol <- view moleculeL >>= readTVarIO
  optSettings <-
    maybe2MThrow (localExc "Missing settings for pysisyphus in the calculation input") $
      mol ^? #calcContext % ix (ONIOMKey Original) % #input % #optimisation
  let pysisWorkDir = optSettings ^. #pysisyphus % #workDir
      initCoordFile = optSettings ^. #pysisyphus % #initCoords
  socketPath <- unixSocket2Path $ optSettings ^. #pysisyphus % #socketAddr
  pysisWrapper <-
    view wrapperConfigsL >>= \wc -> case wc ^. #pysisyphus of
      Nothing -> throwM . localExc $ "Pysisyphus executable could not be located."
      Just (JFilePath path) -> return path

  -- Make the work directory, where pysisyphus is started, and the scratch directory, where the
  -- socket lives.
  liftIO $ Path.createDirectoryIfMissing True pysisWorkDir
  let scratchDir = Path.takeDirectory socketPath
  liftIO $ Path.createDirectoryIfMissing True scratchDir

  -- LOG
  logInfoS logSource $
    "Pysisyphus-server companion thread. Preparing to start pysisyphus:\n"
      <> ("  UNIX socket      : " <> displayShow socketPath <> "\n")
      <> ("  Pysisphus wrapper: " <> displayShow (Path.toString pysisWrapper) <> "\n")
      <> ("  Working directory: " <> path2Utf8Builder pysisWorkDir)

  -- Write initial coordinates for Pysisyphus to a file at the given location.
  initCoordFileAbs <- liftIO . Path.genericMakeAbsoluteFromCwd $ initCoordFile
  writeXYZ mol >>= writeFileUTF8 (Path.toAbsRel initCoordFile)

  logInfoS logSource $ "Wrote initial coordinates for to " <> path2Utf8Builder initCoordFileAbs

  -- Construct a pysisyphus input file.
  pysisInput <- opt2Pysis initCoordFileAbs optSettings

  -- Make a working directory for Pysisyphus and write the input file to it.
  let pysisYamlPath = pysisWorkDir </> Path.relFile "pysis_servers_spicy.yml"
      pysisYaml = decodeUtf8Lenient . encodePretty defConfig $ pysisInput
  writeFileUTF8 pysisYamlPath pysisYaml

  -- LOG
  logInfoS logSource $ "Wrote YAML input to " <> path2Utf8Builder pysisYamlPath

  -- Build Pysis command line arguments.
  let pysisCmdArgs = [Path.toString . Path.takeFileName $ pysisYamlPath]

  -- Mark the Pysis server as ready immediately before starting it.
  logDebugS logSource "Pysisyphus server starts and becomes ready ..."
  (exitCode, pysisOut, pysisErr) <-
    withWorkingDir (Path.toString pysisWorkDir) $
      proc (Path.toString pysisWrapper) pysisCmdArgs readProcess
  logInfoS logSource "Pysisyphus sever terminated."

  -- Pysisyphus output.
  writeFileBinary
    (Path.toString $ pysisWorkDir </> Path.relFile "pysisyphus.out")
    . toStrictBytes
    $ pysisOut

  -- Final check if everything went well. Then return.
  unless (exitCode == ExitSuccess) $ do
    logErrorS logSource $
      "Terminated abnormally with error messages:\n"
        <> (displayBytesUtf8 . toStrictBytes $ pysisErr)
    throwM . PysisException $ "Pysisyphus terminated abnormally with errors."
  where
    localExc = PysisyphusExc

{-
####################################################################################################
-}
-- The JSON instances need to be written by hand unfortunately. The type keyword causes too many
-- problems.

-- | Pysisyphus input file data structure.
data PysisInput = PysisInput
  { geom :: Geom,
    optimiser :: Either MinOpt TSOpt,
    calc :: Calc
  }

instance ToJSON PysisInput where
  toJSON PysisInput {geom, optimiser, calc} =
    object $
      [ "geom" .= geom,
        "calc" .= calc
      ]
        <> case optimiser of
          Left opt -> ["opt" .= opt]
          Right tsopt -> ["tsopt" .= tsopt]

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
    trustRadius :: Double
  }

instance ToJSON MinOpt where
  toJSON
    MinOpt
      { optType,
        maxCycles,
        recalcHessian,
        updateHessian,
        lineSearch,
        minTrust,
        maxTrust,
        trustRadius
      } =
      object
        [ "type" .= optType,
          "max_cycles" .= maxCycles,
          "hessian_recalc" .= recalcHessian,
          "hessian_update" .= updateHessian,
          "line_search" .= lineSearch,
          "trust_radius" .= trustRadius,
          "trust_min" .= minTrust,
          "trust_max" .= maxTrust
        ]

----------------------------------------------------------------------------------------------------

-- | @tsopt@ block of a pysisyphus input.
data TSOpt = TSOpt
  { optType :: TSOptAlg,
    maxCycles :: Int,
    recalcHessian :: Maybe Int,
    minTrust :: Double,
    maxTrust :: Double,
    trustRadius :: Double
  }

instance ToJSON TSOpt where
  toJSON TSOpt {optType, maxCycles, recalcHessian, minTrust, maxTrust, trustRadius} =
    object
      [ "type" .= optType,
        "max_cycles" .= maxCycles,
        "hessian_recalc" .= recalcHessian,
        "trust_radius" .= trustRadius,
        "trust_max" .= maxTrust,
        "trust_min" .= minTrust
      ]

----------------------------------------------------------------------------------------------------

-- | @geom@ input block of Pysisyphus
data Geom = Geom
  { fn :: JFilePathAbs,
    coordType :: CoordType
  }

instance ToJSON Geom where
  toJSON Geom {fn, coordType} =
    object
      [ "fn" .= fn,
        "type" .= coordType
      ]

----------------------------------------------------------------------------------------------------

-- | @calc@ input block of Pysisyphus.
data Calc = Calc
  { calcType :: CalcType,
    address :: Maybe String,
    verbose :: Bool
  }

instance ToJSON Calc where
  toJSON Calc {calcType, address, verbose} =
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

-- | Conversion of Optimisation settings to a Pysisyphus input.
opt2Pysis :: MonadThrow m => Path.AbsFile -> Optimisation -> m PysisInput
opt2Pysis
  initCoordFile
  Optimisation
    { coordType,
      maxCycles,
      hessianRecalc,
      hessianUpdate,
      trustRadius,
      maxTrust,
      minTrust,
      lineSearch,
      optType,
      pysisyphus
    } = do
    scktAddr <- unixSocket2Path $ pysisyphus ^. #socketAddr
    return
      PysisInput
        { geom = geom,
          optimiser = optimiser,
          calc = calc . Path.toString $ scktAddr
        }
    where
      geom = Geom {fn = JFilePathAbs initCoordFile, coordType = coordType}
      optimiser = case optType of
        SaddlePoint alg -> Right (tsOpt {optType = alg} :: TSOpt)
        Minimum alg ->
          Left $
            MinOpt
              { optType = alg,
                maxCycles = maxCycles,
                recalcHessian = hessianRecalc,
                updateHessian = hessianUpdate,
                lineSearch = lineSearch,
                minTrust = minTrust,
                maxTrust = maxTrust,
                trustRadius = trustRadius
              }
      tsOpt =
        TSOpt
          { optType = RS_I_RFO,
            maxCycles = maxCycles,
            recalcHessian = hessianRecalc,
            minTrust = minTrust,
            maxTrust = maxTrust,
            trustRadius = trustRadius
          }
      calc addr =
        Calc
          { calcType = IPIServer,
            address = Just addr,
            verbose = False
          }
