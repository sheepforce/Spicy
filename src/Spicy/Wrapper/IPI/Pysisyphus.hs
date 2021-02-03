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
import Network.Socket
import Optics hiding (view)
import RIO hiding (lens, view, (^.))
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
data PysisyphusExc = PysisyphusExc String deriving (Eq, Show)

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
    HasPysis env,
    HasMolecule env
  ) =>
  RIO env (Async (), Async ())
providePysis = do
  logDebugS logSource "Starting pysisyphus companion threads ..."

  -- Obtain initial information.
  pysisIPI <- view pysisL

  -- Start another thread in the backgroud, that runs Pysisyphus.
  -- logDebugS logSource "Starting pysisyphus i-PI server ..."
  serverThread <- async runPysisServer
  link serverThread

  -- Start another thread that runs the client
  -- logDebugS logSource "Starting i-PI client loop ..."
  clientThread <- async (ipiClient pysisIPI)
  link clientThread

  return (serverThread, clientThread)

----------------------------------------------------------------------------------------------------

-- | Launches a Pysishus i-PI server in the background with the required input and waits for it to
-- finish.
runPysisServer ::
  ( HasWrapperConfigs env,
    HasProcessContext env,
    HasPysis env,
    HasLogFunc env,
    HasMolecule env
  ) =>
  RIO env ()
runPysisServer = do
  -- Obtain initial information
  mol <- view moleculeL >>= atomically . readTVar
  ipi <- view pysisL
  socketPath <- case ipi ^. #socketAddr of
    SockAddrUnix path -> return path
    SockAddrInet _ _ -> do
      logErrorS logSource "Wrong socket type given. Need a UNIX socket but got an INET socket."
      throwM . localExc $ "Wrong socket type given: INET"
    SockAddrInet6 _ _ _ _ -> do
      logErrorS logSource "Wrong socket type given. Need a UNIX socket but got an INET6 socket."
      throwM . localExc $ "Wrong socket type given: INET6"
  pysisWrapperM <- view $ wrapperConfigsL % #pysisyphus
  pysisWrapper <- case pysisWrapperM of
    Just path -> return . Path.toString . getFilePath $ path
    Nothing -> do
      logErrorS logSource "Pysisyphus is not configured. Cannot run optimisations. Quiting ..."
      throwM . localExc $ "pysisyphus not found."

  -- Make the permanent pysisyphus working directory.
  pysisDir <- view $ pysisL % #workDir
  liftIO $ Path.createDirectoryIfMissing True pysisDir

  -- Make the scratch directory, where the socket lives.
  let socketAbsPath = Path.absFile socketPath
      scratchDir = Path.takeDirectory socketAbsPath
  liftIO $ Path.createDirectoryIfMissing True scratchDir

  -- LOG
  logDebugS logSource $
    "Pysisyphus-server companion thread. Preparing to start pysisyphus:\n"
      <> ("  UNIX socket      : " <> displayShow socketPath <> "\n")
      <> ("  Pysisphus wrapper: " <> displayShow pysisWrapper <> "\n")
      <> ("  Working directory: " <> path2Utf8Builder pysisDir)

  -- Write initial coordinates for Pysisyphus to a file at the given location.
  initCoordFile <- view $ pysisL % #initCoords
  initCoordFileAbs <- liftIO . Path.genericMakeAbsoluteFromCwd $ initCoordFile
  writeXYZ mol >>= writeFileUTF8 (Path.toAbsRel initCoordFile)

  logDebugS logSource $
    "Wrote initial coordinates for Pysisyphus to " <> path2Utf8Builder initCoordFileAbs

  -- Construct a pysisyphus input file.
  let pysisInput =
        PysisInput
          { geom =
              Geom -- TODO (phillip|p=50|#Unfinished) - The coordinate type should be obtained from the CalcInput from the molecule.
                { type_ = Cart,
                  fn = JFilePathAbs initCoordFileAbs
                },
            opt =
              Opt -- TODO (phillip|p=50|#Unfinished) - The optimisation type should be obtained from the CalcInput from the molecule.
                { type_ = RFO,
                  align = False,
                  max_cycles = 100
                },
            calc =
              Calc
                { type_ = IPISever,
                  address = Just socketPath
                }
          }

  -- Make a working directory for Pysisyphus and write the input file to it.
  let pysisYamlPath = pysisDir </> Path.relFile "pysis_servers_spicy.yml"
      pysisYaml = decodeUtf8Lenient . encodePretty defConfig $ pysisInput
  writeFileUTF8 pysisYamlPath pysisYaml

  -- LOG
  logDebugS logSource $ "Wrote Pysisyphus YAML input to " <> path2Utf8Builder pysisYamlPath

  -- Build Pysis command line arguments.
  let pysisCmdArgs = [Path.toString . Path.takeFileName $ pysisYamlPath]

  -- Mark the Pysis server as ready immediately before starting it.
  logDebugS logSource "Pysisyphus server starts and becomes ready ..."
  (exitCode, pysisOut, pysisErr) <-
    withWorkingDir (Path.toString pysisDir) $ proc pysisWrapper pysisCmdArgs readProcess
  logDebugS logSource "Pysisyphus sever terminated."

  -- Pysisyphus output.
  writeFileBinary
    (Path.toString $ pysisDir </> Path.relFile "pysisyphus.out")
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

{-
####################################################################################################
-}
-- The JSON instances need to be written by hand unfortunately. The type keyword causes too many
-- problems.

-- | Pysisyphus input file data structure.
data PysisInput = PysisInput
  { geom :: Geom,
    opt :: Opt,
    calc :: Calc
  }

instance FromJSON PysisInput where
  parseJSON = withObject "pysisinput" $ \v -> do
    geom <- v .: "geom"
    opt <- v .: "opt"
    calc <- v .: "calc"
    return PysisInput {geom = geom, opt = opt, calc = calc}

instance ToJSON PysisInput where
  toJSON (PysisInput {geom, opt, calc}) =
    object
      [ "geom" .= geom,
        "opt" .= opt,
        "calc" .= calc
      ]

-- Lenses
instance (k ~ A_Lens, a ~ Geom, b ~ a) => LabelOptic "geom" k PysisInput PysisInput a b where
  labelOptic = lens (\s -> geom s) $ \s b -> s {geom = b}

instance (k ~ A_Lens, a ~ Opt, b ~ a) => LabelOptic "opt" k PysisInput PysisInput a b where
  labelOptic = lens (\s -> opt s) $ \s b -> s {opt = b}

instance (k ~ A_Lens, a ~ Calc, b ~ a) => LabelOptic "calc" k PysisInput PysisInput a b where
  labelOptic = lens (\s -> calc s) $ \s b -> s {calc = b}

{-
====================================================================================================
-}

-- | Settings for the input geometry.
data Geom = Geom
  { type_ :: GType,
    fn :: JFilePathAbs
  }

instance FromJSON Geom where
  parseJSON = withObject "geom" $ \v -> do
    type_ <- v .: "type"
    fn <- v .: "fn"
    return Geom {type_ = type_, fn = fn}

instance ToJSON Geom where
  toJSON (Geom {type_, fn}) =
    object
      [ "type" .= type_,
        "fn" .= fn
      ]

-- Lenses
instance (k ~ A_Lens, a ~ GType, b ~ a) => LabelOptic "gtype" k Geom Geom a b where
  labelOptic = lens (\s -> (type_ :: Geom -> GType) s) $ \s b -> (s {type_ = b} :: Geom)

instance (k ~ A_Lens, a ~ JFilePathAbs, b ~ a) => LabelOptic "fn" k Geom Geom a b where
  labelOptic = lens (\s -> fn s) $ \s b -> s {fn = b}

----------------------------------------------------------------------------------------------------

-- | Types of geometry definitions.
data GType
  = -- | Delocalised internals
    DLC
  | -- | Cartesian coordinates
    Cart
  | -- | Redundant internal coordinates
    Redund

instance FromJSON GType where
  parseJSON v = case v of
    String "dlc" -> pure DLC
    String "cart" -> pure Cart
    String "redund" -> pure Redund
    _ -> fail "encountered unknown field for GType"

instance ToJSON GType where
  toJSON v = case v of
    DLC -> toJSON @Text "dlc"
    Cart -> toJSON @Text "cart"
    Redund -> toJSON @Text "redund"

{-
====================================================================================================
-}

data Opt = Opt
  { type_ :: OType,
    align :: Bool,
    max_cycles :: Int
  }
  deriving (Generic)

instance FromJSON Opt where
  parseJSON = withObject "opt" $ \v -> do
    type_ <- v .: "type"
    align <- v .: "align"
    max_cycles <- v .: "max_cycles"
    return Opt {type_ = type_, align = align, max_cycles = max_cycles}

instance ToJSON Opt where
  toJSON (Opt {type_, align, max_cycles}) =
    object
      [ "type" .= type_,
        "align" .= align,
        "max_cycles" .= max_cycles
      ]

-- Lenses
instance (k ~ A_Lens, a ~ OType, b ~ a) => LabelOptic "type_" k Opt Opt a b where
  labelOptic = lens (\s -> (type_ :: Opt -> OType) s) $ \s b -> (s {type_ = b} :: Opt)

instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "align'" k Opt Opt a b where
  labelOptic = lens (\s -> align s) $ \s b -> s {align = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "max_cycles'" k Opt Opt a b where
  labelOptic = lens (\s -> max_cycles s) $ \s b -> s {max_cycles = b}

----------------------------------------------------------------------------------------------------

-- | Types of geometry optimisers
data OType
  = RFO
  deriving (Generic)

instance FromJSON OType where
  parseJSON v = case v of
    String "rfo" -> pure RFO
    _ -> fail "encountered unknown field for OType"

instance ToJSON OType where
  toJSON v = case v of
    RFO -> toJSON @Text "rfo"

{-
====================================================================================================
-}

data Calc = Calc
  { type_ :: CType,
    address :: Maybe String
  }

instance FromJSON Calc where
  parseJSON = withObject "calc" $ \v -> do
    type_ <- v .: "type"
    address <- v .: "address"
    return Calc {type_ = type_, address = address}

instance ToJSON Calc where
  toJSON (Calc {type_, address}) =
    object
      [ "type" .= type_,
        "address" .= address
      ]

-- Lenses
instance (k ~ A_Lens, a ~ CType, b ~ a) => LabelOptic "type_" k Calc Calc a b where
  labelOptic = lens (\s -> (type_ :: Calc -> CType) s) $ \s b -> (s {type_ = b} :: Calc)

instance (k ~ A_Lens, a ~ Maybe String, b ~ a) => LabelOptic "address''" k Calc Calc a b where
  labelOptic = lens (\s -> address s) $ \s b -> s {address = b}

----------------------------------------------------------------------------------------------------

-- | Calculations types that can be performed with pysisyphus.
data CType
  = -- | Generic calculations, which provide their input by means of a network socket.
    IPISever

-- deriving (Generic)

instance FromJSON CType where
  parseJSON v = case v of
    String "ipiserver" -> pure IPISever
    _ -> fail $ "encountered unknown field for CType"

instance ToJSON CType where
  toJSON v = case v of
    IPISever -> toJSON @Text "ipiserver"
