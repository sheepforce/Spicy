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
  )
where

import Data.Aeson
import Data.Yaml.Pretty
import Network.Socket
import Optics hiding (view)
import RIO hiding (lens, view, (^.))
import qualified RIO.Char as Char
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

----------------------------------------------------------------------------------------------------

-- | Options for JSON/YAML in Pysis.
jsonOpts :: Options
jsonOpts = defaultOptions {constructorTagModifier = fmap Char.toLower . filter (/= '\'')}

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
  RIO env (Async ())
providePysis = do
  -- Obtain initial information.
  pysisIPI <- view pysisL

  -- Start another thread in the backgroud, that runs Pysisyphus.
  pysisServerThread <- async runPysisServer

  -- Now start the i-PI client loop.
  ipiClient pysisIPI

  -- The i-PI client should terminate and return as soon as it receives an "EXIT" message.
  -- Potentially the pysis server continues to run. Its thread will be returned and should be
  -- cancelled after some waiting time (5 s maybe?).
  return pysisServerThread

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
      throwM . PysisException $ "Wrong socket type given: INET"
    SockAddrInet6 _ _ _ _ -> do
      logErrorS logSource "Wrong socket type given. Need a UNIX socket but got an INET6 socket."
      throwM . PysisException $ "Wrong socket type given: INET6"
  pysisWrapperM <- view $ wrapperConfigsL % #pysisyphus
  pysisWrapper <- case pysisWrapperM of
    Just path -> return . Path.toString . getFilePath $ path
    Nothing -> do
      logErrorS logSource "Pysisyphus is not configured. Cannot run optimisations. Quiting ..."
      throwM . PysisException $ "pysisyphus not found."

  -- Write initial coordinates for Pysisyphus to a file at the given location.
  initCoordFile <- view $ pysisL % #initCoords
  initCoordFileAbs <- liftIO . Path.genericMakeAbsoluteFromCwd $ initCoordFile
  writeXYZ mol >>= writeFileUTF8 (Path.toAbsRel initCoordFile)

  -- Construct a pysisyphus input file.
  let pysisInput =
        PysisInput
          { geom =
              Geom -- TODO (phillip|p=50|#Unfinished) - The coordinate type should be obtained from the CalcInput from the molecule.
                { gtype = Cart,
                  fn = JFilePathAbs initCoordFileAbs
                },
            opt =
              Opt -- TODO (phillip|p=50|#Unfinished) - The optimisation type should be obtained from the CalcInput from the molecule.
                { type' = RFO,
                  align = False,
                  max_cycles = 100
                },
            calc =
              Calc
                { type' = IPISever,
                  address = Just socketPath
                }
          }

  -- Make a working directory for Pysisyphus and write the input file to it.
  pysisDir <- view $ pysisL % #workDir
  let pysisYamlPath = pysisDir </> Path.relFile "pysis_servers_spicy.yml"
      pysisYaml = decodeUtf8Lenient . encodePretty defConfig $ pysisInput
  liftIO $ Path.createDirectoryIfMissing True pysisDir
  writeFileUTF8 pysisYamlPath pysisYaml

  -- Make the pysisDir the working directory of Pysis and run it on the input. This should also
  -- start the i-PI server of pysisyphus.
  let pysisCmdArgs =
        [ Path.toString . Path.takeFileName $ pysisYamlPath
        ]
  (exitCode, pysisOut, pysisErr) <-
    withWorkingDir (Path.toString pysisDir) $ proc pysisWrapper pysisCmdArgs readProcess

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

{-
####################################################################################################
-}

-- | Pysisyphus input file data structure.
data PysisInput = PysisInput
  { geom :: Geom,
    opt :: Opt,
    calc :: Calc
  }
  deriving (Generic)

instance FromJSON PysisInput where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON PysisInput where
  toEncoding = genericToEncoding jsonOpts

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
  { gtype :: GType,
    fn :: JFilePathAbs
  }
  deriving (Generic)

instance FromJSON Geom where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Geom where
  toEncoding = genericToEncoding jsonOpts

-- Lenses
instance (k ~ A_Lens, a ~ GType, b ~ a) => LabelOptic "gtype" k Geom Geom a b where
  labelOptic = lens (\s -> gtype s) $ \s b -> s {gtype = b}

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
  deriving (Generic)

instance FromJSON GType where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON GType where
  toEncoding = genericToEncoding jsonOpts

{-
====================================================================================================
-}

data Opt = Opt
  { type' :: OType,
    align :: Bool,
    max_cycles :: Int
  }
  deriving (Generic)

instance FromJSON Opt where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Opt where
  toEncoding = genericToEncoding jsonOpts

-- Lenses
instance (k ~ A_Lens, a ~ OType, b ~ a) => LabelOptic "type'" k Opt Opt a b where
  labelOptic = lens (\s -> (type' :: Opt -> OType) s) $ \s b -> (s {type' = b} :: Opt)

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
  parseJSON = genericParseJSON jsonOpts

instance ToJSON OType where
  toEncoding = genericToEncoding jsonOpts

{-
====================================================================================================
-}

data Calc = Calc
  { type' :: CType,
    address :: Maybe String
  }
  deriving (Generic)

instance FromJSON Calc where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Calc where
  toEncoding = genericToEncoding jsonOpts

-- Lenses
instance (k ~ A_Lens, a ~ CType, b ~ a) => LabelOptic "type''" k Calc Calc a b where
  labelOptic = lens (\s -> (type' :: Calc -> CType) s) $ \s b -> (s {type' = b} :: Calc)

instance (k ~ A_Lens, a ~ Maybe String, b ~ a) => LabelOptic "address''" k Calc Calc a b where
  labelOptic = lens (\s -> address s) $ \s b -> s {address = b}

----------------------------------------------------------------------------------------------------

-- | Calculations types that can be performed with pysisyphus.
data CType
  = -- | Generic calculations, which provide their input by means of a network socket.
    IPISever
  deriving (Generic)

instance FromJSON CType where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON CType where
  toEncoding = genericToEncoding jsonOpts
