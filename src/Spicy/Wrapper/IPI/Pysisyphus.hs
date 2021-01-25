-- |
-- Module      : Spicy.Wrapper.Pysisyphus
-- Description : Interactions with Pysisyphus
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Interaction with Pysisyphus.
module Spicy.Wrapper.Pysisyphus
  ( launchPysisServer,
  )
where

import Optics hiding (view)
import RIO hiding (view)
import RIO.Process
import Spicy.Common
import Spicy.RuntimeEnv

-- | The log source for RIO logger.
logSource :: LogSource
logSource = "Pysisyphus"

----------------------------------------------------------------------------------------------------

-- | Starts a pysisyphus server with extended i-PI protocol
launchPysisServer :: (HasWrapperConfigs env, HasLogFunc env, HasPysis env, HasProcessContext env) => RIO env ()
launchPysisServer = do
  pysisWrapper <-
    view (wrapperConfigsL % #pysisyphus) >>= \wrapper -> case wrapper of
      Just w -> return w
      Nothing -> throwM . localExc $ "Pysisyphus not configured."

  let pysisCmdArgs = mempty

  -- LOG
  logInfo "Starting Pysisyphus server."

  -- Start Pysisyphus.
  (exitCode, pysisOut, pysisErr) <- proc pysisWrapper pysisCmdArgs readProcess

  -- LOG
  logDebugS logSource . displayShow $ pysisOut
  logErrorS logSource . displayShow $ pysisErr

  unless (exitCode == ExitSuccess) . throwM . localExc $ "Pysisyphus crashed with errors."
  where
    localExc = WrapperGenericException "launchPysisServer"
