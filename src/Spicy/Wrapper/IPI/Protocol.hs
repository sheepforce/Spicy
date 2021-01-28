-- |
-- Module      : Spicy.Wrapper.IPI.Protocol
-- Description : Client side implementation of the (augmented) i-PI communication protocol.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX
--
-- Interaction with Pysisyphus.
module Spicy.Wrapper.IPI.Protocol
  ( ipiClient,
  )
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Network.Socket.ByteString.Lazy
import Optics hiding (view)
import RIO hiding (view, (^.))
import Spicy.Common
import Spicy.RuntimeEnv
import Spicy.Wrapper.IPI.Types

logSource :: LogSource
logSource = "i-PI client"

----------------------------------------------------------------------------------------------------

-- | A simple i-PI client whose structure is suitable for optimisations. Energies are expected as
-- Hartree and gradients as Hartree/Bohr. Conversion from Spicy's Hartree/Angstrom must happen
-- outside this function, when constructing and deconstructing the 'ForceData' and 'PosData' types.
ipiClient ::
  (HasLogFunc env) =>
  IPI ->
  RIO env ()
ipiClient ipi = do
  logDebugS logSource "New i-PI loop. Waiting for status message from server."
  -- Start the communication with the server. The server initiates with a string "STATUS".
  fstStatus <- liftIO $ recv sckt (6 * charBytes)
  logDebugS logSource $ "Response from sever: " <> showMsg fstStatus
  unless (fstStatus == "STATUS") . statusExc $ fstStatus
  logDebugS logSource "Server status OK."

  logDebugS logSource "Sending READY to server."
  -- Tell the server we are ready to get nuclear coordinates.
  liftIO . sendAll sckt . encode $ Ready

  -- The server sends again a status. If the server is Pysisyphus and the status is "EXIT", the
  -- optimisation has converged and we are done.
  logDebugS logSource "Waiting for server status response."
  sndStatus <- liftIO $ recv sckt (6 * charBytes)
  logDebugS logSource $ "Response from server: " <> showMsg sndStatus
  when (sndStatus == "EXIT") . logDebugS logSource $
    "Got EXIT from server. Shutting down the client."
  unless (sndStatus == "STATUS" || sndStatus == "EXIT") . statusExc $ sndStatus
  unless (sndStatus == "EXIT") $ do
    -- EXIT is a Pysisyphus extension.
    -- We send "READY" again. Not documented.
    logDebugS logSource "Sending READY to server."
    liftIO . sendAll sckt . encode $ Ready

    -- The server now responds with "POSDATA" and then PosData type.
    posDataMsg <- liftIO $ recv sckt (7 * charBytes)
    logDebugS logSource $ "Got POSDATA message from sever: " <> showMsg posDataMsg
    cell' <- liftIO $ recv sckt (3 * 3 * floatBytes)
    logDebugS logSource $ "Got simulation cell from server."
    iCell' <- liftIO $ recv sckt (3 * 3 * floatBytes)
    logDebugS logSource $ "Got inverse simulation cell from server."
    nAtoms' <- liftIO $ recv sckt intBytes
    let nAtoms = fromIntegral . runGet getInt32host $ nAtoms'
    logDebugS logSource $ "Server about to send position data for " <> display nAtoms <> " atoms."
    coords <- decode <$> (liftIO $ recv sckt (nAtoms * floatBytes))
    logDebugS logSource $ "Got position data from server."
    let cell = decode cell'
        iCell = decode iCell'
        posData =
          PosData
            { cell = cell,
              inverseCell = iCell,
              coords = coords
            }

    -- The posdata are given back to the ONIOM main loop in the shared variable.
    logDebugS logSource $ "Providing Spicy with Position data from server."
    atomically . putTMVar out $ posData
    logDebugS logSource $ "Waiting for energies and forces from Spicy."

    -- We wait for the ONIOM driver to provide new force data. The server should now send another
    -- status request.
    forceData <- atomically . takeTMVar $ inp
    logDebugS logSource "Got ForceData from Spicy. Waiting for server status."
    thrdStatus <- liftIO $ recv sckt (6 * charBytes)
    logDebugS logSource $ "Response from server: " <> showMsg thrdStatus
    unless (thrdStatus == "STATUS") . statusExc $ thrdStatus
    logDebugS logSource "Telling server that we have new data."
    liftIO $ sendAll sckt "HAVEDATA"
    getforce <- liftIO $ recv sckt (8 * charBytes)
    logDebugS logSource $ "Got response from server: " <> showMsg getforce
    unless (getforce == "GETFORCE") . statusExc $ getforce
    logDebugS logSource "Server wants force data. Sending to server."
    liftIO $ sendAll sckt "FORCEREADY"
    liftIO . sendAll sckt . encode $ forceData
    logDebugS logSource "Sent energies and forces to server."

    -- iPI can now wait for additional bytes. We don't need additional information, so we send a 0.
    liftIO $ sendAll sckt . runPut . putInt32host . fromIntegral $ (0 :: Int)

    -- Next communication loop begins.
    logDebugS logSource "Finished i-PI client loop. Reiterating ..."
    ipiClient ipi
  where
    intBytes = 4
    floatBytes = 8
    charBytes = 1
    sckt = ipi ^. #socket
    inp = ipi ^. #input
    out = ipi ^. #output
    showMsg = displayBytesUtf8 . toStrictBytes
    statusExc m = do
      logErrorS logSource $ "Unexpected message from i-PI server: " <> displayShow m
      throwM . localExc $ "expected STATUS message but got: " <> show m
    localExc = IPIException "ipiClient"
