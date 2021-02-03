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
    molToForceData,
  )
where

import Data.Binary
import Data.Binary.Get hiding (Done)
import Data.Binary.Put
import Data.Massiv.Array as Massiv hiding (loop)
import Data.Massiv.Array.Manifest.Vector as Massiv
import Network.Socket
import Network.Socket.ByteString.Lazy
import Optics hiding (view)
import RIO hiding (view, (^.))
import qualified RIO.ByteString.Lazy as BL
import Spicy.Common
import Spicy.Data
import Spicy.Molecule
import Spicy.RuntimeEnv
import Spicy.Wrapper.IPI.Types
import qualified System.Path as Path
import qualified System.Path.Directory as Path

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
  -- Get the socket address and wait for it to become available.
  logDebugS logSource "Waiting for the i-PI server socket to become ready ..."
  scktAbsPath <- case ipi ^. #socketAddr of
    SockAddrUnix path -> return . Path.absFile $ path
    _ -> do
      logErrorS logSource "i-PI client expected a UNIX socket for communication with the server."
      throwM . localExc $ "Got wrong socket type for server communication."
  waitForSocket scktAbsPath
  threadDelay 200000
  logDebugS logSource "i-PI socket became ready."

  -- Connect to the server socket.
  logDebugS logSource $ "Connecting the socket at " <> displayShow (ipi ^. #socketAddr) <> " ..."
  let connectToSocket = connect (ipi ^. #socket) (ipi ^. #socketAddr)
  catchIO (liftIO connectToSocket) $ \e -> do
    logWarnS logSource $ "Could not connect to the socket. Got exception: " <> displayShow e
    threadDelay 2000000
    ipiClient ipi
  logDebugS logSource "Connected!"

  -- Start the loops.
  logDebugS logSource "Starting the communication loops ..."
  loop
  logDebugS logSource "Finished communication loop."

  -- Disconnect the socket.
  logDebugS logSource "Closing the socket connection ..."
  liftIO $ gracefulClose (ipi ^. #socket) 2000
  logDebugS logSource "Closed!"
  where
    -- The i-PI communication loop.
    loop = do
      logDebugS logSource "Starting a new communication loop."
      -- Start the communication with the server. The server initiates with a string "STATUS".
      fstStatus <- getMsg
      logDebugS logSource $ "Response from sever: " <> showMsg fstStatus
      unless (fstStatus == "STATUS") . statusExc $ fstStatus
      logDebugS logSource "Server status OK."

      logDebugS logSource "Sending READY to server."
      -- Tell the server we are ready to get nuclear coordinates.
      liftIO . sendAll sckt . encode $ Ready

      -- The server sends again a status. If the server is Pysisyphus and the status is "EXIT", the
      -- optimisation has converged and we are done.
      logDebugS logSource "Waiting for server status response."
      sndStatus <- getMsg
      logDebugS logSource $ "Response from server: " <> showMsg sndStatus
      case sndStatus of
        -- Done with motion of atoms.
        "EXIT" -> do
          logDebugS logSource "Got EXIT from the server. Stoppin i-PI client."
          atomically . putTMVar (ipi ^. #status) $ Done
        -- Continue to process data.
        "STATUS" -> do
          logDebugS logSource "Server status OK."
          atomically . putTMVar (ipi ^. #status) $ MoreData

          -- We send "READY" again and expect another status message. Not documented.
          logDebugS logSource "Sending READY to server."
          liftIO . sendAll sckt . encode $ Ready

          -- The server now responds with "POSDATA" and then PosData type.
          posDataMsg <- getMsg
          logDebugS logSource $ "Got POSDATA message from sever: " <> showMsg posDataMsg
          cell' <- liftIO $ recv sckt (3 * 3 * floatBytes)
          iCell' <- liftIO $ recv sckt (3 * 3 * floatBytes)
          nAtoms' <- liftIO $ recv sckt intBytes
          let nAtoms = fromIntegral . runGet getInt32host $ nAtoms'
          logDebugS logSource $ "Server about to send position data for " <> display nAtoms <> " atoms."
          coords' <- liftIO $ recv sckt (3 * nAtoms * floatBytes)
          let nCoordElemsBS = runPut . putInt32host . fromIntegral $ (3 * nAtoms)
              cell = decode cell'
              iCell = decode iCell'
              posData =
                PosData
                  { cell = cell,
                    inverseCell = iCell,
                    coords = decode $ nCoordElemsBS <> coords'
                  }
          logDebugS logSource $ "Cell:\n" <> displayShow cell
          logDebugS logSource $ "Inverse Cell:\n" <> displayShow iCell
          logDebugS logSource $ "Coordinate vector:\n" <> displayShow (coords posData)

          -- The posdata are given back to the ONIOM main loop in the shared variable.
          logDebugS logSource $ "Providing Spicy with Position data from server."
          atomically . putTMVar out $ posData
          logDebugS logSource $ "Waiting for energies and forces from Spicy."

          -- Wait for Oniom driver to provide force data. Also Clears the client status.
          forceData <- atomically . takeTMVar $ inp
          logDebugS logSource "Got ForceData from Spicy. Waiting for server status."
          thrdStatus <- getMsg
          logDebugS logSource $ "Response from server: " <> showMsg thrdStatus
          unless (thrdStatus == "STATUS") . statusExc $ thrdStatus
          logDebugS logSource "Telling server that we have new data."
          liftIO $ sendAll sckt "HAVEDATA"
          getforce <- getMsg
          logDebugS logSource $ "Got response from server: " <> showMsg getforce
          unless (getforce == "GETFORCE") . statusExc $ getforce
          logDebugS logSource "Server wants force data. Sending to server."
          liftIO $ sendAll sckt "FORCEREADY"
          liftIO . sendAll sckt . encode $ forceData
          logDebugS logSource "Sent energies and forces to server."

          -- iPI can now wait for additional bytes. We don't need additional information, so we send a 0.
          liftIO $ sendAll sckt . runPut . putInt32host $ (0 :: Int32)

          -- Next communication loop begins.
          logDebugS logSource "Finished i-PI client loop. Reiterating ..."
          loop
        -- Invalid messages
        msg -> statusExc msg

    -- Function to poll if the given socket is available.
    waitForSocket :: MonadIO m => Path.AbsFile -> m ()
    waitForSocket scktPath = do
      scktExists <- liftIO $ Path.doesFileExist scktPath
      unless (scktExists) $ do
        threadDelay 500000
        waitForSocket scktPath

    -- Convenience binds and functions.
    msgSize = 12
    getMsg = fmap (BL.filter (/= 32)) . liftIO $ recv sckt msgSize
    intBytes = 4
    floatBytes = 8
    sckt = ipi ^. #socket
    inp = ipi ^. #input
    out = ipi ^. #output
    showMsg = displayBytesUtf8 . toStrictBytes
    statusExc m = do
      logErrorS logSource $ "Unexpected message from i-PI server: " <> displayShow m
      throwM . localExc $ "expected STATUS message but got: " <> show m
    localExc = IPIException "ipiClient"

----------------------------------------------------------------------------------------------------

-- | Builds a 'ForceData' structure from a collected 'Molecule'. Conversion from Angstrom to Bohr
-- happens here.
molToForceData :: MonadThrow m => Molecule -> m ForceData
molToForceData mol = do
  -- Obtain the potential energy.
  potentialEnergy <-
    maybe2MThrow (localExc "Energy is missing from the molecule") $
      mol ^. #energyDerivatives % #energy

  -- Obtain the forces in Hartree/Angstrom
  forcesAngstrom :: Massiv.Vector S Double <-
    maybe2MThrow (localExc "Gradient is missing from the molecule") $
      getVectorS <$> (mol ^. #energyDerivatives % #gradient)

  -- Construct the virial matrix and convert from Hartree/Angstrom to Bohr/Angstrom.
  let virial = CellVecs (T 0 0 0) (T 0 0 0) (T 0 0 0)
      forcesBohr = compute @S . Massiv.map convertA2B $ forcesAngstrom

  return
    ForceData
      { potentialEnergy = potentialEnergy,
        forces = NetVec . Massiv.toVector $ forcesBohr,
        virial = virial,
        optionalData = mempty
      }
  where
    convertA2B v = v / (angstrom2Bohr 1)
    localExc = SpicyIndirectionException "molToForceData"
