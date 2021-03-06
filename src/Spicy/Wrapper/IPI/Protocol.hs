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
    molToHessianData,
  )
where

import Data.Binary
import Data.Binary.Get hiding (Done)
import Data.Massiv.Array as Massiv hiding (loop)
import Data.Massiv.Array.Manifest.Vector as Massiv
import Network.Socket
import Network.Socket.ByteString.Lazy
import Optics hiding (view)
import RIO hiding (view, (^.))
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Vector.Storable as VectorS
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
{-# SCC ipiClient #-}
ipiClient ::
  (HasLogFunc env) =>
  IPI ->
  RIO env ()
ipiClient ipi = do
  -- Get the socket address and wait for it to become available.
  logDebugS logSourceC "Waiting for the i-PI server socket to become ready ..."
  scktAbsPath <- case ipi ^. #socketAddr of
    SockAddrUnix path -> return . Path.absFile $ path
    _ -> do
      logErrorS logSourceC "i-PI client expected a UNIX socket for communication with the server."
      throwM . localExc $ "Got wrong socket type for server communication."
  waitForSocket scktAbsPath
  threadDelay 200000
  logDebugS logSourceC "i-PI socket became ready."

  -- Connect to the server socket.
  logDebugS logSourceC $ "Connecting the socket at " <> displayShow (ipi ^. #socketAddr) <> " ..."
  let connectToSocket = connect (ipi ^. #socket) (ipi ^. #socketAddr)
  catchIO (liftIO connectToSocket) $ \e -> do
    logWarnS logSourceC $ "Could not connect to the socket. Got exception: " <> displayShow e
    threadDelay 2000000
    ipiClient ipi
  logInfoS logSourceC $ "Succesfully connected to an i-PI socket at: " <> displayShow (ipi ^. #socketAddr)

  -- Start the loops.
  logInfoS logSourceC "Starting the communication loop ..."
  loop
  logInfoS logSourceC "Finished communication loop."

  -- Disconnect the socket.
  logInfoS logSourceC $ "Closing the socket connection at " <> displayShow (ipi ^. #socketAddr)
  liftIO $ gracefulClose (ipi ^. #socket) 2000
  where
    -- The i-PI communication loop.
    loop = do
      logDebugS logSourceC "Starting a new communication loop."
      -- Start the communication with the server. The server initiates with a string "STATUS".
      fstStatus <- getMsg
      logDebugS logSourceC $ "Response from sever: " <> showMsg fstStatus
      unless (fstStatus == "STATUS") . statusExc $ fstStatus
      logDebugS logSourceC "Server status OK."

      logDebugS logSourceC "Sending READY to server."
      -- Tell the server we are ready to get nuclear coordinates.
      liftIO . sendAll sckt . encode $ Ready

      -- The server sends again a status. If the server is Pysisyphus and the status is "EXIT", the
      -- optimisation has converged and we are done.
      logDebugS logSourceC "Waiting for server status response."
      sndStatus <- getMsg
      logDebugS logSourceC $ "Response from server: " <> showMsg sndStatus
      case sndStatus of
        -- Done with motion of atoms.
        "EXIT" -> do
          logInfoS logSourceC "Got EXIT from the server. Stopping i-PI client."
          atomically . putTMVar (ipi ^. #status) $ Done

        -- Pysisyphus extension to allow for client -> server position updates.
        "NEEDPOS" -> do
          logInfoS logSourceC "Server wants position update."
          atomically . putTMVar (ipi ^. #status) $ WantPos

          -- Get current positions from the server.
          liftIO . sendAll sckt . encode $ HavePos
          nAtoms' <- liftIO $ recv sckt intBytes
          let nAtoms = fromIntegral . runGet getInt32host $ nAtoms'
          serverCoords' <- liftIO $ recv sckt (3 * nAtoms * floatBytes)
          let serverCoords = decode @NetVec $ nAtoms' <> serverCoords'
              cell = CellVecs {a = T 1 0 0, b = T 0 1 0, c = T 0 0 1}
              inverseCell = cell
              posDataBohr = PosData {cell, inverseCell, coords = serverCoords}
              posDataAngstrom = posDataToAngstrom posDataBohr
          atomwiseCoords <- do
            let sVec = getNetVec $ posDataAngstrom ^. #coords
            mVec :: Massiv.Vector S Double <- Massiv.fromVectorM Par (Sz $ VectorS.length sVec) sVec
            reshapeCoords3M (fromIntegral nAtoms) mVec
          logDebugS logSourceC $ "Got last coordinates server knew about: " <> displayShow atomwiseCoords
          atomically . putTMVar out $ posDataAngstrom

          -- Get new positions from Spicy main thread and send them to the i-PI server.
          newPos <- atomically . takeTMVar $ inp
          logDebugS logSourceC $ "Coordinates to send to Pysisyphus:\n" <> displayShow newPos
          liftIO . sendAll sckt . encode $ newPos
          logInfoS logSourceC "Sent new positions to i-PI server."

          -- Next communication loop begins.
          logInfoS logSourceC "Finished i-PI client loop. Reiterating ..."
          loop

        -- Continue to process data.
        "STATUS" -> do
          logInfoS logSourceC "Server status OK."

          -- We send "READY" again and expect another status message. Not documented.
          logDebugS logSourceC "Sending READY to server."
          liftIO . sendAll sckt . encode $ Ready

          -- The server now responds with "POSDATA" and then PosData type.
          posDataMsg <- getMsg
          logDebugS logSourceC $ "Got POSDATA message from sever: " <> showMsg posDataMsg
          cell' <- liftIO $ recv sckt (3 * 3 * floatBytes)
          iCell' <- liftIO $ recv sckt (3 * 3 * floatBytes)
          nAtoms' <- liftIO $ recv sckt intBytes
          let nAtoms = fromIntegral . runGet getInt32host $ nAtoms'
          logDebugS logSourceC $ "Server about to send position data for " <> display nAtoms <> " atoms."
          coords' <- liftIO $ recv sckt (3 * nAtoms * floatBytes)
          let cell = decode cell'
              inverseCell = decode iCell'
              coords = decode $ nAtoms' <> coords'
              posDataBohr = PosData {cell, inverseCell, coords}
              posDataAngstrom = posDataToAngstrom posDataBohr
          atomwiseCoords <- do
            let sVec = getNetVec $ posDataAngstrom ^. #coords
            mVec :: Massiv.Vector S Double <- Massiv.fromVectorM Par (Sz $ VectorS.length sVec) sVec
            reshapeCoords3M (fromIntegral nAtoms) mVec
          logDebugS logSourceC $ "Cell:\n" <> displayShow cell
          logDebugS logSourceC $ "Inverse Cell:\n" <> displayShow inverseCell
          logDebugS logSourceC $ "Coordinate vector:\n" <> displayShow coords
          logDebugS logSourceC $ "Atomwise coordinates (Angstrom): " <> displayShow atomwiseCoords

          -- The posdata are given back to the ONIOM main loop in the shared variable.
          logDebugS logSourceC "Providing Spicy with position data from server."
          atomically . putTMVar out $ posDataAngstrom
          logDebugS logSourceC "Waiting for energies and forces from Spicy."

          -- Wait for Oniom driver to provide force or hessian data. Also Clears the client status.
          thrdStatus <- getMsg
          logDebugS logSourceC $ "Response from server: " <> showMsg thrdStatus
          unless (thrdStatus == "STATUS") . statusExc $ thrdStatus
          logDebugS logSourceC "Telling server that we have new data."
          liftIO . sendAll sckt . encode $ HaveData
          getRequest <- getMsg
          logDebugS logSourceC $ "Got response from server: " <> showMsg getRequest
          case getRequest of
            "GETFORCE" -> do
              logInfoS logSourceC "Server wants force data. Requesting calculation."
              atomically . putTMVar (ipi ^. #status) $ WantForces
              forceData <- atomically . takeTMVar $ inp
              logDebugS logSourceC "Got ForceData from Spicy. Waiting for server status."
              liftIO . sendAll sckt . encode $ ForceReady
              liftIO . sendAll sckt . encode $ forceData
              logInfoS logSourceC "Sent energies and forces to server."
            "GETHESSIAN" -> do
              logInfoS logSourceC "Server wants hessian data. Requesting calculation."
              atomically . putTMVar (ipi ^. #status) $ WantHessian
              hessianData <- atomically . takeTMVar $ inp
              liftIO . sendAll sckt . encode $ HessianReady
              liftIO . sendAll sckt . encode $ hessianData
              logInfoS logSourceC "Sent energies and hessian to server."
            string -> statusExc string

          logDebugS logSourceC "Sent data to server."

          -- Next communication loop begins.
          logInfoS logSourceC "Finished i-PI client loop. Reiterating ..."
          loop
        -- Invalid messages
        msg -> statusExc msg

    -- Function to poll if the given socket is available.
    waitForSocket :: MonadIO m => Path.AbsFile -> m ()
    waitForSocket scktPath = do
      scktExists <- liftIO $ Path.doesFileExist scktPath
      unless scktExists $ do
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
    logSourceC = logSource <> " " <> (fromMaybe mempty . fmap tshow $ ipi ^. #oniomDepth)

    statusExc m = do
      logErrorS logSource $ "Unexpected message from i-PI server: " <> displayShow m
      throwM . localExc $ "expected STATUS message but got: " <> show m
    localExc = IPIException "ipiClient"

----------------------------------------------------------------------------------------------------

-- | Builds a 'ForceData' structure from a collected 'Molecule'. Conversion from Angstrom to Bohr
-- happens here.
molToForceData :: MonadThrow m => Molecule -> m InputData
molToForceData mol = do
  -- Obtain the potential energy.
  potentialEnergy <-
    maybe2MThrow (localExc "Energy is missing from the molecule") $
      mol ^. #energyDerivatives % #energy

  -- Obtain the forces in Hartree/Angstrom
  gradientAngstrom :: Massiv.Vector S Double <-
    maybe2MThrow (localExc "Gradient is missing from the molecule") $
      getVectorS <$> (mol ^. #energyDerivatives % #gradient)

  -- Construct the virial matrix and convert from Hartree/Angstrom to Bohr/Angstrom.
  let virial = CellVecs (T 0 0 0) (T 0 0 0) (T 0 0 0)
      forcesBohr = compute @S . Massiv.map (convertA2B . (* (-1))) $ gradientAngstrom

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

-- | Converts a molecule to the hessian data for iPI. Takes care of the Angstrom -> Bohr conversion.
molToHessianData :: MonadThrow m => Molecule -> m InputData
molToHessianData mol = do
  -- Obtain the potential energy.
  potentialEnergy <-
    maybe2MThrow (localExc "Energy is missing from the molecule") $
      mol ^. #energyDerivatives % #energy

  -- Obtain the hessian in Hartree/Angstrom^2
  hessianAngstrom :: Massiv.Matrix S Double <-
    maybe2MThrow (localExc "Hessian is missing from the molecule") $
      getMatrixS <$> (mol ^. #energyDerivatives % #hessian)

  let hessianBohr = compute @S . Massiv.map convertA2B $ hessianAngstrom
  return
    HessianData
      { potentialEnergy = potentialEnergy,
        hessian = hessianBohr
      }
  where
    convertA2B v = v / ((angstrom2Bohr 1) ^ (2 :: Int))
    localExc = SpicyIndirectionException "molToForceData"

-- | Converts the posdata as obtained from the server from bohr to angstrom.
posDataToAngstrom :: PosData -> PosData
posDataToAngstrom PosData {..} =
  let cellA = fmap bohr2Angstrom cell
      invCellA = fmap bohr2Angstrom inverseCell
      coordsA = NetVec . VectorS.map bohr2Angstrom . getNetVec $ coords
   in PosData {cell = cellA, inverseCell = invCellA, coords = coordsA}
