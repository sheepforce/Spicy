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
  -- Start the communication with the server. The server initiates with a string "STATUS".
  fstStatus <- liftIO $ recv sckt (6 * charBytes)
  unless (fstStatus == "STATUS") . statusExc $ fstStatus

  -- Tell the server we are ready to get nuclear coordinates.
  liftIO . sendAll sckt . encode $ Ready

  -- The server sends again a status. If the server is Pysisyphus and the status is "EXIT", the
  -- optimisation has converged and we are done.
  sndStatus <- liftIO $ recv sckt (6 * charBytes)
  unless (sndStatus == "STATUS" || sndStatus == "EXIT") . statusExc $ sndStatus
  unless (sndStatus == "EXIT") $ do
    -- EXIT is a Pysisyphus extension.
    -- We send "READY" again. Not documented.
    liftIO . sendAll sckt . encode $ Ready

    -- The server now responds with "POSDATA" and then PosData type.
    _posDataMsg <- liftIO $ recv sckt (7 * charBytes)
    cell' <- liftIO $ recv sckt (3 * 3 * floatBytes)
    iCell' <- liftIO $ recv sckt (3 * 3 * floatBytes)
    nAtoms' <- liftIO $ recv sckt intBytes
    let nAtoms = fromIntegral . runGet getInt32host $ nAtoms'
    coords' <- liftIO $ recv sckt (nAtoms * floatBytes)
    let coords = decode $ nAtoms' <> coords'
        cell = decode cell'
        iCell = decode iCell'
        posData =
          PosData
            { cell = cell,
              inverseCell = iCell,
              coords = coords
            }

    -- The posdata are given back to the ONIOM main loop in the shared variable.
    atomically . putTMVar out $ posData

    -- We wait for the ONIOM driver to provide new force data. The server should now send another
    -- status request.
    forceData <- atomically . takeTMVar $ inp
    thrdStatus <- liftIO $ recv sckt (6 * charBytes)
    unless (thrdStatus == "STATUS") . statusExc $ thrdStatus
    liftIO $ sendAll sckt "HAVEDATA"
    getforce <- liftIO $ recv sckt (8 * charBytes)
    unless (getforce == "GETFORCE") . statusExc $ getforce
    liftIO $ sendAll sckt "FORCEREADY"
    liftIO . sendAll sckt . encode $ forceData

    -- iPI can now wait for additional bytes. We don't need additional information, so we send a 0.
    liftIO $ sendAll sckt . runPut . putInt32host . fromIntegral $ (0 :: Int)

    -- Next communication loop begins.
    ipiClient ipi
  where
    intBytes = 4
    floatBytes = 8
    charBytes = 1
    sckt = ipi ^. #socket
    inp = ipi ^. #input
    out = ipi ^. #output
    statusExc m = do
      logErrorS logSource $ "Unexpected message from i-PI server: " <> displayShow m
      throwM . localExc $ "expected STATUS message but got: " <> show m
    localExc = IPIException "ipiClient"
