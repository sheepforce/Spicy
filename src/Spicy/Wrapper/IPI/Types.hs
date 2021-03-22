{-# OPTIONS_GHC -Wno-partial-fields #-}

-- |
-- Module      : Spicy.Wrapper.IPI.Types
-- Description : Communication with i-PI servers.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- i-PI type definitions and serialisation to binary network communication. See
-- <<http://ipi-code.org/assets/pdf/manual.pdf i-PI>> protocol (section 3.3.1).
module Spicy.Wrapper.IPI.Types
  ( IPI (..),
    DataRequest (..),
    NetVec (..),
    Status (..),
    T (..),
    CellVecs (..),
    PosData (..),
    InputData (..),
  )
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Massiv.Array as Massiv
import Data.Vector.Binary
import Network.Socket as Net hiding (socket)
import qualified Network.Socket as Net
import Optics
import RIO hiding (lens)
import qualified RIO.ByteString as ByteString
import qualified RIO.Vector.Storable as VectorS
import Spicy.Common
import System.Path ((</>))
import qualified System.Path as Path

-- | i-PI communication settings and variables. Generic over i-PI implementations.
data IPI = IPI
  { -- | The network socket used for communication with the server.
    socket :: Socket,
    -- | The address of the socket in use.
    socketAddr :: SockAddr,
    -- | Input channel. When this variable is filled the i-PI server starts its calculation of
    -- new positions.
    input :: TMVar InputData,
    -- | Output channel. When the i-PI server has finished its calculation, these values will be
    -- filled and are ready to be consumed by Spicy.
    output :: TMVar PosData,
    -- | Working directory of the process.
    workDir :: Path.AbsRelDir,
    -- | The path to a coordinate file, used to initialise the i-PI server with coordinates.
    initCoords :: Path.AbsRelFile,
    -- | The status of the i-PI server.
    status :: TMVar DataRequest
  }

instance DefaultIO IPI where
  defIO = do
    socket <- liftIO $ Net.socket AF_UNIX Stream defaultProtocol
    ipiIn <- newEmptyTMVarIO
    ipiOut <- newEmptyTMVarIO
    status <- newEmptyTMVarIO
    return
      IPI
        { socket = socket,
          socketAddr = SockAddrUnix "./ipi.socket",
          input = ipiIn,
          output = ipiOut,
          workDir = Path.dirPath ".",
          initCoords = Path.dirPath "." </> Path.relFile "InitialCoords.xyz",
          status = status
        }

-- Lenses
instance (k ~ A_Lens, a ~ Socket, b ~ a) => LabelOptic "socket" k IPI IPI a b where
  labelOptic = lens (socket :: IPI -> Socket) $ \s b -> s {socket = b}

instance (k ~ A_Lens, a ~ SockAddr, b ~ a) => LabelOptic "socketAddr" k IPI IPI a b where
  labelOptic = lens socketAddr $ \s b -> s {socketAddr = b}

instance (k ~ A_Lens, a ~ TMVar InputData, b ~ a) => LabelOptic "input" k IPI IPI a b where
  labelOptic = lens (input :: IPI -> TMVar InputData) $ \s b -> (s {input = b} :: IPI)

instance (k ~ A_Lens, a ~ TMVar PosData, b ~ a) => LabelOptic "output" k IPI IPI a b where
  labelOptic = lens (output :: IPI -> TMVar PosData) $ \s b -> (s {output = b} :: IPI)

instance (k ~ A_Lens, a ~ Path.AbsRelDir, b ~ a) => LabelOptic "workDir" k IPI IPI a b where
  labelOptic = lens workDir $ \s b -> s {workDir = b}

instance (k ~ A_Lens, a ~ Path.AbsRelFile, b ~ a) => LabelOptic "initCoords" k IPI IPI a b where
  labelOptic = lens initCoords $ \s b -> s {initCoords = b}

instance (k ~ A_Lens, a ~ TMVar DataRequest, b ~ a) => LabelOptic "status" k IPI IPI a b where
  labelOptic = lens status $ \s b -> s {status = b}

----------------------------------------------------------------------------------------------------

-- | Vectors as transfered over the network. Takes into account that 3N and not N elements are
-- expected.
newtype NetVec = NetVec {getNetVec :: VectorS.Vector Double}
  deriving (Eq, Show)

instance Binary NetVec where
  put (NetVec v) = genericPutVectorWith (putInt32host . fromIntegral . (`div` 3)) putDoublehost v
  get = NetVec <$> genericGetVectorWith (fromIntegral . (* 3) <$> getInt32host) getDoublehost

----------------------------------------------------------------------------------------------------

-- | Possible status messages from the i-PI server.
data Status
  = NeedInit
  | Ready
  | HaveData
  | ForceReady
  | HessianReady
  deriving (Eq, Show)

-- |
instance Binary Status where
  put NeedInit = putByteString "NEEDINIT    "
  put Ready = putByteString "READY       "
  put HaveData = putByteString "HAVEDATA    "
  put ForceReady = putByteString "FORCEREADY  "
  put HessianReady = putByteString "HESSIANREADY"

  get = do
    msg <- getRemainingLazyByteString
    case msg of
      "NEEDINIT" -> return NeedInit
      "READY" -> return Ready
      "HAVEDATA" -> return HaveData
      "FORCEREADY" -> return ForceReady
      "HESSIANREADY" -> return HessianReady
      _ -> fail "invalid status message"

data DataRequest
  = Done
  | WantForces
  | WantHessian
  deriving (Eq, Show)

----------------------------------------------------------------------------------------------------

-- | A triple of values, e.g. a vector in \(R^3\).
data T a = T a a a deriving (Eq, Show)

instance Functor T where
  fmap f (T a b c) = T (f a) (f b) (f c)

-- Serialisation
instance Binary (T Double) where
  put (T a b c) = do
    putDoublehost a
    putDoublehost b
    putDoublehost c

  get = do
    a <- getDoublehost
    b <- getDoublehost
    c <- getDoublehost
    return $ T a b c

----------------------------------------------------------------------------------------------------

-- | The cell vectors of a simulation cell.
data CellVecs a = CellVecs
  { a :: T a,
    b :: T a,
    c :: T a
  }
  deriving (Eq, Show)

instance Functor CellVecs where
  fmap f CellVecs {..} = CellVecs (fmap f a) (fmap f b) (fmap f c)

-- Serialisation
instance Binary (CellVecs Double) where
  put CellVecs {a, b, c} = do
    put a
    put b
    put c

  get = do
    a <- get
    b <- get
    c <- get
    return $ CellVecs {a = a, b = b, c = c}

----------------------------------------------------------------------------------------------------

-- | Position data as in the i-PI protocoll. Usually send by an i-PI server to the client. The client
-- is Spicy.
data PosData = PosData
  { cell :: CellVecs Double,
    inverseCell :: CellVecs Double,
    coords :: NetVec
  }
  deriving (Show)

-- Serialisation
instance Binary PosData where
  put PosData {cell, inverseCell, coords} = do
    put cell
    put inverseCell
    put coords

  get = do
    cell <- get
    inverseCell <- get
    coords <- get
    return $ PosData {cell = cell, inverseCell = inverseCell, coords = coords}

-- Lenses
instance (k ~ A_Lens, a ~ CellVecs Double, b ~ a) => LabelOptic "cell" k PosData PosData a b where
  labelOptic = lens cell $ \s b -> s {cell = b}

instance (k ~ A_Lens, a ~ CellVecs Double, b ~ a) => LabelOptic "inverseCell" k PosData PosData a b where
  labelOptic = lens inverseCell $ \s b -> s {inverseCell = b}

instance (k ~ A_Lens, a ~ NetVec, b ~ a) => LabelOptic "coords" k PosData PosData a b where
  labelOptic = lens coords $ \s b -> s {coords = b}

----------------------------------------------------------------------------------------------------

-- | The force data on the current structure. This data is sent to the server, which calculates new
-- positions from the implicitly existend coordinates. This is the information sent by Spicy.
data InputData
  = ForceData
      { potentialEnergy :: Double,
        forces :: NetVec,
        virial :: CellVecs Double,
        optionalData :: ByteString
      }
  | HessianData
      { potentialEnergy :: Double,
        -- | Molecular cartesian hessian in Hartree/Bohr^2. This must encode the symmetric square
        -- matrix.
        hessian :: Matrix S Double
      }

-- Serialisation
instance Binary InputData where
  put ForceData {potentialEnergy, forces, virial, optionalData} = do
    putDoublehost potentialEnergy
    put forces
    put virial
    let optionalLength = fromIntegral . ByteString.length $ optionalData
    putInt32host optionalLength
    putByteString optionalData
  put HessianData {potentialEnergy, hessian} = do
    putDoublehost potentialEnergy
    let Sz (m :. _) = size hessian
        nAtoms = m `div` 3
        hessianVec = toStorableVector . flatten $ hessian
    putInt32host . fromIntegral $ nAtoms
    genericPutVectorWith (\_ -> putInt32host . fromIntegral $ nAtoms) putDoublehost hessianVec

  get = error "Decoding force or hessian data is not possible"
