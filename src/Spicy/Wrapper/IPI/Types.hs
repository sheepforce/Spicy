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
  ( NetVec(..),
    Status (..),
    T (..),
    CellVecs (..),
    PosData (..),
    ForceData (..),
  )
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Vector.Binary
import Optics
import RIO hiding (lens)
import qualified RIO.ByteString as ByteString
import qualified RIO.Vector.Storable as VectorS

-- | Vectors as transfered over the network.
newtype NetVec = NetVec (VectorS.Vector Double)

instance Binary NetVec where
  put (NetVec v) = genericPutVectorWith (putInt32host . fromIntegral) putDoublehost v
  get = NetVec <$> genericGetVectorWith (fromIntegral <$> getInt32host) getDoublehost

----------------------------------------------------------------------------------------------------

-- | Possible status messages from the i-PI server.
data Status
  = NeedInit
  | Ready
  | HaveData
  deriving (Eq, Show)

instance Binary Status where
  put NeedInit = putByteString "NEEDINIT"
  put Ready = putByteString "READY"
  put HaveData = putByteString "HAVEDATA"

  get = do
    msg <- getRemainingLazyByteString
    case msg of
      "NEEDINIT" -> return NeedInit
      "READY" -> return Ready
      "HAVEDATA" -> return HaveData
      _ -> fail "invalid status message"

----------------------------------------------------------------------------------------------------

-- | A triple of values, e.g. a vector in \(R^3\).
data T a = T a a a

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
data CellVecs = CellVecs
  { a :: T Double,
    b :: T Double,
    c :: T Double
  }

-- Serialisation
instance Binary CellVecs where
  put (CellVecs {a, b, c}) = do
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
  { cell :: CellVecs,
    inverseCell :: CellVecs,
    coords :: NetVec
  }

-- Serialisation
instance Binary PosData where
  put (PosData {cell, inverseCell, coords}) = do
    put cell
    put inverseCell
    put coords

  get = do
    cell <- get
    inverseCell <- get
    coords <- get
    return $ PosData {cell = cell, inverseCell = inverseCell, coords = coords}

-- Lenses
instance (k ~ A_Lens, a ~ CellVecs, b ~ a) => LabelOptic "cell" k PosData PosData a b where
  labelOptic = lens (\s -> cell s) $ \s b -> s {cell = b}

instance (k ~ A_Lens, a ~ CellVecs, b ~ a) => LabelOptic "inverseCell" k PosData PosData a b where
  labelOptic = lens (\s -> inverseCell s) $ \s b -> s {inverseCell = b}

instance (k ~ A_Lens, a ~ NetVec, b ~ a) => LabelOptic "coords" k PosData PosData a b where
  labelOptic = lens (\s -> coords s) $ \s b -> s {coords = b}

----------------------------------------------------------------------------------------------------

-- | The force data on the current structure. This data is sent to the server, which calculates new
-- positions from the implicitly existend coordinates. This is the information sent by Spicy.
data ForceData = ForceData
  { potentialEnergy :: Double,
    forces :: NetVec,
    virial :: CellVecs,
    optionalData :: ByteString
  }

-- Serialisation
instance Binary ForceData where
  put (ForceData {potentialEnergy, forces, virial, optionalData}) = do
    putDoublehost potentialEnergy
    put forces
    put virial
    let optionalLength = fromIntegral . ByteString.length $ optionalData
    putInt32host optionalLength
    putByteString optionalData

  get = do
    potentialEnergy <- getDoublehost
    forces <- get
    virial <- get
    optionalLength <- fromIntegral <$> getInt32host
    optionalData <- getByteString optionalLength
    return $
      ForceData
        { potentialEnergy = potentialEnergy,
          forces = forces,
          virial = virial,
          optionalData = optionalData
        }

-- Lenses
instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "potentialEnergy" k ForceData ForceData a b where
  labelOptic = lens (\s -> potentialEnergy s) $ \s b -> s {potentialEnergy = b}

instance (k ~ A_Lens, a ~ NetVec, b ~ a) => LabelOptic "forces" k ForceData ForceData a b where
  labelOptic = lens (\s -> forces s) $ \s b -> s {forces = b}

instance (k ~ A_Lens, a ~ CellVecs, b ~ a) => LabelOptic "virial" k ForceData ForceData a b where
  labelOptic = lens (\s -> virial s) $ \s b -> s {virial = b}

instance (k ~ A_Lens, a ~ ByteString, b ~ a) => LabelOptic "optionalData" k ForceData ForceData a b where
  labelOptic = lens (\s -> optionalData s) $ \s b -> s {optionalData = b}
