module Spicy.Wrapper.Internal.Output.XTB
  ( parseXTBout
  )
where

import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Vector as V
import Data.Aeson
import Spicy.Math.Spherical
import Spicy.Common
import Spicy.Molecule.Internal.Types

data RawPoles = RawPoles
  { partialCharges :: Vector Double,
    atomicDipoles :: Vector (Vector Double),
    atomicQuadrupoles :: Vector (Vector Double)
  }
  deriving (Generic,Show)

instance FromJSON RawPoles where
  parseJSON = withObject "xtbout" $ \o -> RawPoles
    <$> o .: "partial charges"
    <*> o .: "atomic dipole moments"
    <*> o .: "atomic quadrupole moments"

instance ToJSON RawPoles

orderPoles :: MonadThrow m => RawPoles -> m [CMultipoles]
orderPoles RawPoles{..} = do
  -- Sanity check: Do we have information for every atom?
  let mmono = fmap CMonopole partialCharges
  mdi <- sequenceA $ mkDipole <$> atomicDipoles
  mquad <- sequenceA $ mkQuadrupole <$> atomicQuadrupoles
  return . V.toList $ V.zipWith3 CMultipoles (Just <$> mmono) (Just <$> mdi) (Just <$> mquad)
  where
    (!??) :: MonadThrow m => Vector a -> Int -> m a
    v !?? i = case v V.!? i of
      Nothing -> throwM (DataStructureException "!?" "Vector index out of bounds!")
      Just r -> return r
    -- Assumes x,y,z ordering!
    mkDipole :: MonadThrow m => Vector Double -> m CDipole
    mkDipole vec = do
      -- Sanitiy check - is this vector the right length?
      unless (length vec == 3) . throwM $ DataStructureException "orderPoles.mkDipole" "Dipole has incorrect number of entries!"
      x <- vec !?? 0
      y <- vec !?? 1
      z <- vec !?? 2
      return CDipole
        { c100 = x
        , c010 = y
        , c001 = z }
    -- Assumes xx, xy, yy, xz, yz, zz ordering in the xtb output!
    mkQuadrupole :: MonadThrow m => Vector Double -> m CQuadrupole
    mkQuadrupole vec = do
      unless (length vec == 6) . throwM $ DataStructureException "orderPoles.mkQuadrupole" "Quadrupole has incorrect number of entries!"
      xx <- vec !?? 0
      xy <- vec !?? 1
      yy <- vec !?? 2
      xz <- vec !?? 3
      yz <- vec !?? 4
      zz <- vec !?? 5
      return CQuadrupole
        { c200 = xx
        , c020 = yy
        , c002 = zz
        , c110 = xy
        , c101 = xz
        , c011 = yz }

parseXTBout :: MonadThrow m => BL.ByteString -> m [Multipoles]
parseXTBout out = do
  rawPoles <- maybe (throwM $ ParserException "parseXTBout") return $ decode out
  cartesianPoles <- orderPoles rawPoles
  return $ cartesianToSpherical <$> cartesianPoles