-- |
-- Module      : Spicy.Wrapper.Internal.Output.XTB
-- Description : Support for reading XTB output.
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Provides methods to parse the output of the xtb program suite.
module Spicy.Wrapper.Internal.Output.XTB
  ( fromXTBout,
    parseXTBgradient,
    parseXTBhessian,
  )
where

import Data.Aeson
import Data.Attoparsec.Text
import qualified Data.Massiv.Vector as VM
import RIO
import RIO.Char
import qualified RIO.Vector as V
import Spicy.Common
import Spicy.Math.Spherical
import Spicy.Molecule.Internal.Types
import Spicy.Wrapper.Internal.Output.Generic

-- | Representation for the content of the xtbout.json file.
data RawXTB = RawXTB
  { totalEnergy :: Double,
    partialCharges :: Vector Double,
    atomicDipoles :: Maybe (Vector (Vector Double)),
    atomicQuadrupoles :: Maybe (Vector (Vector Double))
  }
  deriving (Generic, Show)

instance FromJSON RawXTB where
  parseJSON = withObject "xtbout" $ \o -> do
    energy <- o .: "total energy"
    pCharges <- o .: "partial charges"
    -- The following are not present in GFN0 and GFN1
    aDipoles <- o .:? "atomic dipole moments"
    aQuadrupoles <- o .:? "atomic quadrupole moments"
    return
      RawXTB
        { totalEnergy = energy,
          partialCharges = pCharges,
          atomicDipoles = aDipoles,
          atomicQuadrupoles = aQuadrupoles
        }

fromXTBout :: (MonadThrow m) => ByteString -> m (Double, [Multipoles])
fromXTBout = processXTBout <=< readXTBout

-- | From the parsed XTB output, generate the data used by the rest of Spicy.
processXTBout :: (MonadThrow m) => RawXTB -> m (Double, [Multipoles])
processXTBout raw@RawXTB {..} = do
  multipoles <- xtbMultipoles raw
  return (totalEnergy, multipoles)

-- | Parse the JSON file that XTB can be made to write.
readXTBout :: MonadThrow m => ByteString -> m RawXTB
readXTBout = maybe2MThrow (ParserException "parseXTBout") . decode . fromStrictBytes

-- | Take the raw XTB output and transform it into a more workable form.
orderPoles :: MonadThrow m => RawXTB -> m [CMultipoles]
orderPoles RawXTB {..} = do
  let mmono = pure . CMonopole <$> partialCharges
      l = RIO.length partialCharges
      -- If any poles are missing, these blanks will be substituted
      blankDipoles = V.replicate l Nothing
      blankQuadrupoles = V.replicate l Nothing
  mdi <- maybe (return blankDipoles) (traverse $ pure . mkDipole) atomicDipoles
  mquad <- maybe (return blankQuadrupoles) (traverse $ pure . mkQuadrupole) atomicQuadrupoles
  return . V.toList $ V.zipWith3 CMultipoles mmono mdi mquad
  where
    (!??) :: MonadThrow m => Vector a -> Int -> m a
    v !?? i = maybe2MThrow (DataStructureException "!?" "Vector index out of bounds!") (v V.!? i)
    -- Assumes x,y,z ordering!
    mkDipole :: MonadThrow m => Vector Double -> m CDipole
    mkDipole vec = do
      -- Sanitiy check - is this vector the right length?
      unless (RIO.length vec == 3) . throwM $ DataStructureException "orderPoles.mkDipole" "Dipole has incorrect number of entries!"
      x <- vec !?? 0
      y <- vec !?? 1
      z <- vec !?? 2
      return
        CDipole
          { c100 = x,
            c010 = y,
            c001 = z
          }
    -- Assumes xx, xy, yy, xz, yz, zz ordering in the xtb output!
    mkQuadrupole :: MonadThrow m => Vector Double -> m CQuadrupole
    mkQuadrupole vec = do
      unless (RIO.length vec == 6) . throwM $ DataStructureException "orderPoles.mkQuadrupole" "Quadrupole has incorrect number of entries!"
      xx <- vec !?? 0
      xy <- vec !?? 1
      yy <- vec !?? 2
      xz <- vec !?? 3
      yz <- vec !?? 4
      zz <- vec !?? 5
      return
        CQuadrupole
          { c200 = xx,
            c020 = yy,
            c002 = zz,
            c110 = xy,
            c101 = xz,
            c011 = yz
          }

-- | Get the spherical multipoles from the raw (cartesian) xtb output.
xtbMultipoles :: MonadThrow m => RawXTB -> m [Multipoles]
xtbMultipoles =
  fmap fmap fmap cartesianToSpherical
    . maybe2MThrow (DataStructureException "xtbMultipoles" "XTB poles could not be processed")
    . orderPoles

-- | Parse the 'gradient' output file of XTB. XTB must be told to write this file, it will not do so
-- on a normal run. This file can contain multiple gradients if the XTB calculation is a compound
-- calculation -- This isn't currently dealt with (but also never requested by Spicy).
parseXTBgradient :: Parser (VectorS Double)
parseXTBgradient = do
  _ <- string "$grad" <* skipLine
  _ <- skipLine
  _ <- many1 parseCoordinates
  gradients <- many1' parseGradients
  let vec = VectorS . VM.convert . VM.sconcat $ gradients
  _ <- string "$end"
  return vec
  where
    parseCoordinates = do
      x <- skipHorizontalSpace *> double
      y <- skipHorizontalSpace *> double
      z <- skipHorizontalSpace *> double
      name <- skipHorizontalSpace *> many1' (satisfy isAlpha)
      _ <- skipLine
      return (name, x, y, z)
    parseGradients = do
      x <- skipHorizontalSpace *> (double <|> fortranDouble)
      y <- skipHorizontalSpace *> (double <|> fortranDouble)
      z <- skipHorizontalSpace *> (double <|> fortranDouble)
      _ <- skipLine
      return $ VM.sfromList [x, y, z]

-- | Parse the xtb hessian file into a matrix representation.
parseXTBhessian :: Parser (MatrixS Double)
parseXTBhessian = do
  _ <- string "$hessian"
  doubleSquareMatrix <* skipSpace <* endOfInput
