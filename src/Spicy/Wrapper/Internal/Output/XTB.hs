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
import Data.Massiv.Array
import RIO
import RIO.Char
import Spicy.Common
import Spicy.Math.Spherical
import Spicy.Molecule.Internal.Types
import Spicy.Wrapper.Internal.Output.Generic

-- | Representation for the content of the xtbout.json file.
data RawXTB = RawXTB
  { totalEnergy :: Double,
    partialCharges :: VectorG S Double,
    atomicDipoles :: Maybe (MatrixG S Double),
    atomicQuadrupoles :: Maybe (MatrixG S Double)
  }
  deriving (Generic)

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
  let mmono = pure . CMonopole <$> (toStreamArray . getVectorG $ partialCharges)
      l = size . getVectorG $ partialCharges
      -- If any poles are missing, these blanks will be substituted
      blankDipoles = sreplicate l Nothing
      blankQuadrupoles = sreplicate l Nothing
  mdi <- maybe (return blankDipoles) mkDipoles atomicDipoles
  mquad <- maybe (return blankQuadrupoles) mkQuadrupoles atomicQuadrupoles
  return . stoList $ szipWith3 CMultipoles mmono mdi mquad
  where
    mkDipoles (MatrixG m) = do
      let Sz2 _ l = size m
          mm = outerSlices m
          unsafeToCDipole arr =
            CDipole
              { c100 = arr ! 0,
                c010 = arr ! 1,
                c001 = arr ! 2
              }
      unless (l == 3) . throwM $ DataStructureException "orderPoles.mkDipole" "Dipole has incorrect number of entries!"
      return $ smap (Just . unsafeToCDipole) mm
    -- Assumes xx, xy, yy, xz, yz, zz ordering in the xtb output!
    mkQuadrupoles (MatrixG m) = do
      let Sz2 _ l = size m
          mm = outerSlices m
          unsafeToCQuadrupole arr =
            CQuadrupole
              { c200 = arr ! 0,
                c020 = arr ! 1,
                c002 = arr ! 2,
                c110 = arr ! 3,
                c101 = arr ! 4,
                c011 = arr ! 5
              }
      unless (l == 6) . throwM $ DataStructureException "orderPoles.mkQuadrupole" "Quadrupole has incorrect number of entries!"
      return $ smap (Just . unsafeToCQuadrupole) mm

-- | Get the spherical multipoles from the raw (cartesian) xtb output.
xtbMultipoles :: MonadThrow m => RawXTB -> m [Multipoles]
xtbMultipoles =
  fmap fmap fmap cartesianToSpherical
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
  let vec = VectorS . convert . sconcat $ gradients
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
      return $ sfromList [x, y, z]

-- | Parse the xtb hessian file into a matrix representation.
parseXTBhessian :: Parser (MatrixS Double)
parseXTBhessian = do
  _ <- string "$hessian"
  doubleSquareMatrix <* skipSpace <* endOfInput
