-- |
-- Module      : Spicy.Wrapper.Internal.Input.Representation
-- Description : Preparing program-specific text representations
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides functions to construct program-specific
-- text representations for coordinate and multipole data.
module Spicy.Wrapper.Internal.Input.Representation
  ( simpleCartesianAngstrom,
    xtbMultipoleRep,
    psi4MultipoleRep,
  )
where

import qualified Data.Massiv.Array as Massiv
import qualified Data.Text.Lazy.Builder as Builder
import RIO
import qualified RIO.Text as Text
import Spicy.Common
import Spicy.Molecule
import System.IO.Unsafe

-- | Make a simple coordinate representation of the current Molecule layer.
-- This function takes care to remove all dummy atoms.
simpleCartesianAngstrom :: MonadThrow m => Molecule -> m Text
simpleCartesianAngstrom mol = Text.unlines . drop 2 . Text.lines <$> writeXYZ (isolateMoleculeLayer mol)

-- | Generate the multipole representation accepted by XTB. Must be in its own file.
xtbMultipoleRep ::
  (MonadThrow m) =>
  -- | The __current__ 'Molecule' layer for which to perform the calculation. The multipoles must
  -- therefore already be present and multipole centres must be marked as Dummy atoms.
  Molecule ->
  m Text
xtbMultipoleRep mol = do
  let pointChargeVecs = Massiv.innerSlices $ unsafeMolToPointCharges mol
      toText vec =
        let q = Builder.fromText . tShow $ vec Massiv.! 3
            x = Builder.fromText . tShow $ vec Massiv.! 0
            y = Builder.fromText . tShow $ vec Massiv.! 1
            z = Builder.fromText . tShow $ vec Massiv.! 2
         in q <> " " <> x <> " " <> y <> " " <> z <> " 99\n"
      chargeLines = Massiv.foldMono toText pointChargeVecs
      countLine = (Builder.fromText . tShow . length $ pointChargeVecs) <> "\n"
      xtbBuilder = countLine <> chargeLines
  return . tshow $ xtbBuilder

-- | Generates the multipole representation for Psi4.
psi4MultipoleRep ::
  (MonadThrow m) =>
  -- | The __current__ 'Molecule' layer for which to perform the calculation. The multipoles must
  -- therefore already be present and multipole centres must be marked as Dummy atoms.
  Molecule ->
  m Text
psi4MultipoleRep mol = do
  let pointChargeVecs = Massiv.innerSlices $ unsafeMolToPointCharges mol
      toText vec =
        let q = Builder.fromText . tShow $ vec Massiv.! 3
            x = Builder.fromText . tShow $ vec Massiv.! 0
            y = Builder.fromText . tShow $ vec Massiv.! 1
            z = Builder.fromText . tShow $ vec Massiv.! 2
         in "Chrgfield.extern.addCharge(" <> q <> ", " <> x <> ", " <> y <> ", " <> z <> ")\n"
      chargeLines = Massiv.foldMono toText pointChargeVecs
      settingsLine = Builder.fromText "psi4.set_global_option_python('EXTERN', Chrgfield.extern)"
      psi4Builder = "Chrgfield = QMMM()\n" <> chargeLines <> settingsLine
  return . tshow $ psi4Builder

-- | A \"pure\" version of the "molToPointCharges" function. Morally, this is true,
-- as the function performs no side effects and is entirely deterministic.
-- The MonadIO constraint comes from the use of a parallel fold, which could
-- in general produce non-deterministic results, however, both folding
-- and chunk folding function are both commutative and associative, rendering
-- this moot.
unsafeMolToPointCharges :: Molecule -> Massiv.Matrix Massiv.S Double
unsafeMolToPointCharges = unsafePerformIO . molToPointCharges