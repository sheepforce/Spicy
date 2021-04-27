-- |
-- Module      : Spicy.Wrapper.Internal.Input.XTB
-- Description : Support for system calls to XTB.
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Provides methods to generate XTB inputs including electronic embedding.
module Spicy.Wrapper.Internal.Input.XTB
  ( xtbMultipoleFilename,
    xtbMultipoleRepresentation,
    xtbInput,
  )
where

-- XTB extra input files for embedding. May become obsolete with the C API later

import qualified Data.Massiv.Array as Massiv
import qualified Data.Text.Lazy.Builder as Builder
import Optics
import RIO hiding
  ( (^.),
    (^?),
  )
import qualified RIO.Text.Lazy as Text
import Spicy.Common
import Spicy.Molecule.Internal.Multipoles
import Spicy.Molecule.Internal.Types
import Spicy.Molecule.Internal.Util
import System.Path
  ( (<.>),
  )
import qualified System.Path as Path

-- | Generate the multipole representation accepted by XTB. Must be in its own file.
xtbMultipoleRepresentation ::
  (MonadThrow m, MonadIO m) =>
  -- | The __current__ 'Molecule' layer for which to perform the calculation. The multipoles must
  -- therefore already be present and multipole centres must be marked as Dummy atoms.
  Molecule ->
  m Text
xtbMultipoleRepresentation mol = do
  pointChargeVecs <- Massiv.innerSlices <$> molToPointCharges mol
  let toText vec =
        let q = Builder.fromText . tShow $ vec Massiv.! 3
            x = Builder.fromText . tShow $ vec Massiv.! 0
            y = Builder.fromText . tShow $ vec Massiv.! 1
            z = Builder.fromText . tShow $ vec Massiv.! 2
         in q <> " " <> x <> " " <> y <> " " <> z <> " 99\n"
      chargeLines = Massiv.foldMono toText pointChargeVecs
      countLine = (Builder.fromText . tShow . length $ pointChargeVecs) <> "\n"
      xtbBuilder = countLine <> chargeLines
  return . Text.toStrict . Builder.toLazyText $ xtbBuilder

-- | Provide a file name for the XTB multipoles. This function is kept here so that
-- the name is constistent across the program.
xtbMultipoleFilename :: CalcContext -> Path.AbsFile
xtbMultipoleFilename calcContext =
  let inputFilePrefix = Path.relFile . replaceProblematicChars $ calcContext ^. #input % #prefixName
      JDirPathAbs absDir = calcContext ^. #input % #permaDir
   in absDir Path.</> inputFilePrefix <.> ".pc"

-- | Generate the XTB xcontrol detailed input file. XTB will still need the coordinates in a separate file.
xtbInput ::
  (MonadThrow m) =>
  Molecule ->
  CalcID ->
  m Text
xtbInput mol calcID = do
  (calcContext, _) <- mol `getCalcByID` calcID
  let calcInput = calcContext ^. #input
      software = calcInput ^. #software
  case software of
    XTB gfn -> do
      let mxtbSpec = calcInput ^. #qMMMSpec ^? _QM
      xtbSpec <- maybe2MThrow (WrapperGenericException "xtbInput" "Not a QM calculation") mxtbSpec
      let xtbCharge = tShow $ xtbSpec ^. #charge
          xtbMult = xtbSpec ^. #mult
          xtbNOshells = tShow $ xtbMult - 1
          xtbPcFile = path2Text $ {-getDirPathAbs (calcInput ^. #permaDir) Path.</>-} xtbMultipoleFilename calcContext
      return $
        "$chrg " <> xtbCharge <> "\n"
          <> "$spin "
          <> xtbNOshells
          <> "\n"
          <> "$gfn\n" --Refers to nOpenshells, despite the name
          <> " method="
          <> renderGFN gfn
          <> "\n"
          <> "$embedding\n"
          <> " input="
          <> xtbPcFile
          <> "\n"
    _ -> throwM $ WrapperGenericException "xtbInput" "Attempted to generate XTB input for non-XTB calculation"

-- | Convert the enumeration type representation to the digit expected by XTB.
renderGFN ::
  GFN ->
  Text
renderGFN GFNZero = "0"
renderGFN GFNOne = "1"
renderGFN GFNTwo = "2"