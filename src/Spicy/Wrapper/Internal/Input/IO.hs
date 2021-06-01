-- |
-- Module      : Spicy.Wrapper.Internal.Input.IO
-- Description : Writing external program input
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Program-specific preperation of calculation input files.
module Spicy.Wrapper.Internal.Input.IO
  ( writeInputs,
  )
where

import Optics hiding (view)
import RIO hiding (view, (^.))
import Spicy.Common
import Spicy.Molecule
import Spicy.Wrapper.Internal.Input.Language
import Spicy.Wrapper.Internal.Input.Representation
import System.Path

logSource :: LogSource
logSource = "wrapper-input"

-- | Write all necessary input files for the calculation specified
-- by the input CalcID to the directory specified in the input.
-- Returns the path of the (main) input file.
writeInputs :: (HasMolecule env, HasLogFunc env) => CalcID -> RIO env AbsFile
writeInputs calcID = do
  -- Get information
  mol <- view moleculeL >>= readTVarIO
  (calcContext, thisMol) <- getCalcByID mol calcID
  let calcInput = calcContext ^. #input
      program = calcInput ^. #software
      programStr p
        | isPsi4 p = "Psi4"
        | isXTB p = "XTB"
        | otherwise = "Unknown!"
      permanentDir = getDirPathAbs $ calcContext ^. #input % #permaDir
      prefix = relFile . replaceProblematicChars $ calcContext ^. #input % #prefixName
      inputFilePath = permanentDir </> prefix <.> ".inp"

  -- Write input file
  logDebugS logSource $
    "Writing input file for a " <> programStr program <> " Calculation:\n"
      <> "CalcID: "
      <> utf8Show calcID
      <> ", File Path: "
      <> utf8Show inputFilePath
  inputText <- runReaderT makeInput (thisMol, calcInput)
  writeFileUTF8 (toAbsRel inputFilePath) inputText

  -- If this is an XTB calculation, also write separate .xyz and .pc files
  when (program & isXTB) $ do
    -- .xyz file
    let realMol = isolateMoleculeLayer thisMol
        xyzFilePath = toAbsRel $ replaceExtension inputFilePath ".xyz"
    logDebugS logSource $
      "Writing .xyz (coordinate) file for XTB to: "
        <> utf8Show xyzFilePath
    xyzText <- writeXYZ realMol
    writeFileUTF8 xyzFilePath xyzText
    -- .pc file
    let pcFilePath = toAbsRel $ xtbMultPath permanentDir prefix
    logDebugS logSource $
      "Writing .pc (point charge) file for XTB to: "
        <> utf8Show pcFilePath
    pcText <- xtbMultipoleRep thisMol
    writeFileUTF8 pcFilePath pcText

  -- Return the input file path
  return inputFilePath
