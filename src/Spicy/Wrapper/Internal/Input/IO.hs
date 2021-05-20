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

writeInputs :: (HasMolecule env, HasLogFunc env) => CalcID -> RIO env AbsFile
writeInputs calcID = do
  -- Get information
  mol <- view moleculeL >>= readTVarIO
  (calcContext, thisMol) <- getCalcByID mol calcID
  let calcInput = calcContext ^. #input
      program = calcInput ^. #software
      permanentDir = getDirPathAbs $ calcContext ^. #input % #permaDir
      prefix = relFile . replaceProblematicChars $ calcContext ^. #input % #prefixName
      inputFilePath = permanentDir </> prefix <.> ".inp"

  -- Write input file
  logDebug "Writing input file..."
  inputText <- runReaderT makeInput (thisMol, calcInput)
  writeFileUTF8 (toAbsRel inputFilePath) inputText

  -- If this is an XTB calculation, also write separate .xyz and .pc files
  when (program & isXTB) $ do
    -- .xyz file
    logDebug "Writing .xyz file..."
    let realMol = isolateMoleculeLayer thisMol
        xyzFilePath = toAbsRel $ replaceExtension inputFilePath ".xyz"
    xyzText <- writeXYZ realMol
    writeFileUTF8 xyzFilePath xyzText
    -- .pc file
    logDebug "Writing .pc file..."
    let pcFilePath = toAbsRel $ xtbMultPath permanentDir prefix
    pcText <- xtbMultipoleRep thisMol
    writeFileUTF8 pcFilePath pcText

  -- Return the input file path
  return inputFilePath
