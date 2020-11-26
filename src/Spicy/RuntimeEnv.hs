{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- |
-- Module      : Spicy.RuntimeEnv
-- Description : Definition of a runtime environment in Spicy.
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module collects all type and typeclass definitions for Spicy in a single place. This makes the
-- implementation of RIO's @Has*@ typeclasses more consistent, than scattering type definitions across
-- modules.
module Spicy.RuntimeEnv
  ( SpicyEnv (..),
    WrapperConfigs (..),
    HasWrapperConfigs (..),
    Motion (..),
    _Optimisation,
    _MD,
  )
where

import Data.Aeson
import Optics
import RIO hiding (Lens', lens)
import RIO.Process (HasProcessContext (..), ProcessContext)
import Spicy.Aeson
import Spicy.InputFile hiding (MD, molecule)
import Spicy.Molecule

-- | Definition of the current 'State' in the execution of Spicy.
data SpicyEnv = SpicyEnv
  { -- | The input file, which constructs the calculation.
    inputFile :: InputFile,
    -- | The current state of the molecule with all information
    --   up to date. This also includes the 'Seq' of
    --   calculations to perform on each layer.import Spicy.Aeson
    molecule :: !Molecule,
    -- | This is the input file and contains the definition of
    --   which kind of calculation to do. This should be set in
    --   the beginning and never change.
    calculation :: !InputFile,
    -- | Configuration files with Maps of environment variables
    --   to set before launching a program.
    wrapperConfigs :: !WrapperConfigs,
    -- | Contains optimisation counters, MD counters and so on.
    motion :: !(Maybe Motion),
    -- | A logging function for RIO.
    logFunc :: LogFunc,
    -- | A process context for RIO.
    procCntxt :: ProcessContext
  }
  deriving (Generic)

-- Lenses
instance (k ~ A_Lens, a ~ InputFile, b ~ a) => LabelOptic "inputFile" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> inputFile s) $ \s b -> s {inputFile = b}

instance (k ~ A_Lens, a ~ Molecule, b ~ a) => LabelOptic "molecule" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> molecule s) $ \s b -> s {molecule = b}

instance (k ~ A_Lens, a ~ InputFile, b ~ a) => LabelOptic "calculation" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> calculation s) $ \s b -> s {calculation = b}

instance (k ~ A_Lens, a ~ WrapperConfigs, b ~ a) => LabelOptic "wrapperConfigs" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> wrapperConfigs s) $ \s b -> s {wrapperConfigs = b}

instance (k ~ A_Lens, a ~ Maybe Motion, b ~ a) => LabelOptic "motion" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> motion s) $ \s b -> s {motion = b}

instance (k ~ A_Lens, a ~ LogFunc, b ~ a) => LabelOptic "logFunc" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> logFunc s) $ \s b -> s {logFunc = b}

instance (k ~ A_Lens, a ~ ProcessContext, b ~ a) => LabelOptic "procCntxt" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> procCntxt s) $ \s b -> s {procCntxt = b}

-- Reader Classes
instance HasInputFile SpicyEnv where
  inputFileL = #inputFile

instance HasMolecule SpicyEnv where
  moleculeL = #molecule

instance HasWrapperConfigs SpicyEnv where
  wrapperConfigsL = #wrapperConfigs

instance HasLogFunc SpicyEnv where
  logFuncL = toLensVL #logFunc

instance HasProcessContext SpicyEnv where
  processContextL = toLensVL #procCntxt

----------------------------------------------------------------------------------------------------

-- | The command to use for launching the wrapped programs. Alls arguments are meant to be passed to
-- those wrappers.
data WrapperConfigs = WrapperConfigs
  { psi4 :: Maybe FilePath,
    nwchem :: Maybe FilePath,
    gdma :: Maybe FilePath
  }
  deriving (Show, Generic)

instance ToJSON WrapperConfigs where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON WrapperConfigs

-- Lenses
instance (k ~ A_Lens, a ~ Maybe FilePath, b ~ a) => LabelOptic "psi4" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (\s -> psi4 s) $ \s b -> s {psi4 = b}

instance (k ~ A_Lens, a ~ Maybe FilePath, b ~ a) => LabelOptic "nwchem" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (\s -> nwchem s) $ \s b -> s {nwchem = b}

instance (k ~ A_Lens, a ~ Maybe FilePath, b ~ a) => LabelOptic "gdma" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (\s -> gdma s) $ \s b -> s {gdma = b}

-- Reader Classes
class HasWrapperConfigs env where
  wrapperConfigsL :: Lens' env WrapperConfigs

instance HasWrapperConfigs WrapperConfigs where
  wrapperConfigsL = castOptic simple

----------------------------------------------------------------------------------------------------

-- | Defining the curent state of Motion in either an Optimisation or MD run.
data Motion
  = Optimisation
      { -- | The counter of outer optimisation steps (whole system as one with transformed gradients).
        outerCycle :: Int,
        -- | The counter for inner optimisation cycles on each 'MolID' of the 'Molecule' separately.
        innerCycles :: Map MolID Int
      }
  | MD Int
  deriving (Show, Generic)

-- Prisms
_Optimisation :: Prism' Motion (Int, Map MolID Int)
_Optimisation = prism' (\(a, b) -> Optimisation a b) $ \s -> case s of
  Optimisation oC iC -> Just (oC, iC)
  _ -> Nothing

_MD :: Prism' Motion Int
_MD = prism' (\b -> MD b) $ \s -> case s of
  MD c -> Just c
  _ -> Nothing

-- Lenses
instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "outerCycle" k Motion Motion a b where
  labelOptic = lens (\s -> outerCycle s) $ \s b -> s {outerCycle = b}

instance (k ~ A_Lens, a ~ Map MolID Int, b ~ a) => LabelOptic "innerCycles" k Motion Motion a b where
  labelOptic = lens (\s -> innerCycles s) $ \s b -> s {innerCycles = b}
