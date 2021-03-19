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
    HasMotion (..),
    CalcSlot (..),
    HasCalcSlot (..),
    IPI (..),
  )
where

import Data.Aeson
import Optics
import RIO hiding (Lens', Vector, lens)
import RIO.Process (HasProcessContext (..), ProcessContext)
import Spicy.Aeson
import Spicy.Common
import Spicy.InputFile hiding (MD, molecule)
import Spicy.Molecule hiding (pysisyphus)
import Spicy.Wrapper.IPI.Types

----------------------------------------------------------------------------------------------------

-- | Definition of the current 'State' in the execution of Spicy.
data SpicyEnv = SpicyEnv
  { -- | The current state of the molecule with all information
    --   up to date. This also includes the 'Seq' of
    --   calculations to perform on each layer.import Spicy.Aeson
    molecule :: !(TVar Molecule),
    -- | This is the input file and contains the definition of
    --   which kind of calculation to do. This should be set in
    --   the beginning and never change.
    calculation :: !InputFile,
    -- | Configuration files with Maps of environment variables
    --   to set before launching a program.
    wrapperConfigs :: !WrapperConfigs,
    -- | Contains optimisation counters, MD counters and so on.
    motion :: !(TVar Motion),
    -- | A logging function for RIO.
    logFunc :: !LogFunc,
    -- | A process context for RIO.
    procCntxt :: !ProcessContext,
    -- | The current calculation to be performed by the calculator thread.
    calcSlot :: !CalcSlot
  }
  deriving (Generic)

-- Lenses
instance (k ~ A_Lens, a ~ TVar Molecule, b ~ a) => LabelOptic "molecule" k SpicyEnv SpicyEnv a b where
  labelOptic = lens molecule $ \s b -> s {molecule = b}

instance (k ~ A_Lens, a ~ InputFile, b ~ a) => LabelOptic "calculation" k SpicyEnv SpicyEnv a b where
  labelOptic = lens calculation $ \s b -> s {calculation = b}

instance (k ~ A_Lens, a ~ WrapperConfigs, b ~ a) => LabelOptic "wrapperConfigs" k SpicyEnv SpicyEnv a b where
  labelOptic = lens wrapperConfigs $ \s b -> s {wrapperConfigs = b}

instance (k ~ A_Lens, a ~ TVar Motion, b ~ a) => LabelOptic "motion" k SpicyEnv SpicyEnv a b where
  labelOptic = lens motion $ \s b -> s {motion = b}

instance (k ~ A_Lens, a ~ LogFunc, b ~ a) => LabelOptic "logFunc" k SpicyEnv SpicyEnv a b where
  labelOptic = lens logFunc $ \s b -> s {logFunc = b}

instance (k ~ A_Lens, a ~ ProcessContext, b ~ a) => LabelOptic "procCntxt" k SpicyEnv SpicyEnv a b where
  labelOptic = lens procCntxt $ \s b -> s {procCntxt = b}

instance (k ~ A_Lens, a ~ CalcSlot, b ~ a) => LabelOptic "calcSlot" k SpicyEnv SpicyEnv a b where
  labelOptic = lens calcSlot $ \s b -> s {calcSlot = b}

-- Reader Classes
instance HasMotion SpicyEnv where
  motionL = #motion

instance HasInputFile SpicyEnv where
  inputFileL = #calculation

instance HasMolecule SpicyEnv where
  moleculeL = #molecule

instance HasWrapperConfigs SpicyEnv where
  wrapperConfigsL = #wrapperConfigs

instance HasLogFunc SpicyEnv where
  logFuncL = toLensVL #logFunc

instance HasProcessContext SpicyEnv where
  processContextL = toLensVL #procCntxt

instance HasCalcSlot SpicyEnv where
  calcSlotL = #calcSlot

----------------------------------------------------------------------------------------------------

-- | The command to use for launching the wrapped programs. Alls arguments are meant to be passed to
-- those wrappers.
data WrapperConfigs = WrapperConfigs
  { psi4 :: Maybe JFilePath,
    nwchem :: Maybe JFilePath,
    gdma :: Maybe JFilePath,
    ipi :: Maybe JFilePath,
    pysisyphus :: Maybe JFilePath,
    xtb :: Maybe JFilePath
  }
  deriving (Show, Generic)

instance ToJSON WrapperConfigs where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON WrapperConfigs

-- Lenses
instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "psi4" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens psi4 $ \s b -> s {psi4 = b}

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "nwchem" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens nwchem $ \s b -> s {nwchem = b}

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "gdma" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens gdma $ \s b -> s {gdma = b}

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "ipi" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (ipi :: WrapperConfigs -> Maybe JFilePath) $ \s b -> (s {ipi = b} :: WrapperConfigs)

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "pysisyphus" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens pysisyphus $ \s b -> s {pysisyphus = b}

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "xtb" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens xtb $ \s b -> s {xtb = b}

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "xtb" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens xtb $ \s b -> s {xtb = b}

-- Reader Classes
class HasWrapperConfigs env where
  wrapperConfigsL :: Lens' env WrapperConfigs

instance HasWrapperConfigs WrapperConfigs where
  wrapperConfigsL = castOptic simple

----------------------------------------------------------------------------------------------------

-- | Defining the curent state of Motion in either an Optimisation or MD run.
data Motion = Motion
  { -- | The counter of outer optimisation steps (whole system as one with transformed gradients).
    outerCycle :: Int,
    -- | The counter for inner optimisation cycles on each 'MolID' of the 'Molecule' separately.
    innerCycles :: Map MolID Int
  }
  deriving (Show, Generic)

-- Reader classes.
class HasMotion env where
  motionL :: Lens' env (TVar Motion)

-- Lenses
instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "outerCycle" k Motion Motion a b where
  labelOptic = lens outerCycle $ \s b -> s {outerCycle = b}

instance (k ~ A_Lens, a ~ Map MolID Int, b ~ a) => LabelOptic "innerCycles" k Motion Motion a b where
  labelOptic = lens innerCycles $ \s b -> s {innerCycles = b}

----------------------------------------------------------------------------------------------------

-- | Configuration settings for the calculation slot, that executes the wrapper calculations. It has
-- an input and an output variable.
data CalcSlot = CalcSlot
  { -- | The calculation ID that shall be processed. Must be emptied at the **end** of the
    -- calculation, not already after the calculation has started.
    input :: TMVar CalcID,
    -- | The finished result. Should be taken and emptied by the consumer.
    output :: TMVar Molecule
  }

-- Default Class
instance DefaultIO CalcSlot where
  defIO = do
    calcSlotIn <- newEmptyTMVarIO
    calcSlotOut <- newEmptyTMVarIO
    return CalcSlot {input = calcSlotIn, output = calcSlotOut}

-- Reader Classes
class HasCalcSlot env where
  calcSlotL :: Lens' env CalcSlot

instance HasCalcSlot CalcSlot where
  calcSlotL = castOptic simple

-- Lenses
instance (k ~ A_Lens, a ~ TMVar CalcID, b ~ a) => LabelOptic "input" k CalcSlot CalcSlot a b where
  labelOptic = lens (input :: CalcSlot -> TMVar CalcID) $ \s b -> (s {input = b} :: CalcSlot)

instance (k ~ A_Lens, a ~ TMVar Molecule, b ~ a) => LabelOptic "output" k CalcSlot CalcSlot a b where
  labelOptic = lens (output :: CalcSlot -> TMVar Molecule) $ \s b -> (s {output = b} :: CalcSlot)
