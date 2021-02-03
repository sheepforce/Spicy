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
  ( HasPysis (..),
    HasIPI (..),
    SpicyEnv (..),
    WrapperConfigs (..),
    HasWrapperConfigs (..),
    Motion (..),
    HasMotion (..),
    CalcSlot (..),
    HasCalcSlot (..),
    IPIServerStatus(..),
    IPI (..),
  )
where

import Data.Aeson
import Network.Socket hiding (socket)
import Optics
import RIO hiding (Lens', Vector, lens)
import RIO.Process (HasProcessContext (..), ProcessContext)
import Spicy.Aeson
import Spicy.Common
import Spicy.InputFile hiding (MD, molecule)
import Spicy.Molecule
import Spicy.Wrapper.IPI.Types
import System.Path as Path

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
    calcSlot :: !CalcSlot,
    -- | Connection settings for pysisyphus i-PI server.
    pysis :: !IPI,
    -- | Connection settings for the i-PI i-PI server.
    ipi :: !IPI
  }
  deriving (Generic)

-- Lenses
instance (k ~ A_Lens, a ~ TVar Molecule, b ~ a) => LabelOptic "molecule" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> molecule s) $ \s b -> s {molecule = b}

instance (k ~ A_Lens, a ~ InputFile, b ~ a) => LabelOptic "calculation" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> calculation s) $ \s b -> s {calculation = b}

instance (k ~ A_Lens, a ~ WrapperConfigs, b ~ a) => LabelOptic "wrapperConfigs" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> wrapperConfigs s) $ \s b -> s {wrapperConfigs = b}

instance (k ~ A_Lens, a ~ TVar Motion, b ~ a) => LabelOptic "motion" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> motion s) $ \s b -> s {motion = b}

instance (k ~ A_Lens, a ~ LogFunc, b ~ a) => LabelOptic "logFunc" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> logFunc s) $ \s b -> s {logFunc = b}

instance (k ~ A_Lens, a ~ ProcessContext, b ~ a) => LabelOptic "procCntxt" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> procCntxt s) $ \s b -> s {procCntxt = b}

instance (k ~ A_Lens, a ~ CalcSlot, b ~ a) => LabelOptic "calcSlot" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> calcSlot s) $ \s b -> s {calcSlot = b}

instance (k ~ A_Lens, a ~ IPI, b ~ a) => LabelOptic "pysis" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> pysis s) $ \s b -> s {pysis = b}

instance (k ~ A_Lens, a ~ IPI, b ~ a) => LabelOptic "ipi" k SpicyEnv SpicyEnv a b where
  labelOptic = lens (\s -> (ipi :: SpicyEnv -> IPI) s) $ \s b -> (s {ipi = b} :: SpicyEnv)

-- Reader Classes

-- | A reader class, that is aware of connection details to pysisyphus.
class HasPysis env where
  pysisL :: Lens' env IPI

-- | A reader class, that is aware of connection details to i-PI.
class HasIPI env where
  ipiL :: Lens' env IPI

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

instance HasPysis SpicyEnv where
  pysisL = #pysis

instance HasIPI SpicyEnv where
  ipiL = #ipi

----------------------------------------------------------------------------------------------------

-- | The command to use for launching the wrapped programs. Alls arguments are meant to be passed to
-- those wrappers.
data WrapperConfigs = WrapperConfigs
  { psi4 :: Maybe JFilePath,
    nwchem :: Maybe JFilePath,
    gdma :: Maybe JFilePath,
    ipi :: Maybe JFilePath,
    pysisyphus :: Maybe JFilePath
  }
  deriving (Show, Generic)

instance ToJSON WrapperConfigs where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON WrapperConfigs

-- Lenses
instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "psi4" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (\s -> psi4 s) $ \s b -> s {psi4 = b}

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "nwchem" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (\s -> nwchem s) $ \s b -> s {nwchem = b}

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "gdma" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (\s -> gdma s) $ \s b -> s {gdma = b}

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "ipi" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (\s -> (ipi :: WrapperConfigs -> Maybe JFilePath) s) $ \s b -> (s {ipi = b} :: WrapperConfigs)

instance (k ~ A_Lens, a ~ Maybe JFilePath, b ~ a) => LabelOptic "pysisyphus" k WrapperConfigs WrapperConfigs a b where
  labelOptic = lens (\s -> pysisyphus s) $ \s b -> s {pysisyphus = b}

-- Reader Classes
class HasWrapperConfigs env where
  wrapperConfigsL :: Lens' env WrapperConfigs

instance HasWrapperConfigs WrapperConfigs where
  wrapperConfigsL = castOptic simple

----------------------------------------------------------------------------------------------------

-- | Defining the curent state of Motion in either an Optimisation or MD run.
data Motion = Motion
  { -- | Flag of the molecule is ready to be moved (an iteration has passed completely.)
    ready :: Bool,
    -- | The counter of outer optimisation steps (whole system as one with transformed gradients).
    outerCycle :: Int,
    -- | The counter for inner optimisation cycles on each 'MolID' of the 'Molecule' separately.
    innerCycles :: Map MolID Int
  }
  deriving (Show, Generic)

-- Reader classes.
class HasMotion env where
  motionL :: Lens' env (TVar Motion)

-- Lenses
instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "ready" k Motion Motion a b where
  labelOptic = lens (\s -> ready s) $ \s b -> s {ready = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "outerCycle" k Motion Motion a b where
  labelOptic = lens (\s -> outerCycle s) $ \s b -> s {outerCycle = b}

instance (k ~ A_Lens, a ~ Map MolID Int, b ~ a) => LabelOptic "innerCycles" k Motion Motion a b where
  labelOptic = lens (\s -> innerCycles s) $ \s b -> s {innerCycles = b}

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

-- Reader Classes
class HasCalcSlot env where
  calcSlotL :: Lens' env CalcSlot

instance HasCalcSlot CalcSlot where
  calcSlotL = castOptic simple

-- Lenses
instance (k ~ A_Lens, a ~ TMVar CalcID, b ~ a) => LabelOptic "input" k CalcSlot CalcSlot a b where
  labelOptic = lens (\s -> (input :: CalcSlot -> TMVar CalcID) s) $ \s b -> (s {input = b} :: CalcSlot)

instance (k ~ A_Lens, a ~ TMVar Molecule, b ~ a) => LabelOptic "output" k CalcSlot CalcSlot a b where
  labelOptic = lens (\s -> (output :: CalcSlot -> TMVar Molecule) s) $ \s b -> (s {output = b} :: CalcSlot)

----------------------------------------------------------------------------------------------------

-- | The status of the i-PI server. This is nort part of the protocol but there so tell a client if
-- we can already connect and if it needs to continue delivering data.
data IPIServerStatus = MoreData | Done deriving (Eq, Show)

-- | i-PI communication settings and variables. Generic over i-PI implementations.
data IPI = IPI
  { -- | The network socket used for communication with the server.
    socket :: Socket,
    -- | The address of the socket in use.
    socketAddr :: SockAddr,
    -- | Input channel. When this variable is filled the i-PI server starts its calculation of
    -- new positions.
    input :: TMVar ForceData,
    -- | Output channel. When the i-PI server has finished its calculation, these values will be
    -- filled and are ready to be consumed by Spicy.
    output :: TMVar PosData,
    -- | Working directory of the process.
    workDir :: Path.AbsRelDir,
    -- | The path to a coordinate file, used to initialise the i-PI server with coordinates.
    initCoords :: Path.AbsRelFile,
    -- | The status of the i-PI server.
    status :: TMVar IPIServerStatus
  }

-- Lenses
instance (k ~ A_Lens, a ~ Socket, b ~ a) => LabelOptic "socket" k IPI IPI a b where
  labelOptic = lens (\s -> (socket :: IPI -> Socket) s) $ \s b -> s {socket = b}

instance (k ~ A_Lens, a ~ SockAddr, b ~ a) => LabelOptic "socketAddr" k IPI IPI a b where
  labelOptic = lens (\s -> socketAddr s) $ \s b -> s {socketAddr = b}

instance (k ~ A_Lens, a ~ TMVar ForceData, b ~ a) => LabelOptic "input" k IPI IPI a b where
  labelOptic = lens (\s -> (input :: IPI -> TMVar ForceData) s) $ \s b -> (s {input = b} :: IPI)

instance (k ~ A_Lens, a ~ TMVar PosData, b ~ a) => LabelOptic "output" k IPI IPI a b where
  labelOptic = lens (\s -> (output :: IPI -> TMVar PosData) s) $ \s b -> (s {output = b} :: IPI)

instance (k ~ A_Lens, a ~ Path.AbsRelDir, b ~ a) => LabelOptic "workDir" k IPI IPI a b where
  labelOptic = lens (\s -> workDir s) $ \s b -> s {workDir = b}

instance (k ~ A_Lens, a ~ Path.AbsRelFile, b ~ a) => LabelOptic "initCoords" k IPI IPI a b where
  labelOptic = lens (\s -> initCoords s) $ \s b -> s {initCoords = b}

instance (k ~ A_Lens, a ~ TMVar IPIServerStatus, b ~ a) => LabelOptic "status" k IPI IPI a b where
  labelOptic = lens (\s -> status s) $ \s b -> s {status = b}
