{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- |
-- Module      : Spicy.CmdArgs
-- Description : Command line arguments controlling Spicy behaviour
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This defines the spicy command line modes and options to them.
module Spicy.CmdArgs
  ( SpicyArgs (..),
    HasSpicyArgs (..),
    spicyArgs
  )
where

import Optics
import RIO hiding (Lens', lens)
import System.Console.CmdArgs hiding
  ( Default,
    name,
    program,
  )

data SpicyArgs = SpicyArgs
  { -- | Wether to use verbose loggin or not (will include
    --   verbose format and debug messages.)
    verbose :: Bool,
    -- | Path to the YAML input file for Spicy.
    input :: FilePath,
    -- | Path to a log file for Spicy output.
    logfile :: Maybe FilePath,
    -- | Alternative path to the pre-startup configuration file.
    startupconf :: Maybe FilePath
  }
  deriving (Data, Typeable, Show, Eq)

-- Lenses
instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "verbose" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> verbose s) $ \s b -> s {verbose = b}

instance (k ~ A_Lens, a ~ FilePath, b ~ a) => LabelOptic "input" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> input s) $ \s b -> s {input = b}

instance (k ~ A_Lens, a ~ Maybe FilePath, b ~ a) => LabelOptic "logfile" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> logfile s) $ \s b -> s {logfile = b}

instance (k ~ A_Lens, a ~ Maybe FilePath, b ~ a) => LabelOptic "startupconf" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> startupconf s) $ \s b -> s {startupconf = b}

-- Reader Class.
class HasSpicyArgs env where
  cmdArgsL :: Lens' env SpicyArgs

instance HasSpicyArgs SpicyArgs where
  cmdArgsL = castOptic simple

----------------------------------------------------------------------------------------------------

-- | Command line arguments for Spicy for a normal run.
spicyArgs :: SpicyArgs
spicyArgs =
  SpicyArgs
    { verbose = False &= typ "BOOL" &= help "Print debug information and be very chatty.",
      input = def &= typFile &= help "Path to the input file.",
      logfile = Nothing &= typFile &= help "Path to the log file.",
      startupconf =
        Nothing &= typFile
          &= help
            "Path to pre-startup scripts for computational chemistry software."
    }
    &= help "Execute spicy normally."
