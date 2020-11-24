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
    exec,
    translate,
    spicyModes,
  )
where

import Optics
import RIO hiding (Lens', lens)
import Spicy.InputFile
import System.Console.CmdArgs hiding
  ( Default,
    name,
    program,
  )
import qualified System.Console.CmdArgs as CmdArgs

data SpicyArgs
  = Exec
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
  | Translate
      { verbose :: Bool,
        input :: FilePath,
        inputFormat :: FileType
      }
  deriving (Data, Typeable, Show, Eq)

-- Lenses
instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "verbose" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> verbose s) $ \s b -> s {verbose = b}

instance (k ~ A_Lens, a ~ FilePath, b ~ a) => LabelOptic "input" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> input s) $ \s b -> s {input = b}

instance (k ~ A_Lens, a ~ Maybe FilePath, b ~ a) => LabelOptic "logfile" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> logfile s) $ \s b -> case s of
    Translate {} -> s
    Exec {} -> s {logfile = b}

instance (k ~ A_Lens, a ~ Maybe FilePath, b ~ a) => LabelOptic "startupconf" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> startupconf s) $ \s b -> case s of
    Translate {} -> s
    Exec {} -> s {startupconf = b}

instance (k ~ A_Lens, a ~ FileType, b ~ a) => LabelOptic "inputFormat" k SpicyArgs SpicyArgs a b where
  labelOptic = lens (\s -> inputFormat s) $ \s b -> case s of
    Translate {} -> s
    Exec {} -> s {inputFormat = b}

-- Reader Class.
class HasSpicyArgs env where
  cmdArgsL :: Lens' env SpicyArgs

instance HasSpicyArgs SpicyArgs where
  cmdArgsL = castOptic simple

----------------------------------------------------------------------------------------------------

-- | Command line arguments for Spicy for a normal run.
exec :: SpicyArgs
exec =
  Exec
    { verbose = False &= typ "BOOL" &= help "Print debug information and be very chatty.",
      input = def &= typFile &= help "Path to the input file.",
      logfile = Nothing &= typFile &= help "Path to the log file.",
      startupconf =
        Nothing &= typFile
          &= help
            "Path to pre-startup scripts for computational chemistry software."
    }
    &= help "Execute spicy normally."

----------------------------------------------------------------------------------------------------
translate :: SpicyArgs
translate =
  Translate
    { verbose = False &= typ "BOOL" &= help "Print debug information and be very chatty.",
      input = def &= typFile &= help "Path to a molecule file to convert.",
      inputFormat = XYZ &= typ "File type" &= help "Fileformat"
    }
    &= help "Convert a molecule to a different format in Spicy style."

----------------------------------------------------------------------------------------------------

-- | Setup of CmdArgs for Multimode runs.
spicyModes :: SpicyArgs
spicyModes =
  modes [exec, translate]
    &= summary "Multilayer and compound methods for chemistry"
    &= CmdArgs.program "spicy"

-- &= help "Compound and multilayer methods for chemistry."
