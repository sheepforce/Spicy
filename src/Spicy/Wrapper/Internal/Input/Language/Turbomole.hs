{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- |
-- Module      : Spicy.Wrapper.Internal.Input.Language.Turbomole
-- Description : Preparing input for turbomole
-- Copyright   : Phillip Seeber 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This provides data types and abstraction around the most interesting parts of turbomole control
-- files.
-- Turbomole input serialisation. Uses simple input style, starting from 7.5.1.
module Spicy.Wrapper.Internal.Input.Language.Turbomole
  ( -- * Free
    TurbomoleInput,
    TurbomoleInputF (..),

    -- * Control File
    Control (..),
    Atoms (..),
    SCF (..),
    Damp (..),
    RefWfn (..),
    DFT (..),
    Functional (..),
    Grid (..),
    Dispersion (..),
    RI (..),
    CorrelationExc (..),
    CorrModels (..),
    TDApprox (..),
  )
where

import Control.Monad.Free
import Data.Aeson
import Data.Default
import RIO hiding (Lens', lens, view, (^.), (^?))

type TurbomoleInput = Free TurbomoleInputF ()

-- | Wrapper around 'Control' to make it a functor around an abstract type.
data TurbomoleInputF a = TurbomoleInputF Control a deriving (Functor)

----------------------------------------------------------------------------------------------------

-- | User exposed settings of Turbomole's @control@ file.
data Control = Control
  { -- | Settings for the @$atoms@ block. Mainly used for basis set input.
    atoms :: Atoms,
    -- | Control of the SCF loop and convergece
    scf :: SCF,
    -- | Selection of the reference wavefunction. Usually Hartree-Fock or Kohn-Sham DFT
    refWfn :: RefWfn,
    -- | Resolution of Identity approximation to the reference wavefunction.
    ri :: Maybe RI,
    -- | Wavefunction correlation and excitation settings.
    excorr :: Maybe CorrelationExc
  }
  deriving (Generic)

instance FromJSON Control

instance ToJSON Control

----------------------------------------------------------------------------------------------------

-- | Contents of the @$atoms@ block.
data Atoms = Atoms
  { -- | Basis from the turbomole basis set library
    basis :: Text,
    -- | Auxiliary coulomb-fitting basis from the library
    jbas :: Maybe Text,
    -- | Effective core potential from the library
    ecp :: Maybe Text,
    -- | Correlation fitting basis from the library
    cbas :: Maybe Text,
    -- | Complementary auxiliary basis for explicitly correlated calculations
    cabs :: Maybe Text,
    -- | Other keywords to put verbatim in the @$atoms@ block
    other :: Maybe [Text]
  }
  deriving (Generic)

instance FromJSON Atoms

instance ToJSON Atoms

----------------------------------------------------------------------------------------------------

-- | SCF settings. Serialise to multiple fields but control SCF cycles as common ground.
data SCF = SCF
  { -- | SCF convergence threshold as exponent.
    conv :: Natural,
    -- | Maximum number of iterations of the SCF.
    iter :: Natural,
    -- | Damping control.
    damp :: Maybe Damp,
    -- | Orbital level shift
    shift :: Maybe Double
  }
  deriving (Generic)

instance FromJSON SCF

instance ToJSON SCF

instance Default SCF where
  def = SCF {conv = 8, iter = 200, damp = def, shift = Nothing}

----------------------------------------------------------------------------------------------------

-- | Settings of the SCF damping
data Damp = Damp
  { -- | Amount of old fock matrix to be mixed in.
    start :: Double,
    -- | On good convergece how much to reduce the mixing amount in each SCF step.
    step :: Double,
    -- | Minimum amount of the old fock matrix to be used in damping. Will not drop below this.
    min :: Double
  }
  deriving (Generic)

instance FromJSON Damp

instance ToJSON Damp

instance Default Damp where
  def = Damp {start = 0.75, step = 0.05, min = 0.1}

----------------------------------------------------------------------------------------------------

-- | Reference wavefunction specification.
data RefWfn
  = -- | Restricted Hartree-Fock
    RHF
  | -- | Unrestricted Hartree-Fock. Needs a multiplicity.
    UHF Natural
  | -- | Restricted open-shell Hartree-Fock. Needs a multiplicity.
    ROHF Natural
  | -- | Restricted Kohn-Sham DFT
    RKS DFT
  | -- | Unrestricted Kohn-Sham DFT. Needs a multiplicity.
    UKS Natural DFT
  deriving (Generic)

instance FromJSON RefWfn

instance ToJSON RefWfn

----------------------------------------------------------------------------------------------------

-- | Settings for density functional theory.
data DFT = DFT
  { -- | Density functional to be used
    functional :: Functional,
    -- | DFT grid specification
    grid :: Grid,
    -- | Dispersion correction
    disp :: Maybe Dispersion,
    -- | Other keywords to put verbatim in the @$dft@ block
    other :: Maybe [Text]
  }
  deriving (Generic)

instance FromJSON DFT

instance ToJSON DFT

-- | Density functionals available in Turbomole
data Functional
  = SVWN
  | PWLDA
  | BLYP
  | BVWN
  | BP
  | PBE
  | TPSS
  | SCAN
  | BHLYP
  | B3LYP
  | PBE0
  | TPSSh
  | PW6B95
  | M06
  | M06_L
  | M06_2X
  | LHF
  | OEP
  | B97D
  | PBEh_3C
  | B97_3C
  | LH07T_SVWN
  | LH07S_SVWN
  | LH212CT_SSIRPW92
  | LH212CT_SSIFPW92
  | LH14T_CALPBE
  | LH20T
  | B2PLYP
  | HSE06
  | CAM_B3LYP
  | WB97X
  | WB97X_D
  | WB97X_V
  | WB97M_V
  | M11
  | RevM11
  | MN12_SX
  | MN15
  | MN15_L
  | RevTPSS
  | PKZB
  | MPSTS
  | MPSTS_NOA2
  | LHJ14
  | R2SCAN
  deriving (Generic)

instance FromJSON Functional

instance ToJSON Functional

-- | DFT Grids. @Gx@ are the turbomole standard grids, while @Mx@ are the adaptive grids.
data Grid
  = G1
  | G2
  | G3
  | G4
  | G5
  | M1
  | M2
  | M3
  deriving (Generic)

instance FromJSON Grid

instance ToJSON Grid

-- | Dispersion correction for DFT
data Dispersion = D3 | D3BJ | D4 deriving (Generic)

instance FromJSON Dispersion

instance ToJSON Dispersion

----------------------------------------------------------------------------------------------------

-- | Resolution of identity for the reference wavefunction (HF or DFT)
data RI = RIJ | RIJK deriving (Generic)

instance FromJSON RI

instance ToJSON RI

----------------------------------------------------------------------------------------------------

-- TODO - Serialise with a tmpdir
-- TODO - Serialise with gradient calculation. Required for embedding

-- | Correlation and possibly excited states of the reference wavefunction.
data CorrelationExc
  = PNOCCSD
      { -- | Wavefunction correlation models
        model :: CorrModels,
        -- | Maximum number of iterations
        iter :: Maybe Natural,
        -- | Excited states settings
        exci :: Maybe Natural,
        -- | Other keywords to put in the @$pnoccsd@ block verbatim
        other :: Maybe [Text]
      }
  | RICC
      { -- | Wavefunction correlation models
        model :: CorrModels,
        -- | Maximum number of iterations
        iter :: Maybe Natural,
        -- | Excited states settings
        exci :: Maybe Natural,
        -- | Other keywords to put in the @$ricc2@ block verbatim
        other :: Maybe [Text]
      }
  | TDSCF
      { -- | Number of excited states
        exci :: Maybe Natural,
        -- | Which version of excited state calculation to use. Either full RPA or simplified
        -- TDA/CIS
        approx :: TDApprox
      }
  deriving (Generic)

instance FromJSON CorrelationExc

instance ToJSON CorrelationExc

-- | Available wavefunction models in the PNO and RI-CC modules
data CorrModels
  = CIS
  | MP2
  | MP4
  | -- | CIS(D)
    CISd
  | ADC2
  | CC2
  | CC3
  | CCSD
  | -- | CCSD(T)
    CCSDt
  deriving (Generic)

instance FromJSON CorrModels

instance ToJSON CorrModels

-- | For linear response TD-DFT or TD-HF either RPA or TDA can be used.
data TDApprox = RPA | TDA deriving (Generic)

instance FromJSON TDApprox

instance ToJSON TDApprox
