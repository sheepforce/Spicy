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

    -- * Helper functions
  )
where

import Control.Monad.Free
import Data.Aeson
import Data.Default
import RIO hiding (Lens', lens, view, (^.), (^?))

type TurbomoleInput = Free TurbomoleInputF ()

-- | Wrapper around 'Control' to make it a functor around an abstract type.
data TurbomoleInputF a = Input Control a deriving (Functor)

----------------------------------------------------------------------------------------------------

-- | User exposed settings of Turbomole's @control@ file.
data Control = Control
  { -- | Settings for the @$atoms@ block. Mainly used for basis set input.
    atoms :: Atoms,
    -- | Molecular charge
    charge :: Int,
    -- | Control of the SCF loop and convergece
    scf :: SCF,
    -- | Selection of the reference wavefunction. Usually Hartree-Fock or Kohn-Sham DFT
    refWfn :: RefWfn,
    -- | Resolution of Identity approximation to the reference wavefunction.
    ri :: Maybe RI,
    -- | Wavefunction correlation and excitation settings.
    excorr :: Maybe CorrelationExc,
    -- | Memory for turbomole in MiB
    memory :: Natural,
    -- | Arbitrary other content, that will be put verbatim into control
    other :: Maybe Text
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
    -- | Auxiliary JK-fitting basis from the library
    jkbas :: Maybe Text,
    -- | Effective core potential from the library
    ecp :: Maybe Text,
    -- | Correlation fitting basis from the library
    cbas :: Maybe Text,
    -- | Complementary auxiliary basis for explicitly correlated calculations
    cabs :: Maybe Text,
    -- | Other keywords to put verbatim in the @$atoms@ block
    other :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

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
  | -- | Restricted Kohn-Sham DFT
    RKS DFT
  | -- | Unrestricted Kohn-Sham DFT. Needs a multiplicity.
    UKS Natural DFT
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

instance FromJSON Functional where
  parseJSON v = case v of
    String "s-vwn" -> pure SVWN
    String "pwlda" -> pure PWLDA
    String "b-lyp" -> pure BLYP
    String "b-vwn" -> pure BVWN
    String "b-p" -> pure BP
    String "pbe" -> pure PBE
    String "tpss" -> pure TPSS
    String "scan" -> pure SCAN
    String "bh-lyp" -> pure BHLYP
    String "b3-lyp" -> pure B3LYP
    String "pbe0" -> pure PBE0
    String "tpssh" -> pure TPSSh
    String "pw6b95" -> pure PW6B95
    String "m06" -> pure M06
    String "m06-l" -> pure M06_L
    String "m06-2x" -> pure M06_2X
    String "lhf" -> pure LHF
    String "oep" -> pure OEP
    String "b97-d" -> pure B97D
    String "pbeh-3c" -> pure PBEh_3C
    String "b97-3c" -> pure B97_3C
    String "lh07t-svwn" -> pure LH07T_SVWN
    String "lh07s-svwn" -> pure LH07S_SVWN
    String "lh12ct-ssirpw92" -> pure LH212CT_SSIRPW92
    String "lh12ct-ssifpw92" -> pure LH212CT_SSIFPW92
    String "lh14t-calpbe" -> pure LH14T_CALPBE
    String "lh20t" -> pure LH20T
    String "b2-plyp" -> pure B2PLYP
    String "hse06" -> pure HSE06
    String "cam-b3lyp" -> pure CAM_B3LYP
    String "wb97x" -> pure WB97X
    String "wb97x-d" -> pure WB97X_D
    String "wb97x-v" -> pure WB97X_V
    String "wb97m-v" -> pure WB97M_V
    String "m11" -> pure M11
    String "revm11" -> pure RevM11
    String "mn12-sx" -> pure MN12_SX
    String "mn15" -> pure MN15
    String "mn15-l" -> pure MN15_L
    String "revtpss" -> pure RevTPSS
    String "pkzb" -> pure PKZB
    String "mpsts" -> pure MPSTS
    String "mpsts-noa2" -> pure MPSTS_NOA2
    String "lhj14" -> pure LHJ14
    String "r2scan" -> pure R2SCAN
    o -> fail $ "encountered unknown field for functional " <> show o

instance ToJSON Functional where
  toJSON f = case f of
    SVWN -> toJSON @Text "s-vwn"
    PWLDA -> toJSON @Text "pwlda"
    BLYP -> toJSON @Text "b-lyp"
    BVWN -> toJSON @Text "b-vwn"
    BP -> toJSON @Text "b-p"
    PBE -> toJSON @Text "pbe"
    TPSS -> toJSON @Text "tpss"
    SCAN -> toJSON @Text "scan"
    BHLYP -> toJSON @Text "bh-lyp"
    B3LYP -> toJSON @Text "b3-lyp"
    PBE0 -> toJSON @Text "pbe0"
    TPSSh -> toJSON @Text "tpssh"
    PW6B95 -> toJSON @Text "pw6b95"
    M06 -> toJSON @Text "m06"
    M06_L -> toJSON @Text "m06-l"
    M06_2X -> toJSON @Text "m06-2x"
    LHF -> toJSON @Text "lhf"
    OEP -> toJSON @Text "oep"
    B97D -> toJSON @Text "b97-d"
    PBEh_3C -> toJSON @Text "pbeh-3c"
    B97_3C -> toJSON @Text "b97-3c"
    LH07T_SVWN -> toJSON @Text "lh07t-svwn"
    LH07S_SVWN -> toJSON @Text "lh07s-svwn"
    LH212CT_SSIRPW92 -> toJSON @Text "lh12ct-ssirpw92"
    LH212CT_SSIFPW92 -> toJSON @Text "lh12ct-ssifpw92"
    LH14T_CALPBE -> toJSON @Text "lh14t-calpbe"
    LH20T -> toJSON @Text "lh20t"
    B2PLYP -> toJSON @Text "b2-plyp"
    HSE06 -> toJSON @Text "hse06"
    CAM_B3LYP -> toJSON @Text "cam-b3lyp"
    WB97X -> toJSON @Text "wb97x"
    WB97X_D -> toJSON @Text "wb97x-d"
    WB97X_V -> toJSON @Text "wb97x-v"
    WB97M_V -> toJSON @Text "wb97m-v"
    M11 -> toJSON @Text "m11"
    RevM11 -> toJSON @Text "revm11"
    MN12_SX -> toJSON @Text "mn12-sx"
    MN15 -> toJSON @Text "mn15"
    MN15_L -> toJSON @Text "mn15-l"
    RevTPSS -> toJSON @Text "revtpss"
    PKZB -> toJSON @Text "pkzb"
    MPSTS -> toJSON @Text "mpsts"
    MPSTS_NOA2 -> toJSON @Text "mpsts-noa2"
    LHJ14 -> toJSON @Text "lhj14"
    R2SCAN -> toJSON @Text "r2scan"

-- | DFT Grids. @Gx@ are the turbomole standard grids, while @Mx@ are the adaptive grids.
data Grid
  = G1
  | G2
  | G3
  | G4
  | G5
  | M3
  | M4
  | M5
  deriving (Eq, Show, Generic)

instance FromJSON Grid where
  parseJSON g = case g of
    String "1" -> pure G1
    String "2" -> pure G2
    String "3" -> pure G3
    String "4" -> pure G4
    String "5" -> pure G5
    String "m3" -> pure M3
    String "m4" -> pure M4
    String "m5" -> pure M5
    o -> fail $ "encountered unknown field for grid " <> show o

instance ToJSON Grid where
  toJSON g = case g of
    G1 -> toJSON @Text "1"
    G2 -> toJSON @Text "2"
    G3 -> toJSON @Text "3"
    G4 -> toJSON @Text "4"
    G5 -> toJSON @Text "5"
    M3 -> toJSON @Text "m3"
    M4 -> toJSON @Text "m4"
    M5 -> toJSON @Text "m5"

-- | Dispersion correction for DFT
data Dispersion = D3 | D3BJ | D4 deriving (Eq, Show, Generic)

instance FromJSON Dispersion

instance ToJSON Dispersion

----------------------------------------------------------------------------------------------------

-- | Resolution of identity for the reference wavefunction (HF or DFT)
data RI = RIJ | RIJK deriving (Eq, Show, Generic)

instance FromJSON RI

instance ToJSON RI

----------------------------------------------------------------------------------------------------

-- TODO - Serialise with a tmpdir

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
        approx :: TDApprox,
        -- | Other keywords to put in the @$soes@ block
        other :: Maybe [Text]
      }
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

instance FromJSON CorrModels where
  parseJSON v = case v of
    String "cis" -> pure CIS
    String "mp2" -> pure MP2
    String "mp4" -> pure MP4
    String "cis(d)" -> pure CISd
    String "adc(2)" -> pure ADC2
    String "cc2" -> pure CC2
    String "cc3" -> pure CC3
    String "ccsd" -> pure CCSD
    String "ccsd(t)" -> pure CCSDt
    o -> fail $ "encountered unknown field for CorrModels " <> show o

instance ToJSON CorrModels where
  toJSON c = case c of
    CIS -> toJSON @Text "cis"
    MP2 -> toJSON @Text "mp2"
    MP4 -> toJSON @Text "mp4"
    CISd -> toJSON @Text "cis(d)"
    ADC2 -> toJSON @Text "adc(2)"
    CC2 -> toJSON @Text "cc2"
    CC3 -> toJSON @Text "cc3"
    CCSD -> toJSON @Text "ccsd"
    CCSDt -> toJSON @Text "ccsd(t)"

-- | For linear response TD-DFT or TD-HF either RPA or TDA can be used.
data TDApprox = RPA | TDA deriving (Eq, Show, Generic)

instance FromJSON TDApprox

instance ToJSON TDApprox
