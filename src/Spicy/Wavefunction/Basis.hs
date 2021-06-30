{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : Spicy.Wavefunction.Basis
-- Description : GTO Basis functions
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Storing basis functions and -sets internally.
module Spicy.Wavefunction.Basis 
  ( Coordinates,
    Shell,
    ShellType,
    shellTypeToInt
    ,)
  where

-- Need to write the following FCHK blocks:
--  Alpha/Beta MO Coefficients
--  SCF Density (density matrix)
--  Primitive exponents
--  Primitive coefficients
--  Shell types
--  # of Primitives for each shell
--  Coordinates of each shell
--  Shell-to-atom map
--  Number of contracted shells
--  Highest angular momentum
--  Largest degree of contraction
--  Number of primitive shells

import RIO hiding (lens, (^.), (.~))
import Optics
import Spicy.Common

data Coordinates = Coordinates
  { x :: !Double,
    y :: !Double,
    z :: !Double
  }

-- =============================================================================

-- $shells

-- | Shells are groups of basis functions with the same angular momentum l.
data Shell = Shell
  { shellType :: ShellType,
    primitives :: Set Gauss,
    coordinates :: Coordinates
  }

-- | In FChks, the shell type determines what type of basis functions the
-- shell contains.
data ShellType = Cart !Int | Spher !Int | SP

-- | Because Gaussian is Gaussian, FChks represent the type of shell
-- with an integer number. Negative integers represent spherical shells,
-- with the exception of -1, which stands for SP combined shells.
shellTypeToInt :: ShellType -> Int
shellTypeToInt st = case st of
  Cart i -> i
  Spher i | i == 1 -> i
  Spher i | otherwise -> negate i
  SP -> -1

-- =============================================================================

-- $basis functions

-- | A single, primitive Gauss function. Should always be part of a contraction.
-- Coordinates are specified at the contraction level.
data Gauss = Gauss
  { exponent :: Double,
    coefficient :: Double
  }

data Representation = Cartesian | Spherical

data QNs (rep :: Representation) where
  CartesianQNs :: QNsCartesian -> QNs 'Cartesian
  SphericalQNs :: QNsSpherical -> QNs 'Spherical

-- | Cartesian quantum numbers, determining the spatial orientation of
-- cartesian basis functions. Note that these are not _actual_ quantum numbers;
-- they have little physical meaning.
data QNsCartesian = QNsCartesian
  { x :: !Int,
    y :: !Int,
    z :: !Int
  }

-- | Spherical (regular) quantum numbers.
data QNsSpherical = QNsSpherical
  { n :: !Int,
    l :: !Int,
    ml :: !Int
  }

-- | Contractions are groups of primitive Gauss functions sharing quantum
-- numbers and spatial location.
data Contraction (rep :: Representation) where
  Contraction :: Coordinates -> QNs rep -> Seq Gauss -> Contraction rep
  UnknownContraction :: Coordinates -> Seq Gauss -> Contraction x
  deriving Generic

instance (k ~ A_Lens, a ~ Seq Gauss, b ~ a) => LabelOptic "primitives" k (Contraction rep) (Contraction rep) a b where
  labelOptic = lens getThis setThis
    where
      getThis x = case x of
        Contraction _ _ g -> g
        UnknownContraction _ g -> g
      setThis x g = case x of
        Contraction c q _ -> Contraction c q g
        UnknownContraction c _ -> UnknownContraction c g



-- =============================================================================

-- $wavefunctions

-- | A set of MO coefficients, together with a set of basis functions,
-- determines a wave function.
newtype MOCoefficients = MOCoefficients
  {getCoefficients :: VectorS Double}

-- | A density matrix, together with a set of basis functions,
-- determines a wave function.
newtype DensityMatrix = DensityMatrix
  {getDensityMatrix :: MatrixS Double}

-- =============================================================================