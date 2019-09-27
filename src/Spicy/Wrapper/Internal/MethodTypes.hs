{-|
Module      : Spicy.Wrapper.Internal.MethodTypes
Description : JSON translatable type to define actually available methods in wrappers
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module provides a type and JSON instances for method definitions in quantum chemistry programs.
-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Wrapper.Internal.MethodTypes
  ( -- * Computational Chemistry
    Available(..)
  , available_Basis
  , CalculationNiveau
  , _CalculationNiveau_QuantumMechanics
  , _CalculationNiveau_MolecularMechanics
    -- ** Quantum Chemistry
  , QuantumMechanics(..)
  , _QuantumMechanics_HatreeFock
    -- ** Basis Sets and ECPs
  , Basis(..)
  , basis_BuiltIn
  , basis_AcceptsInline
  , basis_AcceptsFile
  , HartreeFock(..)
  , hartreeFock_IntergalApproximation
  , hartreeFock_NumericalEffiency
  )
where
import           Control.Lens
import           Data.Aeson
import           Data.Map.Lazy                  ( Map )
import           Data.Set                       ( Set )
import           Data.Text.Lazy                 ( Text )
import           GHC.Generics                   ( Generic )
import           Spicy.Molecule.Internal.Types
import           Spicy.Wrapper.Internal.InputTypes
                                                ( HartreeFockIntergalApproximation
                                                , NumericalEfficiency
                                                )

----------------------------------------------------------------------------------------------------
{-|
This type provides a structure to define (and read from a config file), if specific methods are
available in a program. This follows closely what is possible in
'Spicy.Wrapper.Internal.InputTypes', but doesn't need to be as flexible, as the pure availability of
a basis set for example tells nothing about if it should be used as an RIJK auxiliary basis, for
example.
-}
data Available = Available
  { _available_Basis             :: Maybe Basis -- ^ Configuration which types of basis sets can be
                                                --   accepted. Only valid ('Just') for QM programs.
  , _available_CalculationNiveau :: Set CalculationNiveau
  }
  deriving ( Eq, Show, Generic )

instance ToJSON Available where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Available

----------------------------------------------------------------------------------------------------
{-|
Definition of the calculation niveau used, QM or MM.
-}
data CalculationNiveau
  = CalculationNiveau_QuantumMechanics QuantumMechanics
  | CalculationNiveau_MolecularMechanics
  deriving ( Eq, Show, Generic )

instance FromJSON CalculationNiveau

instance ToJSON CalculationNiveau where
  toEncoding = genericToEncoding defaultOptions

----------------------------------------------------------------------------------------------------
{-| Definition of quantum chemical hamiltonians.
-}
data QuantumMechanics
  = QuantumMechanics_HatreeFock HartreeFock
  deriving ( Eq, Show, Generic )

instance FromJSON QuantumMechanics

instance ToJSON QuantumMechanics where
  toEncoding = genericToEncoding defaultOptions

----------------------------------------------------------------------------------------------------
{-|
Available basis sets.
-}
data Basis = Basis
  { _basis_BuiltIn       :: Map Text (Set Element) -- ^ A 'Map' from basis set keywords to
                                                   --  'Element's, for which they are available.
  , _basis_AcceptsInline :: Bool                   -- ^ If a program accepts inline basis set
                                                   --   definitons.
  , _basis_AcceptsFile   :: Bool                   -- ^ If a program accepts reading basis set
                                                   --   definitons from a file.
  }
  deriving ( Eq, Show, Generic )

instance FromJSON Basis

instance ToJSON Basis where
  toEncoding = genericToEncoding defaultOptions

----------------------------------------------------------------------------------------------------
data HartreeFock = HartreeFock
  { _hartreeFock_IntergalApproximation :: HartreeFockIntergalApproximation
  , _hartreeFock_NumericalEffiency     :: Set NumericalEfficiency
  }
  deriving ( Eq, Show, Generic )

instance FromJSON HartreeFock

instance ToJSON HartreeFock where
  toEncoding = genericToEncoding defaultOptions

----------------------------------------------------------------------------------------------------
makeLenses ''Available
makePrisms ''CalculationNiveau
makePrisms ''QuantumMechanics
makeLenses ''Basis
makeLenses ''HartreeFock
