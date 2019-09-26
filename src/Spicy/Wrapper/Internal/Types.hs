{-|
Module      : Spicy.Wrapper.Internal.Types
Description : Types to define an atomic chemical computation for wrappers
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module defines a complex, nested data type, which is defining the input, that a quantum
chemistry program/molecular mechanics program needs.
-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Spicy.Wrapper.Internal.Types
  ( -- * Generic Types
    AtomIdentifier
    -- * Computational Chemistry Calculation
  , Wrapper(..)
  , wrapper_Task
  , wrapper_Charge
  , wrapper_Multiplicity
  , wrapper_CalculationNiveau
  , CalculationNiveau(..)
  , Task(..)
  , Property(..)
  , ChargeType(..)
    -- ** Quantum Chemistry calculation
  , QuantumMechanics(..)
    -- *** Basis Sets and ECP definitions
  , Basis(..)
  , BasisSetDefinition(..)
  , ECPDefiniton(..)
    -- *** Hamiltonian
  , QMTheory(..)
    -- **** Hartree-Fock
  , HartreeFock(..)
  , HartreeFockIntergalApproximation(..)
  )
where
import           Data.IntMap.Lazy               ( IntMap )
import           Data.Map                       ( Map )

import           Control.Lens
import           Data.Set                       ( Set )
import           Data.Text.Lazy                 ( Text )
import           GHC.Generics                   ( Generic )
import           Spicy.Molecule.Internal.Types

{-|
This is a unique identifier of an Atom. To have a general counting scheme, this should be the
element symbol of the atom together with its integer key in the IntMap, from the molecule. The key
needs to be 0 based continous, as obtained from 'reIndex2BaseMolecule'.
-}
type AtomIdentifier = IntMap Element

----------------------------------------------------------------------------------------------------
{-|
Complete definition of a calculation, that is supposed to be performed by a wrapper. This is the
data structure, that is meant to be translated to an input file for a wrapped programm.
-}
data Wrapper = Wrapper
  { _wrapper_Task              :: Task  -- ^ A task to be performed by wrapper, e.g. calculate the gradient.
  , _wrapper_Charge            :: Int -- ^ The charge of the system.
  , _wrapper_Multiplicity      :: Int -- ^ The multiplicity of the system.
  , _wrapper_CalculationNiveau :: CalculationNiveau --
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Defines tasks to be performed by the wrapper program.
-}
data CalculationNiveau
  = CalculationNiveau_QuantumMechanics QuantumMechanics
  | CalculationNiveau_MolecularMechanics
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Defines tasks to be performed by the wrapper program.
-}
data Task
  = Task_Energy                  -- ^ Single point energy calculation.
  | Task_Gradient                -- ^ Gradient calculation.
  | Task_Hessian                 -- ^ Hessian calculation.
  | Task_Property (Set Property) -- ^ A set of properties to calculate.
  | Task_StabilityAnalysis       -- ^ A stability analysis on the SCF solution with following.
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Properties, that can be calculated by a wrapper.
-}
data Property
  = Property_Charge ChargeType
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Partial charge, that can be calculated by a wrapper.
-}
data ChargeType
  = ChargeType_RESP
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Defines a quantum mechanical calculation.
-}
data QuantumMechanics = QuantumMechanics
  { _quantumMechanics_BasisSet :: Maybe Basis    -- ^ Definition of the basis sets of a
                                                 --   non-semiempirical QM calculation. Is 'Nothing'
                                                 --   if no basis set is required, e.g. for
                                                 --   semiempiricism.
  , _quantumMechanics_QMTheory :: QMTheory       -- ^ The quantum mechanical calculation niveau,
                                                 --   e.g. Hartree-Fock, DFT, CASSCF, ... .
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Definition of the basis set for the whole molecule. Has the option to arbitrarily mix basis sets, by
mapping 'AtomIdentifier' to 'BasisSetDefinition's.
-}
data Basis = Basis
  { _basis_Primary     :: Map AtomIdentifier BasisSetDefinition         -- ^ Primary basis set,
                                                                        --   needs to be specified
                                                                        --   for every
                                                                        --   non-semiempirical
                                                                        --   calculation.
  , _basis_RIJK        :: Maybe (Map AtomIdentifier BasisSetDefinition) -- ^ Density fitting basis
                                                                        --   set for RI-J, RI-JK,
                                                                        --   RIJCOSX, ... methods.
  , _basis_Correlation :: Maybe (Map AtomIdentifier BasisSetDefinition) -- ^ Correlation auxiliary
                                                                        --   basis for RI-MP2,
                                                                        --   RI-NEVPT2, ...
  , _basis_ECP         :: Maybe (Map AtomIdentifier ECPDefiniton)       -- ^ An effective core
                                                                        --   potential.
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Definition of a basis set. Three options are availabel to define a basis set:

  * A program specific, internal definition of a basis set, e.g. "cc-pVDZ".
  * A program specific inline definition of a basis set, which can be used in the input at the
    specific position.
  * An external file with the program specific basis set definition.
-}
data BasisSetDefinition
  = BasisSetDefinition_BuiltIn Text      -- ^ Use a built in basis set (simnple keyword).
  | BasisSetDefinition_InLine Text       -- ^ Use a multiline program specific basis set defintion
                                         --   for this atom.
  | BasisSetDefinition_FromFile FilePath -- ^ Use an external file, with basis set informations,
                                         --   which can be read by the program.
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Definition of an ECP/PP. Works exactly as 'BasisSetDefinition'.
-}
data ECPDefiniton
 = ECPDefiniton_BuiltIn Text
 | ECPDefiniton_InLine Text
 | ECPDefiniton_FromFile FilePath
 deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Definition of the quantum chemical hamiltonian.
-}
data QMTheory
  = QMTheory_HartreeFock HartreeFock
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Definition of a Hartree-Fock calculation.
-}
data HartreeFock = HartreeFock
  { _hartreeFock_IntergalApproximation :: HartreeFockIntergalApproximation -- ^ Approximation to the
                                                                           --   electron repulsion
                                                                           --   integrals in
                                                                           --   Hartree-Fock.
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Approximations to the integrals in Hartree-Fock.
-}
data HartreeFockIntergalApproximation
  = HartreeFockIntergalApproximation_RIJK
  | HartreeFockIntergalApproximation_RIJCOSX
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
makeLenses ''Wrapper
makeLenses ''CalculationNiveau
makeLenses ''Task
makeLenses ''Property
makeLenses ''ChargeType
makeLenses ''QuantumMechanics
makeLenses ''Basis
makeLenses ''BasisSetDefinition
makeLenses ''ECPDefiniton
makeLenses ''QMTheory
makeLenses ''HartreeFock
makeLenses ''HartreeFockIntergalApproximation
