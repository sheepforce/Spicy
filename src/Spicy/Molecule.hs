-- |
-- Module      : Spicy.Molecule
-- Description : Handling molecular informations
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
module Spicy.Molecule
  ( -- * Types

    -- ** Molecular Structure
    Element (..),
    Atom (..),
    Molecule (..),
    HasMolecule (..),
    MolID,

    -- ** Calculations
    CalcID (..),
    CalcK (..),
    _ONIOMKey,
    ONIOMHierarchy (..),

    -- ** Multipole Properties
    Multipoles (..),
    Monopole (..),
    MultipoleR0,
    Dipole (..),
    MultipoleR1,
    Quadrupole (..),
    MultipoleR2,
    Octopole (..),
    MultipoleR3,
    Hexadecapole (..),
    MultipoleR4,

    -- ** ONIOM Embedding
    Embedding (..),
    _Mechanical,
    _Electronic,

    -- ** Energy Derivatives
    EnergyDerivatives (..),

    -- ** Wrapper Context
    NumericalEfficiency (..),
    WrapperTask (..),
    QMContext (..),
    MMContext (..),
    QMMMSpec (..),
    _QM,
    _MM,
    CalcContext (..),
    CalcInput (..),
    CalcOutput (..),

    -- ** Programs
    Program (..),

    -- * Parser
    xyz,
    txyz,
    mol2,
    pdb,

    -- * Utility Functions
    molMap,
    molMapWithMolID,
    molTraverse,
    molTraverseWithID,
    molFoldl,
    molFoldlWithMolID,
    molIDLensGen,
    calcIDLensGen,
    getCalcByID,
    getMaxAtomIndex,
    newSubLayer,
    createLinkAtom,
    addAtom,
    removeAtom,
    BondOperation (..),
    changeBond,
    neighbourList,
    guessBondMatrix,
    fragmentDetectionDetached,
    getPolarisationCloudFromAbove,
    redistributeLinkMoments,
    isAtomLink,
    removeRealLinkTagsFromModel,
    molToPointCharges,
    getAllCalcIDsHierarchically,
    getAllMolIDsHierarchically,
    updateMolWithPosVec,

    -- * Writer
    writeXYZ,
    writeTXYZ,
    writeMOL2,
    writePDB,
    writeSpicy,
    -- writeLayout,
  )
where

import Spicy.Molecule.Internal.Multipoles
import Spicy.Molecule.Internal.Parser
import Spicy.Molecule.Internal.Types
import Spicy.Molecule.Internal.Util
import Spicy.Molecule.Internal.Writer
