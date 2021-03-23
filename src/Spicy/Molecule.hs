-- |
-- Module      : Spicy.Molecule
-- Description : Handling molecular informations
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
module Spicy.Molecule
  ( -- * Types

    -- ** Molecular Structure
    Element (..),
    Atom (..),
    LinkInfo (..),
    Molecule (..),
    HasMolecule (..),
    HasDirectMolecule (..),
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

    -- ** Optimiser Settings
    Optimisation (..),
    CoordType (..),
    HessianUpdate (..),
    OptType (..),
    _Minimum,
    _SaddlePoint,
    TSOptAlg (..),
    MinOptAlg (..),
    GeomConv (..),

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
    isPsi4,
    isXTB,

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
    getMolByID,
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
    redistributeLinkMoments',
    isAtomLink,
    removeRealLinkTagsFromModel,
    molToPointCharges,
    getAllCalcIDsHierarchically,
    getAllMolIDsHierarchically,
    updatePositionsPosVec,
    updatePositions,
    shrinkNeighbourList,
    isolateMoleculeLayer,
    molID2OniomHumanID,
    horizontalSlices,
    gradDense2Sparse,
    calcGeomConv,

    -- * Writer
    writeXYZ,
    writeXYZSimple,
    writeTXYZ,
    writeMOL2,
    writePDB,
    writeSpicy,
    writeONIOM,
  )
where

import Spicy.Molecule.Internal.Multipoles
import Spicy.Molecule.Internal.Parser
import Spicy.Molecule.Internal.Types
import Spicy.Molecule.Internal.Util
import Spicy.Molecule.Internal.Writer
