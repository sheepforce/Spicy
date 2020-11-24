-- |
-- Module      : Spicy.InputFile
-- Description : Definition of the input file
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module defines the structure of an input file for ONIOM calculations by data types. The actual
-- parsing of the input file will happen as YAML by Aeson.
module Spicy.InputFile
  ( InputFile (..),
    HasInputFile (..),
    Task (..),
    InputMolecule (..),
    FileType (..),
    TopoChanges (..),
    Model (..),
    TheoryLayer (..),
    Execution (..),
  )
where

import Data.Aeson
import Optics
import RIO hiding (Lens', lens)
import Spicy.Aeson
import Spicy.Common
import Spicy.Molecule (Embedding, Program)

-- | Definition of a complete calculation of arbitrary type.
data InputFile = InputFile
  { -- | A sequence of tasks to perform. They will be executed in
    --   order of the most current structure then. For example first
    --   'Optimise', then do a 'Frequency' calculation on the
    --   optimised structure and then run an 'MD' simulation.
    task :: Seq Task,
    -- | Molecular system input by an external file.
    molecule :: InputMolecule,
    -- | Changes in the topology.
    topology :: Maybe TopoChanges,
    -- | Calculation model that is being used (means a compound
    --   method, not a concrete type).
    model :: Model,
    -- | Fast directory used for scratch files of the wrapped
    --   software.
    scratch :: JDirPath,
    -- | Directory for permanent files.
    permanent :: JDirPath
  }
  deriving (Eq, Show, Generic)

instance ToJSON InputFile where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON InputFile

-- Lenses
instance (k ~ A_Lens, a ~ Seq Task, b ~ a) => LabelOptic "task" k InputFile InputFile a b where
  labelOptic = lens (\s -> task s) $ \s b -> s {task = b}

instance (k ~ A_Lens, a ~ InputMolecule, b ~ a) => LabelOptic "molecule" k InputFile InputFile a b where
  labelOptic = lens (\s -> molecule s) $ \s b -> s {molecule = b}

instance (k ~ A_Lens, a ~ Maybe TopoChanges, b ~ a) => LabelOptic "topology" k InputFile InputFile a b where
  labelOptic = lens (\s -> topology s) $ \s b -> s {topology = b}

instance (k ~ A_Lens, a ~ Model, b ~ a) => LabelOptic "model" k InputFile InputFile a b where
  labelOptic = lens (\s -> model s) $ \s b -> s {model = b}

instance (k ~ A_Lens, a ~ JDirPath, b ~ a) => LabelOptic "scratch" k InputFile InputFile a b where
  labelOptic = lens (\s -> scratch s) $ \s b -> s {scratch = b}

instance (k ~ A_Lens, a ~ JDirPath, b ~ a) => LabelOptic "permanent" k InputFile InputFile a b where
  labelOptic = lens (\s -> permanent s) $ \s b -> s {permanent = b}

-- Reader Class
class HasInputFile env where
  inputFileL :: Lens' env InputFile

instance HasInputFile InputFile where
  inputFileL = castOptic simple

----------------------------------------------------------------------------------------------------

-- | A task to perform with the molecule.
data Task
  = -- | Single point energy calculation. No change in structure
    Energy
  | -- | Optimisation of the molecular structure.
    Optimise
  | -- | Frequency calculation with numerical or analytical hessian.
    Frequency
  | -- | Molecular dynamics simulation.
    MD
  deriving (Eq, Show, Generic)

instance ToJSON Task where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Task

----------------------------------------------------------------------------------------------------
data InputMolecule = InputMolecule
  { -- | Optionally a annotation of what file type is to be expected.
    fileType :: Maybe FileType,
    -- | A JSON enabled filepath to the file containing the overall
    --   system.
    path :: JFilePath
  }
  deriving (Eq, Show, Generic)

instance ToJSON InputMolecule where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON InputMolecule

-- Lenses
instance (k ~ A_Lens, a ~ Maybe FileType, b ~ a) => LabelOptic "fileType" k InputMolecule InputMolecule a b where
  labelOptic = lens (\s -> fileType s) $ \s b -> s {fileType = b}

instance (k ~ A_Lens, a ~ JFilePath, b ~ a) => LabelOptic "path" k InputMolecule InputMolecule a b where
  labelOptic = lens (\s -> path s) $ \s b -> s {path = b}

----------------------------------------------------------------------------------------------------

-- | Filetypes for molecules, that can be read as input.
data FileType
  = -- | Molden XYZ file in angstroms.
    XYZ
  | -- | PDB as parsed by 'Spicy.Molecule.Internal.Parser.parsePDB'.
    PDB
  | -- | Sybyl MOL2 file as parsed by 'Spicy.Molecule.Internal.Parser.parseMOL2'.
    MOL2
  | -- | Tinker XYZ file as parsed by 'Spicy.Molecule.Internal.Parser.parseTXYZ'.
    TXYZ
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON FileType where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON FileType

----------------------------------------------------------------------------------------------------

-- | Topology related tasks, such as bond definitions.
data TopoChanges = TopoChanges
  { -- | Ígnore all old bonds and guess them from scratch with
    --   a distance criterion based on covalent radii. Makes
    --   only sense if the file did not provide bonds.
    guessBonds :: Bool,
    -- | Scaling factor for covalent radius distance criterion.
    radiusScaling :: Maybe Double,
    -- | Pairs of atoms between, that are also bonded.
    bondsToRemove :: Maybe [(Int, Int)],
    -- | Pairs of atoms between, between which a bond will be
    --   removed.
    bondsToAdd :: Maybe [(Int, Int)]
  }
  deriving (Eq, Show, Generic)

instance ToJSON TopoChanges where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON TopoChanges

-- Lenses
instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "guessBonds" k TopoChanges TopoChanges a b where
  labelOptic = lens (\s -> guessBonds s) $ \s b -> s {guessBonds = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "radiusScaling" k TopoChanges TopoChanges a b where
  labelOptic = lens (\s -> radiusScaling s) $ \s b -> s {radiusScaling = b}

instance (k ~ A_Lens, a ~ Maybe [(Int, Int)], b ~ a) => LabelOptic "bondsToRemove" k TopoChanges TopoChanges a b where
  labelOptic = lens (\s -> bondsToRemove s) $ \s b -> s {bondsToRemove = b}

instance (k ~ A_Lens, a ~ Maybe [(Int, Int)], b ~ a) => LabelOptic "bondsToAdd" k TopoChanges TopoChanges a b where
  labelOptic = lens (\s -> bondsToAdd s) $ \s b -> s {bondsToAdd = b}

----------------------------------------------------------------------------------------------------

-- | A generic ONIOM-n calculation.
data Model = ONIOMn
  { -- | This defines the layer partitioning and levels of theory.
    --   This is a nested recursive data structure and the top
    --   theory layer can contain an arbitrary stack of deeper
    --   layers and on the same level multiple layers may exists, to
    --   describe multi-centre ONIOM.
    theoryLayer :: TheoryLayer
  }
  deriving (Eq, Show, Generic)

instance ToJSON Model where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Model

-- Lenses
instance (k ~ A_Lens, a ~ TheoryLayer, b ~ a) => LabelOptic "theoryLayer" k Model Model a b where
  labelOptic = lens (\s -> theoryLayer s) $ \s b -> s {theoryLayer = b}

----------------------------------------------------------------------------------------------------

-- | Definition of a calculation for a computational chemistry program in a single layer.
data TheoryLayer = TheoryLayer
  { -- | Label of the layer, just for description.
    name :: Text,
    -- | Path to a template file in Jinja syntax for the
    --   computational chemistry program of this layer.
    templateFile :: JFilePath,
    -- | The program to use for the computation.
    program :: Program,
    -- | Indices of the atoms for this layer. Those are the ones
    --   from the 'Map Int Atom' structure of a 'Molecule'. A check
    --   of sanity of those indices will depend on the methodology
    --   used (ONIOM vs- QM/MM).
    selection :: IntSet,
    -- | Deeper layers of the calculation. If the list is empty,
    --   this is the highest level (with respect to computational
    --   cost) layer. Being a 'Seq', this structure allows to
    --   define multi-centre ONIOM.
    deeperLayer :: Seq TheoryLayer,
    -- | Charge of the (sub)system.
    charge :: Int,
    -- | Multiplicity of the (sub)system.
    mult :: Int,
    -- | Information about the execution of the computational
    --   chemistry software, that is not relevant for system
    --   description.
    execution :: Execution,
    -- | Defines the embedding type for the current layer.
    embedding :: Embedding
  }
  deriving (Eq, Show, Generic)

instance ToJSON TheoryLayer where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON TheoryLayer

-- Lenses
instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "name" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> name s) $ \s b -> s {name = b}

instance (k ~ A_Lens, a ~ JFilePath, b ~ a) => LabelOptic "templateFile" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> templateFile s) $ \s b -> s {templateFile = b}

instance (k ~ A_Lens, a ~ Program, b ~ a) => LabelOptic "program" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> program s) $ \s b -> s {program = b}

instance (k ~ A_Lens, a ~ IntSet, b ~ a) => LabelOptic "selection" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> selection s) $ \s b -> s {selection = b}

instance (k ~ A_Lens, a ~ Seq TheoryLayer, b ~ a) => LabelOptic "deeperLayer" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> deeperLayer s) $ \s b -> s {deeperLayer = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "harge" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> charge s) $ \s b -> s {charge = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "mult" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> mult s) $ \s b -> s {mult = b}

instance (k ~ A_Lens, a ~ Execution, b ~ a) => LabelOptic "execution" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> execution s) $ \s b -> s {execution = b}

instance (k ~ A_Lens, a ~ Embedding, b ~ a) => LabelOptic "embedding" k TheoryLayer TheoryLayer a b where
  labelOptic = lens (\s -> embedding s) $ \s b -> s {embedding = b}

----------------------------------------------------------------------------------------------------

-- |
-- Information about how to execute a program, independent of system description.
data Execution = Execution
  { -- | Number of processes to launch (usually MPI).
    nProcesses :: Int,
    -- | Number of threads per (MPI) process to launch.
    nThreads :: Int,
    -- | The memory in MB per process for the program.
    memory :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON Execution where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Execution

-- Lenses
instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "nProcesses" k Execution Execution a b where
  labelOptic = lens (\s -> nProcesses s) $ \s b -> s {nProcesses = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "nThreads" k Execution Execution a b where
  labelOptic = lens (\s -> nThreads s) $ \s b -> s {nThreads = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "memory" k Execution Execution a b where
  labelOptic = lens (\s -> memory s) $ \s b -> s {memory = b}
