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
    Style (..),
    InputMolecule (..),
    FileType (..),
    TopoChanges (..),
    Model (..),
    TheoryLayer (..),
    Execution (..),
    Opt (..),
    OptTarget (..),
  )
where

import Data.Aeson
import Optics
import RIO hiding (Lens', lens)
import Spicy.Aeson
import Spicy.Common
import Spicy.Molecule (CoordType, Embedding, HessianUpdate, Program)

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

instance FromJSON InputFile where
  parseJSON = genericParseJSON spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ Seq Task, b ~ a) => LabelOptic "task" k InputFile InputFile a b where
  labelOptic = lens task $ \s b -> s {task = b}

instance (k ~ A_Lens, a ~ InputMolecule, b ~ a) => LabelOptic "molecule" k InputFile InputFile a b where
  labelOptic = lens molecule $ \s b -> s {molecule = b}

instance (k ~ A_Lens, a ~ Maybe TopoChanges, b ~ a) => LabelOptic "topology" k InputFile InputFile a b where
  labelOptic = lens topology $ \s b -> s {topology = b}

instance (k ~ A_Lens, a ~ Model, b ~ a) => LabelOptic "model" k InputFile InputFile a b where
  labelOptic = lens model $ \s b -> s {model = b}

instance (k ~ A_Lens, a ~ JDirPath, b ~ a) => LabelOptic "scratch" k InputFile InputFile a b where
  labelOptic = lens scratch $ \s b -> s {scratch = b}

instance (k ~ A_Lens, a ~ JDirPath, b ~ a) => LabelOptic "permanent" k InputFile InputFile a b where
  labelOptic = lens permanent $ \s b -> s {permanent = b}

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
  | -- | Optimisation of the molecular structure. Can do simple optimisations with macro iterarions
    -- only or advanced optimisations with micro iterations. Macroiterations are controlled by
    -- the optimisation settings on the real layer, optimisations with micro iterations are
    -- controlled by each layer individually.
    Optimise Style
  | -- | Frequency calculation with numerical or analytical hessian.
    Frequency
  | -- | Molecular dynamics simulation.
    MD
  deriving (Eq, Show, Generic)

instance ToJSON Task where
  toJSON Energy = toJSON @Text "energy"
  toJSON (Optimise Macro) = toJSON @Text "optimise_macro"
  toJSON (Optimise Micro) = toJSON @Text "optimise_micro"
  toJSON Frequency = toJSON @Text "frequency"
  toJSON MD = toJSON @Text "md"

instance FromJSON Task where
  parseJSON v =
    case v of
      String "energy" -> pure Energy
      String "frequency" -> pure Frequency
      String "md" -> pure MD
      String "optimise_macro" -> pure $ Optimise Macro
      String "optimise_micro" -> pure $ Optimise Micro
      o -> fail $ "encountered unknown field for task" <> show o

----------------------------------------------------------------------------------------------------

-- | Optimisation style. Either simple with macro iterations only or by using microiterations on
-- each layer.
data Style
  = Macro
  | Micro
  deriving (Eq, Show, Generic)

instance ToJSON Style where
  toJSON Macro = toJSON @Text "macro"
  toJSON Micro = toJSON @Text "micro"

instance FromJSON Style where
  parseJSON v = case v of
    String "macro" -> pure Macro
    String "micro" -> pure Micro
    _ -> fail "encountered unknown field for optimisation style"

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

instance FromJSON InputMolecule where
  parseJSON = genericParseJSON spicyJOption

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

instance FromJSON FileType where
  parseJSON = genericParseJSON spicyJOption

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

instance FromJSON TopoChanges where
  parseJSON = genericParseJSON spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ Bool, b ~ a) => LabelOptic "guessBonds" k TopoChanges TopoChanges a b where
  labelOptic = lens guessBonds $ \s b -> s {guessBonds = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "radiusScaling" k TopoChanges TopoChanges a b where
  labelOptic = lens radiusScaling $ \s b -> s {radiusScaling = b}

instance (k ~ A_Lens, a ~ Maybe [(Int, Int)], b ~ a) => LabelOptic "bondsToRemove" k TopoChanges TopoChanges a b where
  labelOptic = lens bondsToRemove $ \s b -> s {bondsToRemove = b}

instance (k ~ A_Lens, a ~ Maybe [(Int, Int)], b ~ a) => LabelOptic "bondsToAdd" k TopoChanges TopoChanges a b where
  labelOptic = lens bondsToAdd $ \s b -> s {bondsToAdd = b}

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

instance FromJSON Model where
  parseJSON = genericParseJSON spicyJOption

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
    embedding :: Embedding,
    -- | Settings for the optimiser on a given layer.
    optimisation :: Maybe Opt
  }
  deriving (Eq, Show, Generic)

instance ToJSON TheoryLayer where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON TheoryLayer where
  parseJSON = genericParseJSON spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ Text, b ~ a) => LabelOptic "name" k TheoryLayer TheoryLayer a b where
  labelOptic = lens name $ \s b -> s {name = b}

instance (k ~ A_Lens, a ~ JFilePath, b ~ a) => LabelOptic "templateFile" k TheoryLayer TheoryLayer a b where
  labelOptic = lens templateFile $ \s b -> s {templateFile = b}

instance (k ~ A_Lens, a ~ Program, b ~ a) => LabelOptic "program" k TheoryLayer TheoryLayer a b where
  labelOptic = lens program $ \s b -> s {program = b}

instance (k ~ A_Lens, a ~ IntSet, b ~ a) => LabelOptic "selection" k TheoryLayer TheoryLayer a b where
  labelOptic = lens selection $ \s b -> s {selection = b}

instance (k ~ A_Lens, a ~ Seq TheoryLayer, b ~ a) => LabelOptic "deeperLayer" k TheoryLayer TheoryLayer a b where
  labelOptic = lens deeperLayer $ \s b -> s {deeperLayer = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "charge" k TheoryLayer TheoryLayer a b where
  labelOptic = lens charge $ \s b -> s {charge = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "mult" k TheoryLayer TheoryLayer a b where
  labelOptic = lens mult $ \s b -> s {mult = b}

instance (k ~ A_Lens, a ~ Execution, b ~ a) => LabelOptic "execution" k TheoryLayer TheoryLayer a b where
  labelOptic = lens execution $ \s b -> s {execution = b}

instance (k ~ A_Lens, a ~ Embedding, b ~ a) => LabelOptic "embedding" k TheoryLayer TheoryLayer a b where
  labelOptic = lens embedding $ \s b -> s {embedding = b}

instance (k ~ A_Lens, a ~ Maybe Opt, b ~ a) => LabelOptic "optimisation" k TheoryLayer TheoryLayer a b where
  labelOptic = lens optimisation $ \s b -> s {optimisation = b}

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

instance FromJSON Execution where
  parseJSON = genericParseJSON spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "nProcesses" k Execution Execution a b where
  labelOptic = lens nProcesses $ \s b -> s {nProcesses = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "nThreads" k Execution Execution a b where
  labelOptic = lens nThreads $ \s b -> s {nThreads = b}

instance (k ~ A_Lens, a ~ Int, b ~ a) => LabelOptic "memory" k Execution Execution a b where
  labelOptic = lens memory $ \s b -> s {memory = b}

----------------------------------------------------------------------------------------------------

-- | Settings for geometry optimisations. Either for macroiterations for the full system or for
-- a single layer if for microiterations. All values are optional and defaults for minima
-- optimisations on small and medium systems will be used.
data Opt = Opt
  { -- | Selects whether to optimise to a minimum or a saddle point.
    target :: Maybe OptTarget,
    -- | Coordinate system in which the optimisation is carried out.
    coords :: Maybe CoordType,
    -- | Maximum number of iterations. Only influences microiterations. Macroiterations are taken
    -- from somewhere else.
    iterations :: Maybe Int,
    -- | Option to recalculate the hessian every n steps. If not given, the hessian will never be
    -- recalculated.
    hessianRecalc :: Maybe Int,
    -- | Hessian update algorithm.
    hessianUpdate :: Maybe HessianUpdate,
    -- | Initial trust radius in optimisations.
    trust :: Maybe Double,
    -- | Maximum the trust radius can reach.
    trustMax :: Maybe Double,
    -- | Minimum the trust radius can reach.
    trustMin :: Maybe Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Opt where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON Opt where
  parseJSON = genericParseJSON spicyJOption

-- Lenses
instance (k ~ A_Lens, a ~ Maybe OptTarget, b ~ a) => LabelOptic "target" k Opt Opt a b where
  labelOptic = lens target $ \s b -> s {target = b}

instance (k ~ A_Lens, a ~ Maybe CoordType, b ~ a) => LabelOptic "coords" k Opt Opt a b where
  labelOptic = lens coords $ \s b -> s {coords = b}

instance (k ~ A_Lens, a ~ Maybe Int, b ~ a) => LabelOptic "iterations" k Opt Opt a b where
  labelOptic = lens iterations $ \s b -> s {iterations = b}

instance (k ~ A_Lens, a ~ Maybe Int, b ~ a) => LabelOptic "hessianRecalc" k Opt Opt a b where
  labelOptic = lens hessianRecalc $ \s b -> s {hessianRecalc = b}

instance (k ~ A_Lens, a ~ Maybe HessianUpdate, b ~ a) => LabelOptic "hessianUpdate" k Opt Opt a b where
  labelOptic = lens hessianUpdate $ \s b -> s {hessianUpdate = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "trust" k Opt Opt a b where
  labelOptic = lens trust $ \s b -> s {trust = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "trustMax" k Opt Opt a b where
  labelOptic = lens trustMax $ \s b -> s {trustMax = b}

instance (k ~ A_Lens, a ~ Maybe Double, b ~ a) => LabelOptic "trustMin" k Opt Opt a b where
  labelOptic = lens trustMin $ \s b -> s {trustMin = b}

----------------------------------------------------------------------------------------------------
data OptTarget
  = Min
  | TS
  deriving (Eq, Show, Generic)

instance ToJSON OptTarget where
  toEncoding = genericToEncoding spicyJOption

instance FromJSON OptTarget where
  parseJSON = genericParseJSON spicyJOption
