{-|
Module      : Spicy.Class
Description : Definitions of all Classes and Types used in Spicy
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

This module collects all type and typeclass definitions for Spicy in a single place. This makes the
implementation of RIO's @Has*@ typeclasses more consistent, than scattering type definitions across
modules.
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Spicy.Class
  ( -- ** Massiv
    -- $jsonTypesAccelerate
    VectorS(..)
  , Array1S
  , MatrixS(..)
  , Array2S
  , Array3S(..)
  , Array4S(..)
    -- ** Paths
    -- $jsonTypesPath
  , JFilePath(..)
  , JFilePathAbs(..)
  , JFilePathRel(..)
  , JDirPath(..)
  , JDirPathAbs(..)
  , JDirPathRel(..)
    -- * Exceptions
    -- $exceptions
  , DataStructureException(..)
  , ParserException(..)
  , MolLogicException(..)
  , WrapperGenericException(..)
  , SpicyIndirectionException(..)
    -- * Input File
    -- $inputFile
  , InputFile(..)
  , task
  , molecule
  , topology
  , model
  , scratch
  , permanent
    -- ** Calculation Task
  , Task(..)
  , _Energy
  , _Optimise
  , _Frequency
  , _MD
    -- ** Molecule to Describe
  , InputMolecule(..)
  , fileType
  , path
  , FileType(..)
  , _XYZ
  , _PDB
  , _MOL2
  , _TXYZ
  , TopoChanges(..)
  , guessBonds
  , radiusScaling
  , bondsToRemove
  , bondsToAdd
    -- ** Setup of the Hamiltonian.
  , Model(..)
  , _ONIOMn
  , theoryLayer
  , TheoryLayer(..)
  , name
  , templateFile
  , program
  , selection
  , deeperLayer
  , charge
  , mult
  , execution
  , embedding
  , Program(..)
  , _Psi4
  , _Nwchem
  , Embedding(..)
  , _Mechanical
  , _Electronic
  , Execution(..)
  , nProcesses
  , nThreads
  , memory
    -- * Molecule
    -- $molecule
    -- ** Molecular Structure and Physical Properties
    -- $moleculeStructure
  , Element(..)
  , AtomLabel
  , FFType(..)
  , _FFMol2
  , _FFTXYZ
  , _FFPDB
  , _FFXYZ
  , Atom(..)
  , atom_Element
  , atom_Label
  , atom_IsPseudo
  , atom_IsCapped
  , atom_IsDummy
  , atom_FFType
  , atom_Coordinates
  , atom_Multipoles
  , BondMatrix
  , Fragment(..)
  , fragment_Label
  , fragment_Chain
  , fragment_Atoms
  , Molecule(..)
  , molecule_Comment
  , molecule_Atoms
  , molecule_Bonds
  , molecule_SubMol
  , molecule_Fragment
  , molecule_EnergyDerivatives
  , molecule_CalcContext
  , Trajectory
  , MolID
  , CalcID(..)
  , calcID_MolID
  , calcID_calcKey
  -- *** Calculations on Molecules
  -- $moleculeCalculation
  , Multipoles(..)
  , multipole_Monopole
  , multipole_Dipole
  , multipole_Quadrupole
  , mutlipole_Octopole
  , mutlipole_Hexadecapole
  , EnergyDerivatives(..)
  , energyDerivatives_Energy
  , energyDerivatives_Gradient
  , energyDerivatives_Hessian
  , NumericalEfficiency
  , _Analytical
  , _Numerical
  , WrapperTask(..)
  , _WTEnergy
  , _WTGradient
  , _WTHessian
  , QMContext(..)
  , qmContext_Charge
  , qmContext_Mult
  , MMContext(..)
  , QMMMSpec(..)
  , _QM
  , _MM
  , CalcInput(..)
  , calcInput_Task
  , calcInput_RestartFile
  , calcInput_Software
  , calcInput_PrefixName
  , calcInput_PermaDir
  , calcInput_ScratchDir
  , calcInput_NProcs
  , calcInput_NThreads
  , calcInput_Memory
  , calcInput_QMMMSpec
  , calcInput_Template
  , calcInput_Embedding
  , CalcOutput(..)
  , calcOutput_EnergyDerivatives
  , calcOutput_Multipoles
  , CalcContext(..)
  , calcContext_Input
  , calcContext_Output
  , CalcK(..)
  , ONIOMHierarchy(..)
    -- * File Format Specific Types
    -- $file
    -- ** Formatted Checkpoint File
    -- $fileFChk
  , ScalarVal(..)
  , _ScalarInt
  , _ScalarDouble
  , _ScalarText
  , _ScalarLogical
  , ArrayVal(..)
  , _ArrayInt
  , _ArrayDouble
  , _ArrayText
  , _ArrayLogical
  , CalcType(..)
  , _SP
  , _FOPT
  , _POPT
  , _FTS
  , _PTS
  , _FSADDLE
  , _PSADDLE
  , _FORCE
  , _FREQ
  , _SCAN
  , _GUESS
  , _LST
  , _STABILITY
  , _REARCHIVE
  , _MSRESTART
  , _MIXED
  , Content(..)
  , _Scalar
  , _Array
  , FChk(..)
  , title
  , calcType
  , basis
  , method
  , blocks
    -- * Command Line Arguments
    -- $cmdArgs
  , SpicyArgs(..)
  , spicyModes
    -- * Class Definitions for Spicy
    -- $classDefinitions
  , Check(..)
  , HumanShow(..)
    -- $classDefinitionsHasRIO
  , HasSpicyArgs(..)
  , HasMolecule(..)
  , HasPreStartUpEnv(..)
  , HasLogFile(..)
  , HasInputFile(..)
    -- * Spicy Execution Environment
    -- $appEnv
  , SpicyEnv(..)
  , sLogFile
  , sMolecule
  , sCalculation
  , sPreStartUp
  , sProcContext
  , MoleculeEnv(..)
  , meNative
  , PreStartUpEnv(..)
  , psEnvPsi4
  , psEnvNwchem
    -- ** Initiator Environmentf
    -- $appEnvInitiator
  , InitEnv(..)
    -- ** Logger
    -- $appEnvLogger
  , MyLogOptions(..)
  , logMinLevel
  , logVerbose
  , logUseColor
  , logUseLoc
  , logFormat
  , logHandle
  , logUseTime
  )
where
import           Control.Lens            hiding ( (:>)
                                                , elements
                                                , ix
                                                , (.=)
                                                )
import           Data.Aeson              hiding ( Array
                                                , Error
                                                )
import           Data.Aeson.TH
import           Data.Default
import qualified Data.IntMap.Strict            as IntMap
import           Data.List.Split
import           Data.Massiv.Array             as Massiv
                                         hiding ( all
                                                , toList
                                                )
import qualified Data.Massiv.Array             as Massiv
                                                ( toList )
import           Formatting
import           GHC.Generics            hiding ( D
                                                , R
                                                )
import           RIO                     hiding ( Lens'
                                                , Vector
                                                , lens
                                                , (^.)
                                                )
import qualified RIO.List                      as List
import           RIO.Process             hiding ( exec )
import qualified RIO.Text                      as Text
import           Spicy.Aeson
import           System.Console.CmdArgs  hiding ( Default
                                                , def
                                                , name
                                                , program
                                                )
import qualified System.Console.CmdArgs        as CmdArgs
import qualified System.Path                   as Path
import           System.Path.PartClass

{-
####################################################################################################
-}
{- $classDefinitions
Definitions of classes used in Spicy.
-}
{-|
A class for various data structures, which use some assumptions in Spicy. Running a check on them
allows to be sure about the correctnes of their assumptions.
-}
class Check a where
  check :: MonadThrow m => a -> m a

----------------------------------------------------------------------------------------------------
{-|
A class for data, that can be printed nicely to a human friendly format with the purpose of logging.
Each line is a separate 'UTF8Builder', which allows prepending the log info string before each line.
-}
class HumanShow a where
  hShow :: a -> [Utf8Builder]

{-
####################################################################################################
-}
{- $jsonTypes
These are 'newtype' wrappers for common data types which come from external libraries, for which a
JSON instance needs to present.
-}
{-
====================================================================================================
-}
{- $jsonTypesMassiv
Newtype wrappers around Massiv's arrays of unboxed elements, which are enabled for JSON
serialisation.
-}
{-|
Newtype wrapper for JSON serialisation around Massiv's unboxed 1D arrays.
-}
newtype VectorS a = VectorS { getVectorS :: Array Massiv.S Ix1 a }
  deriving (Generic, Show, Eq )

instance (ToJSON a, Storable a) => ToJSON (VectorS a) where
  toJSON arr =
    let plainList = Massiv.toList . getVectorS $ arr
        Sz dim1   = Massiv.size . getVectorS $ arr
    in  object ["shape" .= dim1, "elements" .= plainList]

instance (FromJSON a, Storable a) => FromJSON (VectorS a) where
  parseJSON = withObject "VectorS" $ \arr -> do
    dim1 <- arr .: "shape"
    let sizeSupposed = Sz dim1
    elements <- arr .: "elements"
    let parsedArr = Massiv.fromList Par elements
    if Massiv.size parsedArr == sizeSupposed
      then return . VectorS $ parsedArr
      else
        fail
        $  "Size vs number of elements mismatch: Array has size: "
        <> (show . Massiv.size $ parsedArr)
        <> "and expected was: "
        <> show sizeSupposed

type Array1S = VectorS

----------------------------------------------------------------------------------------------------
{-|
Newtype wrapper for JSON serialisation around Massiv's unboxed 2D arrays.
-}
newtype MatrixS a = MatrixS { getMatrixS :: Array Massiv.S Ix2 a }
  deriving (Generic, Show, Eq )

instance (ToJSON a, Storable a) => ToJSON (MatrixS a) where
  toJSON arr =
    let plainList         = Massiv.toList . getMatrixS $ arr
        Sz (dim1 :. dim2) = Massiv.size . getMatrixS $ arr
    in  object ["shape" .= (dim1, dim2), "elements" .= plainList]

instance (FromJSON a, Storable a) => FromJSON (MatrixS a) where
  parseJSON = withObject "MatrixS" $ \arr -> do
    (dim1, dim2) <- arr .: "shape"
    let sizeSupposed = Sz (dim1 :. dim2)
    elements <- arr .: "elements"
    let parsedArr = Massiv.fromLists' Par . chunksOf dim2 $ elements
    if Massiv.size parsedArr == sizeSupposed
      then return . MatrixS $ parsedArr
      else
        fail
        $  "Size vs number of elements mismatch: Array has size: "
        <> (show . Massiv.size $ parsedArr)
        <> "and expected was: "
        <> show sizeSupposed

type Array2S = MatrixS

----------------------------------------------------------------------------------------------------
{-|
Newtype wrapper for JSON serialisation around Massiv's unboxed 3D arrays.
-}
newtype Array3S a = Array3S { getArray3S :: Array Massiv.S Ix3 a }
  deriving (Generic, Show, Eq )

instance (ToJSON a, Storable a) => ToJSON (Array3S a) where
  toJSON arr =
    let plainList                 = Massiv.toList . getArray3S $ arr
        Sz (dim1 :> dim2 :. dim3) = Massiv.size . getArray3S $ arr
    in  object ["shape" .= (dim1, dim2, dim3), "elements" .= plainList]

instance (FromJSON a, Storable a) => FromJSON (Array3S a) where
  parseJSON = withObject "Array3S" $ \arr -> do
    (dim1, dim2, dim3) <- arr .: "shape"
    let sizeSupposed = Sz (dim1 :> dim2 :. dim3)
    elements <- arr .: "elements"
    let parsedArr = Massiv.fromLists' Par . chunksOf dim2 . chunksOf dim3 $ elements
    if Massiv.size parsedArr == sizeSupposed
      then return . Array3S $ parsedArr
      else
        fail
        $  "Size vs number of elements mismatch: Array has size: "
        <> (show . Massiv.size $ parsedArr)
        <> "and expected was: "
        <> show sizeSupposed

----------------------------------------------------------------------------------------------------
{-|
Newtype wrapper for JSON serialisation around Massiv's unboxed 4D arrays.
-}
newtype Array4S a = Array4S { getArray4S :: Array Massiv.S Ix4 a }
  deriving (Generic, Show, Eq )

instance (ToJSON a, Storable a) => ToJSON (Array4S a) where
  toJSON arr =
    let plainList                         = Massiv.toList . getArray4S $ arr
        Sz (dim1 :> dim2 :> dim3 :. dim4) = Massiv.size . getArray4S $ arr
    in  object ["shape" .= (dim4, dim3, dim2, dim1), "elements" .= plainList]

instance (FromJSON a, Storable a) => FromJSON (Array4S a) where
  parseJSON = withObject "Array4S" $ \arr -> do
    (dim1, dim2, dim3, dim4) <- arr .: "shape"
    let sizeSupposed = Sz (dim1 :> dim2 :> dim3 :. dim4)
    elements <- arr .: "elements"
    let parsedArr =
          Massiv.fromLists' Par . chunksOf dim2 . chunksOf dim3 . chunksOf dim4 $ elements
    if Massiv.size parsedArr == sizeSupposed
      then return . Array4S $ parsedArr
      else
        fail
        $  "Size vs number of elements mismatch: Array has size: "
        <> (show . Massiv.size $ parsedArr)
        <> "and expected was: "
        <> show sizeSupposed
{-
====================================================================================================
-}
{-
{- $jsonTypesAccelerate
Custom 'newtype' wrappers are defined around the 'Data.Array.Accelerate' module, which add
additional behaviours to Accelerate types.
-}
{-|
'newtype' wrapper to Accelerate's 'A.Vector's.
-}
newtype AccVector a = AccVector { getAccVector :: A.Vector a }
  deriving ( Generic, Show, Eq )

instance ( ToJSON a, A.Elt a ) => ToJSON (AccVector a) where
  toJSON vec =
    let plainVec        = getAccVector vec
        (A.Z A.:. xDim) = A.arrayShape plainVec
        elements        = A.toList plainVec
    in  object ["shape" .= xDim, "elements" .= elements]

instance ( FromJSON a, A.Elt a ) => FromJSON (AccVector a) where
  parseJSON = withObject "AccVector" $ \vec -> do
    xDim     <- vec .: "shape"
    elements <- vec .: "elements"
    return . AccVector $ A.fromList (A.Z A.:. xDim) elements

type AccTensor1 = AccVector

----------------------------------------------------------------------------------------------------
{-|
'newtype' wrapper to Accelerate's 'A.Matrix's.
-}
newtype AccMatrix a = AccMatrix { getAccMatrix :: A.Matrix a }
  deriving ( Generic, Show, Eq )

instance ( ToJSON a, A.Elt a ) => ToJSON (AccMatrix a) where
  toJSON mat =
    let plainMat                  = getAccMatrix mat
        (A.Z A.:. xDim A.:. yDim) = A.arrayShape plainMat
        elements                  = A.toList plainMat
    in  object ["shape" .= (xDim, yDim), "elements" .= elements]

instance ( FromJSON a, A.Elt a ) => FromJSON (AccMatrix a) where
  parseJSON = withObject "AccMatrix" $ \mat -> do
    (xDim, yDim) <- mat .: "shape"
    elements     <- mat .: "elements"
    return . AccMatrix $ A.fromList (A.Z A.:. xDim A.:. yDim) elements

type AccTensor2 = AccMatrix

----------------------------------------------------------------------------------------------------
{-|
'newtype' wrapper to Accelerate's 3D arrays.
-}
newtype AccTensor3 a = AccTensor3 { getAccTensor3 :: A.Array A.DIM3 a }
  deriving ( Eq, Show, Generic )

instance ( ToJSON a, A.Elt a ) => ToJSON (AccTensor3 a) where
  toJSON tensor =
    let plainT3                             = getAccTensor3 tensor
        (A.Z A.:. dim1 A.:. dim2 A.:. dim3) = A.arrayShape plainT3
        elements                            = A.toList plainT3
    in  object ["shape" .= (dim1, dim2, dim3), "elements" .= elements]

instance ( FromJSON a, A.Elt a ) => FromJSON (AccTensor3 a) where
  parseJSON = withObject "AccTensor3" $ \mat -> do
    (dim1, dim2, dim3) <- mat .: "shape"
    elements           <- mat .: "elements"
    return . AccTensor3 $ A.fromList (A.Z A.:. dim1 A.:. dim2 A.:. dim3) elements

----------------------------------------------------------------------------------------------------
{-|
'newtype' wrapper to Accelerate's 3D arrays.
-}
newtype AccTensor4 a = AccTensor4 { getAccTensor4 :: A.Array A.DIM4 a }
  deriving ( Eq, Show, Generic )

instance ( ToJSON a, A.Elt a ) => ToJSON (AccTensor4 a) where
  toJSON tensor =
    let plainT4  = getAccTensor4 tensor
        (A.Z A.:. dim1 A.:. dim2 A.:. dim3 A.:. dim4) = A.arrayShape plainT4
        elements = A.toList plainT4
    in  object ["shape" .= (dim1, dim2, dim3, dim4), "elements" .= elements]

instance ( FromJSON a, A.Elt a ) => FromJSON (AccTensor4 a) where
  parseJSON = withObject "AccTensor4" $ \mat -> do
    (dim1, dim2, dim3, dim4) <- mat .: "shape"
    elements                 <- mat .: "elements"
    return . AccTensor4 $ A.fromList (A.Z A.:. dim1 A.:. dim2 A.:. dim3 A.:. dim4) elements
-}

{-
====================================================================================================
-}
{- $jsonTypesPath
These are 'newtype'-wrapped types for the pathtype paths, which provide JSON support.
-}
{-|
Wraper for the pathtype 'AbsRelFile', which has JSON serialisation support.
-}
newtype JFilePath = JFilePath { getFilePath :: Path.AbsRelFile }
  deriving ( Generic, Show, Eq )

instance ToJSON JFilePath where
  toJSON path = let plainPath = getFilePath path in object ["filepath" .= Path.toString plainPath]

instance FromJSON JFilePath where
  parseJSON = withObject "JFilePath" $ \path -> do
    plainPath <- path .: "filepath"
    case Path.parse plainPath of
      Left  err -> fail err
      Right res -> return . JFilePath $ res

----------------------------------------------------------------------------------------------------
{-|
Wraper for the pathtype 'AbsFile', which has JSON serialisation support.
-}
newtype JFilePathAbs = JFilePathAbs { getFilePathAbs :: Path.AbsFile }
  deriving ( Generic, Show, Eq )

instance ToJSON JFilePathAbs where
  toJSON path =
    let plainPath = getFilePathAbs path in object ["filepath" .= Path.toString plainPath]

instance FromJSON JFilePathAbs where
  parseJSON = withObject "JFilePath" $ \path -> do
    plainPath <- path .: "filepath"
    case Path.parse plainPath of
      Left  err -> fail err
      Right res -> return . JFilePathAbs $ res

----------------------------------------------------------------------------------------------------
{-|
Wraper for the pathtype 'AbsFile', which has JSON serialisation support.
-}
newtype JFilePathRel = JFilePathRel { getFilePathRel :: Path.RelFile }
  deriving ( Generic, Show, Eq )

instance ToJSON JFilePathRel where
  toJSON path =
    let plainPath = getFilePathRel path in object ["filepath" .= Path.toString plainPath]

instance FromJSON JFilePathRel where
  parseJSON = withObject "JFilePath" $ \path -> do
    plainPath <- path .: "filepath"
    case Path.parse plainPath of
      Left  err -> fail err
      Right res -> return . JFilePathRel $ res

----------------------------------------------------------------------------------------------------
{-|
Wraper for the pathtype 'AbsRelFile', which has JSON serialisation support.
-}
newtype JDirPath = JDirPath { getDirPath :: Path.AbsRelDir }
  deriving ( Generic, Show, Eq )

instance ToJSON JDirPath where
  toJSON path = let plainPath = getDirPath path in object ["dirpath" .= Path.toString plainPath]

instance FromJSON JDirPath where
  parseJSON = withObject "JDirPath" $ \path -> do
    plainPath <- path .: "dirpath"
    case Path.parse plainPath of
      Left  err -> fail err
      Right res -> return . JDirPath $ res

----------------------------------------------------------------------------------------------------
{-|
Wraper for the pathtype 'AbsRelFile', which has JSON serialisation support.
-}
newtype JDirPathAbs = JDirPathAbs { getDirPathAbs :: Path.AbsDir }
  deriving ( Generic, Show, Eq )

instance ToJSON JDirPathAbs where
  toJSON path =
    let plainPath = getDirPathAbs path in object ["dirpath" .= Path.toString plainPath]

instance FromJSON JDirPathAbs where
  parseJSON = withObject "JDirPath" $ \path -> do
    plainPath <- path .: "dirpath"
    case Path.parse plainPath of
      Left  err -> fail err
      Right res -> return . JDirPathAbs $ res

----------------------------------------------------------------------------------------------------
{-|
Wraper for the pathtype 'AbsRelFile', which has JSON serialisation support.
-}
newtype JDirPathRel = JDirPathRel { getDirPathRel :: Path.RelDir }
  deriving ( Generic, Show, Eq )

instance ToJSON JDirPathRel where
  toJSON path =
    let plainPath = getDirPathRel path in object ["dirpath" .= Path.toString plainPath]

instance FromJSON JDirPathRel where
  parseJSON = withObject "JDirPath" $ \path -> do
    plainPath <- path .: "dirpath"
    case Path.parse plainPath of
      Left  err -> fail err
      Right res -> return . JDirPathRel $ res

{-
####################################################################################################
-}
{- $exceptions
-}
{-|
Exception type for operations on data structures, which are not meeting necessary criteria for the
operation to perform.
-}
data DataStructureException = DataStructureException
  { dsExcFunctionName :: String -- ^ Function which is causing the exception.
  , dsExcDescription  :: String -- ^ Description of the problem.
  }

instance Show DataStructureException where
  show (DataStructureException f e) = "DataStructureException in function \"" <> f <> "\":" <> e

instance Exception DataStructureException

----------------------------------------------------------------------------------------------------
{-|
Exception type for textual or binary data, that could not be parsed.
-}
newtype ParserException = ParserException String

instance Show ParserException where
  show (ParserException e) = "ParserException in parser: \"" <> e <> "\""

instance Exception ParserException

----------------------------------------------------------------------------------------------------
{-|
Exception type for operations on 'Molecule's, which lead to a logical error. This can be caused
because some Spicy assumptions are not met, for example.
-}
data MolLogicException = MolLogicException
  { mlExcFunctionName :: !String
  , mlExcDescription  :: !String
  }

instance Show MolLogicException where
  show (MolLogicException f e) = "MoleculeLogicException in function \"" <> f <> "\": " <> e

instance Exception MolLogicException

----------------------------------------------------------------------------------------------------
{-|
Exceptions for a computational chemistry wrapper, that are unspecific to a program.
-}
data WrapperGenericException = WrapperGenericException
  { wgExcFunction    :: !String
  , wgExcDescription :: !String
  }

instance Show WrapperGenericException where
  show (WrapperGenericException f e) = "WrapperGenericException: " <> f <> e

instance Exception WrapperGenericException

----------------------------------------------------------------------------------------------------
{-|
An exception when the program control flow did a wrong turn and the information present are
inadequate to describe the program flow.
-}
data SpicyIndirectionException = SpicyIndirectionException
  { sIndirectionFunctionName :: !String
  , sIndirectionDescription  :: !String
  }

instance Show SpicyIndirectionException where
  show (SpicyIndirectionException f e) =
    "SpicyIndirectionException in function \"" <> f <> "\": " <> e

instance Exception SpicyIndirectionException

{-
####################################################################################################
-}
{- $inputFile
This is the definition of a YAML-structured input file for Spicy. This is the file that describes
what Spicy is supposed to do after execution.
-}
{-|
Definition of a complete calculation of arbitrary type.
-}
data InputFile = InputFile
  { _task      :: Seq Task          -- ^ A sequence of tasks to perform. They will be executed in
                                    --   order of the most current structure then. For example first
                                    --   'Optimise', then do a 'Frequency' calculation on the
                                    --   optimised structure and then run an 'MD' simulation.
  , _molecule  :: InputMolecule     -- ^ Molecular system input by an external file.
  , _topology  :: Maybe TopoChanges -- ^ Changes in the topology.
  , _model     :: Model             -- ^ Calculation model that is being used (means a compound
                                    --   method, not a concrete type).
  , _scratch   :: JDirPath          -- ^ Fast directory used for scratch files of the wrapped
                                    --   software.
  , _permanent :: JDirPath          -- ^ Directory for permanent files.
  }
  deriving ( Eq, Show, Generic )

{-
====================================================================================================
-}
{-|
A task to perform with the molecule.
-}
data Task
  = Energy    -- ^ Single point energy calculation. No change in structure
  | Optimise  -- ^ Optimisation of the molecular structure.
  | Frequency -- ^ Frequency calculation with numerical or analytical hessian.
  | MD        -- ^ Molecular dynamics simulation.
  deriving ( Eq, Show, Generic )

{-
====================================================================================================
-}
data InputMolecule = InputMolecule
  { _fileType :: Maybe FileType -- ^ Optionally a annotation of what file type is to be expected.
  , _path     :: JFilePath      -- ^ A JSON enabled filepath to the file containing the overall
                                --   system.
  }
  deriving ( Eq, Show, Generic )

----------------------------------------------------------------------------------------------------
{-|
Filetypes for molecules, that can be read as input.
-}
data FileType
  = XYZ   -- ^ Molden XYZ file in angstroms as parsed by 'Spicy.Molecule.Internal.Parser.parseXYZ'.
  | PDB   -- ^ PDB as parsed by 'Spicy.Molecule.Internal.Parser.parsePDB'.
  | MOL2  -- ^ Sybyl MOL2 file as parsed by 'Spicy.Molecule.Internal.Parser.parseMOL2'.
  | TXYZ  -- ^ Tinker XYZ file as parsed by 'Spicy.Molecule.Internal.Parser.parseTXYZ'.
  deriving ( Eq, Show, Generic, Data, Typeable )

{-
====================================================================================================
-}
{-| Topology related tasks, such as bond definitions.
-}
data TopoChanges = TopoChanges
  { _guessBonds    :: Bool               -- ^ Ígnore all old bonds and guess them from scratch with
                                         --   a distance criterion based on covalent radii. Makes
                                         --   only sense if the file did not provide bonds.
  , _radiusScaling :: Maybe Double       -- ^ Scaling factor for covalent radius distance criterion.
  , _bondsToRemove :: Maybe [(Int, Int)] -- ^ Pairs of atoms between, that are also bonded.
  , _bondsToAdd    :: Maybe [(Int, Int)] -- ^ Pairs of atoms between, between which a bond will be
                                         --   removed.
  }
  deriving ( Eq, Show, Generic )

{-
====================================================================================================
-}
{-|
Calculation model to employ. This can be a ONIOM method for example.
-}
data Model
  = ONIOMn -- ^ A generic ONIOM-n calculation.
      { _theoryLayer :: TheoryLayer -- ^ This defines the layer partitioning and levels of theory.
                                    --   This is a nested recursive data structure and the top
                                    --   theory layer can contain an arbitrary stack of deeper
                                    --   layers and on the same level multiple layers may exists, to
                                    --   describe multi-centre ONIOM.
      }
  deriving ( Eq, Show, Generic )

----------------------------------------------------------------------------------------------------
{-|
Definition of a calculation for a computational chemistry program in a single layer.
-}
data TheoryLayer = TheoryLayer
  { _name         :: Text            -- ^ Label of the layer, just for description.
  , _templateFile :: JFilePath       -- ^ Path to a template file in Jinja syntax for the
                                     --   computational chemistry program of this layer.
  , _program      :: Program         -- ^ The program to use for the computation.
  , _selection    :: IntSet          -- ^ Indices of the atoms for this layer. Those are the ones
                                     --   from the 'Map Int Atom' structure of a 'Molecule'. A check
                                     --   of sanity of those indices will depend on the methodology
                                     --   used (ONIOM vs- QM/MM).
  , _deeperLayer  :: Seq TheoryLayer -- ^ Deeper layers of the calculation. If the list is empty,
                                     --   this is the highest level (with respect to computational
                                     --   cost) layer. Being a 'Seq', this structure allows to
                                     --   define multi-centre ONIOM.
  , _charge       :: Int             -- ^ Charge of the (sub)system.
  , _mult         :: Int             -- ^ Multiplicity of the (sub)system.
  , _execution    :: Execution       -- ^ Information about the execution of the computational
                                     --   chemistry software, that is not relevant for system
                                     --   description.
  , _embedding   :: Embedding        -- ^ Defines the embedding type for the current layer.
  }
  deriving ( Eq, Show, Generic )

----------------------------------------------------------------------------------------------------
{-|
Available computational chemistry programs.
-}
data Program
  = Psi4
  | Nwchem
  deriving ( Eq, Show, Generic )

----------------------------------------------------------------------------------------------------
{-|
Available embedding methods for layer interaction in ONIOM.
-}
data Embedding
  = Mechanical                      -- ^ Mechanical embedding.
  | Electronic (Maybe (Seq Double)) -- ^ Electrostatic embedding. Scaling factors of charges in a
                                    --   given distance to a capped atom can be given.
  deriving ( Eq, Show, Generic )

----------------------------------------------------------------------------------------------------
{-|
Information about how to execute a program, independent of system description.
-}
data Execution = Execution
  { _nProcesses :: Int -- ^ Number of processes to launch (usually MPI).
  , _nThreads   :: Int -- ^ Number of threads per (MPI) process to launch.
  , _memory     :: Int -- ^ The memory in MB per process for the program.
  }
  deriving ( Eq, Show, Generic )

----------------------------------------------------------------------------------------------------
-- All Lenses and Prisms derivations for the InputFile.
makeLenses ''InputFile
deriveJSON spicyJOption ''InputFile

makePrisms ''Task
deriveJSON spicyJOption ''Task

makeLenses ''InputMolecule
deriveJSON spicyJOption ''InputMolecule

makeLenses ''TopoChanges
deriveJSON spicyJOption ''TopoChanges

makePrisms ''FileType
deriveJSON spicyJOption ''FileType

makePrisms ''Model
makeLenses ''Model
deriveJSON spicyJOption ''Model

makeLenses ''TheoryLayer
deriveJSON spicyJOption ''TheoryLayer

makePrisms ''Program
deriveJSON spicyJOption ''Program

makePrisms ''Embedding
deriveJSON spicyJOption ''Embedding

makeLenses ''Execution
deriveJSON spicyJOption ''Execution


{-
####################################################################################################
-}
{- $molecule
Molecules are defined with a lot of context and 'Molecule' this is the single most important type in
Spicy. The layout of the 'Molecule' data type keeps layered calculations, complex input formats and
fragmentation of molecules in mind and tries to apply to them all.

Regarding the hierarhy of a 'Molecule' in the context of ONIOM, the following conditions apply:

- The top layer of the recursion is the "real" system in ONIOM terminology.
- Stepping down the recursion more "model"-like ONIOM layers are reached.
-}
{-
====================================================================================================
-}
{- $moleculeStructure
This is the definition of a molecular system. The type is flexible enough to describe large systems
and hierarchical order of them as ONIOM layers, fragments, QM/MM regions and so on.

Furthermore, the 'Molecule's and 'Atom's will store the results of calculations but be agnostic of
where these information came from.
-}
{-|
All chemical elements. Have them very clear because force fields and pdb names may interfer and are
just arbitrary strings.
-}
data Element =
  H   |                                                                                                                                                                                     He  |
  Li  | Be                                                                                                                                                  | B   | C   | N   | O   | F   | Ne  |
  Na  | Mg                                                                                                                                                  | Al  | Si  | P   | S   | Cl  | Ar  |
  K   | Ca                                                                                      | Sc  | Ti  | V   | Cr  | Mn  | Fe  | Co  | Ni  | Cu  | Zn  | Ga  | Ge  | As  | Se  | Br  | Kr  |
  Rb  | Sr                                                                                      | Y   | Zr  | Nb  | Mo  | Tc  | Ru  | Rh  | Pd  | Ag  | Cd  | In  | Sn  | Sb  | Te  | I   | Xe  |
  Cs  | Ba  | La  | Ce  | Pr  | Nd  | Pm  | Sm  | Eu  | Gd  | Tb  | Dy  | Ho  | Er  | Tm  | Yb  | Lu  | Hf  | Ta  | W   | Re  | Os  | Ir  | Pt  | Au  | Hg  | Tl  | Pb  | Bi  | Po  | At  | Rn  |
  Fr  | Ra  | Ac  | Th  | Pa  | U   | Np  | Pu  | Am  | Cm  | Bk  | Cf  | Es  | Fm  | Md  | No  | Lr  | Rf  | Db  | Sg  | Bh  | Hs  | Mt  | Ds  | Rg  | Cn  | Uut | Fl  | Uup | Lv  | Uus | Uuo
  deriving ( Show, Eq, Read, Ord, Enum, Generic, NFData )

instance ToJSON Element where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Element

----------------------------------------------------------------------------------------------------
{-|
An atom label. They may come from pdb or force field parameter files or can be assigned by other
ways just to distinguish specific atoms.
-}
type AtomLabel = Text

----------------------------------------------------------------------------------------------------
{-|
These are labels for molecular mechanics software. The strings are basically arbitrary and depending
on the MM software used.
-}
data FFType
  = FFMol2 !Text
  | FFTXYZ !Int
  | FFPDB !Text
  | FFXYZ
  | FFBq
  deriving ( Show, Generic )

instance Eq FFType where
  FFMol2 _ == FFMol2 _ = True
  FFMol2 _ == _        = False
  FFTXYZ _ == FFTXYZ _ = True
  FFTXYZ _ == _        = False
  FFPDB  _ == FFPDB _  = True
  FFPDB  _ == _        = False
  FFXYZ    == FFXYZ    = True
  FFXYZ    == _        = False
  FFBq     == FFBq     = True
  FFBq     == _        = False

instance ToJSON FFType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FFType

----------------------------------------------------------------------------------------------------
{-|
An Atom in a 'Molecule'. Atoms are compared by their indices only and they must therefore be unique.
The coordinates of the 'Atom' are defined as 'Seq', as this is extremely easy to concatenate when
building a coordinate vector.
-}
data Atom = Atom
  { _atom_Element     :: !Element           -- ^ Chemical 'Element' of the atom.
  , _atom_Label       :: !AtomLabel         -- ^ Label, e.g. from a pdb, just for identification,
                                            --   can be empty.
  , _atom_IsPseudo    :: !Bool              -- ^ Boolean, telling if this is a pseudo atom,
                                            --   introduced because a bond was broken. Also known as
                                            --   link atom in ONIOM.
  , _atom_IsCapped    :: !Bool              -- ^ Boolean, telling if this is a high level atom,
                                            --   whose bond to a lower layer has been capped.
  , _atom_IsDummy     :: !Bool              -- ^ Whether the atom is a dummy atom, only providing
                                            --   multipole information.
  , _atom_FFType      :: !FFType            -- ^ Label depending on the MM software used,
                                            --   identifying topological atom.
  , _atom_Coordinates :: !(VectorS Double)  -- ^ Coordinates of the atom, cartesian in R³. Relies on
                                            --   the parser to fill with exactly 3 values.
  , _atom_Multipoles  :: !Multipoles        -- ^ Atom-centred multipole information after a
                                            --   calculation.
  }
  deriving ( Show, Eq, Generic )

instance ToJSON Atom where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Atom

----------------------------------------------------------------------------------------------------
{-|
The bond matrix is represented sparsely by a HashMap with an '(Int, Int)' tuple as the atom indices.
The order is @(Origin, Target)@.
-}
type BondMatrix = HashMap (Int, Int) Bool

----------------------------------------------------------------------------------------------------
{-|
Definition of a fragment. The fragments are strictly a subset of a given layer.
-}
data Fragment = Fragment
  { _fragment_Label :: !Text         -- ^ The name of a fragment. Doesn't need to be unique.
  , _fragment_Chain :: !(Maybe Char) -- ^ Meant for PDB and similiar molecules, where protein chains
                                     --   need to be distinguished.
  , _fragment_Atoms :: !IntSet       -- ^ The atoms and bonds of the fragment. Relative to the
                                     --   molecule layer which contains the fragment.
  } deriving ( Show, Eq, Generic )

instance ToJSON Fragment where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Fragment

----------------------------------------------------------------------------------------------------
{-|
A molecule, which might be the whole system, an ONIOM layer or a fragment of the system, each
containing possibly even higher layers for ONIOM or fragments. Stores all associated informations of
a layer.

For reference layers will be labeled \([0, \dots, n]\) by their recursion depth, with \(0\) being
the "real" system in ONIOM style calculations. Multiple fragments would be on the same recursion
depth but a different index in the '_molecule_SubMol' 'SparseArray'. Therefore, if \(0\) would be
the whole/real system, its fragments would be at recursion depth \(1\) and fragment \(f\) at
recursion depth \(1\) would be labeled \(1.f\).

Starting from a top level molecule, all atoms and bonds of the system are expected to be in the in
this top layer (except pseudoatoms of deeper layers). Therefore if atoms are in a deeper layers of
the recursion, their information is not used to describe a higher lying layer (which is lower with
respect to computational cost). Instead, all atoms of deeper layers (except pseudoatoms) must be
also replicated in a higher layer. While the atoms of deeper layers keep the same indices as in the
higher layers, it is acceptable, that an index, that was used for an atom in layer \(n\) is used for
a pseudoatom in layer \(n + m, m > 0\).

Pseudo atoms are specific to layers and may be passed down the recursion to deeper layers but not
passed up to higher layers. Therefore, while the counting of non-pseudoatoms must be consistent
through all layers, the pseudoatoms must be only consitent the recursion downwards but not
necessarily between fragments of the same layer. If a layer \(n\) contains a pseudoatom with index
\(p\), for example, and layer \(n + 1\) still contains this pseudoatom, it must still be given index
\(p\). If layer \(n + 1\) on the other hand side would be stripped of this pseudoatom, the index
\(p\) could be used for another pseudoatom.

The data structure makes it necessary to construct the 'Molecule' top down, not bottom up.
-}
data Molecule = Molecule
  { _molecule_Comment           :: !Text                      -- ^ Comment or description of a
                                                              --   molecule. This can be empty and is
                                                              --   not to be confused with the unique
                                                              --   identifier used in the 'Map Text'
                                                              --   structures.
  , _molecule_Atoms             :: !(IntMap Atom)             -- ^ An 'IntMap' of 'Atom's, 'Atom's
                                                              --   identified by their 'Int' index.
  , _molecule_Bonds             :: !BondMatrix                -- ^ An sparse representation of the
                                                              --   bond matrix. Is expected to be
                                                              --   defined bidirectorial. Only the
                                                              --   existing bonds would need to be
                                                              --   defined as 'True'. The rest will
                                                              --   be assumed to be 'False'.
  , _molecule_SubMol            :: !(IntMap Molecule)         -- ^ A Molecule might contain deeper
                                                              --   ONIOM layers.
  , _molecule_Fragment          :: !(IntMap Fragment)          -- ^ Fragments definition. They are
                                                              --   meant to be either empty, or
                                                              --   contain the whole system, usually
                                                              --   without bond cuts. The keys of
                                                              --   the 'IntMap' assign numbers to
                                                              --   the fragments, while the 'IntSet'
                                                              --   contains selections of atom
                                                              --   numbers (and indices of the bond
                                                              --   matrix, which belong to a
                                                              --   fragment). Fragments should not
                                                              --   contain deeper layers.
  , _molecule_EnergyDerivatives :: !EnergyDerivatives         -- ^ The potential energy and its
                                                              --   derivatives.
  , _molecule_CalcContext       :: !(Map CalcK CalcContext)   -- ^ Calculations to perform on
                                                              --    __this__ layer of the molecule.
                                                              --   The 'Text' values of the 'Map'
                                                              --   serve as unique identifiers for a
                                                              --   calculation on this molecule.
  }
  deriving ( Show, Eq, Generic )

instance ToJSON Molecule where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Molecule

----------------------------------------------------------------------------------------------------
{-|
Trajectories are simply 'Seq'uences of 'Molecule's.
-}
type Trajectory = Seq Molecule

----------------------------------------------------------------------------------------------------
{-|
A data structure to get a layer in the 'Molecule' contructor. Enables to find a specific fragment.
It works by giving number of lookups stepping down the recursion of the 'Molecule' structure along
the '_molecule_SubMol' structure. An empty sequency of indices is the top layer. Then each element
will be used as a key for the 'IntMap' structure in '_molecule_SubMol' structure, to step down to
the next layer.
-}
type MolID = Seq Int

----------------------------------------------------------------------------------------------------
{-|
Find a specific calculation for a specific layer of the molecule. The calculation is defined by the
'MolID' and the 'Text' key of the '_molecule_CalcContext' 'Map'.
-}
data CalcID = CalcID
  { _calcID_MolID   :: MolID -- ^ Molecule layer of interest.
  , _calcID_calcKey :: CalcK -- ^ The actual calculation.
  }
  deriving ( Eq, Show )

{-
====================================================================================================
-}
{- $moleculeCalculation
These types define the necessary context for calculations on a current layer of the molecule. They
are meant to be updated frequently by the driver routines, which implement the multilayer methods.

They are defined in the context of a wrapper call, which means that they expose a lot of computer
system specific information regarding execution of the program and not only molecular/hamiltonian
information.

Furthermore the definition of outputs obtainable from the wrappers are defined here.
-}
{-|
Representation of multipoles for expansion of the electrostatic potential.
-}
data Multipoles = Multipoles
  { _multipole_Monopole     :: !(Maybe Double)
  , _multipole_Dipole       :: !(Maybe (Array1S Double))
  , _multipole_Quadrupole   :: !(Maybe (Array2S Double))
  , _mutlipole_Octopole     :: !(Maybe (Array3S Double))
  , _mutlipole_Hexadecapole :: !(Maybe (Array4S Double))
  }
  deriving ( Eq, Show, Generic )

instance ToJSON Multipoles where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Multipoles

instance Default Multipoles where
  def = Multipoles { _multipole_Monopole     = Nothing
                   , _multipole_Dipole       = Nothing
                   , _multipole_Quadrupole   = Nothing
                   , _mutlipole_Octopole     = Nothing
                   , _mutlipole_Hexadecapole = Nothing
                   }

instance HumanShow Multipoles where
  hShow poles =
    let -- Monopole printing:
        monopole :: [Utf8Builder]
        monopole = ["Monopole =", fromMaybe "-" . fmap display . _multipole_Monopole $ poles]
        -- Dipole printing
        dipole :: [Utf8Builder]
        dipole =
            let arrayRepresentation = arrayToBuilder . getVectorS <$> _multipole_Dipole poles
            in  "Dipole =" : fromMaybe ["-"] arrayRepresentation
        -- Quadrupole printing
        quadrupole :: [Utf8Builder]
        quadrupole =
            let arrayRepresentation = arrayToBuilder . getMatrixS <$> _multipole_Quadrupole poles
            in  "Quadrupole =" : fromMaybe ["-"] arrayRepresentation
        -- Octopole printing
        octopole :: [Utf8Builder]
        octopole =
            let arrayRepresentation = arrayToBuilder . getArray3S <$> _mutlipole_Octopole poles
            in  "Octopole =" : fromMaybe ["-"] arrayRepresentation
        -- Hexadecapole printing
        hexadecapole :: [Utf8Builder]
        hexadecapole =
            let arrayRepresentation = arrayToBuilder . getArray4S <$> _mutlipole_Hexadecapole poles
            in  "Hexadecapole =" : fromMaybe ["-"] arrayRepresentation
    in  monopole <> dipole <> quadrupole <> octopole <> hexadecapole
    where arrayToBuilder array = fmap display . Text.lines . Text.pack . show $ array

----------------------------------------------------------------------------------------------------
{-|
Derivatives of the potential energy, that can be calculated for a 'Molecule' or molecular layer.
-}
data EnergyDerivatives = EnergyDerivatives
  { _energyDerivatives_Energy   :: !(Maybe Double)           -- ^ The potential energy.
  , _energyDerivatives_Gradient :: !(Maybe (VectorS Double)) -- ^ The nuclear gradient.
  , _energyDerivatives_Hessian  :: !(Maybe (MatrixS Double)) -- ^ The nuclear hessian.
  }
  deriving ( Eq, Show, Generic )

instance ToJSON EnergyDerivatives where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON EnergyDerivatives

instance Default EnergyDerivatives where
  def = EnergyDerivatives { _energyDerivatives_Energy   = Nothing
                          , _energyDerivatives_Gradient = Nothing
                          , _energyDerivatives_Hessian  = Nothing
                          }

instance HumanShow EnergyDerivatives where
  hShow enDeriv =
    let -- Print simply as a double.
      energyPrint :: [Utf8Builder]
      energyPrint = case _energyDerivatives_Energy enDeriv of
        Nothing -> []
        Just energy ->
          ["Energy / Hartree = ", display . format ("  " % (left 16 ' ' %. fixed 8)) $ energy]

      -- Print as a cartesian gradient in the order of atoms. Therefore groups of 3 values per line.
      gradientPrint :: [Utf8Builder]
      gradientPrint = case _energyDerivatives_Gradient enDeriv of
        Nothing -> []
        Just gradient ->
          let
            nAtoms = (Massiv.elemsCount . getVectorS $ gradient) `div` 3
            massiv3NMatrix :: Maybe (Matrix Massiv.S Double)
            massiv3NMatrix = Massiv.resizeM (Sz (nAtoms :. 3)) . getVectorS $ gradient
            massivTextMatrix =
              Massiv.map (display . format (left 16 ' ' %. fixed 8)) <$> massiv3NMatrix
            massivBuilderVector :: Maybe (Vector Massiv.D Utf8Builder)
            massivBuilderVector = Massiv.map ("  " <>) . Massiv.foldInner <$> massivTextMatrix
            gradientUtf8 = fromMaybe ["*Unexpected size of the gradient vector. Cannot print.*"]
                                     (Massiv.toList <$> massivBuilderVector)
          in
            "Gradient / Hartree Angstrom⁻¹ =" : gradientUtf8

      -- Print the hessian in blocks of 5 columns.
      hessianPrint :: [Utf8Builder]
      hessianPrint = case _energyDerivatives_Hessian enDeriv of
        Nothing -> []
        Just hessian ->
          let massivBuilderMatrix =
                  Massiv.computeAs Massiv.B
                    . Massiv.map (display . format (left 16 ' ' %. fixed 8))
                    . getMatrixS
                    $ hessian
              massivColumnGroups :: Seq (Matrix B Utf8Builder)
              massivColumnGroups = chunksOfNColumns 5 massivBuilderMatrix
              massivColumnsBuilder :: Seq (Vector D Utf8Builder)
              massivColumnsBuilder = Massiv.map ("  " <>) . Massiv.foldInner <$> massivColumnGroups
              hessianBuilder :: [Utf8Builder]
              hessianBuilder = List.concat . toList . fmap Massiv.toList $ massivColumnsBuilder
          in  "Hessian / Hartree Angstrom⁻² =" : hessianBuilder
    in
      energyPrint <> gradientPrint <> hessianPrint
   where
    chunksOfNColumns
      :: (Mutable r1 Ix2 e, Manifest r2 Ix2 e)
      => Int
      -> Massiv.Array r2 Ix2 e
      -> Seq (Massiv.Array r1 Ix2 e)
    chunksOfNColumns n matrix = Massiv.compute <$> go n (Massiv.toManifest matrix) Empty
     where
      go :: Int -> Matrix M a -> Seq (Matrix M a) -> Seq (Matrix M a)
      go n' restMatrix groupAcc = case Massiv.splitAtM (Dim 1) n' restMatrix of
        Nothing ->
          let (Sz (_ :. nColsRemaining)) = Massiv.size restMatrix
          in  if nColsRemaining == 0 then groupAcc else groupAcc |> restMatrix
        Just (thisChunk, restMinusThisChunk) -> go n' restMinusThisChunk (groupAcc |> thisChunk)

----------------------------------------------------------------------------------------------------
{-|
How efficient a task can be performed. Used for gradient calculations mainly.
-}
data NumericalEfficiency
  = Analytical -- ^ Analytical derivatives are available and will be used.
  | Numerical  -- ^ Finite displacements will be used for derivatives.
  deriving ( Eq, Show, Generic )

instance ToJSON NumericalEfficiency where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON NumericalEfficiency

----------------------------------------------------------------------------------------------------
{-|
A task the wrapper needs to perform.
-}
data WrapperTask
  = WTEnergy   -- ^ Single point energy calculation.
  | WTGradient -- ^ Gradient calculation.
  | WTHessian  -- ^ Hessian calculation.
  deriving ( Eq, Show, Generic )

instance ToJSON WrapperTask where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON WrapperTask

----------------------------------------------------------------------------------------------------
{-|
Context specific to a QM calculation.
-}
data QMContext = QMContext
  { _qmContext_Charge :: !Int -- ^ Charge of the (sub)system as transparent to the QM software.
  , _qmContext_Mult   :: !Int -- ^ Multiplicity of the (sub)system as transparent to the QM software.
  }
  deriving ( Show, Eq, Generic )

instance ToJSON QMContext where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON QMContext

----------------------------------------------------------------------------------------------------
{-|
Context specific to a MM calculation.
-}
data MMContext = MMContext
  deriving ( Show, Eq, Generic )

instance ToJSON MMContext where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MMContext

----------------------------------------------------------------------------------------------------
{-|
Information needed either by a QM or MM calculation.
-}
data QMMMSpec
  = QM !QMContext -- ^ Information only needed for QM calculations.
  | MM !MMContext -- ^ Information only needed for MM calculations.
  deriving ( Show, Eq, Generic )

instance ToJSON QMMMSpec where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON QMMMSpec

----------------------------------------------------------------------------------------------------
{-|
The context for a wrapper calculation. These are information used by the system call to the wrapper
and the Mustache template engine for the wrapper input file as well as some Spicy internals on
-}
data CalcInput = CalcInput
  { _calcInput_Task        :: !WrapperTask          -- ^ A 'Task' the wrapper needs to perform.
  , _calcInput_RestartFile :: !(Maybe JFilePathAbs) -- ^ Potentially a restart file (wavefunction),
                                                    --   which might me present.
  , _calcInput_Software    :: !Program              -- ^ The 'Program', which is being used as a
                                                    --   wrapped calculator.
  , _calcInput_PrefixName  :: !String               -- ^ Some prefix, which can be used to name
                                                    --   output files. Consider this a project name.
  , _calcInput_PermaDir    :: !JDirPathAbs          -- ^ Directory, where permanent output of the
                                                    --   calculation shall be stored. This is the
                                                    --   final path for a calculation and needs to
                                                    --   be updated between iterations.
  , _calcInput_ScratchDir  :: !JDirPathAbs          -- ^ Directory, with fast access for scratch
                                                    --   files of the software.
  , _calcInput_NProcs      :: !Int                  -- ^ Number of (MPI) processes to run in
                                                    --   parallel for a software.
  , _calcInput_NThreads    :: !Int                  -- ^ Number of threads per MPI process or OpenMP
                                                    --   threads.
  , _calcInput_Memory      :: !Int                  -- ^ Memory per process in MiB for the program.
  , _calcInput_QMMMSpec    :: !QMMMSpec             -- ^ Information specific to either a QM or MM
                                                    --   calculation.
  , _calcInput_Template    :: !Text                 -- ^ A Mustache template for the program.
  , _calcInput_Embedding   :: !Embedding            -- ^ The embedding type for this calculation
                                                    --   part. Might be ignored for Inherited
                                                    --   calculations in ONIOM (low calculation
                                                    --   level on model system).
  }
  deriving ( Eq, Show, Generic )

instance ToJSON CalcInput where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON CalcInput

instance HumanShow CalcInput where
  hShow calcInput =
    let identifiers :: [Utf8Builder]
        identifiers =
            formatIdentifier
              <$> [ "Software"
                  , "Task"
                  , "Restart file"
                  , "Prefix"
                  , "Permanent directory"
                  , "Scratch directory"
                  , "Processes"
                  , "Threads per process"
                  , "Memory (MiB)"
                  , "Charge"
                  , "Multiplicity"
                  ]
        values :: [Utf8Builder]
        values =
            [ displayShow . _calcInput_Software
              , task2Human . _calcInput_Task
              , fromMaybe "-" . fmap (path2Utf8 . getFilePathAbs) . _calcInput_RestartFile
              , displayShow . _calcInput_PrefixName
              , path2Utf8 . getDirPathAbs . _calcInput_PermaDir
              , path2Utf8 . getDirPathAbs . _calcInput_ScratchDir
              , displayShow . _calcInput_NProcs
              , displayShow . _calcInput_NThreads
              , displayShow . _calcInput_Memory
              , charge2Human . _calcInput_QMMMSpec
              , multiplicity2Human . _calcInput_QMMMSpec
              ]
              <*> pure calcInput
    in  display <$> RIO.zipWith (<>) identifiers values
   where
    -- Formatter for the identifiers.
    formatIdentifier :: Text -> Utf8Builder
    formatIdentifier indentifier = display . sformat ((right 20 ' ' %. stext) % ": ") $ indentifier
    -- Task to human readable form.
    task2Human :: WrapperTask -> Utf8Builder
    task2Human task' = case task' of
      WTEnergy   -> "Energy"
      WTGradient -> "Gradient"
      WTHessian  -> "Hessian"
    path2Utf8 :: (AbsRel ar, FileDir fd) => Path.Path ar fd -> Utf8Builder
    path2Utf8 path' = displayShow . Path.toString $ path'
    charge2Human :: QMMMSpec -> Utf8Builder
    charge2Human (MM _        ) = ""
    charge2Human (QM qmContext) = displayShow . _qmContext_Charge $ qmContext
    multiplicity2Human (MM _        ) = ""
    multiplicity2Human (QM qmContext) = displayShow . _qmContext_Mult $ qmContext

----------------------------------------------------------------------------------------------------
{-|
Results from a wrapper calculation on the given niveau. If for example multiple computations are
performed on the same 'Molecule' layer as in ONIOM (high and low level calculation on the model
system), this type allows to store the intermediate information before combining them and putting
them back to the '_molecule_EnergyDerivatives' field.
-}
data CalcOutput = CalcOutput
  { _calcOutput_EnergyDerivatives :: !EnergyDerivatives   -- ^ All information from the calculation,
                                                          --   that will be stored
  , _calcOutput_Multipoles        :: !(IntMap Multipoles) -- ^ Mapping 'Atom' indices to the
                                                          --   'Multipoles', that were calculated
                                                          --   by a wrapper calculation.
  }
  deriving ( Eq, Show, Generic )

instance ToJSON CalcOutput where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON CalcOutput

instance HumanShow CalcOutput where
  hShow calcOutput =
    let -- Normal printing of energy derivatives.
        eDerivativePrinting = hShow . _calcOutput_EnergyDerivatives $ calcOutput
        -- Just a summary for the multipoles, as they otherwise become very long for showing
        -- calculation outputs atomwise.
        mutipolePrinting =
            let
              atomsWithMonopoles     = filterForAtomsWithPoles _multipole_Monopole multipoleInfos
              atomsWithDipoles       = filterForAtomsWithPoles _multipole_Dipole multipoleInfos
              atomsWithQuadrupoles   = filterForAtomsWithPoles _multipole_Quadrupole multipoleInfos
              atomsWithOctopoles     = filterForAtomsWithPoles _mutlipole_Octopole multipoleInfos
              atomsWithHexadecapoles = filterForAtomsWithPoles _mutlipole_Hexadecapole multipoleInfos
            in
              [ "Multipoles:"
              , "  Monopoles    : " <> polePrinter atomsWithMonopoles
              , "  Dipoles      : " <> polePrinter atomsWithDipoles
              , "  Quadrupoles  : " <> polePrinter atomsWithQuadrupoles
              , "  Octopoles    : " <> polePrinter atomsWithOctopoles
              , "  Hexadecapoles: " <> polePrinter atomsWithHexadecapoles
              ]
    in  eDerivativePrinting <> mutipolePrinting
   where
    multipoleInfos = _calcOutput_Multipoles calcOutput
    allAtomInds    = IntMap.keys multipoleInfos
    filterForAtomsWithPoles accessF = IntMap.keys . IntMap.filter (isJust . accessF)
    polePrinter atomsWithPolesMap = case atomsWithPolesMap of
      []   -> "✘"
      inds -> if inds == allAtomInds then "✔" else "(Atoms " <> displayShow inds <> ")"

----------------------------------------------------------------------------------------------------
{-|
The context of a calculation is the combination of its input and output values.
-}
data CalcContext = CalcContext
  { _calcContext_Input  :: !CalcInput          -- ^ Necessary information to define a calculation.
  , _calcContext_Output :: !(Maybe CalcOutput) -- ^ Information produced by the calculation as
                                               --   defined by the 'CalcInput'.
  }
  deriving ( Eq, Show, Generic )

instance ToJSON CalcContext where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON CalcContext

----------------------------------------------------------------------------------------------------
{-|
Identifier for a calculation context supposed to be used as key in the '_molecule_CalcContext'
field.
-}
data CalcK
  = ONIOMKey ONIOMHierarchy -- ^ An ONIOM calculation. The 'ONIOMHierarchy' defines if this
                            --   calculation has been inherited or not.
  deriving ( Eq, Show, Ord, Generic, FromJSONKey, ToJSONKey )

instance ToJSON CalcK where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON CalcK

-- TODO (phillip|p=30|#Improvement) - Implement a HumanShow instance for printing of CalcK.

----------------------------------------------------------------------------------------------------
{-|
For all except the real system layer in an ONIOM calculation, two calculations on the molecular
structure can be defined. The actual one of interest for this system and the one inherited from the
layer above, which is required for subtraction.
-}
data ONIOMHierarchy
  = Original  -- ^ The calculation of actual interest.
  | Inherited -- ^ The calculation required to subtract this part from the layer above.
  deriving ( Eq, Show, Ord, Generic, FromJSONKey, ToJSONKey )

instance ToJSON ONIOMHierarchy where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ONIOMHierarchy

----------------------------------------------------------------------------------------------------
-- All lenses and prisms for molecular types.
makeLenses ''Multipoles
makeLenses ''EnergyDerivatives
makePrisms ''NumericalEfficiency
makePrisms ''WrapperTask
makeLenses ''QMContext
makeLenses ''MMContext
makePrisms ''QMMMSpec
makeLenses ''CalcInput
makeLenses ''CalcOutput
makeLenses ''CalcContext
makePrisms ''FFType
makeLenses ''Atom
makeLenses ''Fragment
makeLenses ''Molecule
makeLenses ''CalcID

{-
####################################################################################################
-}
{- $file
Definitions specific to some file formats.
-}
{-
====================================================================================================
-}
{- $fileFChk
Direct Mapping of an FChk into Haskell.
-}
{-|
Formatted Checkpoint files can contain an arbitrary amount of sections with scalar values. Those
scalar values are either 'Int', 'Double', 'Text' or 'Bool'.
-}
data ScalarVal
  = ScalarInt !Int
  | ScalarDouble !Double
  | ScalarText !Text
  | ScalarLogical !Bool
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Data types an array can hold.
-}
-- The text value is very strange. A normal text is split into chunks of 12 or 8 characters and
-- print without separator. Therefore it appears as a normal text but for Fortran it is actually an
-- array of text fragments. I don't care about the crazy Fortran internals and treat it as a single
-- text, which it actually is.
data ArrayVal
  = ArrayInt !(Vector Massiv.S Int)
  | ArrayDouble !(Vector Massiv.S Double)
  | ArrayText !Text
  | ArrayLogical !(Vector Massiv.S Bool)
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Possible types of calculation types from an FChk file.
-}
data CalcType
  = SP        -- ^ Single point
  | FOPT      -- ^ Full optimisation to minimum
  | POPT      -- ^ Partial optimisation to an minimum
  | FTS       -- ^ Full optimisation to a transition state
  | PTS       -- ^ Partial optimisation to a transition state
  | FSADDLE   -- ^ Full optimisation to a saddle point of higher order
  | PSADDLE   -- ^ Partial optimisation to a saddle point of higher order
  | FORCE     -- ^ Energy + gradient calculation
  | FREQ      -- ^ Frequency calculation (2nd derivative)
  | SCAN      -- ^ Potential energy surface scan
  | GUESS     -- ^ Generates molecular orbitals only, also orbital localisation
  | LST       -- ^ Linear synchronous transit calculation
  | STABILITY -- ^ SCF stability analysis
  | REARCHIVE -- ^ Generate archive information from checkpoint file
  | MSRESTART -- ^ Generate archive information from checkpoint file
  | MIXED     -- ^ Compound models such as G2, G3, ...
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
In the FChk format after 2 lines of header an arbitrary amount of contents will follow. These are
either scalar values or arrays of values. The arrays are always printed as vectors but might
actually refer to matrices. The meaning of the content blocks must be obtained by the label fields.
-}
data Content
  = Scalar !ScalarVal
  | Array !ArrayVal
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
A format fully representing the contents of an formatted checkpoint file version 3.
-}
data FChk = FChk
  { _title    :: !Text               -- ^ Short title from the header of the FChk.
  , _calcType :: !CalcType           -- ^ Calculation type from the header.
  , _basis    :: !Text               -- ^ Primary orbital basis from the FChk header.
  , _method   :: !Text               -- ^ Calculation method (such as CCSD, HF, MP2) from the header.
  , _blocks   :: !(Map Text Content) -- ^ Labeled of 'Content's.
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
-- All lenses and prisms for Fchk.
makePrisms ''ScalarVal
makePrisms ''ArrayVal
makePrisms ''CalcType
makePrisms ''Content
makeLenses ''FChk

{-
####################################################################################################
-}
{- $cmdArgs
Definition of Spicy's command line arguments.
-}
data SpicyArgs
  = Exec
      { verbose     :: Bool           -- ^ Wether to use verbose loggin or not (will include
                                      --   verbose format and debug messages.)
      , input       :: FilePath       -- ^ Path to the YAML input file for Spicy.
      , logfile     :: Maybe FilePath -- ^ Path to a log file for Spicy output.
      , startupconf :: Maybe FilePath -- ^ Alternative path to the pre-startup configuration file.
      }
  | Translate
      { verbose     :: Bool
      , input       :: FilePath
      , inputFormat :: FileType
      }
  deriving ( Data, Typeable, Show, Eq )

----------------------------------------------------------------------------------------------------
{-|
Command line arguments for Spicy for a normal run.
-}
exec :: SpicyArgs
exec =
  Exec
      { verbose     = False &= typ "BOOL" &= help "Print debug information and be very chatty."
      , input       = def &= typFile &= help "Path to the input file."
      , logfile     = Nothing &= typFile &= help "Path to the log file."
      , startupconf = Nothing &= typFile &= help
                        "Path to pre-startup scripts for computational chemistry software."
      }
    &= help "Execute spicy normally."

----------------------------------------------------------------------------------------------------
translate :: SpicyArgs
translate =
  Translate { verbose = False &= typ "BOOL" &= help "Print debug information and be very chatty."
            , input = def &= typFile &= help "Path to a molecule file to convert."
            , inputFormat = XYZ &= typ "File type" &= help "Fileformat"
            }
    &= help "Convert a molecule to a different format in Spicy style."

----------------------------------------------------------------------------------------------------
{-|
Setup of CmdArgs for Multimode runs.
-}
spicyModes :: SpicyArgs
spicyModes =
  modes [exec, translate]
    &= summary "Multilayer and compound methods for chemistry"
    &= CmdArgs.program "spicy"
  -- &= help "Compound and multilayer methods for chemistry."

{-
####################################################################################################
-}
-- Continuation of Classes. Order is relevant here, unfortunately, therefore the split.
{-
====================================================================================================
-}
{- $classDefinitionsHasRIO
This defines some of the import @Has*@ classes for the RIO style of app execution.
-}
{-|
Environment, which has access to the command line arguments of Spicy.
-}
class HasSpicyArgs env where
  cmdArgsL :: Lens' env SpicyArgs

----------------------------------------------------------------------------------------------------
{-|
Enviroments, which define a 'Molecule' with calculation context.
-}
class HasMolecule env where
  moleculeL :: Lens' env Molecule

----------------------------------------------------------------------------------------------------
{-|
Environments, which expose 'PreStartUpScript' to define how to load a program.
-}
class HasPreStartUpEnv env where
  preStartupEnvL :: Lens' env PreStartUpEnv

----------------------------------------------------------------------------------------------------
{-|
Environment which provides a filepath to a log file for the Spicy execution.
-}
class HasLogFile env where
  logFileL :: Lens' env Path.AbsFile

----------------------------------------------------------------------------------------------------
{-|
Environment which provides a 'InputFile' structure.
-}
class HasInputFile env where
  inputFileL :: Lens' env InputFile

{-
####################################################################################################
-}
{- $appEnv
This is the definition of the global read-only environment of the Spicy execution. The initial
structure must be generated from command line arguments and the input file ('_sCalculation'). The
state is then propagated by the drivers for calculations and molecule handling.
-}
{-|
Definition of the current 'State' in the execution of Spicy.
-}
data SpicyEnv = SpicyEnv
  { _sLogFile     :: !Path.AbsFile     -- ^ A log file for the spicy execution.
  , _sMolecule    :: !MoleculeEnv      -- ^ The current state of the molecule with all information
                                       --   up to date. This also includes the 'Seq' of
                                       --   calculations to perform on each layer.
  , _sCalculation :: !InputFile        -- ^ This is the input file and contains the definition of
                                       --   which kind of calculation to do. This should be set in
                                       --   the beginning and never change.
  , _sPreStartUp  :: !PreStartUpEnv    -- ^ Configuration files with Maps of environment variables
                                       --   to set before launching a program.
  , _sLogFunction :: !LogFunc          -- ^ Logging function for RIO.
  , _sProcContext :: !ProcessContext   -- ^ Context for external processes. Must be updated for each
                                       --   wrapper call.
  }

----------------------------------------------------------------------------------------------------
{-|
Possible representations of a molecule. A molecule will always be present as its native internal
representation via the 'Molecule' type and optionally also as different representations, which might
be incomplete. This represents always the full system (not just sublayers), contrary to the
'Molecule' in the 'WrapperInput', where also only fragments might be present.
-}
data MoleculeEnv = MoleculeEnv
  { _meNative :: !Molecule -- ^ Default representation of a molecule, which must always be given.
  }
  deriving ( Eq, Show )

----------------------------------------------------------------------------------------------------
{-|
Configuration for external programs. They should be read from some kind of configuration file. The
definition needs to allow different start up scripts for different programs, so that many different
programs with different MPIs for example can be used.
-}
data PreStartUpEnv = PreStartUpEnv
  { _psEnvPsi4   :: !(Maybe (Map Text Text)) -- ^ Startup script, that brings @psi4@ executable and its
                                             --   dependencies into the @$PATH@.
  , _psEnvNwchem :: !(Maybe (Map Text Text)) -- ^ Startup script, that brings @nwchem@, @mpiexec@ and
                                             --   dependencies into @PATH@.
  }
  deriving ( Eq, Show, Generic )

instance ToJSON PreStartUpEnv where
  toJSON conf =
    object ["psi4" .= (toJSON . _psEnvPsi4 $ conf), "nwchem" .= (toJSON . _psEnvNwchem $ conf)]

instance FromJSON PreStartUpEnv where
  parseJSON = withObject "PreStartUpEnv" $ \conf -> do
    psi4   <- conf .: "psi4"
    nwchem <- conf .: "nwchem"
    return PreStartUpEnv { _psEnvPsi4 = psi4, _psEnvNwchem = nwchem }

----------------------------------------------------------------------------------------------------
makeLenses ''SpicyEnv
makeLenses ''MoleculeEnv
makeLenses ''PreStartUpEnv

----------------------------------------------------------------------------------------------------
-- SpicyEnv
instance HasMolecule SpicyEnv where
  moleculeL = sMolecule . meNative

instance HasLogFunc SpicyEnv where
  logFuncL = sLogFunction

instance HasLogFile SpicyEnv where
  logFileL = sLogFile

instance HasInputFile SpicyEnv where
  inputFileL = sCalculation

instance HasPreStartUpEnv SpicyEnv where
  preStartupEnvL = sPreStartUp

instance HasProcessContext SpicyEnv where
  processContextL = sProcContext

----------------------------------------------------------------------------------------------------
-- MoleculeEnv
instance HasMolecule MoleculeEnv where
  moleculeL = meNative

{-
====================================================================================================
-}
{- $appEnvInitiator
-}
{-|
This is a simple environment for the early start up phases of Spicy, which provide custom logging
functionality and access to the inputs from the command line.
-}
data InitEnv = InitEnv
  { _logFunction :: LogFunc   -- ^ Logging function for RIO.
  , _spicyArgs   :: SpicyArgs -- ^ Command line arguments from CmdArgs to Spic.
  }

----------------------------------------------------------------------------------------------------
makeLenses ''InitEnv

----------------------------------------------------------------------------------------------------
-- InitEnv
instance HasLogFunc InitEnv where
  logFuncL = logFunction

instance HasSpicyArgs InitEnv where
  cmdArgsL = spicyArgs

{-
====================================================================================================
-}
{- $appEnvLogger
-}
{-|
Custom version of RIO's 'LogOptions', that are too deeply hidden to use them.
-}
data MyLogOptions = MyLogOptions
  { _logMinLevel :: !(IO LogLevel)                -- ^ Minimal log level to still print.
  , _logVerbose  :: !(IO Bool)                    -- ^ Use the verbose format for output.
  , _logUseColor :: !(IO Bool)                    -- ^ Use coloured output for the messages.
  , _logUseLoc   :: !Bool                         -- ^ Wether to use location information of the
                                                  --   message.
  , _logFormat   :: !(Utf8Builder -> Utf8Builder) -- ^ Formatter for the string to be printed.
  , _logHandle   :: Handle                        -- ^ The output to which direct this.
  , _logUseTime  :: Bool                          -- ^ Print a time stamp to the log messages?
  }

instance Default MyLogOptions where
  def = MyLogOptions { _logMinLevel = return LevelInfo
                     , _logVerbose  = return False
                     , _logUseColor = return True
                     , _logUseLoc   = False
                     , _logFormat   = id
                     , _logHandle   = stdout
                     , _logUseTime  = False
                     }

makeLenses ''MyLogOptions
