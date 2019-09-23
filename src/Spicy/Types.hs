{-|
Module      : Spicy.Types
Description : Data types used in the Spicy program.
Copyright   : Phillip Seeber, 2019
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

Spicy.Types contains the definition of all classes and data types, that are used in Spicy. Mainly it
takes care of the description of molecules (structure, topology, potential energy surface, ...).
-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Spicy.Types
( MolLogicException(..)
, DataStructureException(..)
, ParserException(..)
, Strat(..)
, AccVector(..)
, AccMatrix(..)
, Element(..)
, AtomLabel
, FFType(..)
, Atom(..)
, atom_Element
, atom_Label
, atom_IsPseudo
, atom_FFType
, atom_PCharge
, atom_Coordinates
, Molecule(..)
, molecule_Label
, molecule_Atoms
, molecule_Bonds
, molecule_SubMol
, molecule_Energy
, molecule_Gradient
, molecule_Hessian
, Trajectory
) where
import           Control.DeepSeq
import           Control.Exception.Safe
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.Array.Accelerate     as A
import qualified Data.ByteString.Lazy.UTF8 as BL
import           Data.IntMap.Lazy          (IntMap)
import           Data.IntSet               (IntSet)
import           Data.Sequence             (Seq)
import           Data.Text.Lazy            (Text)
import           GHC.Generics              (Generic)
import           Lens.Micro.Platform       hiding ((.=))
import           Prelude                   hiding (cycle, foldl1, foldr1, head,
                                            init, last, maximum, minimum, tail,
                                            take, takeWhile, (!!))


{-|
Have a class for the printing of the calculation niveau, which allows to produce a very readable
output.
-}
class NiceShow a where
  niceShow    :: a -> String -- ^ Printing the isolated object
  niceComplex :: a -> String -- ^ Printing the object in the complex form, where everything is meant
                             --   to be printed at once as overview

{-|
Exception type for operations on 'Molecule's, which lead to a logical error. This can be caused
because some Spicy assumptions are not met for example.
-}
data MolLogicException = MolLogicException
  { mlExcFunctionName :: String
  , mlExcDescription  :: String
  }
instance Show MolLogicException where
  show (MolLogicException f e) = "MoleculeLogicException in function \"" ++ f ++ "\":" ++ e
instance Exception MolLogicException

{-|
Exception type for operations on data structures, which are not meeting necessary criteria for the
operation to perform.
-}
data DataStructureException = DataStructureException
  { dsExcFunctionName :: String
  , dsExcDescription  :: String
  }
instance Show DataStructureException where
  show (DataStructureException f e) = "DataStructureException in function \"" ++ f ++ "\":" ++ e
instance Exception DataStructureException

{-|
Exception type for textual or binary data, that could not be parsed.
-}
data ParserException = ParserException String
instance Show ParserException where
  show (ParserException e) = "ParserException in parser: \"" ++ e ++ "\""
instance Exception ParserException

{-|
Use serial or parallel processing for large data structures. This helps deciding on a per use base,
if to evaluate in serial or parallel, to avoid nested parallelism. Every method employing parallel
operations by Control.Parallel.Strategies should provide this switch in Spicy.
-}
data Strat = Serial | Parallel deriving Eq

{-|
'newtype' wrapper to Accelerate's 'A.Vector's.
-}
newtype AccVector a = AccVector { getAccVector :: A.Vector a } deriving (Generic, Show, Eq)
instance (ToJSON a, A.Elt a) => ToJSON (AccVector a) where
  toJSON vec =
    let plainVec        = getAccVector vec
        (A.Z A.:. xDim) = A.arrayShape plainVec
        elements        = A.toList plainVec
    in  object
          [ "shape"    .= xDim
          , "elements" .= elements
          ]
instance (FromJSON a, A.Elt a) => FromJSON (AccVector a) where
  parseJSON = withObject "AccVector" $ \vec -> do
    xDim     <- vec .: "shape"
    elements <- vec .: "elements"
    return . AccVector $ A.fromList (A.Z A.:. xDim) elements

{-|
'newtype' wrapper to Accelerate's 'A.Matrix's.
-}
newtype AccMatrix a = AccMatrix { getAccMatrix :: A.Matrix a } deriving (Generic, Show, Eq)
instance (ToJSON a, A.Elt a) => ToJSON (AccMatrix a) where
  toJSON mat =
    let plainMat                  = getAccMatrix mat
        (A.Z A.:. xDim A.:. yDim) = A.arrayShape plainMat
        elements                  = A.toList plainMat
    in  object
          [ "shape"    .= (xDim, yDim)
          , "elements" .= elements
          ]
instance (FromJSON a, A.Elt a) => FromJSON (AccMatrix a) where
  parseJSON = withObject "AccVector" $ \mat -> do
    (xDim, yDim) <- mat .: "shape"
    elements     <- mat .: "elements"
    return . AccMatrix $ A.fromList (A.Z A.:. xDim A.:. yDim) elements


----------------------------------------------------------------------------------------------------
{- $moleculeTypes
Types to describe a molecule from a computational geometry point of view. The representation of a
molecule aims to be as descriptive as unambigous as possible and capture all quantities relevant for
molecular dynamics and geometry optimisations (energies, gradients, hessians, force fiekd types,
coordinates, connectivities). Electronic properties, such as dipoles, orbitals, the wavefunction and
so on, will be ignored, as Spicy is not aiming to be an analysis program, but a interface to create
multilayer calculations. Analysis of the results is up to postprocessing.
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
  deriving (Show, Eq, Read, Ord, Enum, Generic, NFData)
instance ToJSON Element where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Element

{-|
An atom label. They may come from pdb or force field parameter files or can be assigned by other
ways just to distinguish specific atoms.
-}
type AtomLabel = Text

{-|
These are labels for molecular mechanics software. The strings are basically arbitrary and depending
on the MM software used.
-}
data FFType =
    Mol2 Text
  | TXYZ Int
  | PDB Text
  | XYZ
  deriving (Generic)
makeLenses ''FFType
instance Eq FFType where
  Mol2 _ == Mol2 _ = True
  Mol2 _ == _      = False
  TXYZ _ == TXYZ _ = True
  TXYZ _ == _      = False
  PDB _  == PDB _  = True
  PDB _  == _      = False
  XYZ    == XYZ    = True
  XYZ    == _      = False
instance ToJSON FFType where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FFType

{-|
An Atom in a 'Molecule'. Atoms are compared by their indices only and they must therefore be unique.
The coordinates of the 'Atom' are defined as 'Seq', as this is extremely easy to concatenate when
building a coordinate vector.
-}
data Atom = Atom
  { _atom_Element     :: Element      -- ^ Chemical 'Element' of the atom.
  , _atom_Label       :: AtomLabel    -- ^ Label, e.g. from a pdb, just for identification, can
                                      --   be empty.
  , _atom_IsPseudo    :: Bool         -- ^ Boolean, telling if this is a pseudo atom, introduced
                                      --   because a bond was broken.
  , _atom_FFType      :: FFType       -- ^ Label depending on the MM software used, identifying
                                      --   topological atom.
  , _atom_PCharge     :: Maybe Double -- ^ Possibly a partial charge.
  , _atom_Coordinates :: Seq Double   -- ^ Coordinates of the atom, cartesian in R³. Relies on
                                      --   the parser to fill with exactly 3 values.
  } deriving (Eq, Generic)
makeLenses ''Atom
instance Show Atom where
  show = BL.toString . encodePretty
instance ToJSON Atom where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Atom

{-|
A molecule, which might be the whole system, an ONIOM layer or a fragment of the system, each
containing possibly even higher layers for ONIOM or fragments. Stores all associated informations of
a layer.

Starting from a top level molecule, all atoms and bonds of the system are expected to be in the in
this top layer (except pseudoatoms of deeper layers). Therefore if atoms are in a deeper layers of
the recursion, their information is not used to describe a higher layer. Instead, all atoms of
deeper layers (except pseudoatoms) must be also replicated in a higher layer.
-}
data Molecule = Molecule
  { _molecule_Label    :: Text                     -- ^ Comment or identifier of a molecule. Can be
                                                   --   empty.
  , _molecule_Atoms    :: IntMap Atom              -- ^ An 'IntMap' of 'Atom's, 'Atom's identified
                                                   --   by their 'Int' index.
  , _molecule_Bonds    :: IntMap IntSet            -- ^ An IntMap, mapping the index of an 'Atom' in
                                                   --   the 'Molecule' to the indices of all
                                                   --   'Atom's, to which it binds.
  , _molecule_SubMol   :: Seq Molecule             -- ^ A Molecule might contain other molecules.
                                                   --   These might be fragments or higher level
                                                   --   ONIOM layers.
  , _molecule_Energy   :: Maybe Double             -- ^ An energy, that might have been calculated.
  , _molecule_Gradient :: Maybe (AccVector Double) -- ^ A gradient, that might have been calculated.
  , _molecule_Hessian  :: Maybe (AccMatrix Double) -- ^ A hessian, that might have been calculated.
  } deriving (Eq, Generic)
makeLenses ''Molecule
instance Show Molecule where
  show = BL.toString . encodePretty
instance ToJSON Molecule where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Molecule

{-|
Trajectories are simply 'Seq'uences of 'Molecule's.
-}
type Trajectory = Seq Molecule


----------------------------------------------------------------------------------------------------
{- $compChemTypes
Data types to describe the calculation niveau. They are hierarchically organised and try to cover
everything, so that Spicy is fully aware of the calculation niveau. This has the advantage, that
users basically do not need to know the input of a specific program to perform a Spicy calculation.
-}
{-
{-|
Define commonon tasks for quantum chemistry software.
-}
data Task =
    Energy
  | Gradient
  | Hessian
  | PartialCharge
  deriving (Show, Eq)
instance NiceShow Task where
  niceShow    = show
  niceComplex = show

{-|
Software packages, that have been interfaced by Spicy.
-}
data Software =
    ORCA
  | Psi4
  | NWChem
  | CP2K
  | GAMMES_US
  | Tinker
  | MRCC
  deriving (Show, Eq)
instance NiceShow Software where
  niceShow ORCA      = "ORCA"
  niceShow Psi4      = "Psi4"
  niceShow NWChem    = "NWChem"
  niceShow CP2K      = "CP2K"
  niceShow GAMMES_US = "GAMESS US"
  niceShow Tinker    = "Tinker"
  niceShow MRCC      = "MRCC"
  niceComplex        = niceShow

{-|
Definition of basis sets. Prefer to have a prebuilt list, but allow for the option to use one, that
is not known here by the program specific (!) keyword ('CustomSimple') or by giving the program
specific (!) inline notation of the basis ('CustomComplex').
-}
data Basis =
  -- Ahlrich
    Def2_mSVP
  | Def2_mTZVP
  | Def2_SV_P
  | Def2_SVP
  | Def2_TZVP_f
  | Def2_TZVP
  | Def2_TZVPP
  | Def2_QZVP
  | Def2_QZVPP
  | Def2_SVPD
  | Def2_TZVPD
  | Def2_TZVPPD
  | Def2_QZVPD
  | Def2_QZVPPD
  -- Dunning
  | Cc_pVDZ
  | Cc_pVTZ
  | Cc_pVQZ
  | Cc_pV5Z
  | Cc_pV6Z
  | Aug_cc_pVDZ
  | Aug_cc_pVTZ
  | Aug_cc_pVQZ
  | Aug_cc_pV5Z
  | Aug_cc_pV6Z
  -- Custom basis set
  | CustomSimple String
  | CustomComplex String
  deriving (Show, Eq)
instance NiceShow Basis where
  niceShow Def2_mSVP         = "def2-mSVP"
  niceShow Def2_mTZVP        = "def2-mTZVP"
  niceShow Def2_SV_P         = "def2-SV(P)"
  niceShow Def2_SVP          = "def2-SVP"
  niceShow Def2_TZVP_f       = "def-TZVP(-f)"
  niceShow Def2_TZVP         = "def2-TZVP"
  niceShow Def2_TZVPP        = "def2-TZVPP"
  niceShow Def2_QZVP         = "def2-QZVP"
  niceShow Def2_QZVPP        = "def2-QZVPP"
  niceShow Def2_SVPD         = "def2-SVPD"
  niceShow Def2_TZVPD        = "def2-TZVPD"
  niceShow Def2_TZVPPD       = "def2-TZVPPD"
  niceShow Def2_QZVPD        = "def2-QZVPD"
  niceShow Def2_QZVPPD       = "def2-QZVPPD"
  niceShow Cc_pVDZ           = "cc-pVDZ"
  niceShow Cc_pVTZ           = "cc-pVTZ"
  niceShow Cc_pVQZ           = "cc-pVQZ"
  niceShow Cc_pV5Z           = "cc-pV5Z"
  niceShow Cc_pV6Z           = "cc-pV6Z"
  niceShow Aug_cc_pVDZ       = "aug-cc-pVDZ"
  niceShow Aug_cc_pVTZ       = "aug-cc-pVTZ"
  niceShow Aug_cc_pVQZ       = "aug-cc-pVQZ"
  niceShow Aug_cc_pV5Z       = "aug-cc-pV5Z"
  niceShow Aug_cc_pV6Z       = "aug-cc-pV6Z"
  niceShow (CustomSimple a)  = a
  niceShow (CustomComplex a) = a
  niceComplex                = niceShow

{-|
Charge of the system in the calculation.
-}
type Charge       = Int

{-|
Multiplicity of the system in the calculation.
-}
type Multiplicity = Int

{-|
Type of the referece wavefunction.
-}
data RefWF =
    Restricted
  | Unrestricted
  | RestrictedOpen
  deriving (Show, Eq)
instance NiceShow RefWF where
  niceShow Restricted     = "restricted"
  niceShow Unrestricted   = "unrestricted"
  niceShow RestrictedOpen = "restricted open shell"
  niceComplex Restricted     = "r"
  niceComplex Unrestricted   = "u"
  niceComplex RestrictedOpen = "o"

{-|
Method to calculate derivatives of the energy with respect to coordinates (gradients and hessians).
-}
data DType =
    Analytical
  | Numerical
  | NotAvail
  deriving (Show, Eq)
instance NiceShow DType where
  niceShow Analytical = "analytical"
  niceShow Numerical  = "numerical"
  niceShow NotAvail   = "not available"
  niceComplex Analytical = "a"
  niceComplex Numerical  = "n"
  niceComplex NotAvail   = "x"

{-|
Determines, how the calculation of derivatives can be performed. The records are lists, as it is not
assumed, that e.g. analytical gradients also imply numerical gradients being available.
-}
data Efficiency = Efficiency
  { _has_Gradient          :: [DType]
  , _has_SolventGradient   :: [DType]
  , _has_Hessian           :: [DType]
  , _has_SolventHessian    :: [DType]
  , _has_higherDerivatives :: [DType]
  } deriving (Show, Eq)
makeLenses ''Efficiency
instance NiceShow Efficiency where
  niceShow a =
    "       Gradient        |        Hessian        |     Higher Deriv.     " ++ "\n" ++
    "  GasPhase |  Solvent  |  GasPhase |  Solvent  |                       " ++ "\n" ++
    "-----------+-----------+-----------+-----------+-----------------------" ++ "\n" ++
    printf "  %7s  |  %7s  |  %7s  |  %7s  |  %7s  \n"
      (concatMap niceComplex $ a ^. has_Gradient)
      (concatMap niceComplex $ a ^. has_SolventGradient)
      (concatMap niceComplex $ a ^. has_Hessian)
      (concatMap niceComplex $ a ^. has_SolventHessian)
      (concatMap niceComplex $ a ^. has_higherDerivatives)
  niceComplex a =
    "G-" ++
    (concatMap niceComplex $ a ^. has_Gradient) ++
    "(" ++
    (concatMap niceComplex $ a ^. has_SolventGradient) ++
    ")  " ++
    "H-" ++
    (concatMap niceComplex $ a ^. has_Hessian) ++
    "(" ++
    (concatMap niceComplex $ a ^. has_SolventHessian) ++
    ")  " ++
    "\n"

{-|
Available hamiltonians for semiempirical methods.
-}
data SE_Method =
    SE_XTB1
  | SE_XTB2
  | SE_OM1
  | SE_OM2
  | SE_OM3
  | SE_PM3
  | SE_PM6
  | SE_PM7
  | SE_AM1
  | SE_RM1
  deriving (Eq)
instance Show SE_Method where
  show SE_XTB1 = "GFN-XTB - version 1"
  show SE_XTB2 = "GFN-XTB - version 2"
  show SE_OM1  = "OM(1)"
  show SE_OM2  = "OM(2)"
  show SE_OM3  = "OM(3)"
  show SE_PM3  = "PM3"
  show SE_PM6  = "PM6"
  show SE_PM7  = "PM7"
  show SE_AM1  = "AM1"
  show SE_RM1  = "RM1"
instance NiceShow SE_Method where
  niceShow    = show
  niceComplex = show


-- | Hartree Fock
-- |   -> Approximations
data HF_Approx =
    HF_None
  | HF_RIJK
  | HF_RIJONX
  | HF_RIJCOSX
  deriving (Eq)
instance Show HF_Approx where
  show HF_None    = "None"
  show HF_RIJK    = "RI-JK"
  show HF_RIJONX  = "RI-JONX"
  show HF_RIJCOSX = "RI-JCOSX"

{-|
Available density functionals in density functional theory calculations.
-}
data DFT_Functional =
  -- LDA
    DFT_PWLDA
  | DFT_VWN3
  | DFT_VWN5
  -- GGA
  | DFT_BLYP
  | DFT_OLYP
  | DFT_GLYP
  | DFT_XLYP
  | DFT_PW91
  | DFT_MPWPW
  | DFT_MPWLYP
  | DFT_PBE
  | DFT_RPBE
  | DFT_REVPBE
  | DFT_PWP
  -- metaGGA
  | DFT_M06_L
  | DFT_M11_L
  | DFT_TPSS
  -- Hybrid
  | DFT_B1LYP
  | DFT_B3LYP
  | DFT_O3LYP
  | DFT_X3LYP
  | DFT_BP86
  | DFT_PBE0
  | DFT_BHANDHLYP
  -- metaHybrid
  | DFT_M06
  | DFT_M06_2X
  | DFT_TPSS0
  | DFT_TPSSh
  -- range separated Hybrid
  | WB97
  | WB97X
  | WB97X_D3
  | CAM_B3LYP
  | LV_BLYP
  -- doubleHybrid
  | DFT_B2PLYP
  | DFT_B2PLYP_RI
  | DFT_B2PLYP_D
  | DFT_B2PLYP_D3
  | DFT_MPW2PLYP
  | DFT_MPW2PLYP_D
  | DFT_B2GP_PLYP
  | DFT_B2K_PLYP
  | DFT_B2T_PLYP
  | DFT_PWPB95
  | DFT_PBPB95_RI
  deriving (Eq, Show)

{-|
Available approximations to DFT calculations.
-}
data DFT_Approx =
    DFT_None     -- ^ Conventional DFT without integral approximations
  | DFT_RIJ      -- ^ Density fitting of the Coulomb integrals.
  | DFT_RIJK     -- ^ Density fitting of the Coulomb and exchange integrals.
  | DFT_RIJCOSX  -- ^ ORCA's methods with "normal" fitting of Coulomb integrals and chain of spheres
                 --   fitting for exchange.
  | DFT_RIJONX   -- ^ ORCA's methods with "normal" fitting of Coulomb integrals and alternative
                 --   fitting of exchange integrals
  deriving (Eq)
instance Show DFT_Approx where
  show DFT_None    = "none"
  show DFT_RIJ     = "RI-J"
  show DFT_RIJK    = "RI-JK"
  show DFT_RIJCOSX = "RI-JCOSX"
  show DFT_RIJONX  = "RI-JONX"

{-|
Order of the pertubation theory expansion in Møller-Plesset theory calculations.
-}
data MP_Order =
    MP_N2
  | MP_N2_5
  | MP_N3
  | MP_N4
  | MP_N5
  deriving (Eq)
instance Show MP_Order where
  show MP_N2   = "2"
  show MP_N2_5 = "2.5"
  show MP_N3   = "3"
  show MP_N4   = "4"
  show MP_N5   = "5"

{-|
The type of the Møller-Plesset calculation.
-}
data MP_Flavour =
    MP_Conv -- ^ Canonical/Conventional Møller-Plesset theory.
  | MP_OO   -- ^ Orbital optimised Møller-Plesset methods.
  | MP_F12  -- ^ Explicitly correlated \( F_{12} \) methods.
  deriving (Eq)
instance Show MP_Flavour where
  show MP_Conv = "conventional"
  show MP_OO   = "orbital optimised"
  show MP_F12  = "explicitly correlated"

{-|
Integral approximations in Møller-Plesset calculations.
-}
data MP_Approx =
    MP_None  -- ^ No approximations to Møller-Plesset integrals.
  | MP_RI    -- ^ Use RI-Møller-Plesset methods (will propably need a RI/C basis).
  | MP_DLPNO -- ^ Domain-based local pair natural orbital approximations applied to Møller-Plesset
             --   calculations.
  deriving (Eq)
instance Show MP_Approx where
  show MP_None  = "none"
  show MP_RI    = "RI"
  show MP_DLPNO = "DLPNO"

{-|
Coupled cluster truncation level of the calculation.
-}
data CC_Order =
    CC_D    -- ^ CCD
  | CC_SD   -- ^ CCSD
  | CC_SDT  -- ^ CCSDT
  | CC_SDTQ -- ^ CCSDTQ
  | CC_SDt  -- ^ CCSD(T)
  | CC_SDtq -- ^ CCSD(TQ)
  deriving (Eq)
instance Show CC_Order where
  show CC_D    = "CCD"
  show CC_SD   = "CCSD"
  show CC_SDT  = "CCSDT"
  show CC_SDTQ = "CCSDTQ"
  show CC_SDt  = "CCSD(T)"
  show CC_SDtq = "CCSD(TQ)"

{-|
Type of the coupled cluster calculation.
-}
data CC_Flavour =
    CC_Conv -- ^ Convenctional coupled cluster calculation.
  | CC_OO   -- ^ Orbital optimised coupled cluster calculation.
  | CC_LR   -- ^ Locally renormalised coupled cluster calculation.
  | CC_CR   -- ^ Completely renormalised coupled cluster calculation.
  | CC_F12  -- ^ Explicitly correlated \( F_{12} \) coupled cluster calculation.
  deriving (Eq)
instance Show CC_Flavour where
  show CC_Conv = "conventional"
  show CC_OO   = "orbital optimised"
  show CC_LR   = "locally renormalised"
  show CC_CR   = "completely renormalised"
  show CC_F12  = "explicitly correlated"

{-|
Approximations applied for coupled cluster calculations.
-}
data CC_Approx =
    CC_None  -- ^ No approximations applied.
  | CC_RI    -- ^ Density fitted coupled cluster calculation.
  | CC_DLPNO -- ^ Domain based local natural pair orbitals approximation.
  deriving (Eq)
instance Show CC_Approx where
  show CC_None  = "none"
  show CC_RI    = "RI"
  show CC_DLPNO = "DLPNO"

{-|
CASSCF active space selection.
-}
data CAS_Space = CAS_Space
  { _cas_Space_nElec :: Int   -- ^ Number of electrons in the active space.
  , _cas_Space_Orbs  :: [Int] -- ^ List of orbitals in the active space.
  } deriving (Eq)
makeLenses ''CAS_Space
instance Show CAS_Space where
  show a =
    printf "%10s | %10s\n" "electrons" "orbitals" ++
    printf "%10d | %10d %s"   (a ^. cas_Space_nElec) (length $ a ^. cas_Space_Orbs) (show $ a ^. cas_Space_Orbs)

{-|
In state averaging CASSCF the number of roots to calculate.
-}
type CAS_Roots = Int

{-|
The weights of the individual roots in a state averaging CASSCF calculation. Length of this list
should equal 'CAS_Roots'.
-}
type CAS_Weights = [Double]

{-|
Approximations applied to CASSCF calculations.
-}
data CAS_Approx =
    CAS_Conv    -- ^ No approximations in CASSCF calculation.
  | CAS_RIJK    -- ^ Density fitting of both exchange and Coulomb integrals.
  | CAS_RIJONX  -- ^ Density fitting of Coulomb integrals and ORCA's version of exchange integral
                --   fitting.
  | CAS_RIJCOSX -- ^ Density fitting of Coulomb integrals and chain of spheres fitting of exchange
                --   integrals.
  | CAS_DMRG    -- ^ Density Matrix Renormalisation Group approximation of CASSCF. Makes active
                --   space non-invariant againts active-active orbital rotations.
  deriving (Show, Eq)

{-|
Description of a semiempirical calculation capabilities.
-}
data QC_SE = QC_SE
  { _se_method     :: SE_Method
  , _se_ref        :: RefWF
  , _se_efficiency :: Efficiency
  } deriving (Eq, Show)
makeLenses ''QC_SE

{-|
Description of a Hartree-Fock calculation.
-}
data QC_HF = QC_HF
  { _hf_approx     :: HF_Approx
  , _hf_ref        :: RefWF
  , _hf_efficiency :: Efficiency
  } deriving (Eq, Show)
makeLenses ''QC_HF

{-|
Description of a DFT calculation.
-}
data QC_DFT = QC_DFT
  { _dft_functional :: DFT_Functional
  , _dft_ref        :: RefWF
  , _dft_approx     :: DFT_Approx
  , _dft_efficiency :: Efficiency
  } deriving (Eq, Show)
makeLenses ''QC_DFT

{-|
Description of a Moller-Plesset calculation.
-}
data QC_MPN = QC_MPN
  { _mpn_flavour    :: MP_Flavour
  , _mpn_order      :: MP_Order
  , _mpn_approx     :: MP_Approx
  , _mpn_ref        :: RefWF
  , _mpn_efficiency :: Efficiency
  } deriving (Eq, Show)
makeLenses ''QC_MPN

{-|
Description of a coupled cluster calculation.
-}
data QC_CC = QC_CC
  { _cc_order      :: CC_Order
  , _cc_flavour    :: CC_Flavour
  , _cc_ref        :: RefWF
  , _cc_approx     :: CC_Approx
  , _cc_efficiency :: Efficiency
  } deriving (Eq, Show)
makeLenses ''QC_CC

{-|
Description of a CASSCF calculation capabilities.
-}
data QC_CAS = QC_CAS
  { _cas_approx     :: CAS_Approx
  , _cas_efficiency :: Efficiency
  } deriving (Eq, Show)
makeLenses ''QC_CAS

{-|
A data type, which is designed only for holding quantum chemical capabilities.
-}
data Methods = Methods
  { _methods_qc_SE  :: [QC_SE]  -- ^ Semiempirical methods.
  , _methods_qc_HF  :: [QC_HF]  -- ^ Hartee-Fock.
  , _methods_qc_DFT :: [QC_DFT] -- ^ Density functional theory.
  , _methods_qc_MPN :: [QC_MPN] -- ^ Moller-Plesset theory.
  , _methods_qc_CC  :: [QC_CC]  -- ^ Coupled cluster.
  , _methods_qc_CAS :: [QC_CAS] -- ^ CASSCF.
  } deriving (Eq, Show)
makeLenses ''Methods
-}
