-- |
-- Module      : Spicy.Wrapper.Internal.Input.Language
-- Description : Preparing input for external programs
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides languages to construct inputs for external
-- quantum chemistry programs.
module Spicy.Wrapper.Internal.Input.Language
  ( makeInput,
    xtbMultPath,
  )
where

{-
The approach in this file is intended to be modular and expandable. Some notes:
- In order to add a new input option to an input file, follow these steps:
  * Add a new constructor to the language functor. All necessary information
    should be contained in a field of this constructor. (i.e. XTBNewInputOption Value a)
  * In the serialize<program> function, add instructions for how to write the new
    input options into the file
  * In the <program>Input function, implement the logic regarding the new input
    (when to include it, how to obtain values, etc.)
- The intermediate data representation can be inspected for correctness or
  other properties, although this is not currently done
- We can define different interpreters, for example for logging, or store/serilize
  the representation
-}

import Control.Monad.Free hiding (iter)
import Data.Aeson
import Data.Aeson.Text
import Data.Default
import Data.Massiv.Array as Massiv hiding (forM_, iter, mapM_)
import qualified Data.Massiv.Array as Massiv
import Optics hiding (element)
import RIO hiding (Lens', lens, min, view, (^.), (^?))
import RIO.Text
import qualified RIO.Text as Text
import RIO.Text.Lazy (toStrict)
import RIO.Writer
import Spicy.Common
import Spicy.Data
import Spicy.Molecule
import Spicy.Molecule.Internal.Types
import qualified Spicy.Wrapper.Internal.Input.Language.Turbomole as TM
import Spicy.Wrapper.Internal.Input.Representation
import System.Path ((<.>), (</>))
import qualified System.Path as Path

-- | Construct the appropriate text for an input file, based
-- on the program.
makeInput :: (MonadInput m) => m Text
makeInput = do
  thisSoftware <- getSoftware
  task <- getTask
  mol <- getMolecule
  case thisSoftware of
    Psi4 _ -> psi4Input <&> execWriter . foldFree serialisePsi4
    XTB _ -> xtbInput <&> execWriter . foldFree serialiseXTB
    Turbomole _ -> turbomoleInput <&> execWriter . foldFree (serialiseTurbomole task (mol ^. #atoms))

{-
====================================================================================================
-}

-- Getting input values.

-- | Standard mtl-style typeclass for acquiring input values.
--
-- Some notes:
--  * MonadThrow is a superclass as all of these are likely to fail in some
--    configuration, e.g. A GFN method makes sense only in an XTB calculation.
--  * This is not "RIO"-style (a HasInput typeclass for a reader environment).
--    The reason is that we may want to use different monads, e.g. a pure State
--    for testing, or a non-reader IO for an interactive behaviour.
class MonadThrow m => MonadInput m where
  getSoftware :: m Program
  getCharge :: m Int
  getMult :: m Int
  getMethod :: m GFN
  getBasisSet :: m Text
  getProgramInfo :: m Program
  getMemory :: m Int
  getMolecule :: m Molecule
  getPrefix :: m String
  getPermaDir :: m JDirPathAbs
  getTask :: m WrapperTask
  getAdditionalInput :: m (Maybe Text)

-- This instance expects the /current/ molecule layer, that is,
-- the one for which the input will be prepared.
instance (MonadThrow m, MonadReader (Molecule, CalcInput) m) => MonadInput m where
  getSoftware = gget (_2 % #software) "Software"
  getCharge = gget (_2 % #qMMMSpec % _QM % #charge) "Charge"
  getMult = gget (_2 % #qMMMSpec % _QM % #mult) "Mult"
  getMethod = gget (_2 % #software % _XTB) "Method"
  getBasisSet = gget (_2 % #software % _Psi4 % #basisSet) "BasisName"
  getProgramInfo = gget (_2 % #software) "ProgramInfo"
  getMemory = gget (_2 % #memory) "Memory"
  getMolecule = gget _1 "Molecule"
  getPrefix = gget (_2 % #prefixName) "Prefix"
  getPermaDir = gget (_2 % #permaDir) "PermaDir"
  getTask = gget (_2 % #task) "Tasks"
  getAdditionalInput = gget (_2 % #additionalInput) "AdditionalInput"

-- | General getting action.
gget ::
  (MonadReader env m, Is k An_AffineFold, MonadThrow m) =>
  -- | An optic to retrieve the value from the environment
  Optic' k is env a ->
  -- | Name of the input field, for error message
  String ->
  m a
gget af str = asks (^? af) >>= maybe2MThrow (WrapperGenericException ("get" <> str) "Value could not be found while trying to write calculation input!")

{-
====================================================================================================
-}

-- The XTB Input specification

type XTBInput = Free XTBInputF ()

-- | A functor enumerating all common input options in an XTB input file. This functor is meant
-- to be used as the base functor for a free monad, which will specify the input structure.
-- The dummy type parameter is needed to enable fixpoint recursion.
-- The arbitrary field allows users to insert arbitrary text into the input file.
data XTBInputF a
  = -- | Molecular charge
    XTBCharge Int a
  | -- | Number of open shells
    XTBNOpen Int a
  | -- | Version of the GFN Hamiltonian
    XTBMethod GFN a
  | -- | Multipole representation
    XTBMultipoleInput Text a
  | -- | User-specified Text
    XTBArbitrary Text a
  deriving (Functor)

-- | This function embodies the /logical/ structure of the xtb input file,
-- and produces a /data/ representation of said file.
xtbInput :: (MonadInput m) => m XTBInput
xtbInput = do
  -- Get input information
  chrg <- getCharge
  mult <- getMult
  let nopen = mult - 1
  mthd <- getMethod
  (JDirPathAbs perma) <- getPermaDir
  prefix <- getPrefix
  arb <- getAdditionalInput

  -- Build input representation
  return $ do
    liftF $ XTBCharge chrg ()
    liftF $ XTBNOpen nopen ()
    liftF $ XTBMethod mthd ()
    let pth = path2Text $ xtbMultPath perma (Path.path prefix)
    liftF $ XTBMultipoleInput pth ()
    case arb of
      Nothing -> return ()
      Just t -> liftF $ XTBArbitrary t ()

-- | This function specifies how to serialize the information contained in the
-- abstract input representation into a text representation.
serialiseXTB :: MonadWriter Text m => XTBInputF a -> m a
serialiseXTB (XTBCharge chrg a) = do
  tell $ "$chrg " <> tshow chrg <> "\n"
  return a
serialiseXTB (XTBNOpen nopen a) = do
  tell $ "$spin " <> tshow nopen <> "\n"
  return a
serialiseXTB (XTBMethod gfn a) = do
  tell $ "$gfn\n method=" <> renderGFN gfn <> "\n"
  return a
serialiseXTB (XTBMultipoleInput t a) = do
  tell $ "$embedding\n input=" <> t <> "\n"
  return a
serialiseXTB (XTBArbitrary t a) = do
  tell $ t <> "\n"
  return a

-- | Using this function to build the path to the XTB multipole file
-- ensures that it is consistent across the program.
xtbMultPath :: Path.AbsDir -> Path.RelFile -> Path.AbsFile
xtbMultPath perma prefix = perma </> prefix <.> ".pc"

----------------------------------------------------------------------------------------------------

-- The Psi4 input specification

type Psi4Input = Free Psi4InputF ()

-- | A functor enumerating all common input options in a Psi4 input file. This functor is meant
-- to be used as the base functor for a free monad, which will specify the input structure.
-- The dummy type parameter is needed to enable fixpoint recursion.
-- The arbitrary field allows users to insert arbitrary text into the input file.
data Psi4InputF a
  = -- | Memory in MB
    Psi4Memory Int a
  | -- | Charge, multiplicity , molecule representation
    Psi4Molecule Int Int Text a
  | -- | Basis set
    Psi4Set Text a
  | -- | Output name, wavefunction name, method, Task
    Psi4Define Text Text Text WrapperTask a
  | -- | Output identifier, .fchk file prefix
    Psi4FCHK Text String a
  | -- | Wavefunction identifier
    Psi4Hessian Text a
  | -- | Multiple representation
    Psi4Multipoles Text a
  | -- | User-specified Text
    Psi4Arbitrary Text a
  deriving (Functor)

-- | This function embodies the /logical/ structure of the psi4 input file,
-- and produces a /data/ representation of said file.
psi4Input :: (MonadInput m) => m Psi4Input
psi4Input = do
  -- Acquire all necessary values
  mem <- getMemory
  chrg <- getCharge
  mult <- getMult
  mol <- getMolecule
  molRepr <- simpleCartesianAngstrom mol
  programInfo <- getProgramInfo
  calcType <-
    maybe2MThrow (WrapperGenericException "psi4Input" "Calculation type could not be found") $
      programInfo ^? _Psi4 % #calculationType
  basis <- getBasisSet
  prefix <- getPrefix
  task <- getTask
  multipoleRep <- psi4MultipoleRep mol
  arb <- getAdditionalInput

  -- Form the monadic input construct
  return $ do
    liftF $ Psi4Memory mem ()
    liftF $ Psi4Molecule chrg mult molRepr ()
    liftF $ Psi4Set basis ()
    case arb of
      Nothing -> return ()
      Just t -> liftF $ Psi4Arbitrary t ()
    liftF $ Psi4Multipoles multipoleRep ()
    (o, wfn) <- defaultDefine task calcType
    liftF $ Psi4FCHK wfn prefix ()
    when (task == WTHessian) . liftF $ Psi4Hessian o ()
  where
    defaultDefine tsk mthd =
      let (o, wfn) = ("o", "wfn")
       in liftF $ Psi4Define o wfn mthd tsk (o, wfn)

-- | This function specifies how to serialize the information contained in the
-- abstract input representation into a text representation.
serialisePsi4 :: MonadWriter Text m => Psi4InputF a -> m a
serialisePsi4 (Psi4Memory m a) = do
  tell $ "memory " <> tShow m <> "MB\n"
  return a
serialisePsi4 (Psi4Molecule c m mol a) = do
  tell "molecule {\n"
  tell $ "  " <> tShow c <> " " <> tShow m <> "\n"
  tell $ mol <> "\n}\n"
  return a
serialisePsi4 (Psi4Set basis a) = do
  tell "set {\n"
  tell $ "  basis " <> basis
  tell "\n}\n"
  return a
serialisePsi4 (Psi4Define o wfn mthd task a) = do
  let tskStr = case task of
        WTEnergy -> "energy"
        WTGradient -> "gradient"
        WTHessian -> "hessian"
  tell $ o <> ", " <> wfn <> " = " <> tskStr <> "(\"" <> mthd <> "\", return_wfn = True)\n"
  return a
serialisePsi4 (Psi4FCHK wfn prefix a) = do
  tell $ "fchk( " <> wfn <> ", \"" <> pack prefix <> ".fchk\" )\n"
  return a
serialisePsi4 (Psi4Hessian o a) = do
  tell $ "np.array(" <> o <> ")\n"
  return a
serialisePsi4 (Psi4Multipoles multipoles a) = do
  tell $ multipoles <> "\n"
  return a
serialisePsi4 (Psi4Arbitrary t a) = do
  tell $ t <> "\n"
  return a

----------------------------------------------------------------------------------------------------

turbomoleInput :: MonadInput m => m TM.TurbomoleInput
turbomoleInput = do
  -- Acquire information about turbomole run.
  mem <- getMemory
  charge <- getCharge
  programInfo <- getProgramInfo
  tmInput <-
    maybe2MThrow (WrapperGenericException "turbomoleInput" "Turbomole settings could not be found!") $
      programInfo ^? _Turbomole
  other <- getAdditionalInput

  -- Construct the 'TM.Control' structure.
  let control =
        TM.Control
          { atoms = tmInput ^. #basis,
            charge,
            scf = fromMaybe def $ tmInput ^. #scf,
            refWfn = tmInput ^. #ref,
            ri = tmInput ^. #ri,
            excorr = tmInput ^. #corr,
            memory = fromIntegral mem,
            other
          }

  return $ do
    liftF $ TM.Input control ()

-- | Key-Value type line, that is only printed, if a value is availabel for this key.
optKW :: Show a => Text -> Maybe a -> Text
optKW _ Nothing = mempty
optKW k (Just v) = k <> tshow v

-- | 'tell' with a appended line break.
tellN :: (IsString w, MonadWriter w m) => w -> m ()
tellN c = tell $ c <> "\n"

-- | Use a JSON instance to serialise to a turbomole value
json2TM :: ToJSON a => a -> Text
json2TM a = Text.filter (/= '"') . toStrict . encodeToLazyText . toJSON $ a

-- | Serialise the turbomole record to a control file.
serialiseTurbomole :: (MonadWriter Text m, Traversable t) => WrapperTask -> t Atom -> TM.TurbomoleInputF a -> m a
serialiseTurbomole task ats (TM.Input TM.Control {..} a) = do
  -- Serialise always required fields, that are not part of the input.
  tellN "$symmetry c1"

  -- Serialise the record into a control file
  serialiseTMAtoms atoms
  serialiseTMCoord ats
  serialiseTMSCF scf
  serialiseTMRefWfn charge ats refWfn
  serialiseRI ri
  serialiseExCorr task refWfn excorr
  tellN $ "$maxcor " <> tshow memory <> " MiB per_core"
  forM_ other tellN

  -- End the control file
  tellN "$end"

  return a

-- | Serialise the @$atoms@ block of a turbomole input.
serialiseTMAtoms :: MonadWriter Text m => TM.Atoms -> m ()
serialiseTMAtoms TM.Atoms {..} = do
  tellN "$atoms"
  tellN $ "  basis = " <> basis
  tellN . optKW "  jbas  = " $ jbas
  tellN . optKW "  jkbas = " $ jkbas
  tellN . optKW "  ecp   = " $ ecp
  tellN . optKW "  cbas  = " $ cbas
  tellN . optKW "  cabs  = " $ cabs
  forM_ other (mapM_ tellN)

-- | Serialise the various scf settings into various scf relatex blocks.
serialiseTMSCF :: MonadWriter Text m => TM.SCF -> m ()
serialiseTMSCF TM.SCF {..} = do
  tellN $ "$scfiterlimit " <> tshow iter
  tellN $ "$scfconv" <> tshow conv
  case damp of
    Nothing -> return ()
    Just TM.Damp {..} -> tellN $ "$scfdamp start=" <> tshow start <> " step=" <> tshow step <> " min=" <> tshow min
  tellN . optKW "$scforbitalshift automatic " $ shift

-- | Serialise the @$coord@ block of a turbomole input.
serialiseTMCoord :: (MonadWriter Text m, Traversable t) => t Atom -> m ()
serialiseTMCoord atoms = do
  tellN "$coord"
  forM_ atoms $ \Atom {..} -> do
    let coords = compute @U . Massiv.map angstrom2Bohr . getVectorS $ coordinates
    tell "  "
    Massiv.forM_ coords $ tell . tshow
    tell "  "
    tellN . Text.toLower . tshow $ element

-- | Serialise the input for the reference wavefunction.
serialiseTMRefWfn :: (MonadWriter Text m, Traversable t) => Int -> t Atom -> TM.RefWfn -> m ()
serialiseTMRefWfn charge atoms ref = case ref of
  TM.RHF -> commonClosed
  TM.UHF m -> tellN "$uhf" >> commonOpen m
  TM.RKS dft -> commonClosed >> commonDFT dft
  TM.UKS m dft -> tellN "$uhf" >> commonOpen m >> commonDFT dft
  where
    nElectrons = RIO.foldl' (\acc a -> acc + fromEnum (a ^. #element)) (- charge) atoms
    nExcEl m = fromIntegral $ m - 1
    nClosed m = (nElectrons - nExcEl m) `div` 2
    commonClosed = do
      tellN "$closed shells"
      tellN $ "  a  1-" <> tshow (nClosed 1) <> " ( 2 )"
    commonOpen m = do
      tellN "$alpha shells"
      tellN $ "  a   1-" <> tshow (nClosed m + nExcEl m) <> " ( 1 )"
      tellN "$beta shells"
      tellN $ "  a   1-" <> tshow (nClosed m) <> " ( 1 )"
    commonDFT TM.DFT {..} = do
      tellN "$dft"
      tellN $ "  functional" <> json2TM functional
      tellN $ "  gridsize " <> json2TM functional
      case disp of
        Nothing -> return ()
        Just TM.D3 -> tellN "$disp3"
        Just TM.D3BJ -> tellN "$disp bj"
        Just TM.D4 -> tellN "$disp4"

-- | Serialise the optional RI settings
serialiseRI :: MonadWriter Text m => Maybe TM.RI -> m ()
serialiseRI ri = case ri of
  Nothing -> return ()
  Just TM.RIJ -> tellN "$rij"
  Just TM.RIJK -> tellN "$rij" >> tellN "$rik"

-- | Serialise the wavefunction correlation and excited state settings.
serialiseExCorr :: MonadWriter Text m => WrapperTask -> TM.RefWfn -> Maybe TM.CorrelationExc -> m ()
serialiseExCorr task refwfn excorr = case excorr of
  Nothing -> return ()
  Just TM.PNOCCSD {..} -> do
    tellN "$denconv 1.0e-7"
    tellN "$pnoccsd"
    tellN $ "  " <> json2TM model
    tellN . optKW "  maxiter=" $ iter
    forM_ other (mapM_ tellN)
    case exci of
      Nothing -> return ()
      Just nStates -> tellN "  prepno davidso" >> commonExcBlock nStates
  Just TM.RICC {..} -> do
    tellN "$denconv 1.0e-7"
    tellN "$ricc2"
    tellN $ "  " <> (if task == WTGradient then "geoopt model=" else mempty) <> json2TM model
    tellN . optKW "  maxiter=" $ iter
    forM_ other (mapM_ tellN)
    forM_ exci commonExcBlock
  Just TM.TDSCF {..} -> do
    tellN $
      "$scfinstab " <> case (approx, refwfn) of
        (TM.TDA, TM.RHF) -> "rpas"
        (TM.TDA, TM.RKS _) -> "rpas"
        (TM.TDA, _) -> "urpa"
        (TM.RPA, TM.RHF) -> "ciss"
        (TM.RPA, TM.RKS _) -> "ciss"
        (TM.RPA, _) -> "ucis"
    tellN "$soes"
    tellN $ "  a " <> tshow (fromMaybe 1 exci)
  where
    commonExcBlock ns = do
      tellN "$excitations"
      tell "  irrep=a "
      case refwfn of
        TM.RKS _ -> tell "multiplicity=1 "
        TM.RHF -> tell "multiplicity=1 "
        _ -> return ()
      tellN $ "nexc=" <> tshow ns
      tellN "  spectrum states=all operators=diplen"

----------------------------------------------------------------------------------------------------

-- Input verification

-- | Check whether charge is reasonable, and generate a warning if not.
checkCharge :: MonadWriter [Text] m => Int -> m ()
checkCharge c = when (abs c > 10) $ tell ["Very large charge in this layer - are you sure this is what you want?"]

-- | Check whether multiplicity is reasonable, and generate appropriate warnings.
checkMult :: MonadWriter [Text] m => Int -> m ()
checkMult m = do
  when (abs m > 10) $ tell ["Very large multiplicity in this layer - are you sure this is what you want?"]
  when (m < 0) $ tell ["Negative number of open shells - this will likely cause problems."]

-- | Check whether memory is reasonable, and generate appropriate warnings.
checkMemory :: MonadWriter [Text] m => Int -> m ()
checkMemory mem = do
  when (mem < 100 && signum mem >= 0) $ tell ["Very little memory (<100 MB) specified - are you sure this is what you want?"]
  when (mem < 0) $ tell ["Negative memory specified - this will likely cause problems."]

-- | Go through an XTB input and generate warnings for the user.
checkXTB :: (MonadWriter [Text] m) => XTBInputF a -> m a
checkXTB (XTBCharge c a) = checkCharge c >> return a
checkXTB (XTBNOpen nopen a) = checkMult nopen >> return a
checkXTB (XTBMethod _ a) = return a
checkXTB (XTBMultipoleInput _ a) = return a
checkXTB (XTBArbitrary _ a) = return a

checkPsi4 :: MonadWriter [Text] m => Psi4InputF a -> m a
checkPsi4 (Psi4Memory m a) = checkMemory m >> return a
checkPsi4 (Psi4Molecule c m _ a) = checkCharge c >> checkMult m >> return a
checkPsi4 (Psi4Set _ a) = return a
checkPsi4 (Psi4Define _ _ _ _ a) = return a
checkPsi4 (Psi4FCHK _ _ a) = return a
checkPsi4 (Psi4Hessian _ a) = return a
checkPsi4 (Psi4Multipoles _ a) = return a
checkPsi4 (Psi4Arbitrary _ a) = return a
