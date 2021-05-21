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

import Control.Monad.Free
import Optics
import RIO hiding (Lens', lens, view, (^.), (^?))
import RIO.Text
import RIO.Writer
import Spicy.Common
import Spicy.Molecule
import Spicy.Molecule.Internal.Types
import Spicy.Wrapper.Internal.Input.Representation
import System.Path ((<.>), (</>))
import qualified System.Path as Path

-- | Construct the appropriate text for an input file, based
-- on the program.
makeInput :: (MonadInput m) => m Text
makeInput = do
  thisSoftware <- getSoftware
  case thisSoftware of
    Psi4 _ -> psi4Input <&> execWriter . foldFree serialisePsi4
    Nwchem -> error "NWChem not implemented!"
    XTB _ -> xtbInput <&> execWriter . foldFree serialiseXTB

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
  getCalculationType :: m Text
  getMemory :: m Int
  getMolecule :: m Molecule
  getPrefix :: m String
  getPermaDir :: m JDirPathAbs
  getTask :: m WrapperTask
  getAdditionalInput :: m (Maybe Text)

-- This instance expects the /current/ molecule layer, that is,
-- the one for which the input will be prepared.
instance (MonadThrow m, MonadReader (Molecule,CalcInput) m) => MonadInput m where
  getSoftware = gget (_2 % #software) "Software"
  getCharge = gget (_2 % #qMMMSpec % _QM % #charge) "Charge"
  getMult = gget (_2 % #qMMMSpec % _QM % #mult) "Mult"
  getMethod = gget (_2 % #software % _XTB) "Method"
  getBasisSet = gget (_2 % #software % _Psi4 % #basisSet) "BasisName"
  getCalculationType = gget (_2 % #software % _Psi4 % #calculationType) "CalculationType"
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
  calcType <- getCalculationType
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
    liftF $ Psi4Multipoles multipoleRep ()
    (o, wfn) <- defaultDefine task calcType
    liftF $ Psi4FCHK wfn prefix ()
    when (task == WTHessian) . liftF $ Psi4Hessian o ()
    case arb of
      Nothing -> return ()
      Just t -> liftF $ Psi4Arbitrary t ()
  where
    defaultDefine tsk mthd =
      let (o, wfn) = ("o", "wfn")
       in liftF $ Psi4Define o wfn ("\"" <> mthd <> "\"") tsk (o, wfn)

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
  tell $ o <> ", " <> wfn <> " = " <> tskStr <> "(" <> mthd <> ", return_wfn = True)\n"
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

-- Input verification

-- | Check whether charge is reasonable, and generate a warning if not.
checkCharge :: MonadWriter [Text] m => Int -> m ()
checkCharge c = when (abs c > 10) $ tell ["Very large charge in this layer - are you sure this is what you want?"]

checkMult :: MonadWriter [Text] m => Int -> m ()
checkMult m = do
  when (abs m > 10) $ tell ["Very large multiplicity in this layer - are you sure this is what you want?"]
  when (m < 0) $ tell ["Negative number of open shells - this will likely cause problems."]

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