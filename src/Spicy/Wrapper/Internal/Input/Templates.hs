-- |
-- Module      : Spicy.Wrapper.Internal.Input.Templates
-- Description : Preparing input for external programs
-- Copyright   : Phillip Seeber, Sebastian Seidenath, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides a framework to construct inputs for external
-- quantum chemistry programs.
module Spicy.Wrapper.Internal.Input.Templates
  ( makeInput,
  )
where

{-
Previously, Spicy used mustache templates to construct input files.
This had the advantage of allowing for a single function consistent
through programs. However, Text-templates are "stupid":
not typesafe, difficult to modify and inflexible.

This approach consist of specifying a "vocabulary" using a functor,
a Reader specifying the logic of each input file,
and an interpretation function, specifying the details of the final representation.

Since the input information is stored as haskell data, we can pass it around,
inspect it for correctness, modify it after construction, etc.
-}

import Control.Monad.Free
import qualified Data.Massiv.Array as Massiv
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Builder
import Optics
import RIO hiding (Lens', lens, view, (^.), (^?))
import qualified RIO.Text as RText
import RIO.Writer
import Spicy.Common
import Spicy.Molecule
import Spicy.Molecule.Internal.Types
import System.IO.Unsafe
import System.Path

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
instance MonadThrow m => MonadInput (ReaderT (Molecule, CalcInput) m) where
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
data XTBInputF a
  = -- | Molecular charge
    XTBCharge Int a
  | -- | Number of open shells
    XTBNOpen Int a
  | -- | Version of the GFN Hamiltonian
    XTBMethod GFN a
  | -- | Multipole representation
    XTBMultipoleInput Text a
    -- | User-specified Text
  | XTBArbitrary Text a
  deriving (Functor)

-- | This function embodies the /logical/ structure of the input file,
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
    let pth = path2Text $ perma </> path prefix <.> ".pc"
    liftF $ XTBMultipoleInput pth ()
    case arb of
      Nothing -> return ()
      Just t -> liftF $ XTBArbitrary t ()

-- | This function specifies how to serialize the information contained in the
-- abstract input representation
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

----------------------------------------------------------------------------------------------------

-- The Psi4 input specification

type Psi4Input = Free Psi4InputF ()

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
    -- | User-specified Text
  | Psi4Arbitrary Text a
  deriving (Functor)

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
  tell $ "fchk( " <> wfn <> ", \"" <> RText.pack prefix <> ".fchk\" )\n"
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

-- Various utility functions, to be moved to a separate module later

-- | Make a simple coordinate representation of the current Molecule layer.
-- This function takes care to remove all dummy atoms.
simpleCartesianAngstrom :: MonadThrow m => Molecule -> m Text
simpleCartesianAngstrom mol = RText.unlines . drop 2 . RText.lines <$> writeXYZ (isolateMoleculeLayer mol)

-- | Generate the multipole representation accepted by XTB. Must be in its own file.
xtbMultipoleRep ::
  (MonadThrow m) =>
  -- | The __current__ 'Molecule' layer for which to perform the calculation. The multipoles must
  -- therefore already be present and multipole centres must be marked as Dummy atoms.
  Molecule ->
  m Text
xtbMultipoleRep mol = do
  let pointChargeVecs = Massiv.innerSlices $ unsafeMolToPointCharges mol
      toText vec =
        let q = Builder.fromText . tShow $ vec Massiv.! 3
            x = Builder.fromText . tShow $ vec Massiv.! 0
            y = Builder.fromText . tShow $ vec Massiv.! 1
            z = Builder.fromText . tShow $ vec Massiv.! 2
         in q <> " " <> x <> " " <> y <> " " <> z <> " 99\n"
      chargeLines = Massiv.foldMono toText pointChargeVecs
      countLine = (Builder.fromText . tShow . length $ pointChargeVecs) <> "\n"
      xtbBuilder = countLine <> chargeLines
  return . Text.toStrict . Builder.toLazyText $ xtbBuilder

-- | Generates the multipole representation for Psi4.
psi4MultipoleRep ::
  (MonadThrow m) =>
  -- | The __current__ 'Molecule' layer for which to perform the calculation. The multipoles must
  -- therefore already be present and multipole centres must be marked as Dummy atoms.
  Molecule ->
  m Text
psi4MultipoleRep mol = do
  let pointChargeVecs = Massiv.innerSlices $ unsafeMolToPointCharges mol
      toText vec =
        let q = Builder.fromText . tShow $ vec Massiv.! 3
            x = Builder.fromText . tShow $ vec Massiv.! 0
            y = Builder.fromText . tShow $ vec Massiv.! 1
            z = Builder.fromText . tShow $ vec Massiv.! 2
         in "Chrgfield.extern.addCharge(" <> q <> ", " <> x <> ", " <> y <> ", " <> z <> ")\n"
      chargeLines = Massiv.foldMono toText pointChargeVecs
      settingsLine = Builder.fromText "psi4.set_global_option_python('EXTERN', Chrgfield.extern)"
      psi4Builder = "Chrgfield = QMMM()\n" <> chargeLines <> settingsLine
  return . Text.toStrict . Builder.toLazyText $ psi4Builder

-- | A \"pure\" version of the "molToPointCharges" function. Morally, this is true,
-- as the function performs no side effects and is entirely deterministic.
-- The MonadIO constraint comes from the use of a parallel fold, which could
-- in general produce non-deterministic results, however, both folding
-- and chunk folding function are both commutative and associative, rendering
-- this moot.
unsafeMolToPointCharges :: Molecule -> Massiv.Matrix Massiv.S Double
unsafeMolToPointCharges = unsafePerformIO . molToPointCharges