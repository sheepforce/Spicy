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
import Optics
import RIO hiding (Lens', lens, view, (^.), (^?))
import qualified RIO.Text as Text
import RIO.Writer
import Spicy.Common
import Spicy.Molecule
import Spicy.Molecule.Internal.Types

makeInput :: (MonadInput m) => m Text
makeInput = do
  thisSoftware <- getSoftware
  case thisSoftware of
    Psi4 -> undefined -- To be implemented
    Nwchem -> error "NWChem not implemented!" -- To be implemented...eventually.
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
  getMemory :: m Int
  getMolecule :: m Molecule
  getPrefix :: m String
  getPermaDir :: m JDirPathAbs
  getTask :: m WrapperTask

-- This instance expects the /current/ molecule layer, that is,
-- the one for which the input will be prepared.
instance MonadThrow m => MonadInput (ReaderT (Molecule, CalcInput) m) where
  getSoftware = gget (_2 % #software) "Software"
  getCharge = gget (_2 % #qMMMSpec % _QM % #charge) "Charge"
  getMult = gget (_2 % #qMMMSpec % _QM % #mult) "Mult"
  getMethod = gget (_2 % #software % _XTB) "Method"
  getMemory = gget (_2 % #memory) "Memory"
  getMolecule = gget _1 "Molecule"
  getPrefix = gget (_2 % #prefixName) "Prefix"
  getPermaDir = gget (_2 % #permaDir) "PermaDir"
  getTask = gget (_2 % #task) "Tasks"

-- | General getting action.
gget ::
  (MonadReader env m, Is k An_AffineFold, MonadThrow m) =>
  -- | An optic to retrieve the value from the environment
  Optic' k is env a ->
  -- | Name of the input field, used to generate error messages
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
  = XTBCharge Int a
  | XTBNOpen Int a
  | XTBMethod GFN a
  | XTBMultipoleInput Text a
  deriving (Functor)

-- | This function embodies the /logical/ structure of the input file,
-- and produces a /data/ representation of said file.
xtbInput :: (MonadInput m) => m XTBInput
xtbInput = do
  chrg <- getCharge
  mult <- getMult
  let nopen = mult - 1
  mthd <- getMethod
  return $ do
    liftF $ XTBCharge chrg ()
    liftF $ XTBNOpen nopen ()
    liftF $ XTBMethod mthd ()

-- TODO: Multipoles

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

----------------------------------------------------------------------------------------------------

-- The Psi4 input specification

type Psi4Input = Free Psi4InputF ()

data Psi4InputF a
  = Psi4Memory Int a -- ^ Memory in MB
  | Psi4Molecule Int Int Text a -- ^ Charge, multiplicity , molecule representation
  | Psi4Set Text a -- ^ Basis set
  | Psi4Define Text Text Text a -- ^ Output name, wavefunction name, DFT functional
  | Psi4FCHK Text String a -- ^ Output identifier, .fchk file prefix
  | Psi4Hessian Text a -- ^ Wavefunction identifier
  | Psi4Multipoles Text a
  deriving (Functor)

psi4Input :: (MonadInput m) => m Psi4Input
psi4Input = do
  -- Acquire all necessary values
  mem <- getMemory
  chrg <- getCharge
  mult <- getMult
  mol <- getMolecule
  molRepr <- simpleCartesianAngstrom mol
  prefix <- getPrefix
  task <- getTask
  -- Form the monadic input construct
  return $ do
    liftF $ Psi4Memory mem ()
    liftF $ Psi4Molecule chrg mult molRepr ()
    liftF $ Psi4Set "def2-svp" () -- Placeholder
    (o, wfn) <- defaultDefine
    liftF $ Psi4FCHK wfn prefix ()
    when (task == WTHessian) . liftF $ Psi4Hessian o ()
  where
    --TODO: add multipoles here

    defaultDefine =
      let (o, wfn) = ("o", "wfn")
       in liftF $ Psi4Define o wfn "bp86" (o, wfn) -- Placeholder

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
serialisePsi4 (Psi4Define o wfn func a) = undefined -- TODO
serialisePsi4 (Psi4FCHK wfn prefix a) = do
  tell $ "fchk(" <> wfn <> ", \"" <> Text.pack prefix <> ".fchk\" )\n"
  return a
serialisePsi4 (Psi4Hessian o a) = do
  tell $ "np.array(" <> o <> ")\n"
  return a
serialisePsi4 (Psi4Multipoles multipoles a) = do
  tell $ "{" <> multipoles <> "}\n"
  return a

----------------------------------------------------------------------------------------------------

-- Various utility functions, to be moved to a separate module later

-- | Make a simple coordinate representation of the current Molecule layer.
-- This function takes care to remove all dummy atoms.
simpleCartesianAngstrom :: MonadThrow m => Molecule -> m Text
simpleCartesianAngstrom mol = Text.unlines . drop 2 . Text.lines <$> writeXYZ (isolateMoleculeLayer mol)
