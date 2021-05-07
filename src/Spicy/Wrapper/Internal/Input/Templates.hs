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
  ( makeInput
  ) where

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
import RIO.Writer
import qualified RIO.Text as Text
import Spicy.Common
import Spicy.Molecule.Internal.Types
import Spicy.Molecule

makeInput :: (HasInput env, MonadThrow m) => ReaderT env m Text
makeInput = do
  mthisSoftware <- asks (^? softwareAF)
  thisSoftware <- maybe2MThrow (WrapperGenericException "makeInput" "Could not determine software!") mthisSoftware
  case thisSoftware of
    Psi4 -> undefined -- To be implemented
    Nwchem -> error "NWChem not implemented!" -- To be implemented...eventually.
    XTB _ -> xtbInput <&> execWriter . foldFree serializeXTB

{-
====================================================================================================
-}

-- Getting input values.

-- | Standard RIO-esque class for inputs. However, this uses AffineFolds
-- instead of Lens'es. AffineFolds are essentially half a prism with just
-- the 'preview' function -- which makes sense, since there's no sensible
-- setters here. They can be used with the standard '^?' operator.
class HasInput env where
  chargeAF :: AffineFold env Int
  multAF :: AffineFold env Int
  methodAF :: AffineFold env GFN
  taskAF :: AffineFold env WrapperTask
  softwareAF :: AffineFold env Program
  memoryAF :: AffineFold env Int
  moleculeAF :: AffineFold env Molecule
  prefixAF :: AffineFold env String
  permaDirAF :: AffineFold env JDirPathAbs

-- This instance expects the complete molecule from the top level onwards. IMO, this is somewhat ugly.
-- In the future, we might be interested in reworking this somehow - at the very least, add
-- a newtype wrapper that differentiates the top level complete system from sub-molecules.
instance HasInput (Molecule, CalcID) where
  chargeAF = thisInputAF % #qMMMSpec % _QM % #charge
  multAF = thisInputAF % #qMMMSpec % _QM % #mult
  methodAF = thisInputAF % #software % _XTB
  taskAF = thisInputAF % #task
  softwareAF = thisInputAF % #software
  memoryAF = thisInputAF % #memory
  moleculeAF = thisMolAF % _2
  prefixAF = thisInputAF % #prefixName
  permaDirAF = thisInputAF % #permaDir

thisMolAF :: AffineFold (Molecule, CalcID) (CalcContext, Molecule)
thisMolAF = afolding $ uncurry getCalcByID

thisInputAF :: AffineFold (Molecule, CalcID) CalcInput
thisInputAF = thisMolAF % _1 % #input

-- | General getting action.
gget ::
  (MonadReader env m, Is k An_AffineFold, MonadThrow m) =>
  -- | An optic to retrieve the value from the environment
  Optic' k is env a ->
  -- | Name of the input field, used to generate error messages
  String ->
  m a
gget af str = asks (^? af) >>= maybe2MThrow (WrapperGenericException ("get" <> str) "Value could not be found!")

getCharge :: (MonadReader env m, MonadThrow m, HasInput env) => m Int
getCharge = gget chargeAF "Charge"

getMult :: (MonadReader env m, MonadThrow m, HasInput env) => m Int
getMult = gget multAF "Mult"

getMethod :: (MonadReader env m, MonadThrow m, HasInput env) => m GFN
getMethod = gget methodAF "Method"

getMemory :: (MonadReader env m, MonadThrow m, HasInput env) => m Int
getMemory = gget memoryAF "Memory"

getMolecule :: (MonadReader env m, MonadThrow m, HasInput env) => m Molecule
getMolecule = gget moleculeAF "Molecule"

getPrefix :: (MonadReader env m, MonadThrow m, HasInput env) => m String
getPrefix = gget prefixAF "Prefix"

getTask :: (MonadReader env m, MonadThrow m, HasInput env) => m WrapperTask
getTask = gget taskAF "Tasks"

{-
====================================================================================================
-}

-- The XTB Input specification

-- | A functor enumerating all common input options in an XTB input file. This functor is meant
-- to be used as the base functor for a free monad, which will specify the input structure.
-- The dummy type parameter is needed to enable fixpoint recursion.
data XTBInput a
  = XTBCharge Int a
  | XTBNOpen Int a
  | XTBMethod GFN a
  | XTBMultipoleInput Text a
  deriving (Functor)

-- | This function embodies the /logical/ structure of the input file,
-- and produces a /data/ representation of said file.
xtbInput :: (HasInput env, MonadReader env m, MonadThrow m) => m (Free XTBInput ())
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
serializeXTB :: MonadWriter Text m => XTBInput a -> m a
serializeXTB (XTBCharge chrg a) = do
  tell $ "$chrg " <> tshow chrg <> "\n"
  return a
serializeXTB (XTBNOpen nopen a) = do
  tell $ "$spin " <> tshow nopen <> "\n"
  return a
serializeXTB (XTBMethod gfn a) = do
  tell $ "$gfn\n method=" <> renderGFN gfn <> "\n"
  return a
serializeXTB (XTBMultipoleInput t a) = do
  tell $ "$embedding\n input=" <> t <> "\n"
  return a

----------------------------------------------------------------------------------------------------

-- The Psi4 input specification

data Psi4Input a
  = PsiMemory Int a
  | PsiMolecule Int Int Text a
  | PsiSet Text a
  | PsiDefine Text Text a
  | PsiFCHK Text String a
  | PsiHessian Text a
  | PsiMultipoles Text a
  deriving (Functor)

psi4Input :: (HasInput env, MonadReader env m, MonadThrow m) => m (Free Psi4Input ())
psi4Input = do
  mem <- getMemory
  chrg <- getCharge
  mult <- getMult
  mol <- getMolecule
  molRepr <- simpleCartesianAngstrom mol
  prefix <- getPrefix
  task <- getTask
  return $ do
    liftF $ PsiMemory mem ()
    liftF $ PsiMolecule chrg mult molRepr ()
    liftF $ PsiSet "def2-svp" () -- Placeholder
    (o,wfn) <- defaultDefine
    liftF $ PsiFCHK wfn prefix ()
    when (task == WTHessian) . liftF $ PsiHessian o ()
    --TODO: add multipoles here
  where
    defaultDefine =
      let (o,wfn) = ("o","wfn")
      in liftF $ PsiDefine o wfn (o,wfn)

----------------------------------------------------------------------------------------------------

-- Various utility functions, to be moved to a separate module later

-- | Make a simple coordinate representation of the current Molecule layer.
-- This function takes care to remove all dummy atoms.
simpleCartesianAngstrom :: MonadThrow m => Molecule -> m Text
simpleCartesianAngstrom mol = Text.unlines . drop 2 . Text.lines <$> writeXYZ (isolateMoleculeLayer mol)
