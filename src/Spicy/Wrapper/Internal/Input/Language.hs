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

import Data.Default
import qualified Data.IntMap as IntMap
import Data.Massiv.Array as Massiv hiding (forM_, iter, mapM_)
import Optics hiding (element)
import RIO hiding (Lens', lens, min, view, (^.), (^?))
import qualified RIO.Text as Text
import RIO.Writer
import Spicy.Common
import Spicy.Molecule
import Spicy.Wrapper.Internal.Input.Representation
import System.Path ((<.>), (</>))
import qualified System.Path as Path

-- | Key-Value type line, that is only printed, if a value is availabel for this key.
optKW :: Show a => Text -> Maybe a -> Text
optKW _ Nothing = mempty
optKW k (Just v) = k <> tshow v

-- FIXME - Why is the MonadInput constraint not working here?

-- | Construct the appropriate text for an input file, based
-- on the program.
makeInput :: (MonadReader (Molecule, CalcInput) m, MonadThrow m) => m Text
makeInput =
  getSoftware >>= \soft -> case soft of
    Psi4 _ -> execWriterT serialisePsi4
    XTB _ -> execWriterT serialiseXTB
    Turbomole _ -> execWriterT serialiseTurbomole

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
  getCharge :: m Int
  getMult :: m Int
  getMemory :: m Int
  getMolecule :: m Molecule
  getPrefix :: m String
  getPermaDir :: m JDirPathAbs
  getTask :: m WrapperTask
  getSoftware :: m Program
  getQCHamiltonian :: m QCHamiltonian

-- This instance expects the /current/ molecule layer, that is,
-- the one for which the input will be prepared.
instance (MonadThrow m, MonadReader (Molecule, CalcInput) m) => MonadInput m where
  getCharge = gget (_2 % #qMMMSpec % _QM % #charge) "Charge"
  getMult = gget (_2 % #qMMMSpec % _QM % #mult) "Mult"
  getMemory = gget (_2 % #memory) "Memory"
  getMolecule = gget _1 "Molecule"
  getPrefix = gget (_2 % #prefixName) "Prefix"
  getPermaDir = gget (_2 % #permaDir) "PermaDir"
  getTask = gget (_2 % #task) "Tasks"
  getSoftware = gget (_2 % #software) "Program"
  getQCHamiltonian = gget (_2 % #software % _ProgramHamiltonian) "QCHamiltonian"

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

-- | Serialise the XTB input into a control-like input file.
serialiseXTB :: (MonadInput m, MonadWriter Text m) => m ()
serialiseXTB = do
  charge <- getCharge
  nOpen <- getMult >>= \m -> pure $ m - 1
  permaDir <- getPermaDir
  prefix <- getPrefix
  software <- getSoftware
  gfnVersion <- maybe2MThrow (localExc "GFN Hamiltonian unspecified. Cannot continue") $ software ^? _XTB
  tellN $ "$chrg " <> tshow charge
  tellN $ "$spin " <> tshow nOpen
  tellN "$gfn"
  tellN $ "  method=" <> renderGFN gfnVersion
  tellN "$embedding"
  tellN $ "  input=" <> path2Text (xtbMultPath (getDirPathAbs permaDir) (Path.path prefix))
  where
    localExc = WrapperGenericException "serialiseXTB"

-- | Using this function to build the path to the XTB multipole file
-- ensures that it is consistent across the program.
xtbMultPath :: Path.AbsDir -> Path.RelFile -> Path.AbsFile
xtbMultPath perma prefix = perma </> prefix <.> ".pc"

----------------------------------------------------------------------------------------------------

-- | Serialise a Psi4 input in a standard Psithon input file. No excited states yet in Psi4.
serialisePsi4 :: (MonadInput m, MonadWriter Text m) => m ()
serialisePsi4 = do
  psi4SerialiseMemory
  psi4SerialiseMolecule
  psi4SerialiseBasis
  psi4SerialiseRefWfnSettings
  psi4SerialiseSCF
  psi4SerialiseRI
  psi4SerialiseCorrelation
  psi4SerialiseOther
  psi4SerialiseEmbedding
  psi4SerialiseCall

-- | Serialise the memory into a psithon file.
psi4SerialiseMemory :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseMemory = getMemory >>= \mem -> tellN $ "memory " <> tshow mem <> " MiB"

-- | XYZ representation of the molecule in Psi4, that ensures C1 symmetry.
psi4SerialiseMolecule :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseMolecule = do
  prefix <- getPrefix
  molRep <- getMolecule >>= simpleCartesianAngstrom
  charge <- getCharge
  mult <- getMult
  tellN $ "molecule " <> Text.pack prefix <> " {"
  tellN $ "  " <> tshow charge <> " " <> tshow mult
  tellN "  symmetry c1"
  tell molRep
  tellN "}"

-- | Serialise the basis sets for Psi4
psi4SerialiseBasis :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseBasis = do
  basisSets <- getQCHamiltonian <&> (^. #basis)
  tellN $ "set basis " <> (basisSets ^. #basis)
  tellN . optKW "  set df_basis_cc " $ (basisSets ^. #cbas)
  tellN . optKW "  set df_basis_mp " $ (basisSets ^. #cbas)
  tellN . optKW "  set df_basis_mp2 " $ (basisSets ^. #cbas)
  tellN . optKW "  set df_basis_sapt " $ (basisSets ^. #cbas)
  tellN . optKW "  set df_basis_scf " $ (basisSets ^. #jkbas)
  forM_ (basisSets ^. #other) (mapM_ tellN)

-- | Serialises settings for the reference wavefunction. Does not call it and sets only the
-- reference type. The rest goes into the call of the function.
psi4SerialiseRefWfnSettings :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseRefWfnSettings = do
  ref <- getQCHamiltonian <&> (^. #ref)
  case ref of
    RHF -> tellN "set reference rhf"
    UHF -> tellN "set reference uhf"
    RKS DFT {other} -> tellN "set reference rks" >> forM_ other (mapM_ tellN)
    UKS DFT {other} -> tellN "set reference rks" >> forM_ other (mapM_ tellN)

-- | Serialise the Psi4 SCF settings.
psi4SerialiseSCF :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseSCF =
  getQCHamiltonian <&> (^. #scf) >>= \scf -> case scf of
    Nothing -> return ()
    Just SCF {..} -> do
      tellN $ "set d_convergence 1.0e-" <> tshow conv
      tellN $ "set maxiter " <> tshow iter
      case damp of
        Nothing -> return ()
        Just Damp {start} -> tellN $ "set damping_percentage " <> tshow (start * 100)
      forM_ other (mapM_ tellN)

-- | Serialise the Psi4 RI settings for the reference wavefunction. Psi4 does always RI-JK, so both
-- RI types will just give ri type "DF" in Psi4.
psi4SerialiseRI :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseRI = do
  getQCHamiltonian <&> (^. #ri) >>= \ri -> case ri of
    Nothing -> do
      tellN "set scf_type pk"
      tellN "set mp2_type conv"
      tellN "set mp_type conv"
      tellN "set cc_type conv"
    Just (OtherRI ritype) -> tellN $ "set scf_type " <> ritype
    Just _ -> do
      tellN "set scf_type df"
      tellN "set mp2_type df"
      tellN "set mp_type df"
      tellN "set cc_type df"

-- | Serialise correlation of the wavefunction. Does not call the proper hamiltonian, but just puts
-- settings in set blocks.
psi4SerialiseCorrelation :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseCorrelation =
  getQCHamiltonian <&> (^. #corr) >>= \corr -> case corr of
    Nothing -> return ()
    Just Correlation {..} -> do
      tellN . optKW "set mo_maxiter " . fmap tshow $ iter
      tellN . optKW "set maxiter " . fmap tshow $ iter
      forM_ other (mapM_ tellN)

-- | Serialises all other fields verbatim into the Psi4 input, before calling the hamiltonian and
-- actual functions.
psi4SerialiseOther :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseOther = getQCHamiltonian >>= \QCHamiltonian {other} -> forM_ other (mapM_ tellN)

-- | Serialises the point charges into a Psi4 representaiton.
psi4SerialiseEmbedding :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseEmbedding = getMolecule >>= psi4MultipoleRep >>= tellN

-- | Serialise the actual call and file handling of Psi4. If the reference wavefunction was DFT,
-- and a correlation is specified, the correlation part will take precendence over DFT. It is tried
-- to make sure that a relaxed density is available for correlation methods. If this is not
-- availabel in Psi4 (and it will crash), the method is also simply not suitable for ONIOM.
psi4SerialiseCall :: (MonadInput m, MonadWriter Text m) => m ()
psi4SerialiseCall = do
  prefix <- getPrefix
  hamiltonian <- getQCHamiltonian
  task <- getTask
  let corr = hamiltonian ^. #corr
      ref = hamiltonian ^. #ref

  -- Call the QC method
  case (ref, corr) of
    (RKS DFT {..}, Nothing) -> tellN $ "o, wfn = " <> taskString task <> "(\"" <> functional <> "\", return_wfn = True)"
    (UKS DFT {..}, Nothing) -> tellN $ "o, wfn = " <> taskString task <> "(\"" <> functional <> "\", return_wfn = True)"
    (_, Nothing) -> tellN $ "o, wfn = " <> taskString task <> "(\"scf\", return_wfn = True)"
    (RKS DFT {functional}, Just Correlation {..}) -> do
      tellN $ "oRef, wfnRef = " <> "o, wfn = " <> taskString task <> "(\"" <> functional <> "\", return_wfn = True)"
      tellN $ "o, wfn = " <> taskString task <> "(\"" <> method <> "\", ref_wfn = wfnRef, return_wfn = True)"
    (UKS DFT {functional}, Just Correlation {..}) -> do
      tellN $ "oRef, wfnRef = " <> "o, wfn = " <> taskString task <> "(\"" <> functional <> "\", return_wfn = True)"
      tellN $ "o, wfn = " <> taskString task <> "(\"" <> method <> "\", ref_wfn = wfnRef, return_wfn = True)"
    (_, Just Correlation {..}) -> tellN $ "o, wfn = " <> taskString task <> "(\"" <> method <> "\", return_wfn = True)"

  -- Write the wavefunction with relaxed 1PDM to FCHK and a hessian to a file, if requested.
  tellN $ "fchk(wfn, \"" <> Text.pack prefix <> ".fchk\")"
  when (task == WTHessian) . tellN $ "np.array(o)"
  where
    -- Ensures that the density matrix is always relaxed.
    taskString t = case t of
      WTEnergy -> "gradient"
      WTGradient -> "gradient"
      WTHessian -> "hessian"

----------------------------------------------------------------------------------------------------

-- TODO - Serialise embedding charges

-- | Serialise the turbomole information into a control file.
serialiseTurbomole :: (MonadInput m, MonadWriter Text m) => m ()
serialiseTurbomole = do
  -- Always required fields
  tellN "$symmetry c1"
  tellN "$energy file=energy"
  tellN "$grad file=gradient"
  tellN "$noproj"
  tellN "$nprhessian file=nprhessian"

  tmSerialiseBasis
  tmSerialiseCoords
  tmSerialiseSCF
  tmSerialiseRI
  tmSerialiseRefWfn
  tmSerialiseCorr
  tmSerialiseExcitations
  tmSerialiseMemory

  -- Other fields to print verbatim
  getQCHamiltonian >>= \hamil -> forM_ (hamil ^. #other) (mapM_ tellN)

  -- Multipoles at the end. They are potentially very long ...
  tmSerialiseMultipoles

  -- End the control file
  tellN "$end"

-- | Serialise basis set information for Turbomole. This constitutes the @$atoms@ block of a control
-- file.
tmSerialiseBasis :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseBasis = do
  basisSets <- getQCHamiltonian <&> (^. #basis)
  tellN "$atoms"
  tellN $ "  basis = " <> (basisSets ^. #basis)
  tellN . optKW "  jbas  = " $ basisSets ^. #jbas
  tellN . optKW "  jkbas = " $ basisSets ^. #jkbas
  tellN . optKW "  ecp   = " $ basisSets ^. #ecp
  tellN . optKW "  cbas  = " $ basisSets ^. #cbas
  tellN . optKW "  cabs  = " $ basisSets ^. #cabs
  forM_ (basisSets ^. #other) (mapM_ tellN)

-- | Serialise various SCF settings. Forms multiple blocks.
tmSerialiseSCF :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseSCF = do
  scfSettings <- fromMaybe def <$> (getQCHamiltonian <&> (^. #scf))
  tellN $ "$scfiterlimit " <> tshow (scfSettings ^. #iter)
  tellN $ "$scfconv " <> tshow (scfSettings ^. #conv)
  case scfSettings ^. #damp of
    Nothing -> return ()
    Just Damp {..} -> tellN $ "$scfdamp start=" <> tshow start <> " step=" <> tshow step <> " min=" <> tshow lower
  tellN . optKW "$scforbitalshift automatic " $ (scfSettings ^. #shift)

-- | Write the coordinates inline to turbomole.
tmSerialiseCoords :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseCoords = do
  tellN "$coord"
  getMolecule >>= coord >>= tell

-- | Serialise the multipoles to point charges in turbomole. Makes sure not to include selfenergy
-- in the energy and its derivatives and disables checks, that could mess up the multipoles.
tmSerialiseMultipoles :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseMultipoles = do
  mPoleRep <- getMolecule >>= turbomoleMultipoleRep
  tellN "$point_charges thr=0 nocheck"
  tell mPoleRep

-- | Serialise the reference wavefunction to the control file.
tmSerialiseRefWfn :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseRefWfn = do
  atoms <- getMolecule <&> (^. #atoms)
  let realAtoms = IntMap.filter (not . isDummy) atoms
  charge <- getCharge
  mult <- getMult
  getQCHamiltonian <&> (^. #ref) >>= \ref -> case ref of
    RHF -> commonClosed realAtoms charge
    UHF -> tellN "$uhf" >> commonOpen realAtoms charge mult
    RKS dft -> commonClosed realAtoms charge >> commonDFT dft
    UKS dft -> commonOpen realAtoms charge mult >> commonDFT dft
  where
    nElectrons atoms charge = RIO.foldl' (\acc a -> acc + 1 + fromEnum (a ^. #element)) (- charge) atoms
    nExcEl mult = fromIntegral @Int $ mult - 1
    nClosed atoms charge mult = (nElectrons atoms charge - nExcEl mult) `div` 2
    commonClosed atoms charge = do
      tellN "$closed shells"
      tellN $ "  a  1-" <> tshow (nClosed atoms charge 1) <> " ( 2 )"
    commonOpen atoms charge mult = do
      tellN "$alpha shells"
      tellN $ "  a   1-" <> tshow (nClosed atoms charge mult + nExcEl mult) <> " ( 1 )"
      tellN "$beta shells"
      tellN $ "  a   1-" <> tshow (nClosed atoms charge mult) <> " ( 1 )"
    commonDFT DFT {..} = do
      tellN "$dft"
      tellN $ "  functional" <> functional
      tellN $ "  gridsize " <> fromMaybe "m4" grid
      case disp of
        Nothing -> return ()
        Just "d3" -> tellN "$disp3"
        Just "d3bj" -> tellN "$disp bj"
        Just "d4" -> tellN "$disp4"
        Just o -> tellN o

-- | Serialise RI settings.
tmSerialiseRI :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseRI =
  getQCHamiltonian <&> (^. #ri) >>= \ri -> case ri of
    Nothing -> return ()
    Just RIJ -> tellN "$rij"
    Just RIJK -> tellN "$rij" >> tellN "$rik"
    Just (OtherRI a) -> tellN a

-- | Serialise correlation of the wavefunction.
tmSerialiseCorr :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseCorr = do
  task <- getTask
  hamiltonian <- getQCHamiltonian
  let corr = hamiltonian ^. #corr
      exc = hamiltonian ^. #exc
  case corr of
    Nothing -> return ()
    Just Correlation {..} -> case corrModule of
      Nothing -> throwM . localExc $ "The correlation module must be specified"
      Just "pnoccsd" -> do
        tellN "$denconv 1.0e-7"
        tellN "$pnoccsd"
        tellN $ "  " <> method
        tellN . optKW "  maxiter=" $ iter
        case exc of
          Nothing -> return ()
          Just _ -> tellN "  prepno davidson"
        forM_ other (mapM_ tellN)
      Just "ricc" -> do
        tellN "$denconv 1.0e-7"
        tellN "$ricc2"
        tellN $ "  " <> method
        when (task == WTGradient) . tellN $ "  geoopt model=" <> method
        tellN . optKW "  maxiter=" $ iter
        forM_ other (mapM_ tellN)
      Just u -> throwM . localExc $ "Unkown correlation module " <> show u
  where
    localExc = WrapperGenericException "serialiseTurbomole"

-- | Serialise excitations block.
tmSerialiseExcitations :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseExcitations = do
  hamiltonian <- getQCHamiltonian
  let ref = hamiltonian ^. #ref
      corr = hamiltonian ^. #corr
      exc = hamiltonian ^. #exc
  case exc of
    Nothing -> return ()
    Just Excitations {..} -> case corr of
      -- TD-DFT or TD-HF
      Nothing -> case ref of
        RHF -> tellN "$scfinstab rpas"
        RKS _ -> tellN "$scfinstab rpas"
        UHF -> tellN "$scfinstab urpa"
        UKS _ -> tellN "$scfinstab urpa"
      Just _ -> do
        tellN "$excitations"
        tell "  irrep=a "
        case ref of
          RHF -> tell "multiplicity=1 "
          RKS _ -> tell "multiplicity=1 "
          _ -> return ()
        tellN $ "nexc=" <> tshow states

-- | Serialise the memory requirements for Turbomole
tmSerialiseMemory :: (MonadInput m, MonadWriter Text m) => m ()
tmSerialiseMemory = getMemory >>= \mem -> tellN $ "$maxcor " <> tshow mem <> " MiB per_core"
