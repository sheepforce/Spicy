{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Spicy.Outputter
-- Description : Facilties for generating structured output file.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
module Spicy.Outputter
  ( -- * Exposed Metadata
    spicyLogo,
    spicyLogoColour,
    versionInfo,

    -- * Section Fonts

    -- Generated with the ASCII art generator <https://patorjk.com/software/taag/#p=display&h=3&v=2&f=Slant&t=%C2%B5%20opt%0A>
    sep,
    txtInput,
    txtSetup,
    txtDirectOpt,
    txtMicroOpt,
    txtSinglePoint,
    txtMD,

    -- * Settings
    HasOutputter (..),
    Outputter (..),
    Verbosity (..),
    PrintVerbosity (..),
    HasPrintVerbosity (..),
    defPrintVerbosity,
    PrintEvent (..),
    MotionEvent (..),
    StartEnd (..),
    PrintEnv (..),
    getCurrPrintEvn,

    -- * Logging Facilities
    loggingThread,
    printSpicy,

    -- * Log Generators
    PrintTarget (..),
    SpicyLog,
    spicyLog,
    molID2OniomHumandID,
    printEnergy,
    printGradient,
    printHessian,
    printCoords,
    printTopology,
    printMultipoles,
    spicyLogMol,

    -- * Utility
    renderBuilder,
  )
where

import Data.Aeson
import Data.Default
import Data.FileEmbed
import Data.Foldable
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Massiv.Array as Massiv hiding (forM, mapM_)
import qualified Data.Text.Lazy.Builder as TB
import Data.Version (showVersion)
import Formatting hiding ((%))
import qualified Formatting as F
import Optics hiding (view)
import Paths_spicy (version)
import RIO hiding (Lens, Lens', Vector, lens, view, (%~), (.~), (^.), (^?))
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.Seq as Seq
import qualified RIO.Text as Text
import RIO.Writer
import Spicy.Common
import Spicy.Molecule
import qualified System.Path as Path

-- | The Spicy Logo as ASCII art.
spicyLogo :: Utf8Builder
spicyLogo = displayBytesUtf8 $(embedFile . Path.toString . Path.relFile $ "data/logo/spicy.ascii")

-- | The Spicy Logo as coloured ASCII art with ANSI colour codes.
spicyLogoColour :: ByteString
spicyLogoColour = $(embedFile . Path.toString . Path.relFile $ "data/logo/spicy.ansi")

-- | Get information from the build by TemplateHaskell to be able to show a reproducible version.
versionInfo :: Utf8Builder
versionInfo = displayShow . showVersion $ version

----------------------------------------------------------------------------------------------------

sep :: Utf8Builder
sep = display $ Text.replicate 100 "-" <> "\n"

txtInput :: Utf8Builder
txtInput = displayBytesUtf8 $(embedFile . Path.toString . Path.relFile $ "data/text/input.txt")

txtSetup :: Utf8Builder
txtSetup = displayBytesUtf8 $(embedFile . Path.toString . Path.relFile $ "data/text/setup.txt")

txtDirectOpt :: Utf8Builder
txtDirectOpt = displayBytesUtf8 $(embedFile . Path.toString . Path.relFile $ "data/text/directopt.txt")

txtMicroOpt :: Utf8Builder
txtMicroOpt = displayBytesUtf8 $(embedFile . Path.toString . Path.relFile $ "data/text/microopt.txt")

txtSinglePoint :: Utf8Builder
txtSinglePoint = displayBytesUtf8 $(embedFile . Path.toString . Path.relFile $ "data/text/singlepoint.txt")

txtMD :: Utf8Builder
txtMD = displayBytesUtf8 $(embedFile . Path.toString . Path.relFile $ "data/text/md.txt")

----------------------------------------------------------------------------------------------------

class HasOutputter env where
  outputterL :: Lens' env Outputter

data Outputter = Outputter
  { outChan :: TBQueue Utf8Builder,
    outFile :: Path.AbsRelFile,
    printVerbosity :: PrintVerbosity HashSet
  }

instance HasOutputter Outputter where
  outputterL = castOptic simple

instance (k ~ A_Lens, a ~ TBQueue Utf8Builder, b ~ a) => LabelOptic "outChan" k Outputter Outputter a b where
  labelOptic = lens outChan $ \s b -> s {outChan = b}

instance (k ~ A_Lens, a ~ Path.AbsRelFile, b ~ a) => LabelOptic "outFile" k Outputter Outputter a b where
  labelOptic = lens outFile $ \s b -> s {outFile = b}

instance (k ~ A_Lens, a ~ PrintVerbosity HashSet, b ~ a) => LabelOptic "printVerbosity" k Outputter Outputter a b where
  labelOptic = lens printVerbosity $ \s b -> s {printVerbosity = b}

----------------------------------------------------------------------------------------------------

-- | Abstract shortcuts to set define collections of print levels. Sets reasonable defaults.
data Verbosity
  = Low
  | Medium
  | High
  | Debug
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Verbosity

instance FromJSON Verbosity

-- | Environments, that provide print verbosity depending on the event.
class HasPrintVerbosity env where
  printVerbosityL :: Lens' env (PrintVerbosity HashSet)

-- | Information that can be printed and when to print.
data PrintVerbosity f = PrintVerbosity
  { -- | Energy of the full ONIOM tree
    oniomE :: f PrintEvent,
    -- | Gradient of the full ONIOM tree
    oniomG :: f PrintEvent,
    -- | Hessian of the full ONIOM tree
    oniomH :: f PrintEvent,
    -- | Coordinates of the full ONIOM tree
    oniomC :: f PrintEvent,
    -- | Bond topology on the full ONIOM tree
    oniomT :: f PrintEvent,
    -- | ONIOM multipoles of the real system
    oniomMP :: f PrintEvent,
    -- | High and low level energy of a layer
    layerE :: f PrintEvent,
    -- | High and low level gradient of a layer
    layerG :: f PrintEvent,
    -- | High and low level hessian of a layer
    layerH :: f PrintEvent,
    -- | Coordinates of a layer
    layerC :: f PrintEvent,
    -- | Bond topology on the full ONIOM tree
    layerT :: f PrintEvent,
    -- | High and low level multipole moments of a layer
    layerMP :: f PrintEvent,
    -- | Optimisation information
    opt :: f PrintEvent,
    -- | MD information
    md :: f PrintEvent
  }

instance HasPrintVerbosity (PrintVerbosity HashSet) where
  printVerbosityL = castOptic simple

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "oniomE" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens oniomE $ \s b -> s {oniomE = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "oniomG" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens oniomG $ \s b -> s {oniomG = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "oniomH" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens oniomH $ \s b -> s {oniomH = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "oniomC" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens oniomC $ \s b -> s {oniomC = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "oniomT" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens oniomT $ \s b -> s {oniomT = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "oniomMP" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens oniomMP $ \s b -> s {oniomMP = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "layerE" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens layerE $ \s b -> s {layerE = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "layerG" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens layerG $ \s b -> s {layerG = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "layerH" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens layerH $ \s b -> s {layerH = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "layerC" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens layerC $ \s b -> s {layerC = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "layerT" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens layerT $ \s b -> s {layerT = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "layerMP" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens layerMP $ \s b -> s {layerMP = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "opt" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens opt $ \s b -> s {opt = b}

instance (k ~ A_Lens, a ~ f PrintEvent, b ~ a) => LabelOptic "md" k (PrintVerbosity f) (PrintVerbosity f) a b where
  labelOptic = lens md $ \s b -> s {md = b}

instance Default (PrintVerbosity HashSet) where
  def = defPrintVerbosity Medium

-- | Predefined print verbosities.
defPrintVerbosity :: Verbosity -> PrintVerbosity HashSet
defPrintVerbosity v = case v of
  Low ->
    PrintVerbosity
      { oniomE = f [Task End],
        oniomG = f [Task End],
        oniomH = f [Task End],
        oniomC = mempty,
        oniomT = mempty,
        oniomMP = mempty,
        layerE = mempty,
        layerG = mempty,
        layerH = mempty,
        layerC = mempty,
        layerT = mempty,
        layerMP = mempty,
        opt = f [Motion Macro],
        md = f [Motion Macro]
      }
  Medium ->
    PrintVerbosity
      { oniomE = f [Task End, FullTraversal],
        oniomG = f [Task End, FullTraversal],
        oniomH = f [Task End],
        oniomC = f [Spicy Start, Spicy End, Motion Macro],
        oniomT = f [Spicy Start],
        oniomMP = mempty,
        layerE = mempty,
        layerG = mempty,
        layerH = mempty,
        layerC = mempty,
        layerT = mempty,
        layerMP = mempty,
        opt = f [Motion Micro, Motion Macro],
        md = f [Motion Macro]
      }
  High ->
    PrintVerbosity
      { oniomE = f [Always],
        oniomG = f [Always],
        oniomH = f [Always],
        oniomC = f [Spicy Start, Spicy End, Motion Macro],
        oniomT = f [Spicy Start],
        oniomMP = f [Spicy End],
        layerE = f [Motion Micro],
        layerG = f [Motion Micro],
        layerH = mempty,
        layerC = mempty,
        layerT = mempty,
        layerMP = f [FullTraversal],
        opt = f [Motion Micro, Motion Macro],
        md = f [Motion Macro]
      }
  Debug ->
    PrintVerbosity
      { oniomE = f [Always],
        oniomG = f [Always],
        oniomH = f [Always],
        oniomC = f [Always],
        oniomT = f [Spicy Start],
        oniomMP = f [Motion Macro],
        layerE = f [FullTraversal, Motion Micro],
        layerG = f [FullTraversal, Motion Micro],
        layerH = f [FullTraversal],
        layerC = f [Always],
        layerT = f [Spicy Start],
        layerMP = f [Always],
        opt = f [Motion Micro, Motion Macro],
        md = f [Motion Macro]
      }
  where
    f :: (Hashable a, Eq a) => [a] -> HashSet a
    f = HashSet.fromList

----------------------------------------------------------------------------------------------------

-- | Events when to something.
data PrintEvent
  = -- | Always print.
    Always
  | -- | On motions of anything in the system
    Motion MotionEvent
  | -- | On any full traversal of the ONIOM tree
    FullTraversal
  | -- | When a 'Task' starts or ends
    Task StartEnd
  | -- | Final information before Spicy exits
    Spicy StartEnd
  deriving (Show, Eq, Generic, Hashable)

data MotionEvent
  = -- | The full ONIOM tree took a complete motion as a whole.
    Macro
  | -- | Anything in the ONIOM tree moved.
    Micro
  deriving (Show, Eq, Generic, Hashable)

-- | Beginning and start of tasks.
data StartEnd
  = Start
  | End
  deriving (Show, Eq, Generic, Hashable)

-- | A print environemt, that contains necessary data.
data PrintEnv = PrintEnv
  { mol :: Molecule,
    printV :: PrintVerbosity HashSet
  }

instance HasDirectMolecule PrintEnv where
  moleculeDirectL = #mol

instance HasPrintVerbosity PrintEnv where
  printVerbosityL = #printV

instance (k ~ A_Lens, a ~ Molecule, b ~ a) => LabelOptic "mol" k PrintEnv PrintEnv a b where
  labelOptic = lens mol $ \s b -> s {mol = b}

instance (k ~ A_Lens, a ~ PrintVerbosity HashSet, b ~ a) => LabelOptic "printV" k PrintEnv PrintEnv a b where
  labelOptic = lens printV $ \s b -> s {printV = b}

-- | From the current runtime environment make a logging environment.
getCurrPrintEvn :: (HasMolecule env, HasOutputter env) => RIO env PrintEnv
getCurrPrintEvn = do
  mol <- view moleculeL >>= readTVarIO
  printV <- view $ outputterL % #printVerbosity
  return PrintEnv {mol = mol, printV = printV}

----------------------------------------------------------------------------------------------------

-- | A simple logging thread, that listens on a message queue forever. All messages from the message
-- queue are written to an output file.
loggingThread :: (HasOutputter env, MonadReader env m, MonadIO m) => m ()
loggingThread = do
  q <- view $ outputterL % #outChan
  f <- view $ outputterL % #outFile
  forever $ do
    msg <- atomically . readTBQueue $ q
    appendFileUtf8 f msg

-- | Log function for arbitrary content in Spicy. Meant to be the content of the output file.
printSpicy :: (HasOutputter env) => Utf8Builder -> RIO env ()
printSpicy t = do
  q <- view $ outputterL % #outChan
  atomically . writeTBQueue q $ t

{-
====================================================================================================
-}

-- $prettyPrinters

-- | A monad transformer, that allows to read an environment and log information about the
-- environment
type LogM r w = WriterT w (Reader r) ()

type SpicyLog r = LogM r TB.Builder

-- | Runs the 'LogM' monad with a given environment to read from.
execLogM :: r -> LogM r w -> w
execLogM r lm = snd . (\rm -> runReader rm r) . runWriterT $ lm

-- | Runs the Spicy Logger monad to obtain a 'Utf8Builder' log value.
spicyLog :: r -> SpicyLog r -> TB.Builder
spicyLog r lm = execLogM r lm

----------------------------------------------------------------------------------------------------

-- | Parameter type to give the line length.
lw :: Int
lw = 100

-- | Parameter type, giving the width of an element to print.
ew :: Int
ew = 18

-- | Default formatter for floating point numbers.
nf :: (Real a) => Format r (a -> r)
nf = left ew ' ' F.%. fixed (ew - 8)

-- | String for non-available data.
nAv :: IsString a => a
nAv = "(Not Available)\n"

-- | Wether to print full ONIOM tree properties or per layer properties.
data PrintTarget = ONIOM | Layer MolID | All

-- | Removes the dummy atoms from the molecule, so that printers only print real atoms.
removeDummy :: (HasDirectMolecule env, MonadReader env m) => m a -> m a
removeDummy = local (& moleculeDirectL %~ molMap rmDummies)
  where
    rmDummies :: Molecule -> Molecule
    rmDummies m = m & #atoms %~ IntMap.filter (\a -> not $ a ^. #isDummy)

-- | Printer for a molecule energy.
printEnergy ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printEnergy pt = do
  tell "\n\n"
  case pt of
    ONIOM -> do
      me <- view $ moleculeDirectL % #energyDerivatives % #energy
      case me of
        Nothing -> tell $ oHeader <> nAv
        Just e -> tell $ bformat (builder F.% nf F.% "\n") oHeader e
    Layer i -> do
      mol <- view moleculeDirectL
      let layer = mol ^? molIDLensGen i
          e = layer ^? _Just % #energyDerivatives % #energy % _Just
          el = layer ^? _Just % #calcContext % ix (ONIOMKey Inherited) % #output % _Just % #energyDerivatives % #energy % _Just
          eh = layer ^? _Just % #calcContext % ix (ONIOMKey Original) % #output % _Just % #energyDerivatives % #energy % _Just
      tell $ lHeader i
      tell $ "  ONIOM SubTree -> " <> fromMaybe nAv (bformat (nf F.% "\n") <$> e)
      tell $ "  High Level    -> " <> fromMaybe nAv (bformat (nf F.% "\n") <$> eh)
      tell $ "  Low Level     -> " <> fromMaybe nAv (bformat (nf F.% "\n") <$> el)
    All -> do
      mol <- view moleculeDirectL
      printEnergy ONIOM
      mapM_ printEnergy . fmap Layer $ getAllMolIDsHierarchically mol
  where
    oHeader =
      "@ Energy (ONIOM) / Hartree\n\
      \--------------------------\n"
    lHeader i =
      let txt = "@ Energy (Layer " <> molID2OniomHumanID i <> " ) / Hartree"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

-- | Printer for the ONIOM gradient of the full tree.
printGradient ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printGradient pt = removeDummy $ do
  tell "\n\n"
  case pt of
    ONIOM -> do
      msg <- view $ moleculeDirectL % #energyDerivatives % #gradient
      atoms <- view $ moleculeDirectL % #atoms
      let mg = atomGradAssoc atoms . getVectorS =<< msg
      case mg of
        Nothing -> tell $ oHeader <> nAv
        Just g -> tell $ oHeader <> tableHeader <> tableContent g
    Layer i -> do
      mol <- view moleculeDirectL
      let layer = mol ^? molIDLensGen i
          atoms = layer ^? _Just % #atoms
          g = layer ^? _Just % #energyDerivatives % #gradient % _Just
          aG = join $ atomGradAssoc <$> atoms <*> (getVectorS <$> g)
          gl = layer ^? _Just % #calcContext % ix (ONIOMKey Inherited) % #output % _Just % #energyDerivatives % #gradient % _Just
          aGL = join $ atomGradAssoc <$> atoms <*> (getVectorS <$> gl)
          gh = layer ^? _Just % #calcContext % ix (ONIOMKey Original) % #output % _Just % #energyDerivatives % #gradient % _Just
          aGH = join $ atomGradAssoc <$> atoms <*> (getVectorS <$> gh)
      tell $ lHeader i
      tell $ "  ONIOM SubTree ->\n" <> fromMaybe nAv (tableContent <$> aG)
      tell $ "  High Level    ->\n" <> fromMaybe nAv (tableContent <$> aGL)
      tell $ "  Low Level     ->\n" <> fromMaybe nAv (tableContent <$> aGH)
    All -> do
      mol <- view moleculeDirectL
      printGradient ONIOM
      mapM_ printGradient . fmap Layer $ getAllMolIDsHierarchically mol
  where
    oHeader =
      "@ Gradient (ONIOM) / (Hartree / Angstrom)\n\
      \-----------------------------------------\n"

    lHeader i =
      let txt = "@ Gradient (Layer " <> molID2OniomHumanID i <> ") / (Hartree / Angstrom)"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

    -- Header for the table of gradients.
    tableHeader =
      let hf = center ew ' ' F.%. builder
          headerRow = bformat (hf F.% " | " F.% hf F.% " | " F.% hf F.% " | " F.% hf F.% "\n") "Atom (IX)" "X" "Y" "Z"
          rule =
            TB.fromText $
              Text.replicate (ew + 1) "-"
                <> "+"
                <> Text.replicate (ew + 2) "-"
                <> "+"
                <> Text.replicate (ew + 2) "-"
                <> "+"
                <> Text.replicate (ew + 1) "-"
                <> "\n"
       in headerRow <> rule

    -- Atom associated gradients.
    atomGradAssoc :: MonadThrow m => IntMap Atom -> Vector S Double -> m (IntMap (Atom, Vector M Double))
    atomGradAssoc atoms g = do
      let aIx = IntMap.keysSet atoms
      gR <- Massiv.toList . outerSlices <$> resizeM (Sz $ IntMap.size atoms :. 3) g
      let ixGrad = IntMap.fromAscList $ RIO.zip (IntSet.toAscList aIx) gR
          assoc = IntMap.intersectionWith (,) atoms ixGrad
      return assoc

    -- Gradient table body
    tableContent agMap =
      let af = right ew ' ' F.%. (right 4 ' ' F.%. string) F.% ("(" F.% (left 7 ' ' F.%. int) F.% ")")
       in IntMap.foldlWithKey'
            ( \acc i (a, grad) ->
                let l =
                      bformat
                        (af F.% " | " F.% nf F.% " | " F.% nf F.% " | " F.% nf F.% "\n")
                        (show $ a ^. #element)
                        i
                        (grad Massiv.! 0)
                        (grad Massiv.! 1)
                        (grad Massiv.! 2)
                 in acc <> l
            )
            mempty
            agMap

-- | Printer for Hessians.
printHessian ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printHessian pt = removeDummy $ do
  tell "\n\n"
  case pt of
    ONIOM -> do
      msh <- view $ moleculeDirectL % #energyDerivatives % #hessian
      atoms <- view $ moleculeDirectL % #atoms
      let mh = getMatrixS <$> msh
      tell oHeader
      case mh of
        Nothing -> tell nAv
        Just h -> tell $ fromMaybe mempty (tableContent atoms h)
    Layer i -> do
      mol <- view moleculeDirectL
      let atoms = mol ^. #atoms
          layer = mol ^? molIDLensGen i
          h = layer ^? _Just % #energyDerivatives % #hessian % _Just
          hl = layer ^? _Just % #calcContext % ix (ONIOMKey Inherited) % #output % _Just % #energyDerivatives % #hessian % _Just
          hh = layer ^? _Just % #calcContext % ix (ONIOMKey Original) % #output % _Just % #energyDerivatives % #hessian % _Just
      tell $ lHeader i
      tell $ "  ONIOM SubTree ->\n" <> fromMaybe nAv (h >>= \(MatrixS h') -> tableContent atoms h')
      tell $ "  High Level    ->\n" <> fromMaybe nAv (hh >>= \(MatrixS hh') -> tableContent atoms hh')
      tell $ "  Low Level     ->\n" <> fromMaybe nAv (hl >>= \(MatrixS hl') -> tableContent atoms hl')
    All -> do
      mol <- view moleculeDirectL
      printHessian ONIOM
      mapM_ (printHessian . Layer) $ getAllMolIDsHierarchically mol
  where
    oHeader =
      "@ Hessian (ONIOM) / (Hartree / Angstrom^2)\n\
      \------------------------------------------\n"

    lHeader i =
      let txt = "@ Hessian (Layer " <> molID2OniomHumanID i <> ") / (Hartree / Angstrom^2)"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

    -- Atom and cartesian components to print as left column and as chunks as header row.
    cmpnts :: IntMap Atom -> Vector B TB.Builder
    cmpnts atoms =
      compute @B . sconcat . fmap (\(x, y, z) -> Massiv.sfromList [x, y, z]) . IntMap.elems
        . IntMap.map
          ( \a ->
              let e = a ^. #element
               in (bShow e <> "(x)", bShow e <> "(y)", bShow e <> "(z)")
          )
        $ atoms
      where
        bShow = fromString . show

    -- Top row printer chunks. Prints components header
    headerRow :: Source r Ix1 TB.Builder => Vector r TB.Builder -> TB.Builder
    headerRow chunk =
      bformat (center 7 ' ' F.%. builder F.% "\n") "Cmpnt"
        <> Massiv.foldMono (bformat $ center ew ' ' F.%. builder) chunk

    -- Columns of the Hessian matrix.
    tableContent :: MonadThrow m => IntMap Atom -> Matrix S Double -> m TB.Builder
    tableContent atoms hessian = do
      -- Make chunked print groups of hessian columns. Header missing.
      printValueGroup <- forM colsGroups $ \hc ->
        Massiv.fold . compute @B
          <$> concatM (Dim 1) [leftCmpntArr, hc, rightNewLineArr]
      -- Add the header to a print group and then combine everything into the final builder.
      let printGroup =
            Seq.zipWith
              (\h hc -> headerRow (flatten h) <> hc <> "\n")
              cmpntGroups
              printValueGroup
          table = foldl (<>) mempty printGroup
      return table
      where
        Sz (r :. _) = size hessian
        hessPrintV = compute @B . Massiv.map (bformat ("|" F.% nf)) $ hessian
        colsGroups = innerChunksOfN 3 hessPrintV
        rightNewLineArr = Massiv.makeArrayLinear @B Par (Sz $ r :. 1) (const "\n")
        leftCmpntArr = compute . Massiv.expandInner @Ix2 (Sz 1) const . cmpnts $ atoms
        cmpntGroups = innerChunksOfN @B 3 leftCmpntArr

-- | Coordinate printer.
printCoords ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printCoords pt = removeDummy $ do
  tell "\n\n"
  case pt of
    ONIOM -> do
      atoms <- view $ moleculeDirectL % #atoms
      tell $ oHeader <> table atoms
    Layer i -> do
      mol <- view moleculeDirectL
      let mAtoms = mol ^? molIDLensGen i % #atoms
      tell $ lHeader i <> fromMaybe mempty (table <$> mAtoms)
    All -> do
      mol <- view moleculeDirectL
      printCoords ONIOM
      mapM_ (printCoords . Layer) $ getAllMolIDsHierarchically mol
  where
    oHeader =
      "@ Coordinates (ONIOM) / Angstrom\n\
      \--------------------------------\n"

    lHeader i =
      let txt = "@ Coordinates (Layer " <> molID2OniomHumanID i <> ") / Angstrom"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

    tableHeader =
      let hf = center ew ' ' F.%. builder
          topRow = bformat (hf F.% " | " F.% hf F.% " | " F.% hf F.% " | " F.% hf F.% "\n") "Atom (IX)" "x" "y" "z"
          midRule =
            TB.fromText $
              Text.replicate (ew + 1) "-"
                <> "+"
                <> Text.replicate (ew + 2) "-"
                <> "+"
                <> Text.replicate (ew + 2) "-"
                <> "+"
                <> Text.replicate (ew + 1) "-"
                <> "\n"
       in topRow <> midRule

    table atoms =
      let af = right ew ' ' F.%. (right 4 ' ' F.%. string) F.% ("(" F.% (left 7 ' ' F.%. int) F.% ")")
       in IntMap.foldlWithKey'
            ( \acc i a ->
                let coords = getVectorS $ a ^. #coordinates
                    line =
                      bformat
                        (af F.% " | " F.% nf F.% " | " F.% nf F.% " | " F.% nf F.% "\n")
                        (show $ a ^. #element)
                        i
                        (coords Massiv.! 0)
                        (coords Massiv.! 1)
                        (coords Massiv.! 2)
                 in acc <> line
            )
            tableHeader
            atoms

-- | Print the bond matrix / topology.
printTopology ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printTopology pt = removeDummy $ do
  tell "\n\n"
  case pt of
    ONIOM -> do
      bondMat <- view $ moleculeDirectL % #bonds
      tell $ oHeader <> table bondMat <> "\n"
    Layer i -> do
      mol <- view moleculeDirectL
      let mBondMat = mol ^? molIDLensGen i % #bonds
      tell $ lHeader i <> fromMaybe mempty (table <$> mBondMat) <> "\n"
    All -> do
      mol <- view moleculeDirectL
      printTopology ONIOM
      mapM_ (printHessian . Layer) $ getAllMolIDsHierarchically mol
  where
    oHeader =
      "@ Bond Topology (ONIOM)\n\
      \-----------------------\n"

    lHeader i =
      let txt = "@ Bond Topology (Layer" <> molID2OniomHumanID i <> ")"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

    table bondMat =
      snd $
        HashMap.foldlWithKey'
          ( \(cnt, acc) (o, t) b ->
              let newCnt = cnt + 1
                  newAcc =
                    if b
                      then
                        acc <> bformat ("    " F.% oF F.% " - " F.% tF) o t
                          <> if (cnt + 1) `mod` 3 == 0 then "\n" else mempty
                      else acc
               in (newCnt, newAcc)
          )
          (0 :: Int, mempty)
          (makeBondMatUnidirectorial bondMat)
      where
        oF = left 8 ' ' F.%. int
        tF = right 8 ' ' F.%. int

-- | Multipole printer for a given layer. Prints up to quadrupoles.
printMultipoles ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printMultipoles pt = removeDummy $ do
  tell "\n\n"
  case pt of
    ONIOM -> do
      atoms <- view $ moleculeDirectL % #atoms
      tell $ oHeader <> content atoms
    Layer i -> do
      mol <- view moleculeDirectL
      let mAtoms = mol ^? molIDLensGen i % #atoms
      tell $ lHeader i <> fromMaybe mempty (content <$> mAtoms)
    All -> do
      mol <- view moleculeDirectL
      printMultipoles ONIOM
      mapM_ (printMultipoles . Layer) $ getAllMolIDsHierarchically mol
  where
    oHeader =
      "@ Multipoles (ONIOM) / ea_0^k for rank k\n\
      \----------------------------------------\n"

    lHeader i =
      let txt = "\n\n@ Multipoles (Layer " <> molID2OniomHumanID i <> ") / ea_0^k for rank k"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

    content atoms = foldl' (\acc a -> acc <> atomPrinter a) mempty atoms

    atomPrinter :: Atom -> TB.Builder
    atomPrinter a =
      let atomHeader =
            bformat
              ("  " F.% (right 3 ' ' F.%. string) F.% "  x = " F.% nf F.% "  y = " F.% nf F.% "  z = " F.% nf F.% "\n")
              (show $ a ^. #element)
              (getVectorS (a ^. #coordinates) Massiv.! 0)
              (getVectorS (a ^. #coordinates) Massiv.! 1)
              (getVectorS (a ^. #coordinates) Massiv.! 2)
          monopole = do
            mp <- a ^? #multipoles % #monopole % _Just % #q00
            return $ lp <> vf "00" mp <> "\n"
          dipole = do
            dp <- a ^. #multipoles % #dipole
            let q10 = dp ^. #q10
                q11c = dp ^. #q11c
                q11s = dp ^. #q11s
                mag = sqrt $ q10 ** 2 + q11c ** 2 + q11s ** 2
            return $ mf 1 mag <> vf "10" q10 <> vf "11c" q11c <> vf "11s" q11s <> "\n"
          quadrupole = do
            qp <- a ^. #multipoles % #quadrupole
            let q20 = qp ^. #q20
                q21c = qp ^. #q21c
                q21s = qp ^. #q21s
                q22c = qp ^. #q22c
                q22s = qp ^. #q22s
                mag = sqrt $ q20 ** 2 + q21c ** 2 + q21s ** 2 + q22c ** 2 + q22s ** 2
            return $
              mf 2 mag <> vf "21c" q21c <> vf "21s" q21s <> "\n"
                <> lp
                <> vf "22c" q22c
                <> vf "22s" q22s
                <> "\n"
       in atomHeader <> fm monopole <> fm dipole <> fm quadrupole <> "\n"
      where
        fm :: Monoid a => Maybe a -> a
        fm = fromMaybe mempty

        lp :: TB.Builder
        lp = bformat (left 19 ' ' F.%. builder) mempty

        mf :: Real a => Int -> a -> TB.Builder
        mf k num = bformat ("  |Q" F.% int F.% "| = " F.% (left 10 ' ' F.%. fixed 6)) k num

        vf :: Real a => TB.Builder -> a -> TB.Builder
        vf sub num =
          bformat
            ("  Q" F.% (right 3 ' ' F.%. builder) F.% " = " F.% (left 10 ' ' F.%. fixed 6))
            sub
            num

-- | Log string constructor monad for molecular information on full ONIOM trees.
spicyLogMol ::
  ( HasDirectMolecule env,
    HasPrintVerbosity env
  ) =>
  HashSet PrintEvent ->
  PrintTarget ->
  SpicyLog env
spicyLogMol pe pt = do
  pv <- view printVerbosityL

  -- Event printer.
  tell $ "\nnEvents: " <> (TB.fromText . tshow $ pe) <> "\n"

  -- Full ONIOM tree writers
  when (doLog pv #oniomE) $ printEnergy ONIOM
  when (doLog pv #oniomG) $ printGradient ONIOM
  when (doLog pv #oniomH) $ printHessian ONIOM
  when (doLog pv #oniomC) $ printCoords ONIOM
  when (doLog pv #oniomT) $ printTopology ONIOM
  when (doLog pv #oniomMP) $ printMultipoles ONIOM

  -- Layer writers
  case pt of
    ONIOM -> return ()
    Layer i -> do
      when (doLog pv #layerE) $ printEnergy (Layer i)
      when (doLog pv #layerG) $ printGradient (Layer i)
      when (doLog pv #layerH) $ printHessian (Layer i)
      when (doLog pv #layerC) $ printCoords (Layer i)
      when (doLog pv #layerT) $ printTopology (Layer i)
      when (doLog pv #layerMP) $ printMultipoles (Layer i)
    All -> do
      when (doLog pv #layerE) $ printEnergy All
      when (doLog pv #layerG) $ printGradient All
      when (doLog pv #layerH) $ printHessian All
      when (doLog pv #layerC) $ printCoords All
      when (doLog pv #layerT) $ printTopology All
      when (doLog pv #layerMP) $ printMultipoles All

  tell . TB.fromText . textDisplay $ sep
  where
    doLog :: PrintVerbosity HashSet -> Lens' (PrintVerbosity HashSet) (HashSet PrintEvent) -> Bool
    doLog pv l =
      let pvt = pv ^. l
       in not . HashSet.null $ pvt `HashSet.intersection` pe

----------------------------------------------------------------------------------------------------

-- | Render a text builder as Utf8Builder.
renderBuilder :: TB.Builder -> Utf8Builder
renderBuilder = display . TB.toLazyText
