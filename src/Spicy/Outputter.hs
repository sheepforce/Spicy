-- |
-- Module      : Spicy.Outputter
-- Description : Facilties for generating structured output file.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
module Spicy.Outputter
  ( -- * Settings
    HasOutputter (..),
    Outputter (..),
    Verbosity (..),
    PrintVerbosity (..),
    defPrintVerbosity,
    PrintEvent (..),
    MotionEvent (..),
    StartEnd (..),

    -- * Logging Facilities
    loggingThread,
    printSpicy,

    -- * Log Generators
    SpicyLog,
    spicyLog,
    spicyLogMol,
  )
where

import Data.Aeson
import Data.Default
import Data.Foldable
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Massiv.Array as Massiv hiding (forM)
import qualified Data.Text.Lazy.Builder as TB
import Formatting hiding ((%))
import qualified Formatting as F
import Optics hiding (view)
import RIO hiding (Lens, Lens', Vector, lens, view, (%~), (.~), (^.), (^?))
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.Partial as RIO'
import qualified RIO.Seq as Seq
import qualified RIO.Text as Text
import RIO.Writer
import Spicy.Common
import Spicy.Molecule
import qualified System.Path as Path

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
        oniomC = f [Spicy Start, Spicy End],
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
        oniomC = f [Spicy Start, Spicy End],
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
        layerE = f [Motion Micro],
        layerG = f [Motion Micro],
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
  deriving (Eq, Generic, Hashable)

data MotionEvent
  = -- | The full ONIOM tree took a complete motion as a whole.
    Macro
  | -- | Anything in the ONIOM tree moved.
    Micro
  deriving (Eq, Generic, Hashable)

-- | Beginning and start of tasks.
data StartEnd
  = Start
  | End
  deriving (Eq, Generic, Hashable)

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
nAv = "(Not Available)"

-- | Wether to print full ONIOM tree properties or per layer properties.
data PrintTarget = ONIOM | Layer MolID

-- | Conversion of a molecular ID into the human readable version.
molID2OniomHumandID :: MolID -> Text
molID2OniomHumandID Seq.Empty = "0"
molID2OniomHumandID molID =
  let depth = Seq.length molID
      idTree =
        foldr'
          ( \currentID textAcc ->
              let offSet = fromEnum 'A'
                  idLetter = RIO'.toEnum $ currentID + offSet
               in textAcc `Text.snoc` idLetter
          )
          (tShow depth)
          molID
   in Text.pack . show $ idTree

-- | Printer for a molecule energy.
printEnergy ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printEnergy pt = case pt of
  ONIOM -> do
    me <- view $ moleculeDirectL % #energyDerivatives % #energy
    case me of
      Nothing -> tell $ oHeader <> nAv
      Just e -> tell $ bformat (builder F.% nf) oHeader e
  Layer i -> do
    mol <- view moleculeDirectL
    let mes :: Maybe (Double, Double, Double) = do
          layer <- mol ^? molIDLensGen i
          e <- layer ^. #energyDerivatives % #energy
          el <- layer ^? #calcContext % ix (ONIOMKey Inherited) % #output % _Just % #energyDerivatives % #energy % _Just
          eh <- layer ^? #calcContext % ix (ONIOMKey Original) % #output % _Just % #energyDerivatives % #energy % _Just
          return (e, el, eh)
    case mes of
      Nothing -> tell $ lHeader i <> nAv
      Just (e, el, eh) ->
        tell $
          lHeader i
            <> bformat ("  ONIOM SubTree -> " F.% nf F.% "\n") e
            <> bformat ("  High Level    -> " F.% nf F.% "\n") eh
            <> bformat ("  Low Level     -> " F.% nf F.% "\n") el
  where
    oHeader =
      "@ Energy (ONIOM) / Hartree\n\
      \--------------------------\n"
    lHeader i =
      let txt = "@ Energy (Layer " <> molID2OniomHumandID i <> " ) / Hartree"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

-- | Printer for the ONIOM gradient of the full tree.
printGradient ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printGradient pt = case pt of
  ONIOM -> do
    msg <- view $ moleculeDirectL % #energyDerivatives % #gradient
    atoms <- view $ moleculeDirectL % #atoms
    let mg = atomGradAssoc atoms . getVectorS =<< msg
    case mg of
      Nothing -> tell $ oHeader <> nAv
      Just g -> tell $ oHeader <> tableHeader <> tableContent g
  Layer i -> do
    mol <- view moleculeDirectL
    let atoms = mol ^. #atoms
        mgs = do
          layer <- mol ^? molIDLensGen i
          VectorS g <- layer ^. #energyDerivatives % #gradient
          VectorS gl <- layer ^? #calcContext % ix (ONIOMKey Inherited) % #output % _Just % #energyDerivatives % #gradient % _Just
          VectorS gh <- layer ^? #calcContext % ix (ONIOMKey Original) % #output % _Just % #energyDerivatives % #gradient % _Just
          aG <- atomGradAssoc atoms g
          aGL <- atomGradAssoc atoms gl
          aGH <- atomGradAssoc atoms gh
          return (aG, aGL, aGH)
    case mgs of
      Nothing -> tell $ lHeader i <> nAv
      Just (g, gl, gh) -> do
        tell $
          lHeader i
            <> ("ONIOM SubTree ->\n" <> tableHeader <> tableContent g)
            <> ("High Level    ->\n" <> tableHeader <> tableContent gh)
            <> ("Low Level     ->\n" <> tableHeader <> tableContent gl)
  where
    oHeader =
      "@ Gradient (ONIOM) / (Hartree / Angstrom)\n\
      \-----------------------------------------\n"

    lHeader i =
      let txt = "@ Gradient (Layer " <> molID2OniomHumandID i <> ") / (Hartree / Angstrom)"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

    -- Header for the table of gradients.
    tableHeader =
      let hf = center ew ' ' F.%. builder
       in bformat (hf F.% " | " F.% hf F.% " | " F.% hf F.% " | " F.% hf F.% "\n") "Atom (IX)" "X" "Y" "Z"

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
printHessian pt = case pt of
  ONIOM -> do
    msh <- view $ moleculeDirectL % #energyDerivatives % #hessian
    atoms <- view $ moleculeDirectL % #atoms
    let mh = getMatrixS <$> msh
    case mh of
      Nothing -> tell $ oHeader <> nAv
      Just h -> tell $ oHeader <> fromMaybe mempty (tableContent atoms h)
  Layer i -> do
    mol <- view moleculeDirectL
    let atoms = mol ^. #atoms
        mhs = do
          layer <- mol ^? molIDLensGen i
          MatrixS h <- layer ^. #energyDerivatives % #hessian
          MatrixS hl <- layer ^? #calcContext % ix (ONIOMKey Inherited) % #output % _Just % #energyDerivatives % #hessian % _Just
          MatrixS hh <- layer ^? #calcContext % ix (ONIOMKey Original) % #output % _Just % #energyDerivatives % #hessian % _Just
          return (h, hl, hh)
    case mhs of
      Nothing -> tell $ lHeader i <> nAv
      Just (h, hl, hh) ->
        tell $
          lHeader i
            <> ("ONIOM SubTree ->\n" <> fromMaybe mempty (tableContent atoms h))
            <> ("High Level    ->\n" <> fromMaybe mempty (tableContent atoms hh))
            <> ("Low Level     ->\n" <> fromMaybe mempty (tableContent atoms hl))
  where
    oHeader =
      "@ Hessian (ONIOM) / (Hartree / Angstrom^2)\n\
      \------------------------------------------\n"

    lHeader i =
      let txt = "@ Hessian (Layer " <> molID2OniomHumandID i <> ") / (Hartree / Angstrom^2)"
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
printCoords pt = case pt of
  ONIOM -> do
    atoms <- view $ moleculeDirectL % #atoms
    tell $ oHeader <> table atoms
  Layer i -> do
    mol <- view moleculeDirectL
    let mAtoms = mol ^? molIDLensGen i % #atoms
    tell $ lHeader i <> fromMaybe mempty (table <$> mAtoms)
  where
    oHeader =
      "@ Coordinates (ONIOM) / Angstrom\n\
      \------------------------\n"

    lHeader i =
      let txt = "@ Coordinates (Layer " <> molID2OniomHumandID i <> ") / Angstrom"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

    tableHeader =
      let hf = center ew ' ' F.%. builder
       in bformat (hf F.% " | " F.% hf F.% " | " F.% hf F.% " | " F.% hf) "Atom (IX)" "x" "y" "z"

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
printTopology pt = case pt of
  ONIOM -> do
    bondMat <- view $ moleculeDirectL % #bonds
    tell $ oHeader <> table bondMat
  Layer i -> do
    mol <- view moleculeDirectL
    let mBondMat = mol ^? molIDLensGen i % #bonds
    tell $ lHeader i <> fromMaybe mempty (table <$> mBondMat)
  where
    oHeader =
      "@ Bond Topology (ONIOM)\n\
      \-----------------------\n"

    lHeader i =
      let txt = "@ Bond Topology (Layer" <> molID2OniomHumandID i <> ")"
          line = Text.replicate (Text.length txt) "-"
       in TB.fromText . Text.unlines $ [txt, line]

    table bondMat =
      HashMap.foldlWithKey'
        ( \acc (o, t) b ->
            if b
              then acc <> bformat ("    " F.% oF F.% " - " F.% tF) o t
              else acc
        )
        mempty
        (makeBondMatUnidirectorial bondMat)
      where
        oF = left 8 ' ' F.%. int
        tF = right 8 ' ' F.%. int

-- | Multipole printer for a given layer. Prints up to quadrupoles.
printMultipoles ::
  (HasDirectMolecule env, MonadReader env m, MonadWriter TB.Builder m) =>
  PrintTarget ->
  m ()
printMultipoles pt = case pt of
  ONIOM -> do
    atoms <- view $ moleculeDirectL % #atoms
    tell $ oHeader <> content atoms
  Layer i -> do
    mol <- view moleculeDirectL
    let mAtoms = mol ^? molIDLensGen i % #atoms
    tell $ lHeader i <> fromMaybe mempty (content <$> mAtoms)
  where
    oHeader =
      "@ Multipoles (ONIOM) / ea_0^k for rank k\n\
      \----------------------------------------\n"

    lHeader i =
      let txt = "@ Multipoles (Layer " <> molID2OniomHumandID i <> ") / ea_0^k for rank k"
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
  Maybe MolID ->
  SpicyLog env
spicyLogMol pe mi = do
  pv <- view printVerbosityL

  -- Full ONIOM tree writers
  when (doLog pv #oniomE) $ printEnergy ONIOM
  when (doLog pv #oniomG) $ printGradient ONIOM
  when (doLog pv #oniomH) $ printHessian ONIOM
  when (doLog pv #oniomC) $ printCoords ONIOM
  when (doLog pv #oniomT) $ printTopology ONIOM
  when (doLog pv #oniomMP) $ printMultipoles ONIOM

  -- Layer writers
  case mi of
    Nothing -> return ()
    Just i -> do
      when (doLog pv #layerE) $ printEnergy (Layer i)
      when (doLog pv #layerG) $ printGradient (Layer i)
      when (doLog pv #layerH) $ printHessian (Layer i)
      when (doLog pv #layerC) $ printCoords (Layer i)
      when (doLog pv #layerT) $ printTopology (Layer i)
      when (doLog pv #layerMP) $ printMultipoles (Layer i)
  where
    doLog :: PrintVerbosity HashSet -> Lens' (PrintVerbosity HashSet) (HashSet PrintEvent) -> Bool
    doLog pv l =
      let pvt = pv ^. l
       in not . HashSet.null $ pvt `HashSet.intersection` pe
