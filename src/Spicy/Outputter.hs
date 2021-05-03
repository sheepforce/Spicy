-- |
-- Module      : Spicy.Outputter
-- Description : Facilties for generating structured output file.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
module Spicy.Outputter
  ( HasOutputter (..),
    Outputter (..),
    Verbosity (..),
    PrintVerbosity (..),
    PrintEvent (..),
    MotionEvent (..),
    StartEnd (..),
  )
where

import Data.Default
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Massiv.Array as Massiv
import qualified Data.Text.Lazy.Builder as TB
import Formatting hiding ((%))
import qualified Formatting as F
import Optics hiding (view)
import RIO hiding (Lens, Lens', Vector, lens, view, (%~), (.~), (^.), (^?))
import qualified RIO.HashSet as HashSet
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
  deriving (Eq, Ord)

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
loggingThread :: (HasOutputter env) => RIO env ()
loggingThread = do
  q <- view $ outputterL % #outChan
  f <- view $ outputterL % #outFile
  forever $ do
    msg <- atomically . readTBQueue $ q
    appendFileUtf8 f msg

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

----------------------------------------------------------------------------------------------------

-- | Parameter type to give the line length.
lw :: Int
lw = 100

-- | Parameter type, giving the width of an element to print.
ew :: Int
ew = 18

-- | Default formatter for floating point numbers.
nf :: (Buildable a, Real a) => Format r (a -> r)
nf = left ew ' ' F.%. fixed (ew - 8)

-- | String for non-available data.
nAv :: IsString a => a
nAv = "(Not Available)"

-- | Printer for ONIOM energy of the full tree.
-- oniomEnergy :: SpicyLog
oniomEnergy :: HasDirectMolecule env => SpicyLog env
oniomEnergy = do
  me <- view $ moleculeDirectL % #energyDerivatives % #energy
  case me of
    Nothing -> tell $ bformat (builder F.% builder) header nAv
    Just e -> tell $ bformat (builder F.% nf) header e
  where
    header =
      "@ Energy (ONIOM) / Hartree\n\
      \--------------------------\n"

-- | Printer for the ONIOM gradient of the full tree.
oniomGradient :: HasDirectMolecule env => SpicyLog env
oniomGradient = do
  msg <- view $ moleculeDirectL % #energyDerivatives % #gradient
  atoms <- view (moleculeDirectL % #atoms)
  let mg = msg >>= atomGradAssoc atoms . getVectorS
  case mg of
    Nothing -> tell $ bformat (builder F.% builder) header nAv
    Just g -> tell $ bformat (builder F.% builder) header (tableHeader <> tableContent g)
  where
    header =
      "@ Gradient (ONIOM) / (Hartree / Angstrom)\n\
      \-----------------------------------------\n"

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

-- | Log string constructor monad for molecular information.
spicyMolLog ::
  ( HasDirectMolecule env,
    HasPrintVerbosity env
  ) =>
  HashSet PrintEvent ->
  SpicyLog env
spicyMolLog pe = do
  pv <- view printVerbosityL
  when (doLog pv #oniomE) oniomEnergy
  when (doLog pv #oniomG) oniomGradient
  where
    doLog :: PrintVerbosity HashSet -> Lens' (PrintVerbosity HashSet) (HashSet PrintEvent) -> Bool
    doLog pv l =
      let pvt = pv ^. l
       in not . HashSet.null $ pvt `HashSet.intersection` pe
