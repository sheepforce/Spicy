-- |
-- Module      : Spicy.ONIOM.Layout
-- Description : Layouting the molecule for ONIOM calculations.
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module takes care of initial preparation of the 'Molecule' type from a simple structured input
-- molecule to the ONIOM setup used in subsequent steps.
module Spicy.ONIOM.Layout
  ( mcOniomNLayout,
  )
where

import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Network.Socket as Net
import Optics hiding (Empty, view)
import RIO hiding
  ( Lens',
    view,
    (%~),
    (.~),
    (^.),
    (^?),
  )
import qualified RIO.Map as Map
import RIO.Seq (Seq (..))
import qualified RIO.Seq as Seq
import qualified RIO.Text as Text
import Spicy.Common
import Spicy.InputFile
import Spicy.Molecule
import Spicy.Outputter
import System.Path as Path

mcOniomNLayout :: (HasInputFile env, HasMolecule env) => RIO env ()
mcOniomNLayout = do
  -- Obtain environment
  inputFile <- view inputFileL
  molT <- view moleculeL
  originalMolecule <- readTVarIO molT

  -- Calculate initial values.
  maxAtomKey <- getMaxAtomIndex originalMolecule

  -- Layout the molecule by recursion through the theory layers. The real system is potentially
  -- not the full molecule but constructed as every other part. Therefore the result of this call
  -- will be the input molecule in the top level without
  (topMolWithRealChild, _) <-
    go
      (inputFile ^. #permanent)
      (inputFile ^. #scratch)
      maxAtomKey
      Empty
      originalMolecule
      (Seq.singleton $ inputFile ^. #model % #theoryLayer)

  realMol <- case IntMap.minView (topMolWithRealChild ^. #subMol) of
    Nothing -> throwM . localExc $ "A real layer should have been constructed but could not be found"
    Just (mol, _) -> return mol

  atomically . writeTVar molT $ realMol
  where
    localExc = SpicyIndirectionException "mcOniomNLayout"

-- | Create multiple centres below a given molecule.
go ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  -- | Permanent working directory as per input file.
  JDirPath ->
  -- | Scratch directory as per input file.
  JDirPath ->
  -- | Maximum atom key that has been used yet in the overall layout.
  Int ->
  -- | ID for which to build subsystems, aka the "i want children" layer ID.
  MolID ->
  -- | Molecule for which to build subsystems, aka the "i want children" layer ID.
  Molecule ->
  -- | Theorylayer description for children of the current layer.
  Seq TheoryLayer ->
  -- | The molecule after it got all its children.
  m (Molecule, Int)
go _ _ maxKey _ parentMol Empty = return (parentMol, maxKey)
go workDir scratchDir maxKey parentID parentMol (tl :<| rest) = do
  -- Insert a new child layer into the original molecule.
  molWithEmptyChild <- newSubLayer maxKey parentMol modelAtomSelection Nothing Nothing
  (childKey, childMolEmpty) <- case IntMap.maxViewWithKey (molWithEmptyChild ^. #subMol) of
    Nothing -> throwM $ localIndirExc "A child layer must have been created but could not be found."
    Just ((key, mol), _) -> return (key, mol)

  -- Build values for the child, that need context from above.
  workDirAbs <- liftIO . Path.genericMakeAbsoluteFromCwd . getDirPath $ workDir
  scratchDirAbs <- liftIO . Path.genericMakeAbsoluteFromCwd . getDirPath $ scratchDir
  opt <- optSettings <$> defIO
  let calcInheritOld = parentMol ^? #calcContext % ix (ONIOMKey Original) % #input
      childComment = textDisplay $ "Layer" <> molID2OniomHumandID (parentID |> childKey)
      childID = parentID |> childKey
      pysisSocket = dirByIDAndCalc scratchDirAbs childID (ONIOMKey Original) </> Path.relFile "pysis.socket"
      pysisDir = dirByIDAndCalc workDirAbs childID (ONIOMKey Original) </> Path.relDir "pysis"
  pysis <-
    defIO >>= \p ->
      return $
        p
          & #socketAddr .~ (Net.SockAddrUnix . Path.toString $ pysisSocket)
          & #workDir .~ Path.toAbsRel pysisDir
  let calcOriginal =
        CalcInput
          { task = WTEnergy,
            restartFile = Nothing,
            software = tl ^. #program,
            prefixName = Text.unpack $ childComment <> "_high",
            permaDir = JDirPathAbs $ dirByIDAndCalc workDirAbs childID (ONIOMKey Original),
            scratchDir = JDirPathAbs $ dirByIDAndCalc scratchDirAbs childID (ONIOMKey Original),
            nProcs = tl ^. #execution % #nProcesses,
            nThreads = tl ^. #execution % #nThreads,
            memory = tl ^. #execution % #memory,
            qMMMSpec = QM QMContext {charge = tl ^. #charge, mult = tl ^. #mult},
            embedding = tl ^. #embedding,
            optimisation = opt & #pysisyphus .~ pysis,
            additionalInput = tl ^. #additionalInput
          }
      calcInherited = case calcInheritOld of
        Nothing -> Nothing
        Just calcInh ->
          Just
            calcInh
              { permaDir = JDirPathAbs $ dirByIDAndCalc workDirAbs childID (ONIOMKey Inherited),
                scratchDir = JDirPathAbs $ dirByIDAndCalc scratchDirAbs childID (ONIOMKey Inherited),
                qMMMSpec = QM QMContext {charge = tl ^. #charge, mult = tl ^. #mult},
                prefixName = Text.unpack $ childComment <> "_low"
              }

  -- Update the child with context.
  let childMol =
        childMolEmpty
          { comment = childComment,
            calcContext =
              Map.fromList $
                (ONIOMKey Original, CalcContext {input = calcOriginal, output = Nothing}) :
                ( case calcInherited of
                    Nothing -> []
                    Just calcInh -> [(ONIOMKey Inherited, CalcContext {input = calcInh, output = Nothing})]
                )
          }

  -- Make a depth first recursion through deeper layers of this layer instead of walking through
  -- centres of the same hierarchy.
  newMaxKey1 <- getMaxAtomIndex childMol
  let subTL = tl ^. #deeperLayer
  (childMolDepthBranch, newMaxKey2) <- go workDir scratchDir newMaxKey1 childID childMol subTL

  -- Reinsert the child branch into the parent before continuing to add other childs.
  let parentWithThisBranch = parentMol & #subMol %~ IntMap.insert childKey childMolDepthBranch

  -- After construction of the entire branch, construct the next branch. Breadth first recursion
  -- now.
  go workDir scratchDir newMaxKey2 parentID parentWithThisBranch rest
  where
    localIndirExc = SpicyIndirectionException "mcOniomNLayout"

    -- Atoms to keep for the model layer
    modelAtomSelection = tl ^. #selection

    -- Updater for default values of the optimisation.
    defUp ::
      Optimisation ->
      (Optic' A_Lens NoIx Optimisation a, Optic' A_Lens NoIx Opt (Maybe a)) ->
      Optimisation
    defUp opt (targetL, originL) = case tl ^? #optimisation % _Just % originL % _Just of
      Nothing -> opt
      Just v -> opt & targetL .~ v

    -- Optimisation settings of this layer.
    optSettings :: Optimisation -> Optimisation
    optSettings optS =
      optS
        -- Simple updates
        `defUp` (#coordType, #coords)
        `defUp` (#maxCycles, #iterations)
        `defUp` (#hessianUpdate, #hessianUpdate)
        `defUp` (#maxTrust, #trustMax)
        `defUp` (#minTrust, #trustMin)
        `defUp` (#minTrust, #trustMin)
        -- Other updates without direct match.
        & #optType
          .~ ( case tl ^? #optimisation % _Just % #target % _Just of
                 Nothing -> Minimum RFO
                 Just Min -> Minimum RFO
                 Just TS -> SaddlePoint RS_I_RFO
             )
        & #hessianRecalc .~ fromMaybe Nothing (tl ^? #optimisation % _Just % #hessianRecalc)

-- | Creat a directory hierarchy based on the ONIOM layer and the calculation key
dirByIDAndCalc :: Path.AbsDir -> MolID -> CalcK -> Path.AbsDir
dirByIDAndCalc dir' molID calcK = calcDir
  where
    idDir' = foldl (\acc i -> acc </> (Path.relDir . show $ i)) dir' molID
    calcDir =
      idDir'
        </> Path.relDir
          ( case calcK of
              ONIOMKey Original -> "high"
              ONIOMKey Inherited -> "low"
          )
