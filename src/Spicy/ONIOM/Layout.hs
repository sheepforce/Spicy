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
import Spicy.Logger
import Spicy.Molecule
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
  template <- readFileUTF8 . getFilePath $ tl ^. #templateFile
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
            template = template,
            embedding = tl ^. #embedding,
            optimisation = opt & #pysisyphus .~ pysis
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

-- The new

-- childCK = ON

{-
-- | Prepare the layout for an ONIOM-n calculation, potentially with multi-centre layout at each level.
-- The top level (real system) is the one obtained from the uppermost theorylayer, not necessarily the
-- complete system as read from file.
oniomNLayout :: (HasInputFile env, HasMolecule env, HasLogFunc env) => RIO env ()
oniomNLayout = do
  logInfo "Preparing ONIOM layout of input molecule."

  inputFile <- view inputFileL
  -- originalMolecule <- view moleculeL
  molT <- view moleculeL
  originalMolecule <- atomically . readTVar $ molT

  -- Step through the theorylayers of the input file and create new layers accordingly. Fail if no
  -- ONIOMn method is specified.
  (theoryLayers :: TheoryLayer) <- case (inputFile ^? #model % #theoryLayer) of
    Just layers -> return layers
    Nothing ->
      throwM $
        SpicyIndirectionException
          "oniomNLayout"
          "This function can not be used to prepare the molecule layout for calculations,\
          \ that are not ONIOM-n."

  -- Create a calculation context for the top layer, which will be inherited for the next layer.
  template <- readFileUTF8 $ getFilePath (theoryLayers ^. #templateFile)
  permaDir <- liftIO . makeJDirPathAbsFromCwd $ inputFile ^. #permanent
  scratchDir <- liftIO . makeJDirPathAbsFromCwd $ inputFile ^. #scratch
  let prefixNameTop =
        let saneOniomID = "ONIOMn_Level-0"
            saneDescriptor =
              replaceProblematicChars . Text.unpack . removeWhiteSpace $ theoryLayers ^. #name
         in saneOniomID <> "@" <> saneDescriptor
      calcInput =
        CalcInput
          { task = WTEnergy,
            restartFile = Nothing,
            software = theoryLayers ^. #program,
            prefixName = prefixNameTop,
            permaDir = permaDir,
            scratchDir = scratchDir,
            nProcs = theoryLayers ^. #execution % #nProcesses,
            nThreads = theoryLayers ^. #execution % #nThreads,
            memory = theoryLayers ^. #execution % #memory,
            qMMMSpec =
              prepareQMMMSpec
                (theoryLayers ^. #program)
                (theoryLayers ^. #charge)
                (theoryLayers ^. #mult),
            template = template,
            embedding = theoryLayers ^. #embedding
          }
      calcContextTop =
        CalcContext
          { input = calcInput,
            output = Nothing
          }

  -- Clean the molecule, that was read from the file from all its sublayers, that might have been
  -- generated by the parser.
  let molCleaned = originalMolecule & #subMol .~ IntMap.empty

  -- Find the maximum atom index in the input molecule (usually parsed from file).
  maxAtomKey <- getMaxAtomIndex molCleaned

  -- Create the new sublayer, which is the topmost ONIOM layer (the real system) and use this one as
  -- the new top. Also has first calculation context for the real system added.
  let atomSelection = theoryLayers ^. #selection

  logInfo "  Layer 0 (real system) with atoms:"
  mapM_ (logInfo . ("    " <>)) . formatPadFoldable 6 int "," . IntSet.toAscList $ atomSelection

  molONIOMTopLayer <- newSubLayer maxAtomKey molCleaned atomSelection Nothing Nothing
  molRealSystemNoContext <-
    if IntMap.size (molONIOMTopLayer ^. #subMol) == 1
      then case IntMap.lookupMax $ molONIOMTopLayer ^. #subMol of
        Just (_, realSystemLayer) -> return realSystemLayer
        Nothing ->
          throwM $
            MolLogicException
              "oniomNLayout"
              "During creation of the real system ONIOM-layout something went wrong."
      else
        throwM $
          MolLogicException
            "oniomNLayout"
            "Tried to create the real system layer for ONIOM but the molecule was not clean and\
            \ already contained submolecules."
  let molRealSystem =
        molRealSystemNoContext
          & #calcContext
          .~ Map.singleton (ONIOMKey Original) calcContextTop

  -- Now step through the deeper ONIOM layers recursively and do the layout for everything.
  let deeperLayers = theoryLayers ^. #deeperLayer
  maxKeyOniomReal <- getMaxAtomIndex molRealSystem
  case deeperLayers of
    Empty -> atomically . writeTVar molT $ molRealSystem
    moreLayers -> do
      finalMol <- (^. _1) <$> createNextLayers maxKeyOniomReal Empty molRealSystem calcContextTop moreLayers
      atomically . writeTVar molT $ finalMol
  where
    createNextLayers ::
      (HasInputFile env, HasLogFunc env) =>
      -- | The max key of atoms that is currently present in the overall
      --   system.
      Int ->
      -- | MolID of the molecule layer, which requests to build the
      --   deeper layers.
      MolID ->
      -- | Current ONIOM layer, below which no submolecules should exist
      --   yet.
      Molecule ->
      -- | Calculation context of the current layer, which needs to be
      --   inherited for the new layers below.
      CalcContext ->
      -- | The 'TheoryLayer's, from which deeper submolecules will be
      --   created.
      Seq TheoryLayer ->
      -- | The original input molecule with layers added as by
      --   theorylayers and the ID of the last oniom layer reached in the
      --   recursion. The ID is only used internally as a fold
      --   accumulator
      RIO env (Molecule, MolID, Int)
    createNextLayers maxKeyFullSystem oniomIDAbove molAbove contextAbove theoryLayers =
      foldl'
        ( \accumulator currentTheoLayer -> do
            -- Unwrap the current accumulator. Contains the accumulator of the molecule and the ID, which
            -- are updated synchronously.
            (molAcc, idAcc, mayKeyAcc) <- accumulator

            -- Get input information.
            inputFile <- view inputFileL

            -- Handle the MolIDs for the layers. The ID of the first foldl recursion (ONIOM centre) is
            -- given by the idAcc and subsequent centre IDs will be updated here. Deeper layers are
            -- handled outside the recurion of this fold by updating the accumulator.
            (oniomInitID, oniomLastID) <- case idAcc of
              initID :|> lastID -> return (initID, lastID)
              Empty ->
                throwM $
                  MolLogicException
                    "createNextLayers"
                    "The inner fold cannot create the real system, only use this for deeper ONIOM layers."
            let oniomIDNextCentre :: MolID
                oniomIDNextCentre = oniomInitID |> (oniomLastID + 1)

            -- For logging and context prepare a human readable identifier of this ONIOM layer.
            let oniomID = molID2OniomHumandID idAcc

            -- Get the atom selection of this layer.
            let atomSelection = currentTheoLayer ^. #selection

            -- Logging for the current layer.
            logInfo $ oniomID <> " (MolID " <> displayShow idAcc <> ") with atoms:"
            mapM_ (logInfo . ("    " <>))
              . formatPadFoldable 6 int ","
              . IntSet.toAscList
              $ atomSelection

            -- Prepare stuff for the calculation context.
            template <- readFileUTF8 $ getFilePath (currentTheoLayer ^. #templateFile)
            permaDir <- liftIO . makeJDirPathAbsFromCwd $ inputFile ^. #permanent
            scratchDir <- liftIO . makeJDirPathAbsFromCwd $ inputFile ^. #scratch
            let prefixNameThis =
                  let saneOniomID = Text.unpack . removeWhiteSpace . textDisplay $ oniomID
                      saneDescriptor =
                        replaceProblematicChars . Text.unpack . removeWhiteSpace $ currentTheoLayer ^. #name
                   in saneOniomID <> "_Original@" <> saneDescriptor
                calcInputThis =
                  CalcInput
                    { task = WTEnergy,
                      restartFile = Nothing,
                      software = currentTheoLayer ^. #program,
                      prefixName = prefixNameThis,
                      permaDir = permaDir,
                      scratchDir = scratchDir,
                      nProcs = currentTheoLayer ^. #execution % #nProcesses,
                      nThreads = currentTheoLayer ^. #execution % #nThreads,
                      memory = currentTheoLayer ^. #execution % #memory,
                      qMMMSpec =
                        prepareQMMMSpec
                          (currentTheoLayer ^. #program)
                          (currentTheoLayer ^. #charge)
                          (currentTheoLayer ^. #mult),
                      template = template,
                      embedding = currentTheoLayer ^. #embedding
                    }
                calcContextThis =
                  CalcContext
                    { input = calcInputThis,
                      output = Nothing
                    }

                prefixNameInherited =
                  let saneOniomID = Text.unpack . removeWhiteSpace . textDisplay $ oniomID
                      saneDescriptor =
                        replaceProblematicChars
                          . Text.unpack
                          . removeWhiteSpace
                          . Text.pack
                          $ contextAbove
                            ^. #input
                              % #prefixName
                   in saneOniomID <> "_Inherited@" <> saneDescriptor
                calcContextInherited =
                  contextAbove
                    & #input
                      % #prefixName
                    .~ prefixNameInherited
                    & #input
                      % #permaDir
                    .~ JDirPathAbs (getDirPathAbs permaDir </> Path.relDir "Inherited")
                    & #input
                      % #scratchDir
                    .~ JDirPathAbs (getDirPathAbs scratchDir </> Path.relDir "Inherited")
                    & #input
                      % #qMMMSpec
                    .~ prepareQMMMSpec
                      (contextAbove ^. #input % #software)
                      (currentTheoLayer ^. #charge)
                      (currentTheoLayer ^. #mult)

            -- Create the new sub layer below the current one.
            molLayerAddedNoContext <- newSubLayer mayKeyAcc molAcc atomSelection Nothing Nothing
            (newHighestSubMolIndex, newSubMolOnly) <-
              case IntMap.lookupMax $ molLayerAddedNoContext ^. #subMol of
                Just (newIndex, newSubMol) -> return (newIndex, newSubMol)
                Nothing ->
                  throwM $
                    MolLogicException
                      "oniomNLayout"
                      "A new layer should have been added to the molecule, but none could be found."

            -- Add calculation context information to the new sublayer.
            let subMolCalcContext =
                  Map.fromList
                    [(ONIOMKey Original, calcContextThis), (ONIOMKey Inherited, calcContextInherited)]
                subMolWithContext = newSubMolOnly & #calcContext .~ subMolCalcContext

            -- Get the new maximum key after the submolecule has been created.
            newMaxAtomKey <- getMaxAtomIndex subMolWithContext

            -- Recursively also step through all deeper theory layers of this one, creating the even
            -- deeper layers here.
            let deeperLayersOfThis = currentTheoLayer ^. #deeperLayer
            finalNewSubMol <- case deeperLayersOfThis of
              -- If no deeper layers are found, we are done here with creation of the deeper layers and
              -- the submolecule can be used.
              Empty -> return subMolWithContext
              -- If deeper layers can be found, use this function recursively to also create thus.
              moreLayers ->
                (^. _1)
                  <$> createNextLayers
                    newMaxAtomKey
                    idAcc
                    subMolWithContext
                    calcContextThis
                    moreLayers

            -- Now that the context of this sublayer and all its sublayers below have been prepared,
            -- return the layer that was given as input with the new sublayer created. Use it as the new
            -- accumulator.
            let molUpdated = molAcc & #subMol %~ IntMap.insert newHighestSubMolIndex finalNewSubMol
            return (molUpdated, oniomIDNextCentre, newMaxAtomKey)
        )
        (return (molAbove, oniomIDAbove |> 0, maxKeyFullSystem))
        theoryLayers
-}
