-- |
-- Module      : Spicy.Wrapper
-- Description : Abstract routines to perform calculations with external software.
-- Copyright   : Phillip Seeber, 2021
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Abstract methods to perform computational chemistry tasks by calling external programs.
module Spicy.Wrapper
  ( provideCalcSlot,
  )
where

import Optics hiding (view)
import RIO hiding (view, (^.))
import RIO.Process
import Spicy.Common
import Spicy.Molecule
import Spicy.RuntimeEnv
import Spicy.Wrapper.Internal.Executor

logSource :: LogSource
logSource = "calc provider"

-- | Loop that provides the companion thread, which executes the wrapper calculations.
provideCalcSlot ::
  ( HasLogFunc env,
    HasMolecule env,
    HasCalcSlot env,
    HasProcessContext env,
    HasWrapperConfigs env
  ) =>
  RIO env ()
provideCalcSlot = do
  -- Obtain initial information.
  calcSlotIn <- view $ calcSlotL % #input
  calcSlotOut <- view $ calcSlotL % #output

  -- Run the main loop
  forever $ do
    -- LOG
    logDebugS logSource "Waiting for calculation ..."

    -- Wait for a calculation to perform.
    calcID <- atomically . takeTMVar $ calcSlotIn

    -- LOG
    logDebugS logSource $
      "Got calculation:\n"
        <> ("  Layer: " <> (display . molID2OniomHumanID $ calcID ^. #molID))
        <> ( "  Type : " <> case calcID ^. #calcKey of
               ONIOMKey Original -> "high level calculation"
               ONIOMKey Inherited -> "low level calculation"
           )

    -- Perform the given calculation.
    molWithRes <- runCalculation calcID

    -- Mark calculation as ready to be consumed and return.
    atomically . putTMVar calcSlotOut $ molWithRes
