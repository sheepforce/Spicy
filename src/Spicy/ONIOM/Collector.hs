{-|
Module      : Spicy.ONIOM.Collector
Description : Transforming individual information from calculations to an ONIOM result.
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

Collector functions to gather the results from the 'CalcOutput's of a molecule and build the ONIOM
result from it.
-}
module Spicy.ONIOM.Collector
  ( multicentreOniomNCollector
  )
where
import           RIO hiding ((^.), view)
import           Control.Lens
import           Spicy.Class

{-|
Collector for multicentre ONIOM-N calculations.
-}
multicentreOniomNCollector
  :: (HasMolecule env, HasInputFile env) => RIO env Molecule
multicentreOniomNCollector = do
  mol <- view moleculeL
  _inputFile <- view inputFileL

  -- TODO (phillip|p=100|#Unfinished) - Obviously processing needs to be implemented here before returning any molecule.
  return mol
