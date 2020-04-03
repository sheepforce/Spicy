{-|
Module      : Spicy.Wrapper.Internal.Generic
Description : Functions for wrappers but agnostic of the software
Copyright   : Phillip Seeber, 2020
License     : GPL-3
Maintainer  : phillip.seeber@uni-jena.de
Stability   : experimental
Portability : POSIX, Windows

Provides generic functions for wrapper software.
-}
module Spicy.Wrapper.Internal.Generic
  ( prepareQMMMSpec
  )
where
import           RIO
import           Spicy.Class

prepareQMMMSpec :: Program -> Int -> Int -> QMMMSpec
prepareQMMMSpec program' charge' mult' = case program' of
  Psi4   -> QM qmContext
  Nwchem -> QM qmContext
 where
  qmContext = QMContext { _qmContext_Charge = charge', _qmContext_Mult = mult' }
  -- mmContext = MMContext
