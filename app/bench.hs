import Criterion.Main
import Data.Massiv.Array as Massiv
import Data.Massiv.Array.Manifest.Vector (toVector)
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LA
import RIO
import qualified RIO.Text as Text
import Spicy.Common
import Spicy.Math

main :: IO ()
main =
  defaultMain
    [ ]
