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
    [ mathBenchmarks
    ]

mathBenchmarks :: Benchmark
mathBenchmarks =
  bgroup
    "LUP decomposition"
    [ bgroup
        "Massiv Immutable"
        [ bench "10 x 10" $ nfIO . benchLUP $ 10,
          bench "100 x 100" $ nfIO . benchLUP $ 100
        ],
      bgroup
        "Massiv Mutable"
        [ bench "10 x 10" $ nfIO . benchLUP' $ 10,
          bench "100 x 100" $ nfIO . benchLUP' $ 100
        ],
      bgroup
        "HMatrix"
        [ bench "10 x 10" $ nfIO . benchHMat $ 10,
          bench "100 x 100" $ nfIO . benchHMat $ 100,
          bench "1000 x 1000" $ nfIO . benchHMat $ 1000,
          bench "10000 x 10000" $ nfIO . benchHMat $ 10000
        ]
    ]
  where
    -- Generator for test arrays.
    testArr :: Int -> Matrix S Double
    testArr n = makeArrayLinear Par (Sz $ n :. n) (fromIntegral . (+ 1))

    -- Wrapped benchmark function for LUP decomposition.
    benchLUP n = do
      lup <- lupDecomp . testArr $ n
      return lup

    benchLUP' n = do
      lup <- lupDecomp' . testArr $ n
      return . lu $ lup

    benchHMat n = do
      let vec = toVector . testArr $ n
          mat = LA.reshape n vec
          res = LA.lu mat

      return res
