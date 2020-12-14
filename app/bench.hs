import Criterion.Main
import Data.Massiv.Array as Massiv
import RIO
import Spicy.Math
import qualified RIO.Text as Text
import Spicy.Common

main :: IO ()
main =
  defaultMain
    [ mathBenchmarks
    ]

mathBenchmarks :: Benchmark
mathBenchmarks =
  bgroup
    "LUP decomposition"
    [ bench "10 x 10" $ nfIO . benchLUP $ 10,
      bench "100 x 100" $ nfIO . benchLUP $ 100,
      bench "1000 x 1000" $ nfIO . benchLUP $ 1000,
      bench "10000 x 10000" $ nfIO . benchLUP $ 10000
    ]
  where
    -- Generator for test arrays.
    testArr :: Int -> Matrix S Double
    testArr n = makeArrayLinear Par (Sz $ n :. n) (fromIntegral . (+1))

    -- Wrapped benchmark function for LUP decomposition.
    benchLUP n = do
      lup <- lupDecomp . testArr $ n
      let luSum = Massiv.sum . lu $ lup
      return luSum
