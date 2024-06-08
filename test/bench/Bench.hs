import Test.Tasty.Bench
import Test.Tasty.QuickCheck

-- TODO get tasty-discover to pick up these bench_* functions
import System.Directory.BigTrees.HashTree (bench_fibo)

myFibo :: Int -> Integer
myFibo n = if n < 3 then toInteger n else myFibo (n - 1) + myFibo (n - 2)

main :: IO ()
main = Test.Tasty.Bench.defaultMain -- not Test.Tasty.defaultMain
  [ bench "fibo   20" $ nf bench_fibo   20
  -- , bench "myFibo 20" $ nf myFibo 20
  -- , testProperty "myFibo = fibo" $ \n -> fibo n === myFibo n
  ]
