import Test.Tasty.Bench
import Test.Tasty.QuickCheck -- from tasty-quickcheck package

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

myFibo :: Int -> Integer
myFibo n = if n < 3 then toInteger n else myFibo (n - 1) + myFibo (n - 2)

main :: IO ()
main = Test.Tasty.Bench.defaultMain -- not Test.Tasty.defaultMain
  [ bench "fibo   20" $ nf fibo   20
  , bench "myFibo 20" $ nf myFibo 20
  , testProperty "myFibo = fibo" $ \n -> fibo n === myFibo n
  ]
