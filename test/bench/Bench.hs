import Test.Tasty.Bench
import Test.Tasty.QuickCheck

-- TODO get tasty-discover to pick up these bench_* functions
import System.Directory.BigTrees.HashTree (bench_fibo, bench_roundtrip_ProdTree_to_ByteString)

myFibo :: Int -> Integer
myFibo n = if n < 3 then toInteger n else myFibo (n - 1) + myFibo (n - 2)

main :: IO ()
main = Test.Tasty.Bench.defaultMain $
  [ bench "fibo   20" $ nf bench_fibo   20
  -- , bench "myFibo 20" $ nf myFibo 20
  -- , testProperty "myFibo = fibo" $ \n -> fibo n === myFibo n
  -- , bench "round-trip ProdTree to ByteString 1"    $ nfIO $ bench_roundtrip_ProdTree_to_ByteString 1
  -- , bench "round-trip ProdTree to ByteString 10"   $ nfIO $ bench_roundtrip_ProdTree_to_ByteString 10
  -- , bench "round-trip ProdTree to ByteString 100"  $ nfIO $ bench_roundtrip_ProdTree_to_ByteString 100
  -- , bench "round-trip ProdTree to ByteString 1000" $ nfIO $ bench_roundtrip_ProdTree_to_ByteString 1000
  ] ++

  -- TODO more specific tests until we find the performance problem here
  ((flip map) [1, 2, 5] $ \n ->
    bench
      ("round-trip ProdTree to ByteString " ++ show n)
      (nfIO $ bench_roundtrip_ProdTree_to_ByteString n))
