import Test.Tasty.Bench

-- TODO get tasty-discover to pick up these bench_* functions
import System.Directory.BigTrees.HashTree (bench_roundtrip_ProdTree_to_ByteString)

main :: IO ()
main = Test.Tasty.Bench.defaultMain $

  -- [ bench "fibo   20" $ nf bench_fibo   20
  -- , bench "myFibo 20" $ nf myFibo 20

  -- TODO can prop_ tests be used almost directly?
  -- , testProperty "myFibo = fibo" $ \n -> fibo n === myFibo n

  -- ] ++

  -- TODO more specific tests until we find the performance problem here
  ((flip map) [1, 2, 5] $ \n ->
    bench
      ("round-trip ProdTree (" ++ show n ++ " nodes) to ByteString ")
      (nfIO $ bench_roundtrip_ProdTree_to_ByteString n))
