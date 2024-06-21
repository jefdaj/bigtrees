import Test.Tasty.Bench

-- TODO get tasty-discover to pick up these bench_* functions
import Control.DeepSeq (force)
import Control.Monad (forM)
import System.Directory.BigTrees.HashLine (genHashLinesBS, parseHashLinesBS)
import System.Directory.BigTrees.HashTree (bench_roundtrip_ProdTree_to_ByteString)

main :: IO ()
main = do

  -- There's probably a cleaner way to do this, but for now I like that it
  -- clearly happens before any of the bench time anything.
  hlData <- fmap force $ forM [1, 10, 100, 1000, 2000] $ \n -> do
    bs <- genHashLinesBS n
    return (n, bs)

  Test.Tasty.Bench.defaultMain $

    map (\(n, bs) ->
      bench ("parse " ++ show n ++ " HashLines") (nf parseHashLinesBS bs)) hlData

    -- [ bench "fibo   20" $ nf bench_fibo   20
    -- , bench "myFibo 20" $ nf myFibo 20

    -- TODO can prop_ tests be used almost directly?
    -- , testProperty "myFibo = fibo" $ \n -> fibo n === myFibo n

    -- ] ++

    -- ((flip map) [10^x | x <- [1..5]] $ \n ->
    --   bench
    --     ("round-trip " ++ show n ++ " HashLines to ByteString ")
    --     (nfIO $ bench_roundtrip_HashLines_to_ByteString n))

    ++

    -- TODO more specific tests until we find the performance problem here

    -- TODO more specific tests until we find the performance problem here

    -- TODO more specific tests until we find the performance problem here

    -- TODO more specific tests until we find the performance problem here
    map (\n ->
      bench
        ("round-trip ProdTree (" ++ show n ++ " nodes) to ByteString ")
        (nfIO $ bench_roundtrip_ProdTree_to_ByteString n)) [1, 2, 5, 10, 20, 50]
