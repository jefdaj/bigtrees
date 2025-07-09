import Test.Tasty.Bench

-- TODO get tasty-discover to pick up these bench_* functions
import Control.DeepSeq (force)
import Control.Monad (forM)
import System.Directory.BigTrees.HashLine (genHashLinesBS, parseHashLinesBS, bench_roundtrip_HashLines_to_ByteString)
import System.Directory.BigTrees.HashTree (bench_roundtrip_ProdTree_to_bigtree_file)
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Char8 as B8

-- range from 1 to 25251
testSizes :: [Int]
testSizes = [floor (1.5^(n :: Int)) | n <- [0..25]]

-- There's probably a cleaner way to do this, but for now I like that it
-- clearly happens before any of the bench time anything.
genTestData :: IO [(Int, B8.ByteString)]
genTestData = do
  putStr "generating test data..."
  hFlush stdout
  hashLinesData <- fmap force $ forM testSizes $ \n -> do
    bs <- genHashLinesBS n
    return (n, bs)
  putStrLn " ok"
  hFlush stdout
  return hashLinesData

main :: IO ()
main = do
  hashLinesData <- genTestData
  putStrLn "running tests"
  hFlush stdout
  Test.Tasty.Bench.defaultMain $

    map (\(n, bs) ->
      bench ("parse " ++ show n ++ " HashLines") (nf parseHashLinesBS bs)) hashLinesData

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

    -- TODO generate data first as with the hashlines above?
    map (\n ->
      bench
        ("round-trip " ++ show n ++ "-node ProdTree to .bigtree file ")
        (nfIO $ bench_roundtrip_ProdTree_to_bigtree_file n))
        testSizes

    ++

    -- TODO is this allocating way more memory than it should?
    -- TODO ohhh, or is it also increasing the size of each bs as it goes?
    map (\(n, bs) ->
      bench
        ("round-trip " ++ show n ++ " HashLines to ByteString ")
        (nfIO $ bench_roundtrip_HashLines_to_ByteString bs))
        hashLinesData
