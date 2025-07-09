import Test.Tasty.Bench

-- TODO get tasty-discover to pick up these bench_* functions?
import Control.DeepSeq (deepseq)
import Control.Monad (forM)
import System.Directory.BigTrees.HashLine (genHashLinesBS, parseHashLinesBS, bench_roundtrip_HashLines_to_ByteString)
import System.Directory.BigTrees.HashTree (bench_roundtrip_ProdTree_to_bigtree_file)
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Char8 as B8

-- range from 1 to 7481
-- TODO either go a lot higher or be precise with expected timing
testSizes :: [Int]
testSizes = [floor (1.5^(n :: Int)) | n <- [0..22]]

-- https://old.reddit.com/r/haskell/comments/qy990/suggestion_for_flip_map/
-- TODO move somewhere more reusable
for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

genTestHashLines :: IO [(Int, B8.ByteString)]
genTestHashLines = forM testSizes $ \n -> do
  bs <- genHashLinesBS n
  putStrLn $ deepseq bs $ "generated " ++ show n ++ " size-" ++ show n ++ " HashLines"
  hFlush stdout
  return (n, bs)

main :: IO ()
main = do

  -- There's probably a cleaner way to do this, but for now I like that it
  -- clearly happens before any of the benchmark timing stuff.
  testHashLines <- genTestHashLines

  -- TODO also generate the ProdTrees here the same way?

  Test.Tasty.Bench.defaultMain $

    (for testHashLines $ \(n, bs) -> bench
      ("parse " ++ show n ++ " size-" ++ show n ++ " HashLines")
      (nf parseHashLinesBS bs))

    ++

    (for testHashLines $ \(n, bs) -> bench
      ("round-trip " ++ show n ++ " size-" ++ show n ++ " HashLines to ByteString ")
      (nfIO $ bench_roundtrip_HashLines_to_ByteString bs))

    ++

    (for testSizes $ \n -> bench
      ("round-trip " ++ show n ++ "-node ProdTree to .bigtree file ")
      (nfIO $ bench_roundtrip_ProdTree_to_bigtree_file n))

    -- old stuff for reference:
    -- [ bench "fibo   20" $ nf bench_fibo   20
    -- , bench "myFibo 20" $ nf myFibo 20
    -- TODO can prop_ tests be used almost directly?
    -- , testProperty "myFibo = fibo" $ \n -> fibo n === myFibo n
