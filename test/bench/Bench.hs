import Test.Tasty.Bench

-- TODO get tasty-discover to pick up these bench_* functions?
import Control.DeepSeq (deepseq)
import Control.Monad (forM)
import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees.HashLine (bench_roundtrip_HashLines_to_ByteString, genHashLinesBS,
                                           parseHashLinesBS)
import System.Directory.BigTrees.HashTree (bench_roundtrip_ProdTree_to_bigtree_file)
import System.IO (hFlush, stdout)

-- TODO empirically test how arbsize relates to number of nodes in the tree
-- TODO test dupes command, because that's the one that takes the most RAM

-- Up to ~16 million for now because that's what my old Thinkpad x220 can handle.
-- TODO go back to powers of 1.5? was that significantly more accurate?
testSizes :: [Int]
testSizes = [floor (2^(n :: Int)) | n <- [0..24]]

-- https://old.reddit.com/r/haskell/comments/qy990/suggestion_for_flip_map/
-- TODO move somewhere more reusable
for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

genTestHashLines :: IO [(Int, B8.ByteString)]
genTestHashLines = forM testSizes $ \n -> do
  bs <- genHashLinesBS n
  putStrLn $ deepseq bs $ "generated " ++ show n ++ " size " ++ show n ++ " HashLines"
  hFlush stdout
  return (n, bs)

main :: IO ()
main = do

  -- There's probably a cleaner way to do this, but for now I like that it
  -- clearly happens before any of the benchmark timing stuff.
  -- testHashLines <- genTestHashLines

  -- TODO also generate the ProdTrees here the same way?

  Test.Tasty.Bench.defaultMain $

    -- TODO works, but is it necessary?
    -- for testHashLines (\(n, bs) -> bench
    --   ("parse " ++ show n ++ " size " ++ show n ++ " HashLines")
    --   (nf parseHashLinesBS bs))

    -- ++

    -- TODO works, but is it necessary?
    -- for testHashLines (\(n, bs) -> bench
    --   ("round-trip " ++ show n ++ " size " ++ show n ++ " HashLines to ByteString ")
    --   (nfIO $ bench_roundtrip_HashLines_to_ByteString bs))

    -- ++

    for testSizes (\n -> bench
      ("round-trip size " ++ show n ++ " ProdTree to .bigtree file ")
      (nfIO $ bench_roundtrip_ProdTree_to_bigtree_file n))

    -- old stuff for reference:
    -- [ bench "fibo   20" $ nf bench_fibo   20
    -- , bench "myFibo 20" $ nf myFibo 20
    -- TODO can prop_ tests be used almost directly?
    -- , testProperty "myFibo = fibo" $ \n -> fibo n === myFibo n
