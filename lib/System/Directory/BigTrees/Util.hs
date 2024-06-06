{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
-- TODO wait, is the empty string also a valid filename?

module System.Directory.BigTrees.Util

  -- $canonicalpaths
  -- TODO document these individually
  ( absolutePath
  , pathComponents -- TODO replace with builtin split path or similar?
  -- , stripExtraDotdot

  -- TODO cleaner explanation of all the tests as a group here
  , prop_absolutePaths_is_idempotent
  , prop_absolutePaths_strips_redundant_dot
  -- , prop_absolutePaths_strips_redundant_dotdot
  , unit_absolutePath_expands_tildes
  , unit_absolutePath_fixes_invalid_dotdot
  , unit_absolutePath_rejects_null_path

  -- TODO pathComponents tests
  -- TODO symlink handling tests

  , isAnnexSymlink
  , isNonAnnexSymlink

  , hTakePrevUntil
  )
  where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Filesystem.Path.CurrentOS as OS
import GHC.Generics (Generic)
import Prelude hiding (log)
import System.Directory (canonicalizePath, getHomeDirectory)
import qualified System.Directory.Tree as DT
import qualified System.FilePath as SF
import System.FilePath ((</>))
import System.Info (os)
import System.IO (Handle, hGetChar, hSeek, SeekMode(..))
import Control.Exception.Safe (handleAnyDeep)
import System.IO.Temp (withSystemTempDirectory)
import System.Path.NameManip (absolute_path, guess_dotdot)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck (Arbitrary (..), Gen, Property, listOf, oneof, suchThat)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import TH.Derive (Deriving, derive)

-- describe "Util" $ do
--   describe "absolute" $ do
--       it "strips dots from paths" pending
--       it "does not follow symlinks" pending
--
--     describe "findAnnex" $ do
--       it "returns Nothing if given a nonexistent path" pending
--       it "returns Nothing if given a non-annex path" pending
--       it "returns (Just path) if path is an annex" pending
--       it "returns only absolute paths" pending
--
--     describe "isAnnexSymlink" $ do
--       it "returns False if given a non-symlink" pending
--       it "returns True if given a symlink pointing into a .git/annex/objects dir" pending
--       it "returns False if given a symlink pointing somewhere else" pending
--
--     describe "isNonAnnexSymlink" $ do
--       it "returns False if given a non-symlink" pending
--       it "returns False if given a symlink pointing into a .git/annex/objects dir" pending
--       it "returns True if given a symlink pointing somewhere else" pending

-- TODO haddocks
-- TODO fp2ns?
-- TODO does this go in Name.hs instead?
-- | Split a `FilePath` into a list of `Name`s
pathComponents :: FilePath -> [FilePath]
pathComponents f = filter (not . null)
                 $ map (filter (/= SF.pathSeparator))
                 $ SF.splitPath f

-- n2bs :: Name -> BU.ByteString
-- n2bs = BU.fromString . n2fp

-- TODO should this have the option for a decoding error?
-- bs2n :: BU.ByteString -> Name
-- bs2n = fp2n . BU.toString

newtype Path
  = Path FilePath
  deriving (Eq, Ord, Read, Show)

instance Arbitrary Path where
  arbitrary :: Gen Path
  arbitrary =

    -- make sure the whole thing isn't empty at the end
    flip suchThat (\(Path p) -> p /= "") $ do

      -- generate the main path body
      -- (a single path would work here too, but i want more complex test trees)
      -- TODO would bytestring for this more closely match the actual OS?
      (body :: [FilePath]) <- listOf (arbitrary `suchThat` (notElem '\NUL'))

      -- make sure we can handle various prefix styles
      -- note that "" here generates "/" below
      -- TODO also ones with variables?
      (prefix :: String) <- oneof $ map pure ["", ".", "..", "~"]

      -- ... or none at all
      (usePrefix :: Bool) <- arbitrary

      let path = SF.joinPath $ if usePrefix then prefix:body else body
      return $ Path path

  -- TODO the individual strings will shrink automatically, right?
  shrink :: Path -> [Path]
  shrink (Path p) = map (Path . SF.joinPath) $ shrink $ pathComponents p

-- newtype PathWithParent
--   = PathWithParent FilePath
--   deriving (Eq, Ord, Read, Show)

-- instance Arbitrary PathWithParent where
--   arbitrary :: Gen PathWithParent
--   arbitrary = fmap sayHasParent (arbitrary :: Gen Path) `suchThat` reallyHasParent
--     where
--       sayHasParent (Path p) = PathWithParent p
--       reallyHasParent (PathWithParent p) = length (SF.splitPath p) > 1

-- * Canonical paths
--
-- $canonicalpaths
--
-- Resolve various `FilePath`s to their "real" absolute paths,

-- TODO rename it cleanPath?
-- TODO is there a potential for infinite recursion bugs here?
-- | Do some IO and return the canonical absolute path.
absolutePath :: FilePath -> IO (Maybe FilePath)
absolutePath path = do
  path' <- absolutePath' path
  case path' of
    Nothing -> return Nothing
    Just p' -> if p' == path -- fixpoint
                 then Just <$> canonicalizePath p'
                 else absolutePath p'

-- based on: schoolofhaskell.com/user/dshevchenko/cookbook
absolutePath' :: FilePath -> IO (Maybe FilePath)
absolutePath' aPath
    | null aPath = return Nothing
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ Just $ SF.normalise $ SF.addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = do
        -- let aPath' = guess_dotdot aPath
        aPath' <- absolute_path aPath
        case guess_dotdot aPath' of
          Nothing -> return $ Just aPath
          Just p  -> return $ Just p
        -- return $ guess_dotdot pathMaybeWithDots -- TODO this is totally wrong sometimes!

-- >>> let x = 23
-- >>> x + 42
-- 65
unit_absolutePath_expands_tildes :: Assertion
unit_absolutePath_expands_tildes = do
  home <- getHomeDirectory
  let explicit = home </> "xyz"
  (Just implicit) <- absolutePath "~/xyz"
  implicit @=? explicit

-- TODO is the empty string a valid relative path?
unit_absolutePath_rejects_null_path :: Assertion
unit_absolutePath_rejects_null_path = do
  reject <- absolutePath ""
  reject @=? Nothing

unit_absolutePath_fixes_invalid_dotdot :: Assertion
unit_absolutePath_fixes_invalid_dotdot = do
  fixed <- absolutePath "/.."
  fixed @=? Just "/"

prop_absolutePaths_is_idempotent :: Path -> Property
prop_absolutePaths_is_idempotent (Path path) = monadicIO $ do
  (Just path' ) <- liftIO $ absolutePath path
  (Just path'') <- liftIO $ absolutePath path'
  assert $ path' == path''

-- TODO is this technically correct, or can "<something>/.." be different from "" with symlinks?
-- TODO is this technically correct in the absence of symlinks?
-- prop_absolutePaths_strips_redundant_dotdot :: PathWithParent -> Property
-- prop_absolutePaths_strips_redundant_dotdot (PathWithParent path) = monadicIO $ do
--   (Just a ) <- fmap (fmap SF.takeDirectory) $ liftIO $ absolutePath path
--   (Just a') <- liftIO $ absolutePath $ path </> ".."
--   assert $ a == a'

prop_absolutePaths_strips_redundant_dot :: Path -> Property
prop_absolutePaths_strips_redundant_dot (Path path) = monadicIO $ do
  (Just a ) <- liftIO $ absolutePath path
  (Just a') <- liftIO $ absolutePath $ path </> "."
  assert $ a == a'

-- * git-annex paths
--
-- $gitannexpaths
--
-- Special handling of git-annex symlinks. If we trust the links, we can
-- read sha256sums from them rather than re-hashing the referenced files.

-- We reuse the existing SHA256SUM from the link
-- TODO also check that the link points *inside* the current tree!
-- TODO separate function for this vs for checking whether a link points in the tree?
isAnnexSymlink :: FilePath -> IO Bool
isAnnexSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return False
    else do
      l <- readSymbolicLink path
      return $ ".git/annex/objects/" `isInfixOf` l
             && "SHA256E-" `isPrefixOf` (SF.takeBaseName l)

isNonAnnexSymlink :: FilePath -> IO Bool
isNonAnnexSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return False
    else not <$> isAnnexSymlink path

--- read backwards from the end of a file ---

-- Note that max is a positive number, the max chars to take,
-- whereas the seek index is a negative number from the end.
-- TODO handle the case where we get to the beginning of the file?
--
-- Example usage:
--
-- >>> withFile "stack-work.bigtree"  ReadMode $ hTakePrevUntil (\s -> "\n# {" `isPrefixOf` s) 100
-- "\n# {\n#   \"scanEnd\": 1717518711\n#"
--
-- >>> withFile "stack-work.bigtree"  ReadMode $ hTakePrevUntil (\s -> "\n# {" `isPrefixOf` s) 10
-- Nothing
--
hTakePrevUntil :: (String -> Bool) -> Int -> Handle -> IO (Maybe String)
hTakePrevUntil cond maxChars hdl = handleAnyDeep (\_ -> return Nothing) $ do
  hSeek hdl SeekFromEnd 0
  hTakePrevUntil' cond maxChars hdl ""

-- Internal helper that also takes a seek position and the accumulated string.
hTakePrevUntil' :: (String -> Bool) -> Int -> Handle -> String -> IO (Maybe String)
hTakePrevUntil' _ maxChars _ _ | maxChars < 0 = return Nothing
hTakePrevUntil' cond maxChars hdl cs = do
  hSeek hdl RelativeSeek (-2)
  c <- hGetChar hdl
  let cs' = c:cs
  if cond cs'
    then return $ Just cs'
    else hTakePrevUntil' cond (maxChars-1) hdl cs'

--------------
-- old code --
--------------

-- userSaysYes :: String -> IO Bool
-- userSaysYes question = do
--   putStr $ question ++ " (yes/no) "
--   hFlush stdout
--   let answers = [("yes", True), ("no", False)]
--   answer <- getLine
--   case lookup answer answers of
--     Nothing -> userSaysYes question
--     Just b  -> return b
