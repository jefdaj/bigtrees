{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Description: FilePath-related utils

This module is for `FilePath`-related utility functions mainly used in the app.
It doesn't interact directly with `Names` as used in the tree structures.
-}

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
-- TODO wait, is the empty string also a valid filename?

module System.Directory.BigTrees.Path

  ( absolute
  , components -- TODO replace with builtin split path or similar?

  -- tests
  , prop_absolute_is_idempotent
  , prop_absolute_strips_redundant_dot
  , prop_absolute_strips_redundant_dotdot
  , unit_absolute_expands_tildes
  , unit_absolute_fixes_invalid_dotdot
  , unit_absolute_rejects_null_path

  )
  where

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf, isPrefixOf)
import Data.Store (Store (..))
import GHC.Generics (Generic)
import Prelude hiding (log)
import System.Directory (canonicalizePath, getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Path.NameManip (absolute_path, guess_dotdot)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import TH.Derive (Deriving, derive)
import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck (Arbitrary (..), Gen, Property, listOf, oneof, suchThat)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Filesystem.Path.CurrentOS as OS
import qualified System.Directory.Tree as DT
import qualified System.FilePath as SF

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
components :: FilePath -> [FilePath]
components f = filter (not . null)
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
  arbitrary = do
    prefix <- oneof $ map pure ["", ".", "..", "~"] -- TODO remove?
    -- a single path would work here too, but i want more complex test trees
    body <- listOf (arbitrary :: Gen FilePath)
    let path = SF.joinPath (prefix:body)
    return $ Path $ if null path then "/" else path

newtype PathWithParent
  = PathWithParent FilePath
  deriving (Eq, Ord, Read, Show)

instance Arbitrary PathWithParent where
  arbitrary :: Gen PathWithParent
  arbitrary = fmap sayHasParent (arbitrary :: Gen Path) `suchThat` reallyHasParent
    where
      sayHasParent (Path p) = PathWithParent p
      reallyHasParent (PathWithParent p) = length (SF.splitPath p) > 1

-- * Canonical paths
--
-- $canonicalpaths
--
-- Resolve various `FilePath`s to their "real" absolute paths,

-- TODO rename it cleanPath?
-- TODO is there a potential for infinite recursion bugs here?
-- | Do some IO and return the canonical absolute path.
absolute :: FilePath -> IO (Maybe FilePath)
absolute path = do
  path' <- absolute' path
  case path' of
    Nothing -> return Nothing
    Just p' -> if p' == path
                 then Just <$> canonicalizePath p'
                 else absolute p'

-- based on: schoolofhaskell.com/user/dshevchenko/cookbook
absolute' :: FilePath -> IO (Maybe FilePath)
absolute' aPath
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
unit_absolute_expands_tildes :: Assertion
unit_absolute_expands_tildes = do
  home <- getHomeDirectory
  let explicit = home </> "xyz"
  (Just implicit) <- absolute "~/xyz"
  implicit @=? explicit

-- TODO is the empty string a valid relative path?
unit_absolute_rejects_null_path :: Assertion
unit_absolute_rejects_null_path = do
  reject <- absolute ""
  reject @=? Nothing

unit_absolute_fixes_invalid_dotdot :: Assertion
unit_absolute_fixes_invalid_dotdot = do
  fixed <- absolute "/.."
  fixed @=? Just "/"

prop_absolute_is_idempotent :: Path -> Property
prop_absolute_is_idempotent (Path path) = monadicIO $ do
  (Just path' ) <- liftIO $ absolute path
  (Just path'') <- liftIO $ absolute path'
  assert $ path' == path''

prop_absolute_strips_redundant_dotdot :: PathWithParent -> Property
prop_absolute_strips_redundant_dotdot (PathWithParent path) = monadicIO $ do
  (Just a ) <- fmap (fmap SF.takeDirectory) $ liftIO $ absolute path
  (Just a') <- liftIO $ absolute $ path </> ".."
  assert $ a == a'

prop_absolute_strips_redundant_dot :: Path -> Property
prop_absolute_strips_redundant_dot (Path path) = monadicIO $ do
  (Just a ) <- liftIO $ absolute path
  (Just a') <- liftIO $ absolute $ path </> "."
  assert $ a == a'

-- * git-annex paths
--
-- $gitannexpaths
--
-- Special handling of git-annex symlinks. If we trust the links, we can
-- read sha256sums from them rather than re-hashing the referenced files.

-- | We treat these as files rather than following to avoid infinite cycles
-- TODO refactor to use isAnnexSymlink?
isNonAnnexSymlink :: FilePath -> IO Bool
isNonAnnexSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return False
    else do
      link <- readSymbolicLink path
      return $ not $ (".git/annex/objects/" `isInfixOf` link)
                  && ("SHA256E-" `isPrefixOf` SF.takeBaseName link)

--------------
-- old code --
--------------

-- absolute :: FilePath -> IO FilePath
-- absolute p = do
--   wd <- getCurrentDirectory
--   canonicalizePath (wd </> p)

-- TODO this fails on the leading / in a full path?
-- dropDir :: FilePath -> FilePath
-- dropDir path = case path of
--   ('/':p) -> dropDirHelper p
--   p       -> dropDirHelper p

-- dropDirHelper :: FilePath -> FilePath
-- dropDirHelper = SF.joinPath . tail . SF.splitPath

-- noSlash :: FilePath -> FilePath
-- noSlash = reverse . dropWhile (== '/') . reverse

-- userSaysYes :: String -> IO Bool
-- userSaysYes question = do
--   putStr $ question ++ " (yes/no) "
--   hFlush stdout
--   let answers = [("yes", True), ("no", False)]
--   answer <- getLine
--   case lookup answer answers of
--     Nothing -> userSaysYes question
--     Just b  -> return b

-- TODO should this return the main dir or .git/annex inside it?
-- findAnnex :: FilePath -> IO (Maybe FilePath)
-- findAnnex path = do
--   absPath <- fromJust <$> absolute path -- TODO can this fail?
--   let aPath = absPath </> ".git" </> "annex"
--   foundIt <- doesDirectoryExist aPath
--   if foundIt
--     then return $ Just $ SF.takeDirectory $ SF.takeDirectory aPath
--     else if null $ components absPath
--       then return Nothing
--       else findAnnex $ SF.takeDirectory absPath

-- inAnnex :: FilePath -> IO Bool
-- inAnnex = fmap (not . null) . findAnnex

-- withAnnex :: FilePath -> (FilePath -> IO a) -> IO a
-- withAnnex path fn = do
--   aPath <- findAnnex path
--   case aPath of
--     Nothing -> error $ "'" ++ path ++ "' is not in a git-annex repo"
--     Just dir -> do
--       -- log cfg $ "using git-annex repo '" ++ dir ++ "'"
--       fn dir

-- We reuse the existing SHA256SUM from the link
-- TODO is this less efficient than putting all the logic in one function?
-- isAnnexSymlink :: FilePath -> IO Bool
-- isAnnexSymlink path = do
--   status <- getSymbolicLinkStatus path
--   if not (isSymbolicLink status)
--     then return False
--     else do
--       l <- readSymbolicLink path
--       return $ ".git/annex/objects/" `isInfixOf` l && "SHA256E-" `isPrefixOf` SF.takeBaseName l
