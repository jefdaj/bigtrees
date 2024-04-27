{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs      #-}

{-|
Description: FilePath handling

I've had issues properly encoding some filenames using the standard libraries.
This fixes most of them.
It uses the standard FilePath (String) type, just with custom functions to and from `Name`s.
-}

-- TODO am I just using the standard ones wrong?
-- TODO recombine with the Name module?

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

module System.Directory.BigTrees.FilePath

  ( FilePath -- ^ Re-export of the standard type for convenience
  , absolute
  , fp2n
  , isNonAnnexSymlink
  , n2fp
  , pathComponents

  -- tests
  , prop_absolute_is_idempotent
  , prop_absolute_strips_redundant_dot
  , prop_absolute_strips_redundant_dotdot
  , prop_roundtrip_name_to_filename
  , prop_roundtrip_name_to_filepath
  , unit_absolute_expands_tildes
  , unit_absolute_fixes_invalid_dotdot
  , unit_absolute_rejects_null_path

  )
  where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Prelude hiding (log)
import System.Directory (canonicalizePath, getHomeDirectory)
import System.Directory.BigTrees.Name (Name (..))
import qualified System.FilePath as SF
import System.FilePath ((</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Path.NameManip (absolute_path, guess_dotdot)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, listOf, oneof)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import qualified System.Directory.Tree as DT

-- * Convert paths to/from names
--
-- $convertnamespaths
--
-- Functions for converting between `Name`s and (regular Haskell) `FilePath`s.
-- They should work on Linux and MacOS.

-- | Convert a `Name` to a `FilePath`
n2fp :: Name -> FilePath
n2fp (Name t) = (if os == "darwin"
                      then B.unpack . TE.encodeUtf8
                      else T.unpack) t

-- TODO this should actually convert to a list of names, right?
-- TODO and does that make it more like pathComponents?
-- | Convert a `FilePath` to a `Name`
fp2n :: FilePath -> Name
fp2n = Name . (if os == "darwin"
                    then TE.decodeUtf8 . B.pack
                    else T.pack)

-- | I plan to make a PR to the directory-tree package adding TreeName
-- TODO should probably unify FilePath and Name again and make this non-orphan
instance DT.IsName Name where
  n2p = n2fp
  p2n = fp2n

-- TODO haddocks
-- TODO fp2ns?
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

prop_roundtrip_name_to_filepath :: Name -> Bool
prop_roundtrip_name_to_filepath n = fp2n (n2fp n) == n

roundtrip_name_to_filename :: Name -> IO ()
roundtrip_name_to_filename n = withSystemTempDirectory "roundtriptemp" $ \d -> do
  let f = d </> n2fp n
  B.writeFile f "this is a test"
  _ <- B.readFile f
  return ()

prop_roundtrip_name_to_filename :: Property
prop_roundtrip_name_to_filename = monadicIO $ do
  n <- pick arbitrary
  run $ roundtrip_name_to_filename n
  assert True

newtype ValidFilePath
  = ValidFilePath FilePath
  deriving (Eq, Ord, Read, Show)

-- TODO wait, why does the built-in instance not work here? did no one write it?
instance Arbitrary ValidFilePath where
  arbitrary :: Gen ValidFilePath
  arbitrary = do
    prefix <- oneof $ map pure ["", ".", "..", "~"]
    comps  <- map (\ (Name t) -> T.unpack t)
      <$> listOf (arbitrary :: Gen Name)
    let path = SF.joinPath (prefix:comps)
    return $ ValidFilePath $ if null path then "/" else path

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

prop_absolute_is_idempotent :: ValidFilePath -> Property
prop_absolute_is_idempotent (ValidFilePath path) = monadicIO $ do
  (Just path' ) <- liftIO $ absolute path
  (Just path'') <- liftIO $ absolute path'
  assert $ path' == path''

prop_absolute_strips_redundant_dotdot :: ValidFilePath -> Property
prop_absolute_strips_redundant_dotdot (ValidFilePath path) = monadicIO $ do
  (Just a ) <- fmap (fmap SF.takeDirectory) $ liftIO $ absolute path
  (Just a') <- liftIO $ absolute $ path </> ".."
  assert $ a == a'

prop_absolute_strips_redundant_dot :: ValidFilePath -> Property
prop_absolute_strips_redundant_dot (ValidFilePath path) = monadicIO $ do
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
--     else if null $ pathComponents absPath
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
