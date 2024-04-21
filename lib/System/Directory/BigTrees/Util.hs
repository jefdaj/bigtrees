{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Directory.BigTrees.Util where
  -- ( absolutize
  -- , dropDir
  -- , dropDir'
  -- , findAnnex
  -- , inAnnex
  -- , noSlash
  -- , pathComponents
  -- , userSaysYes
  -- , withAnnex
  -- , isAnnexSymlink
  -- , isNonAnnexSymlink
  -- , FileName(..)
  -- , n2p
  -- , p2n
  -- -- , n2bs
  -- -- , bs2n
  -- )
  -- where

-- TODO remove this from Util
-- import BigTrees.Config (Config(..))

import           Prelude                          hiding (log)

import           Data.List                        (isInfixOf, isPrefixOf)
import           Data.Maybe                       (fromJust)
import           System.Directory                 (canonicalizePath,
                                                   doesDirectoryExist,
                                                   getCurrentDirectory,
                                                   getHomeDirectory)
import           System.FilePath                  (addTrailingPathSeparator,
                                                   joinPath, normalise,
                                                   pathSeparator, splitPath,
                                                   takeBaseName, takeDirectory,
                                                   (</>))
import           System.IO                        (hClose, hFlush, stdout)
import           System.Path.NameManip            (absolute_path, guess_dotdot)
import           System.Posix.Files               (getSymbolicLinkStatus,
                                                   isSymbolicLink,
                                                   readSymbolicLink)

-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.UTF8             as BU
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE

import           Data.Store                       (Store (..), decodeIO, encode)
import qualified Filesystem.Path.CurrentOS        as OS
import           System.Info                      (os)
import           TH.Derive

import           Control.DeepSeq
import           GHC.Generics

import           Test.HUnit                       (Assertion, (@=?))
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
-- import Test.Hspec
import           Control.Exception                (evaluate)
import           Control.Monad.IO.Class           (liftIO)
import           Test.QuickCheck.Instances

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8            as B
import           System.IO.Temp                   (withSystemTempDirectory)
import           Test.QuickCheck.Unicode          (char, list)

pathComponents :: FilePath -> [FilePath]
pathComponents f = filter (not . null)
                 $ map (filter (/= pathSeparator))
                 $ splitPath f

-- TODO is there a potential for infinite recursion bugs here?
absolutize :: FilePath -> IO (Maybe FilePath)
absolutize path = do
  path' <- absolutize' path
  case path' of
    Nothing -> return Nothing
    Just p' -> if p' == path
                 then Just <$> canonicalizePath p'
                 else absolutize p'

-- based on: schoolofhaskell.com/user/dshevchenko/cookbook
absolutize' :: FilePath -> IO (Maybe FilePath)
absolutize' aPath
    | null aPath = return Nothing
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ Just $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = do
        -- let aPath' = guess_dotdot aPath
        aPath' <- absolute_path aPath
        case guess_dotdot aPath' of
          Nothing -> return $ Just aPath
          Just p  -> return $ Just p
        -- return $ guess_dotdot pathMaybeWithDots -- TODO this is totally wrong sometimes!

-- absolutize :: FilePath -> IO FilePath
-- absolutize p = do
--   wd <- getCurrentDirectory
--   canonicalizePath (wd </> p)

-- TODO this fails on the leading / in a full path?
dropDir :: FilePath -> FilePath
dropDir = joinPath . tail . splitPath

dropDir' :: FilePath -> FilePath
dropDir' path = case path of
  ('/':p) -> dropDir p
  p       -> dropDir p

noSlash :: FilePath -> FilePath
noSlash = reverse . dropWhile (== '/') . reverse

userSaysYes :: String -> IO Bool
userSaysYes question = do
  putStr $ question ++ " (yes/no) "
  hFlush stdout
  let answers = [("yes", True), ("no", False)]
  answer <- getLine
  case lookup answer answers of
    Nothing -> userSaysYes question
    Just b  -> return b

-- TODO should this return the main dir or .git/annex inside it?
findAnnex :: FilePath -> IO (Maybe FilePath)
findAnnex path = do
  absPath <- fromJust <$> absolutize path -- TODO can this fail?
  let aPath = absPath </> ".git" </> "annex"
  foundIt <- doesDirectoryExist aPath
  if foundIt
    then return $ Just $ takeDirectory $ takeDirectory aPath
    else if null $ pathComponents absPath
      then return Nothing
      else findAnnex $ takeDirectory absPath

inAnnex :: FilePath -> IO Bool
inAnnex = fmap (not . null) . findAnnex

withAnnex :: FilePath -> (FilePath -> IO a) -> IO a
withAnnex path fn = do
  aPath <- findAnnex path
  case aPath of
    Nothing -> error $ "'" ++ path ++ "' is not in a git-annex repo"
    Just dir -> do
      -- log cfg $ "using git-annex repo '" ++ dir ++ "'"
      fn dir

-- We reuse the existing SHA256SUM from the link
-- TODO is this less efficient than putting all the logic in one function?
isAnnexSymlink :: FilePath -> IO Bool
isAnnexSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return False
    else do
      l <- readSymbolicLink path
      return $ ".git/annex/objects/" `isInfixOf` l && "SHA256E-" `isPrefixOf` takeBaseName l


-- We treat these as files rather than following to avoid infinite cycles
isNonAnnexSymlink :: FilePath -> IO Bool
isNonAnnexSymlink path = do
  status <- getSymbolicLinkStatus path
  if not (isSymbolicLink status)
    then return False
    else do
      link <- readSymbolicLink path
      return $ not $ (".git/annex/objects/" `isInfixOf` link)
                  && ("SHA256E-" `isPrefixOf` takeBaseName link)

-- from System.Directory.Tree --

-- | an element in a FilePath:
-- The newtype is needed to prevent overlapping with the standard Arbitrary
-- Text instance in the tests
newtype FileName
  = FileName T.Text
  deriving (Eq, Generic, Ord, Read, Show)

deriving instance NFData FileName

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
  instance Deriving (Store FileName)
  |]))

n2p :: FileName -> FilePath
n2p (FileName t) = (if os == "darwin"
                      then B8.unpack . TE.encodeUtf8
                      else T.unpack) t

p2n :: FilePath -> FileName
p2n = FileName . (if os == "darwin"
                    then TE.decodeUtf8 . B8.pack
                    else T.pack)

-- n2bs :: FileName -> BU.ByteString
-- n2bs = BU.fromString . n2p

-- TODO should this have the option for a decoding error?
-- bs2n :: BU.ByteString -> FileName
-- bs2n = p2n . BU.toString

-----------
-- tests --
-----------

{- My `FileName` type is defined in `Util` as `Text` for efficiency, but
 - what it really means is "Text without slashes or null chars". So I have to
 - define my own Arbitrary instance here.
 -
 - TODO why is the not . null thing required to prevent empty strings? list1 should be enough
 - TODO wait, is the empty string also a valid filename?
 -}
instance Arbitrary FileName where
  arbitrary = FileName <$> (arbitrary :: Gen T.Text) `suchThat` validFileName
  shrink (FileName t) = FileName <$> filter validFileName (shrink t)

validFileName :: T.Text -> Bool
validFileName t = notElem t ["", ".", ".."]
               && (not . T.any (== '/')) t -- no separators
               && (OS.valid . OS.fromText) t

prop_roundtrip_filename_to_bytestring :: FileName -> Bool
prop_roundtrip_filename_to_bytestring n = p2n (n2p n) == n

roundtrip_filename_to_name_of_tmpfile :: FileName -> IO ()
roundtrip_filename_to_name_of_tmpfile n = withSystemTempDirectory "roundtriptemp" $ \d -> do
  let f = d </> n2p n
  B.writeFile f "this is a test"
  _ <- B.readFile f
  return ()

prop_roundtrip_filename_to_name_of_tmpfile :: Property
prop_roundtrip_filename_to_name_of_tmpfile = monadicIO $ do
  n <- pick arbitrary
  run $ roundtrip_filename_to_name_of_tmpfile n
  assert True

-- describe "Util" $ do
--   describe "absolutize" $ do
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

newtype ValidFilePath
  = ValidFilePath FilePath
  deriving (Eq, Ord, Read, Show)

instance Arbitrary ValidFilePath where
  arbitrary = do
    prefix <- oneof $ map pure ["", ".", "..", "~"]
    comps  <- map (\ (FileName t) -> T.unpack t)
      <$> listOf (arbitrary :: Gen FileName)
    let path = joinPath (prefix:comps)
    return $ ValidFilePath $ if null path then "/" else path

-- |
-- >>> let x = 23
-- >>> x + 42
-- 65
unit_absolutize_expands_tildes :: Assertion
unit_absolutize_expands_tildes = do
  home <- getHomeDirectory
  let explicit = home </> "xyz"
  (Just implicit) <- absolutize "~/xyz"
  implicit @=? explicit

unit_absolutize_rejects_the_null_path :: Assertion
unit_absolutize_rejects_the_null_path = do
  reject <- absolutize ""
  reject @=? Nothing

unit_absolutize_fixes_invalid_dotdot_path :: Assertion
unit_absolutize_fixes_invalid_dotdot_path = do
  fixed <- absolutize "/.." -- one level above / is invalid
  fixed @=? Just "/"

prop_absolutize_is_idempotent :: ValidFilePath -> Property
prop_absolutize_is_idempotent (ValidFilePath path) = monadicIO $ do
  (Just path' ) <- liftIO $ absolutize path
  (Just path'') <- liftIO $ absolutize path'
  assert $ path' == path''

prop_absolutize_strips_redundant_dotdot :: ValidFilePath -> Property
prop_absolutize_strips_redundant_dotdot (ValidFilePath path) = monadicIO $ do
  (Just a ) <- fmap (fmap takeDirectory) $ liftIO $ absolutize path
  (Just a') <- liftIO $ absolutize $ path </> ".."
  assert $ a == a'

prop_absolutize_strips_redundant_dot :: ValidFilePath -> Property
prop_absolutize_strips_redundant_dot (ValidFilePath path) = monadicIO $ do
  (Just a ) <- liftIO $ absolutize path
  (Just a') <- liftIO $ absolutize $ path </> "."
  assert $ a == a'
