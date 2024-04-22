{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Custom FilePath type

I've had issues properly encoding some filenames using the standard libraries.
This fixes most of them.
-}

-- TODO am I just using the standard ones wrong?

module System.Directory.BigTrees.FilePath where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Prelude hiding (log)
import System.Directory (canonicalizePath, getHomeDirectory)
import System.Directory.BigTrees.FileName
import qualified System.FilePath as SF
import System.FilePath ((</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Path.NameManip (absolute_path, guess_dotdot)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

-- TODO name this something less confusing
n2p :: FileName -> FilePath
n2p (FileName t) = (if os == "darwin"
                      then B.unpack . TE.encodeUtf8
                      else T.unpack) t

-- TODO name this something less confusing
p2n :: FilePath -> FileName
p2n = FileName . (if os == "darwin"
                    then TE.decodeUtf8 . B.pack
                    else T.pack)

-- TODO FilePath?
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

-- n2bs :: FileName -> BU.ByteString
-- n2bs = BU.fromString . n2p

-- TODO should this have the option for a decoding error?
-- bs2n :: BU.ByteString -> FileName
-- bs2n = p2n . BU.toString

newtype ValidFilePath
  = ValidFilePath FilePath
  deriving (Eq, Ord, Read, Show)

instance Arbitrary ValidFilePath where
  arbitrary = do
    prefix <- oneof $ map pure ["", ".", "..", "~"]
    comps  <- map (\ (FileName t) -> T.unpack t)
      <$> listOf (arbitrary :: Gen FileName)
    let path = SF.joinPath (prefix:comps)
    return $ ValidFilePath $ if null path then "/" else path

-- TODO haddocks
pathComponents :: FilePath -> [FilePath]
pathComponents f = filter (not . null)
                 $ map (filter (/= SF.pathSeparator))
                 $ SF.splitPath f

-- TODO own section
-- TODO haddocks
-- TODO is there a potential for infinite recursion bugs here?
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


-- TODO haddocks
-- We treat these as files rather than following to avoid infinite cycles
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

-----------
-- tests --
-----------
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

-- |
-- >>> let x = 23
-- >>> x + 42
-- 65
unit_absolute_expands_tildes :: Assertion
unit_absolute_expands_tildes = do
  home <- getHomeDirectory
  let explicit = home </> "xyz"
  (Just implicit) <- absolute "~/xyz"
  implicit @=? explicit

unit_absolute_rejects_the_null_path :: Assertion
unit_absolute_rejects_the_null_path = do
  reject <- absolute ""
  reject @=? Nothing

unit_absolute_fixes_invalid_dotdot_path :: Assertion
unit_absolute_fixes_invalid_dotdot_path = do
  fixed <- absolute "/.." -- one level above / is invalid
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
