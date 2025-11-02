{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-|
Description: Name handling

I've had issues properly encoding some filenames using the standard libraries.
This fixes most of them.
This module is for the custom `Name` type used in trees.
There is also a `Path` module, but it holds unrelated-in-principle utility functions.

-}

-- TODO actually remove the overview at the top? the individual types + functions make more sense

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
-- TODO wait, is the empty string also a valid filename?

module System.Directory.BigTrees.Name

  -- TODO document these individually
  ( Name(..)
  , NamesFwd
  , NamesRev

  , bytes2n
  -- TODO n2bytes
  , n2op
  , n2sbs
  , sbs2n
  , sbs2op
  , fp2n
  , fp2ns
  , n2bs
  , bs2n
  , breadcrumbs2bs
  , op2breadcrumbs
  , joinNames
  , names2bs
  , os2ns
  , op2ns
  , op2bs
  , op2s
  , bs2op

  , nameP
  , b64Name
  , debugName

  -- tests
  -- TODO document tests as a group
  , isValidName
  , roundtripNameToActualFileName
  , roundtripNameToActualDirName
  , prop_roundtrip_Name_to_actual_file_name
  , prop_roundtrip_Name_to_actual_dir_name

  )
  where

import Test.QuickCheck
import Test.QuickCheck.Gen

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.List (isInfixOf, isPrefixOf, nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Filesystem.Path.CurrentOS as OS
import GHC.Generics (Generic)
import Prelude hiding (log)
import qualified System.Directory.Tree as DT
-- import qualified System.FilePath as SF
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Path.NameManip (absolute_path, guess_dotdot)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import TH.Derive (Deriving, derive)

-- attempt at proper new string types:
-- import System.FilePath ((</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS
import qualified System.Directory.OsPath as SDO
import qualified System.File.OsPath as SFO
import qualified System.OsPath as SOP
import qualified System.OsPath.Internal as SOPI
import qualified System.OsString as SOS
import qualified System.OsString.Internal.Types as SOS
import Test.QuickCheck.Instances.ByteString

import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, digit, endOfInput,
                                         endOfLine, isEndOfLine, manyTill, parseOnly, take,
                                         takeTill)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.Combinator (lookAhead, option, sepBy')
import System.OsPath (OsPath)
import Data.Word (Word8)


-- | An element in a FilePath. My `Name` type is defined as `OsPath` for
-- efficiency, but what it really means is "OsPath without slashes". Based on
-- the one in `System.Directory.Tree`. The newtype is needed to prevent
-- overlapping with the standard Arbitrary Text instance in the tests. There's
-- no point using OsPath here because Windows is already unsupported.
-- TODO why doesn't the tree link work right
newtype Name
  = Name { unName :: SOS.OsString } -- TODO OsPath? It's the exact same thing as far as I can tell
  deriving (Eq, Generic, Ord)

deriving instance NFData Name

-- A hack to get Tasty to print usable test failures without garbling the Names by `show`ing them.
instance Show Name where
  show name = "(b64Name \"" ++ B8.unpack (B64.encode $ n2bs name) ++ "\")"

-- Helper for the Show hack.
-- Example usage:
--
-- ghci> :set -XOverloadedStrings
-- ghci> :m System.Directory.BigTrees
-- ghci> let t1 = <paste failing tree from Tasty output here>
-- ghci> writeTree emptySearchConfig NoLog [osp|/tmp/t1.bigtree|] t1
--
b64Name :: String -> Name
b64Name base64str = bs2n $ B64.decodeLenient $ B8.pack base64str

-- Helper for the Show hack.
-- Example usage:
--
-- ghci> debugName (b64Name "JBIHKh0+HxM/Nww=")
-- Base64: "JBIHKh0+HxM/Nww=
-- Chars:  "$\DC2\a*\GS>\US\DC3?7\f"
-- Bytes:  [36,18,7,42,29,62,31,19,63,55,12]
--
debugName :: Name -> IO ()
debugName name = do
  putStrLn $ "Base64: " ++ drop 10 (Prelude.take (length s - 2) s)
  putStrLn $ "Chars:  " ++ show (B8.unpack $ n2bs name) -- As characters
  putStrLn $ "Bytes:  " ++ show (BS.unpack $ n2bs name) -- As Word8 values
  where s = show name

validFilenameBytes :: [Word8]
validFilenameBytes = filter isValidFilenameByte [1..255]

isValidFilenameByte :: Word8 -> Bool
isValidFilenameByte b =
  b /= 0        -- no null bytes
  && b /= 47    -- no forward slash (/)
  && not (isProblematicByte b)

{- I'm not sure how to handle these yet.
 - They should technically be valid, but are causing errors.
 - I could dig into improving other libraries' handling, or ignore it.
 - TODO are the problems to do with single bytes or sequences of 2+?
 - TODO separate list on linux vs macos? per filesystem?
 - TODO what about: >= 32 (most control chars), 127 (DEL)
 -}
isProblematicByte :: Word8 -> Bool
isProblematicByte 46 = True -- start of header (the byte displayed as . and ..?)
isProblematicByte _  = False

instance Arbitrary Name where
  arbitrary = do
    len <- chooseInt (1, 255) -- max filename length on most systems
    bytes <- vectorOf len $ elements validFilenameBytes
    let name = bytes2n bytes
    if isValidName name
      then pure name
      else arbitrary  -- retry if we got "." or ".."

  shrink (Name osStr) =
    let sbs = SOS.getPosixString (SOS.getOsString osStr) -- TODO is this == n2sbs without Name?
        bytes = SBS.unpack sbs
        shorterBytes = filter (not . null) $ shrink bytes
        candidateNames = [bytes2n bs | bs <- shorterBytes]
    in filter isValidName candidateNames  -- filter shrunk results too

isValidName :: Name -> Bool
isValidName name =
  -- let sbs = SOS.getPosixString (SOS.getOsString osStr) -- TODO is this == n2sbs without Name?
  -- let sbs = _ name
  let sbs = n2bytes name
  in not (null sbs) -- not empty
     && all isValidFilenameByte sbs
     && sbs /= [46]      -- not "."
     && sbs /= [46, 46]  -- not ".."


-- * Convert paths to/from names
--
-- $convertnamespaths
--
-- Functions for converting between `Name`s and (regular Haskell) `FilePath`s.
-- They should work on Linux and MacOS.

-- n2sbs :: Name -> SBS.ShortByteString
-- n2sbs = SOS.unPS . SBS.unOsString . unName

bytes2n :: [Word8] -> Name
bytes2n bs = Name $ SOS.OsString $ SOS.PosixString $ SBS.pack bs

n2bytes :: Name -> [Word8]
n2bytes = SBS.unpack . SOS.getPosixString . SOS.getOsString . unName

n2op :: Name -> SOS.OsString
n2op = unName

-- | Note this does NOT check whether it's a valid Name.
-- sbs2n :: SBS.ShortByteString -> Name
-- sbs2n = Name . sbs2op

sbs2op :: SBS.ShortByteString -> OsPath
sbs2op = SOS.OsString . SOS.PosixString

-- TODO not available until later version? n2sbs (Name (SOS.OsString ps)) = SOS.unPFP ps
n2sbs :: Name -> SBS.ShortByteString
n2sbs (Name (SOS.OsString (SOS.PS sbs))) = sbs

sbs2n :: SBS.ShortByteString -> Name
sbs2n sbs = Name (SOS.OsString (SOS.PS sbs))

-- | Convert a `FilePath` to a `Name` using the current filesystem's encoding,
-- or explain why the conversion failed.
fp2n :: FilePath -> IO (Either String Name)
fp2n fp = do
  ns <- fp2ns fp -- TODO catch error here and wrap it in Left too
  return $ case ns of
    []  -> Left "fp2n with null path"
    [n] -> if isValidName n then Right n else Left $ "invalid name: " ++ show n
    ns  -> Left "fp2n with slash in path"

-- | Convert a `FilePath` to a list of `Name`s using the current filesystem's encoding.
-- TODO or explain why the conversion failed?
fp2ns :: FilePath -> IO [Name]
fp2ns fp = do
  osstr <- SOS.encodeFS fp -- TODO catch error here and wrap in Left?
  let osstrs = SOP.splitDirectories osstr
  return $ map Name osstrs

-- | Direct conversion from a Name to a ByteString for serializing.
-- TODO make it an instance of Bytable, Binary, similar?
n2bs :: Name -> B8.ByteString
n2bs = SBS.fromShort . n2sbs

-- | Direct conversion from a ByteString to a Name for deserializing.
-- TODO make it an instance of Bytable, Binary, similar?
bs2n :: B8.ByteString -> Name
bs2n = sbs2n . SBS.toShort

os2ns :: SOS.OsString -> [Name]
os2ns = map Name . SOP.splitDirectories

op2ns :: SOP.OsPath -> [Name]
op2ns = os2ns

-- | Extra type alias to distinguish lists of Names representing a path in
-- forward vs reverse order. Both can be converted to/from OsPaths.
type NamesFwd = [Name]

-- | NamesRev are a list of names leading to the current node, like an anchor
-- path but sorted in reverse order because we want `cons` to be fast.
-- Sometimes called "breadcrumbs", although I'll try to be more consistent.
type NamesRev = [Name]

-- TODO rename?
breadcrumbs2bs :: NamesRev -> B8.ByteString
breadcrumbs2bs = joinNames . reverse

op2breadcrumbs :: SOP.OsPath -> NamesRev
op2breadcrumbs = reverse . op2ns

-- TODO was this needed for anything else?
-- TODO can it be done better via SOP.joinPath?
joinNames :: [Name] -> B8.ByteString
joinNames = B8.intercalate (B8.singleton '/') . map n2bs

names2bs :: NamesFwd -> B8.ByteString
names2bs = SBS.fromShort . SOS.getPosixString . SOS.getOsString . SOP.joinPath . map unName

-- TODO is this valid?
op2bs :: OsPath -> B8.ByteString
op2bs = SBS.fromShort . SOS.getPosixString . SOS.getOsString

-- TODO is this valid?
op2s :: OsPath -> String
op2s = B8.unpack . op2bs

-- TODO is this valid?
bs2op :: B8.ByteString -> OsPath
bs2op = SOS.OsString . SOS.PosixString . SBS.toShort

-- TODO is Attoparsec.ByteString suitable for this, or do I need to parse them some other way?
nameP :: Parser Name
nameP = do
  -- TODO sepP here?
  bs <- takeTill (== '\NUL')
  _  <- char '\NUL'
  -- _  <- option undefined $ char '\t' -- TODO if this works, move sepP from HashLine
  return $ bs2n bs

-- Fails if there's an error writing the file, or if after writing it doesn't
-- exist. Example manual usage:
--
-- >>> ns <- generate (resize 99 $ arbitrary :: Gen [Name])
-- >>> fmap (all id) $ mapM_ (roundtripNameToActualFileName False) ns
-- >>> True
--
-- Set verbose=True to show the paths, but beware! They might mess up your terminal.
--
-- TODO is there a standard variant of `all` that works like this?
--
roundtripNameToActualFileName :: Bool -> Name -> IO Bool
roundtripNameToActualFileName verbose n =
  withSystemTempDirectory "bigtrees" $ \d -> do
    d' <- SOP.encodeFS d
    let f = d' SOP.</> unName n
    let txt = "this is a test"
    SFO.writeFile f txt
    when verbose $ print f
    txt' <- SFO.readFile f
    return $ txt == txt'

prop_roundtrip_Name_to_actual_file_name :: Property
prop_roundtrip_Name_to_actual_file_name = monadicIO $ do
  n <- pick arbitrary
  ok <- run $ roundtripNameToActualFileName False n
  assert ok

roundtripNameToActualDirName :: Bool -> Name -> IO Bool
roundtripNameToActualDirName verbose n =
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    tmpDir' <- SOP.encodeFS tmpDir
    let testDir = tmpDir' SOP.</> unName n
    SDO.createDirectory testDir
    when verbose $ print testDir
    cs <- SDO.getDirectoryContents tmpDir'
    return $ (unName n) `elem` cs

prop_roundtrip_Name_to_actual_dir_name :: Property
prop_roundtrip_Name_to_actual_dir_name = monadicIO $ do
  n <- pick arbitrary
  ok <- run $ roundtripNameToActualDirName False n
  assert ok
