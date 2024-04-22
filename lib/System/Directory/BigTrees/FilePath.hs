{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.Directory.BigTrees.FilePath where

import Control.DeepSeq
import qualified Data.ByteString.Char8 as B
import Data.Store (Store (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Filesystem.Path.CurrentOS as OS
import GHC.Generics
import qualified System.FilePath as SF
import System.FilePath ((</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic
import TH.Derive

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
                      then B.unpack . TE.encodeUtf8
                      else T.unpack) t

p2n :: FilePath -> FileName
p2n = FileName . (if os == "darwin"
                    then TE.decodeUtf8 . B.pack
                    else T.pack)

-- n2bs :: FileName -> BU.ByteString
-- n2bs = BU.fromString . n2p

-- TODO should this have the option for a decoding error?
-- bs2n :: BU.ByteString -> FileName
-- bs2n = p2n . BU.toString

{- My `FileName` type is defined as `Text` for efficiency, but
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
