{-# LANGUAGE OverloadedStrings #-}

import System.Directory.BigTrees
import System.Directory.BigTrees.HashTree
import System.Directory.BigTrees.Delta

-- how to print them out nicely in stack repl:
-- >>> import Text.Pretty.Simple
-- >>> pPrint failsRoundTripToDir1

-- how to diff before vs after:
-- >>> res <- roundTripTestTreeToDir failsRoundTripToDir1
-- >>> diff failsRoundTripToDir1 res
-- [Rm "\160431/\1288",Rm "\160431/\53199]"]

-- wait, not an encoding error! works when written + read manually
-- TODO so probably also related to disappearing tmpfiles? hooray :D
failsRoundTripToDir1 :: TestTree
failsRoundTripToDir1 =
  Dir
    { name = Name "\xf58e6\x1057cc"
    , hash = Hash
        { unHash = "NjAxYWM0OTY1M2RkOGNm" }
    , contents =
        [ Dir
            { name = Name "𧊯"
            , hash = Hash
                { unHash = "NWQxZTY4ZGRlNmFmNTRj" }
            , contents =
                [ File
                    { name = Name "Ԉ"
                    , hash = Hash
                        { unHash = "ZTNiMGM0NDI5OGZjMWMx" }
                    , fileData = ""
                    }
                , File
                    { name = Name "쿏]"
                    , hash = Hash
                        { unHash = "YjdkMjUyOTZlN2JjNmE2" }
                    , fileData = "Û"
                    }
                ]
            , nFiles = 2
            }
        ]
    , nFiles = 2
    }


-- TODO agian, not an encoding error:
--
-- >>> writeTree "failsRoundTripToDir2.bigtree" failsRoundTripToDir2
-- >>> res2 <- readTree Nothing "failsRoundTripToDir2.bigtree" 
-- >>> dropFileData failsRoundTripToDir2 == res2
-- True
--
failsRoundTripToDir2 :: TestTree
failsRoundTripToDir2 =
  Dir
    { name = Name "\xf6847"
    , hash = Hash
        { unHash = "ODkzNTQzYjU1MjljNWFh" }
    , contents =
        [ Dir
            { name = Name "*\xfc5a1-"
            , hash = Hash
                { unHash = "OTkxNjI4OWVhNjUyYmE0" }
            , contents =
                [ File
                    { name = Name "🮡"
                    , hash = Hash
                        { unHash = "ZTNiMGM0NDI5OGZjMWMx" }
                    , fileData = ""
                    }
                , File
                    { name = Name "\xfec76_"
                    , hash = Hash
                        { unHash = "ZDA3NTJiNjBhZGIxNDhj" }
                    , fileData = "ç"
                    }
                ]
            , nFiles = 2
            }
        ]
    , nFiles = 2
    }
