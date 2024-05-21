{-# LANGUAGE OverloadedStrings #-}

import System.Directory.BigTrees
import System.Directory.BigTrees.HashTree
import System.Directory.BigTrees.Delta
import Text.Pretty.Simple

-- how to print them out nicely in stack repl:
-- >>> pPrint failsRoundTripToDir1

-- how to diff before vs after:
-- >>> res <- roundTripTestTreeToDir failsRoundTripToDir1
-- >>> diff failsRoundTripToDir1 res
-- [Rm "\160431/\1288",Rm "\160431/\53199]"]

-- how to diff them:
--
-- >>> writeTestTreeDir "failsRoundTripToDir1" failsRoundTripToDir1
-- >>> res1 <- buildProdTree False [] "./failsRoundTripToDir1"
-- >>> diff (dropFileData failsRoundTripToDir1) res1
-- ... big diff ...

-- TODO wait, it is a real issue uncovered with my read/write function(s)!
-- TODO see if you can manually shrink it: to just one dir without any contents? just one file?
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
