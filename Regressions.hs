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

-- TODO looks like an encoding error? based on the diff above
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

