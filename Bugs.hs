{-# LANGUAGE OverloadedStrings #-}

import System.Directory.BigTrees
import System.Directory.BigTrees.Delta
import System.Directory.BigTrees.HashTree
import Text.Pretty.Simple

-- how to run tests:
-- TASTY_PATTERN='/roundtrip TestTree to dir/' stack test
-- TASTY_QUICKCHECK_REPLAY=844840 stack test
-- TASTY_QUICKCHECK_REPLAY=58121 TASTY_QUICKCHECK_TESTS=300 TASTY_PATTERN='/roundtrip TestTree to dir/' stack test

--------------------------------------
-- issue #1: round-trip to dirs bug --
--------------------------------------

-- how to print failing trees nicely in stack repl:
-- >>> pPrint issue11

-- how to diff them:
-- >>> writeTestTreeDir "issue11" issue11
-- >>> res1 <- buildProdTree False [] "./issue11"
-- >>> diff (dropFileData issue11) res1
-- ... big diff ...

-- TODO wait, it is a real issue uncovered with my read/write function(s)!
-- TODO see if you can manually shrink it: to just one dir without any contents? just one file?
issue01example1 :: TestTree
issue01example1 =
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

issue01example2 :: TestTree
issue01example2 =
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

issue02example1 :: TestTree
issue02example1 =
  File {name = Name "\1082166", hash = Hash {unHash = "ZTNiMGM0NDI5OGZjMWMx"}, fileData = ""}

-------------------------------
-- issue #3: cyclic symlinks --
-------------------------------

-- bigtrees: /home/user/.local/state/nix/profiles/channels-7-link/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/pkgs/test/nixpkgs-check-by-name/tests/symlink-invalid/pkgs/by-name/fo/foo/foo.nix: /home/user/.local/state/nix/profiles/channels-7-link/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/nixpkgs/pkgs/test/nixpkgs-check-by-name/tests/symlink-invalid/pkgs/by-name/fo/foo/foo.nix: getDirectoryContents:openDirStream: invalid argument (Too many levels of symbolic links)
-- CallStack (from HasCallStack):
  -- error, called at lib/System/Directory/BigTrees/HashTree/Build.hs:57:50 in bigtrees-1.0.0.0-GDWfkf2Hgs6Cl1vLfxoa8N:System.Directory.BigTrees.HashTree.Build

-----------------------------------
-- issue #4: unicode during find --
-----------------------------------
--
-- Presumably, the solution is to keep everything natively as bytestrings,
-- and only decode (but not re-encode) the printable chars when outputting to the terminal?
--
-- bigtrees find /etc/
-- ...
-- bigtrees: recoverEncode: invalid argument (cannot encode character '\65533')
