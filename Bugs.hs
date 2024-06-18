{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import System.Directory.BigTrees
import System.Directory.BigTrees.Delta
import System.Directory.BigTrees.HashLine
import System.Directory.BigTrees.HashTree
import System.OsPath
import Text.Pretty.Simple
import qualified Data.ByteString.Char8 as B8

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
-- issue01example1 :: TestTree
-- issue01example1 =
--   Dir
--     { name = Name "\xf58e6\x1057cc"
--     , hash = Hash
--         { unHash = "NjAxYWM0OTY1M2RkOGNm" }
--     , modTime = ModTime 0
--     , contents =
--         [ Dir
--             { name = Name "𧊯"
--             , hash = Hash
--                 { unHash = "NWQxZTY4ZGRlNmFmNTRj" }
--             , modTime = ModTime 0
--             , contents =
--                 [ File
--                     { name = Name "Ԉ"
--                     , hash = Hash
--                         { unHash = "ZTNiMGM0NDI5OGZjMWMx" }
--                     , modTime = ModTime 0
--                     , fileData = ""
--                     }
--                 , File
--                     { name = Name "쿏]"
--                     , hash = Hash
--                         { unHash = "YjdkMjUyOTZlN2JjNmE2" }
--                     , modTime = ModTime 0
--                     , fileData = "Û"
--                     }
--                 ]
--             , nINodes = 2
--             }
--         ]
--     , nINodes = 2
--     }

-- issue01example2 :: TestTree
-- issue01example2 =
--   Dir
--     { name = Name "\xf6847"
--     , hash = Hash
--         { unHash = "ODkzNTQzYjU1MjljNWFh" }
--     , modTime = ModTime 0
--     , contents =
--         [ Dir
--             { name = Name "*\xfc5a1-"
--             , hash = Hash
--                 { unHash = "OTkxNjI4OWVhNjUyYmE0" }
--             , modTime = ModTime 0
--             , contents =
--                 [ File
--                     { name = Name "🮡"
--                     , hash = Hash
--                         { unHash = "ZTNiMGM0NDI5OGZjMWMx" }
--                     , modTime = ModTime 0
--                     , fileData = ""
--                     }
--                 , File
--                     { name = Name "\xfec76_"
--                     , hash = Hash
--                         { unHash = "ZDA3NTJiNjBhZGIxNDhj" }
--                     , modTime = ModTime 0
--                     , fileData = "ç"
--                     }
--                 ]
--             , nINodes = 2
--             }
--         ]
--     , nINodes = 2
--     }

-- issue02example1 :: TestTree
-- issue02example1 =
--   File {name = Name "\1082166", hash = Hash {unHash = "ZTNiMGM0NDI5OGZjMWMx"}, modTime = ModTime 0, fileData = ""}

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

-- fail05 = File {nodeData = NodeData {name = Name {unName = [osp|\SOH|]}, hash = Hash {unHash = "ZTNiMGM0NDI5OGZjMWMx"}, modTime = ModTime 1224912839, nBytes = NBytes 0}, fileData = ()}

-- this round-trips fine to a .bigtree file, but not to a dir
-- fail06 :: TestTree
-- fail06 = Dir {nodeData = NodeData {name = Name {unName = [osp|\SOH|]}, hash = Hash {unHash = "MjQ0MWI1ODE3NGNiZDU5"}, modTime = ModTime 1515282806, nBytes = NBytes 4097}, nNodes = NNodes 2, dirContents = [File {nodeData = NodeData {name = Name {unName = [osp|\SOH|]}, hash = Hash {unHash = "MWRkODMxMjYzNmY2YTBi"}, modTime = ModTime 1333427991, nBytes = NBytes 1}, fileData = "\204"}]}
-- 
-- fail07 :: TestTree
-- fail07 = Dir {nodeData = NodeData {name = Name {unName = [osp|\EOT\SOH\ETX|]}, hash = Hash {unHash = "MjYzNzdjNTVkZWI0NzRh"}, modTime = ModTime 1320735778, nBytes = NBytes 4096}, nNodes = NNodes 2, dirContents = [File {nodeData = NodeData {name = Name {unName = [osp|\STX|]}, hash = Hash {unHash = "ZTNiMGM0NDI5OGZjMWMx"}, modTime = ModTime 1007930439, nBytes = NBytes 0}, fileData = ""}]}
-- 
-- fail08 :: [HashLine]
-- fail08 = [HashLine (D,Depth 0,Hash {unHash = "MDc1MTk4YmZlNjE3NjVk"},ModTime 1204008124,NBytes 8500,NNodes 4151,Name {unName = [osp|\SOH|]},Nothing)]
-- 
-- fail09 = [HashLine (D,Depth 6,Hash {unHash = "N2E1MjNkYTRiNzg4Nzcz"},ModTime 1009817894,NBytes 654,NNodes 5647,Name {unName = [osp|\SOH|]},Nothing),HashLine (F,Depth 5,Hash {unHash = "OThjZjNmM2U3M2MwOTFm"},ModTime 1668486477,NBytes 9717,NNodes 1,Name {unName = [osp|\SOH|]},Nothing)]
-- 
-- fail10 = File {nodeData = NodeData {name = Name {unName = [osp|\SOH|]}, hash = Hash {unHash = "ZTNiMGM0NDI5OGZjMWMx"}, modTime = ModTime 1471508216, nBytes = NBytes 0}, fileData = ()}
-- 
-- fail11 :: TestTree
-- fail11 = Dir {nodeData = NodeData {name = Name {unName = [osp|\STX|]}, hash = Hash {unHash = "MDVhZGY3YzA3M2RmZjcw"}, modTime = ModTime 1537245810, nBytes = NBytes 4097}, nNodes = NNodes 2, dirContents = [File {nodeData = NodeData {name = Name {unName = [osp|\SOH\STX|]}, hash = Hash {unHash = "NjdjODcyZDQ5MTJjNzFm"}, modTime = ModTime 1054765421, nBytes = NBytes 1}, fileData = "\148"}]}
-- 
-- fail12 :: TestTree
-- fail12 = File {nodeData = NodeData {name = Name {unName = [osp|\SOH|]}, hash = Hash {unHash = "ZTNiMGM0NDI5OGZjMWMx"}, modTime = ModTime 1555224467, nBytes = NBytes 0}, fileData = ""}

-- fails because there's an initial \n, which should probably be consumed by the prev lexeme?
fail14 :: B8.ByteString
fail14 = "\nD\t4\tN2QxYmFmNjIyMGFjMjZj\t1191453738\t12288\t5\t\ENQ\a\ETX\NUL\nF\t4\tZjJmMmQ3ZGY5ODlkMDM2\t1119567412\t10\t1\tq\196\239YkU\178\243\206\213\NUL\nD\t3\tM2MxOGZkNWJmMWY3M2Vl\t1684178999\t40972\t18\t\246\204\202\ETB\138\195\SI\167\144\173\191\231\129\148\NUL\nD\t2\tNmE0MzkwMjc0Yjc1MjAx\t1312297158\t53262\t24\t\208\234\156\&3\NUL\nD\t1\tNWVlZjBjMDExZWFlYjQ4\t1120357081\t139301\t64\t\161t\US\237h\254qq\SYNi\243\136\151*\232\230;\130\ENQ\168\217R\253\&6\221\174\b\166\176L\226J\224\EMnD\161\232f\162!\208@\133z\159<\NUL\nF\t1\tODYwZTNhNDg3Y2FlYjdm\t1108713965\t55\t1\t\188,\186\255R\224\248\255\176b>\SOH(\ACK\v\156,\bo\253\198PW\201\190v\244\179\198P\175\235\237Pd\162cRK\189Is|R\250\157\221\STXNU>\129L\162\138\ACK\ACKHd\167\240\220\&8<\170z\NUL\nD\t0\tYWMzNzFiZDQ4NjAzNTZi\t1065258297\t192633\t90\t\250\206H\NUL\n# {\n#   \"scanEnd\": 1718741812\n# }\n\NUL\n"
