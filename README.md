<img src="docs/src/images/bigtrees.png"></img>

### BigTrees

Performant hash trees to dedup large collections of files.
See [the website](https://jefdaj.github.io/bigtrees) for downloads, examples, and user-facing docs.
The rest of this readme is about development.

``` .sh
git clone https://github.com/jefdaj/bigtrees
cd bigtrees
```

``` .sh
# incremental dev build using nix + stack
nix develop
stack repl
stack build
stack exec bigtrees -- <bigtrees args>
stack test bigtrees
```

Control test size and filter using env variables:

``` .sh
$ TASTY_QUICKCHECK_TESTS=10000 stack test bigtrees
bigtrees> test (suite: test-app)

test/app/Test.hs
  diff demo1 -> demo2:                                    OK (0.22s)
  dupes demo1:                                            OK (0.21s)
  cmdFind paths match unix find:                          OK (57.03s)
    +++ OK, passed 10000 tests.
  hash files extracted from tarballs
    /home/jefdaj/myrepos/bigtrees/test/app/annex1.tar.xz: OK (0.23s)
    /home/jefdaj/myrepos/bigtrees/test/app/demo1.tar.xz:  OK (0.21s)
    /home/jefdaj/myrepos/bigtrees/test/app/demo2.tar.xz:  OK (0.21s)

All 6 tests passed (58.11s)

bigtrees> Test suite test-app passed
bigtrees> test (suite: test-lib)

test/lib/Test.hs
  hash ByteString:                        OK
  hash empty file:                        OK
  hash file contents:                     OK
  hash image:                             OK
  roundtrip HashLines to ByteString:      OK (13.41s)
    +++ OK, passed 10000 tests.
  roundtrip HashSet to ByteString:        OK (4.74s)
    +++ OK, passed 10000 tests.
  roundtrip ProdTree to ByteString:       OK (3.56s)
    +++ OK, passed 10000 tests.
  roundtrip ProdTree to bigtree file:     OK (4.64s)
    +++ OK, passed 10000 tests.
  roundtrip TestTree to actual tmpdir:    OK (7.20s)
    +++ OK, passed 10000 tests.
  tree from bad path is Err:              OK
  roundtrip Err to bigtree file:          OK
  buildProdTree catches permission error: OK
  confirm file hashes:                    OK (0.91s)
    +++ OK, passed 10000 tests.
  roundtrip Name to actual file name:     OK (1.25s)
    +++ OK, passed 10000 tests.
  roundtrip Name to actual dir name:      OK (0.91s)
    +++ OK, passed 10000 tests.

All 15 tests passed (36.65s)

bigtrees> Test suite test-lib passed
```

``` .sh
$ TASTY_QUICKCHECK_TESTS=100000 TASTY_PATTERN='/Name/' stack test bigtrees
bigtrees> test (suite: test-app)

All 0 tests passed (0.00s)

bigtrees> Test suite test-app passed
bigtrees> test (suite: test-lib)

test/lib/Test.hs
  roundtrip Name to actual file name: OK (11.98s)
    +++ OK, passed 100000 tests.
  roundtrip Name to actual dir name:  OK (8.74s)
    +++ OK, passed 100000 tests.

All 2 tests passed (20.72s)
```

``` .sh
# final static build for use outside a nix environment
nix build
ldd result/bin/bigtrees # should say "not a dynamic executable"
```

``` .sh
# update the website
# uncomment the bigtrees-doc-site target in package.yaml first
stack build --flag bigtrees:build-docs-site
stack exec bigtrees-docs-site build
```

``` .sh
# benchmarking
stack bench --ba --baseline=test/bench/bench.csv --timeout=60s
```
