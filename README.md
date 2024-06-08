<img align="right" src="bigtrees.png"></img>

### BigTrees

A rewrite of [gander](https://github.com/jefdaj/gander) focusing more on
usability of the data structures as a library, rather than on my own "dedup
backups" use case.


#### Quick Start

``` .sh
git clone https://github.com/jefdaj/bigtrees
cd bigtrees

# old way, still works:
nix-shell
stack test

# new way, static build in progress:
nix build

# benchmarking
stack bench --ba --baseline=test/bench/bench.csv --timeout=60s
```

#### Done

* Moved `Gander.Cmd` -> `BigTrees.OldCmd`, leaving old commands functional during the rewrite
* Wrote a meta lint script (hlint, stan, stylish-haskell, weeder) and applied some basic suggestions
* Some initial work in progress writing haddocs
* Moved tests into lib/ + app/ alongside the functions they test, wrote more of them
* Broke HashTree into smaller modules by operation: Build, Write, etc
* Rewrote my old directory-tree code using a typeclass, started [a PR upstream](https://github.com/jberryman/directory-tree/pull/18)
* Wrote comparison of text vs binary format file sizes, realized binary is always larger, removed it
* Added mod time, size (bytes), n files (nodes) to tree data
* Added header + footer to hashes describing filters, version used, start/end time, table format
* Rename data structures: Depth, NFiles, NBytes


#### Todo

* Static build so it can be used offline without Nix
* "`find` mode": list full paths, filter by metadata and glob/regex
* Rewrite command line interface
* Add `Graft` nodes that import other tree files
* Add `Link` nodes that indicate whether their target data is present in the tree
* Add `Error` nodes to wrap errors, the same way directory-tree does it
* Intelligent re-hashing of only the files whose mod times have changed
* Clean up: write haddocks, hide partial constructors, etc
* Upload to Hackage
* Example screencasts of using the binary + data structures in repl

```
bigtrees hash   <src> [-o <tree>]
bigtrees update <tree> [-i <src>]
bigtrees cut    <tree> <branch> [-o <tree>]
bigtrees rm     <tree> <branch>
bigtrees graft  <tree> <branch> [-i <tree>]
bigtrees mv     <tree> <oldbranch> <newbranch>
bigtrees diff   <oldtree> <newtree>
bigtrees dupes  <tree> [<condition>..] [-s <sortby>] [-n <nhits>] [-p <branch>] [-d <script>]
```
