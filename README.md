<img align="right" src="bigtrees.png"></img>

### BigTrees

A rewrite of [gander](https://github.com/jefdaj/gander) focusing more on
usability of the data structures as a library, rather than on my own "dedup
backups" use case.


#### Quick Start

``` .sh
git clone https://github.com/jefdaj/bigtrees
cd bigtrees
nix-shell
```

#### Done

* Moved `Gander.Cmd` -> `BigTrees.OldCmd`, leaving old commands functional during the rewrite
* Wrote a meta lint script (hlint, stan, stylish-haskell, weeder) and applied some basic suggestions
* Some initial work in progress writing haddocs
* Moved tests into lib/ + app/ alongside the functions they test, wrote more of them
* Broke HashTree into smaller modules by operation: Build, Write, etc
* Rewrote my old directory-tree code using a typeclass, started a PR upstream for it
* Wrote comparison of text vs binary format file sizes, realized binary is always larger, removed it


#### Todo

#### Known bugs & debugging

See [Bugs.hs](Bugs.hs) for details.

* [round-trip to dir fails with certain unicode chars?](https://github.com/jefdaj/bigtrees/issues/1)
