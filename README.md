<img align="right" src="docs/src/images/bigtrees.png"></img>

### BigTrees

A re-imagining of [gander](https://github.com/jefdaj/gander) that gives up on
some aspirational "easy mode" features in favor of simplicity for power users.

The core data structures and algorithms perform very well already!
In fact I'm not aware of any open source program that's better at large-scale file deduplication.
(Scale is mainly limited by your RAM; I've tried up to ~15 million files on my laptop which required ~30G)

Now the main things left to do are:

- clean up the interface
- get all the tests passing
- write some documentation


#### Quick Start

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
stack test # TODO fix failing tests
```

``` .sh
# final static build for use outside a nix environment
nix build
ldd result/bin/bigtrees # should say "not a dynamic executable"
```

```.sh
# update the website
# uncomment the bigtrees-doc-site target in package.yaml first
stack build --flag bigtrees:build-docs-site
stack exec bigtrees-docs-site build
```

``` .sh
# benchmarking
# TODO still works?
stack bench --ba --baseline=test/bench/bench.csv --timeout=60s
```
