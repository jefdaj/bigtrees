# BigTrees

<img align="right" src="bigtrees.png"></img>

A rewrite of [gander][1] focusing more on usability of the data
structures as a library, rather than on my own "dedup backups" use case.

## Dev quick start

This is the fastest way to get hacking:

``` .sh
git clone https://github.com/jefdaj/bigtrees
cd bigtrees
nix-shell --run 'stack test'
```

[1]: https://github.com/jefdaj/gander
