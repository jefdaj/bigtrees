<img align="right" src="bigtrees.png"></img>

### BigTrees

A rewrite of [gander][1] focusing more on usability of the data
structures as a library, rather than on my own "dedup backups" use case.

#### Dev quick start

``` .sh
git clone https://github.com/jefdaj/bigtrees
cd bigtrees
nix-shell --run 'stack test'
```

#### Usage

`bigtrees` recursively hashes folders and uses the hashes for later comparison.
File hashes are standard `sha256sum`s, but to save space a 20-character base64
digest is used in place of the full hash.

The `F` or `D` and number before each one is for recreating the tree structure:
"this is a file at indent level 1", etc.

Directory hashes are the hash of their sorted content hashes. That way you know
that all the contents are identical, even if some file names changed.

```
$ bigtrees hash backup > backup-hashes.txt
$ cat backup-hashes.txt
F 1 ZTgwZDhlZDM3NDUxN2Uy file1.txt
F 2 Y2ZjOTQ5NGVjMTQ4M2I2 file3.txt
F 3 OGMwODk5YWZhOTllMWVh file2.txt
D 2 ZDUzYWU5MWIyNTAwNjU5 folder2
D 1 NDEzYWJiZjYyZDY3MmI1 folder1
D 0 OTY2NjU5NzE5MjczZGMx backup
```

[1]: https://github.com/jefdaj/gander
