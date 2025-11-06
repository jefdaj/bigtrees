---
title: Save hashes to a .bigtree file
...

This is useful any time you want to scan something now and use it later.
That might be because:

* You want to diff the current state with a future one
* You want to search the tree later without plugging in the corresponding backup drive
* The scan takes a long time and you don't want to have to repeat it

```.sh
FILES="/path/to/files/here"
bigtrees hash "$FILES" --output files.bigtree
```

Then you can use the `files.bigtree` file place of `$FILES` in most of the other commands:
`diff`, `set-add`, `find`, `dupes`, ...
