---
title: Save hashes to a .bigtree file
...

This is useful any time you want to scan something now and use it later.
That might be so you can:

* diff the current state with a future one
* search the tree without plugging in the corresponding backup drive
* pre-run the long hashing step to speed up other operations

```.sh
FILES="/path/to/files/here"
bigtrees hash "$FILES" --output files.bigtree
```

You can use `files.bigtree` in place of `$FILES` in most of the other commands:
`diff`, `set-add`, `find`, `dupes`, ...
