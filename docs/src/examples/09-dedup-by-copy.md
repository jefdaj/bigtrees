---
title: Dedup by copying
...

This is useful if you want to be conservative and test the dedup process without destroying any data, or if you're working with a read-only copy---for example because you're reading a Windows or MacOS drive from Linux.

_Warning: rsync filter output is experimental and could use some more testing._

```.sh
SRC="/path/to/source/folder/here"
DST="/path/to/destination/folder/here"

bigtrees dupes "$SRC" \
  --output dedup-filters.txt \
  --dupes-out-fmt rsync-filter-script

rsync "$SRC"/ "$DST"/ \
  -arv --delete --delete-excluded --itemize-changes \
  --filter 'merge dedup-filters.txt'
```
