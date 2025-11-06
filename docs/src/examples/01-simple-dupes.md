---
title: Simple two-step dedup
...

BigTrees generates a shell script, which you can look through and optionally tweak before running.

```.sh
FILES="/path/to/files/to/dedup/here"

bigtrees dupes "$FILES" \
  --output dedup.sh \
  --dupes-out-fmt dedup-script

bash dedup.sh
```
