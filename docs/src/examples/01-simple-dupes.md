---
title: Simple two-line dedup
...

```.sh
bigtrees dupes PATH \
  --output dedup.sh \
  --dupes-out-fmt dedup-script
bash dedup.sh
```

Where `PATH` is a path to the folder you want to dedup.
That generates a shell script, which you can look through and optionally tweak before running.
