```.sh
# dedup.sh

#!/usr/bin/env bash

# This is the 'dedup-script' output format.
# Be careful with this! Don't just run it without at least skimming...

# You can comment, uncomment, or delete lines in your text editor
# to change how specific files/dirs/links are handled.

# For each set of dupes, it will leave the first (commented out) one alone and
# delete all the others in place by default.

skip_group=FALSE
keep() { [[ -e "$1" ]] && echo "KEEP '$1'" && skip_group=FALSE || { echo "MISSING '$1'" >&2; skip_group=TRUE; }; }
skip() { [[ $skip_group == TRUE || ! -e "$1" ]] && echo "SKIP '$1'"; }
rm_X() { skip "$3" || { rm $1 "$3" && echo "OK $2 '$3'"; } || { echo "ERROR $2 '$3'" >&2; return $?; }; }
rm_d() { rm_X '-r' 'dir ' "$1"; }
rm_f() { rm_X '' 'file' "$1"; }
rm_l() { rm_X '' 'link' "$1"; }

# 3 duplicate directories with hash yefChv7y4LiL0Lw+vjmXOm
keep 'example01/pdf_1'
rm_d 'example01/pdf_2'
rm_d 'example01/pdf_3'

# 4 duplicate files with hash xRddK/EUyJ+AdIJCRZM2ib
keep 'example01/mozart (copy 1).mp3'
rm_f 'example01/mozart (copy 2).mp3'
rm_f 'example01/mozart (copy 3).mp3'
rm_f 'example01/mozart.mp3'
```
