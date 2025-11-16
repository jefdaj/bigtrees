```.bash
# dedup.sh

#!/usr/bin/env bash

# This is the dedup-script output format.
# Be careful with this! Don't just run it without at least skimming...

# You can comment, uncomment, or delete lines in your text editor
# to change how specific files/dirs/links are handled.

# For each set of dupes, it will confirm that the first one exists and
# then delete all the others.

skip_group=FALSE
keep() { [[ -e "$1" ]] && echo "KEEP    '$1'" && skip_group=FALSE || { echo "MISSING '$1'" >&2; skip_group=TRUE; }; }
skip() { [[ $skip_group == TRUE || ! -e "$1" ]] && echo "SKIP '$1'"; }
rm_X() { skip "$3" || { rm $1 "$3" && echo "rm $2 '$3'"; } || { echo "ERROR $2 '$3'" >&2; return $?; }; }
rm_d() { rm_X '-r' 'dir ' "$1"; }
rm_f() { rm_X '' 'file' "$1"; }
rm_l() { rm_X '' 'link' "$1"; }

# 3 duplicate directories with hash yefChv7y4LiL0Lw+vjmXOm
keep 'example03/pdf_1'
rm_d 'example03/pdf_2'
rm_d 'example03/pdf_3'

# 4 duplicate files with hash xRddK/EUyJ+AdIJCRZM2ib
keep 'example03/mozart.mp3'
rm_f 'example03/mozart (copy 1).mp3'
rm_f 'example03/mozart (copy 2).mp3'
rm_f 'example03/mozart (copy 3).mp3'
```
