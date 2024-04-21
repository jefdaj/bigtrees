#!/usr/bin/env bash

set -x

LOG='lint.log'

rm -f "$LOG"
rm -rf .stack-work/

stack build | tee -a "$LOG"

find * -name '*.hs' | while read hs; do
  # TODO can this take a config file?
  # TODO can it be auto-accepted? the prompts don't show through tee
  hlint "$hs" --refactor --refactor-options="-i -s" | tee -a "$LOG"
done

stylish-haskell --config .lint/stylish-haskell.yaml -r -i . | tee -a "$LOG"

# TODO add to ignores instead?
git checkout lib/System/Directory/Tree.hs

weeder --config .lint/weeder.toml | tee -a "$LOG"

stack test | tee -a "$LOG"

git status | tee -a "$LOG"
