#!/usr/bin/env bash

set -x

LOG='lint.log'

rm -f "$LOG"
rm -rf .stack-work/

stack build 2>&1 | tee -a "$LOG"

find * -name '*.hs' | while read hs; do
  # TODO can it be auto-accepted? the prompts don't show through tee
  hlint --hint .lint/hlint.yml "$hs" --refactor --refactor-options="-i -s" 2>&1 | tee -a "$LOG"
done

stylish-haskell --config .lint/stylish-haskell.yaml -r -i . 2>&1 | tee -a "$LOG"

# TODO add to ignores instead?
git checkout lib/System/Directory/Tree.hs

weeder --config .lint/weeder.toml 2>&1 | tee -a "$LOG"

stack test 2>&1 | tee -a "$LOG"

git status 2>&1 | tee -a "$LOG"
