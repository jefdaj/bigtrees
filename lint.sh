#!/usr/bin/env bash

set -x

LOG="$PWD/.lint/lint.log"

rm -f "$LOG"
rm -rf .stack-work/

# generates .hie files, tests that build works before any changes
stack test 2>&1 | tee -a "$LOG"

pushd .lint

find ../* -name '*.hs' | while read hs; do
  # TODO can it be auto-accepted? the prompts don't show through tee
  hlint --hint hlint.yml "$hs" --refactor --refactor-options="-i" 2>&1 | tee -a "$LOG"
done

stylish-haskell --config stylish-haskell.yaml -r -i .. 2>&1 | tee -a "$LOG"

# TODO add to ignores instead?
git checkout ../lib/System/Directory/Tree.hs

stan --hiedir ../.stack-work --config-file stan.toml report 2>&1 | tee -a "$LOG"

popd

echo "weeder weeds:" | tee -a "$LOG"
weeder --config .lint/weeder.toml 2>&1 | uniq | tee -a "$LOG"

# tests that build works after any changes
stack test 2>&1 | tee -a "$LOG"

git status 2>&1 | tee -a "$LOG"
