#!/usr/bin/env bash

set -x

LOG="$PWD/.lint/lint.log"

rm -f "$LOG"
# rm -rf .stack-work/
stack clean

# generates .hie files, tests that build works before any changes
stack test 2>&1 | tee -a "$LOG"

pushd .lint

find ../{lib,app,test} -name '*.hs' | while read hs; do
  hlint --hint hlint.yml "$hs" --refactor --refactor-options="-i" 2>&1 | tee -a "$LOG"
done

stylish-haskell --config stylish-haskell.yaml -r -i .. 2>&1 | tee -a "$LOG"

# TODO why is this still being edited?
pushd directory-tree && git checkout .; popd

stan --hiedir ../.hie --config-file stan.toml report 2>&1 | tee -a "$LOG"

popd

weeder --config .lint/weeder.toml 2>&1 | uniq | tee -a "$LOG"

# tests that build works after any changes
stack test 2>&1 | tee -a "$LOG"

git status 2>&1 | tee -a "$LOG"
