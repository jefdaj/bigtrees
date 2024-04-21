#!/usr/bin/env bash

set -x

find * -name '*.hs' | while read hs; do
  hlint "$hs" --refactor --refactor-options="-i -s"
done

stylish-haskell -r -i lib/

# TODO add to ignores instead?
git checkout lib/System/Directory/Tree.hs

stack test

git status
