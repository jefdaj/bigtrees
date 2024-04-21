#!/usr/bin/env bash

set -x

find * -name '*.hs' | while read hs; do
  # TODO can this take a config file?
  hlint "$hs" --refactor --refactor-options="-i -s"
done

stylish-haskell --config .lint/stylish-haskell.yaml -r -i .

# TODO add to ignores instead?
git checkout lib/System/Directory/Tree.hs

weeder --config .lint/weeder.toml

stack test

git status
