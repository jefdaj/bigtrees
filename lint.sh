#!/usr/bin/env bash

set -x

find * -name '*.hs' | while read hs; do
  [[ "$hs" =~ Tree.hs ]] && continue
  hlint "$hs" --refactor --refactor-options="-i -s"
done

stack test

git status
