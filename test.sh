#!/usr/bin/env bash

# Usage: ./test.sh
#
# No need to be in a Nix shell first.
# Comment out steps and adjust env vars as needed during development,
# or use it as a guide and just run stack test or bats manually.

# TODO move golden tests to BATS?
# TODO why aren't the doctests being run?
# TODO when/how to run benchmarks?

# set -x
set -e

echo "### TESTING STATIC BINARY WITH BATS ###"
nix build
nix develop -c bash -c 'bats test/bats'

echo "### RUNNING HASKELL TEST SUITE ###"

# Some useful env vars:
#
# TASTY_QUICKCHECK_TESTS=10000
# TASTY_PATTERN="/ByteString/"
nix develop -c bash -c '
  TASTY_QUICKCHECK_TESTS=1000 \
  stack test bigtrees
'

# OK to inturrupt if you get bored at this point...
echo "### RUNNING HASKELL BENCHMARKS ###"
nix develop -c bash -c '
  stack bench bigtrees
'
