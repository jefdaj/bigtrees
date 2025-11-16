#!/usr/bin/env bash

# Usage: ./test.sh
#
# No need to be in a Nix shell first; each step below handles that.
# Comment out steps and adjust env vars as needed during development,
# or use it as a guide and just run stack test or bats manually.

# TODO move golden tests to BATS?
# TODO why aren't the doctests being run?

set -e

echo "### TESTING STATIC BINARY WITH BATS ###"

nix build

# How to debug bats examples:
#
#   BATSLIB_TEMP_PRESERVE=1 bats ...
#   sudo find /tmp | grep bats

nix develop -c bash -c 'bats test/bats'

echo "### RUNNING HASKELL TEST SUITE ###"

# Some useful TASTY_ vars you can add:
#
# TASTY_QUICKCHECK_TESTS=10000
# TASTY_PATTERN="/ByteString/"

nix develop -c bash -c '
  TASTY_QUICKCHECK_TESTS=1000 \
  stack test bigtrees
'

echo "### RUNNING HASKELL BENCHMARKS ###"

# OK to inturrupt it if you get bored at this point

nix develop -c bash -c '
  stack bench bigtrees
'
