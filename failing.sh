#!/usr/bin/env bash
# Quick list of reproducible failing test commands:

TASTY_QUICKCHECK_REPLAY=185889 TASTY_QUICKCHECK_TESTS=2 TASTY_PATTERN='/roundtrip TestTree to dir/' stack test
