#!/usr/bin/env bash

# Build release binaries suitable for github.
# Note that it might take a long time to cross compile everything!

set -x

# broken? armv7l-linux \
# armv7l-hf-multiplatform \
for arch in \
	x86_64-linux \
	armv7l-linux \
	aarch64-linux \
  ; do

  rm -f result
  nix build ".#${arch}" || continue

  if [[ -z "$version" ]]; then
    version="$(./result/bin/bigtrees version)"
    release_dir="release-v${version}"
    mkdir -p "$release_dir"
  fi

  outfile="${release_dir}/bigtrees-v${version}-${arch}"
  [[ -e "$outfile" ]] && rm -f "$outfile"
  cp result/bin/bigtrees "$outfile"

done
