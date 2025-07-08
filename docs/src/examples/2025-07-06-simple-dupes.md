---
title: Simple one-line dedup
---

```.sh
bigtrees dupes PATH --output dupes.txt
```

Where `PATH` is a path to the folder you want to scan.
On some systems, you can drop the folder in the terminal window to paste the path instead of typing it.

I have a big `tmprepos` folder for trying new code.
Here's what it looks like when I scan it for dupes:

```.sh
bigtrees dupes ~/tmprepos --output dupes.txt
```

```.sh
# these 2 dirs with 2050 files total can be removed:
tmprepos/hello-world/node_modules/@cardano-sdk/dapp-connector/node_modules/@cardano-sdk/core
tmprepos/hello-world/node_modules/@cardano-sdk/key-management/node_modules/@cardano-sdk/key-management/node_modules/@cardano-sdk/core

# these 452 files can be removed:
tmprepos/aiken/crates/uplc/test_data/conformance/v2/builtin/interleaving/iteForceAppForce/iteForceAppForce.uplc.budget.expected
tmprepos/aiken/crates/uplc/test_data/conformance/v2/builtin/interleaving/iteForceAppForce/iteForceAppForce.uplc.expected
tmprepos/aiken/crates/uplc/test_data/conformance/v2/builtin/interleaving/iteForcedForced/iteForcedForced.uplc.budget.expected
tmprepos/aiken/crates/uplc/test_data/conformance/v2/builtin/interleaving/iteForcedForced/iteForcedForced.uplc.expected
tmprepos/aiken/crates/uplc/test_data/conformance/v2/builtin/interleaving/iteUnforcedFullyApplied/iteUnforcedFullyApplied.uplc.budget.expected

# ...about 44,000 lines later...

# these 2 files can be removed:
tmprepos/hello-world/node_modules/blake2b/test.js
tmprepos/try-aiken-mpt/off-chain/node_modules/blake2b/test.js

# these 2 links can be removed:
tmprepos/nixos-root/home/jefdaj/.nix-defexpr/channels_root
tmprepos/nixos-root/root/.nix-defexpr/channels
```

As you can see, it's a list of sets of duplicate files, starting with the biggest (most dupes) at the top. Just manually delete the ones you don't want, and stop when they no longer seem big enough to worry about.
