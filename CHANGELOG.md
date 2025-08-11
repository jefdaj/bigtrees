# BigTrees Changelog

## Work in Progress

### Changed

- Add a G node type for grafting a .bigtree file onto the main tree

## v0.19.6

### Fixed

### Changed

- Added hashes to dupes output comments for easier debugging.
- Added custom comments for the hashes of empty dirs, files, links, etc in dupes output.
- Added --verbose logging of dropped sets in `simplifyDupes`
- Added a header with format version to .bigset files like the .bigtree ones.
- Factored dupes render functions out of `DupeMap` into `Cmd.Dupes`

## v0.19.1

### Changed

- Rewrote `Logging` code to include a `LogCfg` and pass that around rather than `Maybe LogFn`.
- Replaced all `error` calls with `die` version that logs before crashing.

## v0.19

### Fixed

- **Breaking:** Hashes of empty directories previously matched those of empty
files, which seems risky. Fixed by prepending a letter for tree type (`D`, `L`,
...) to the content hashed in every case except `F` (file), so that files
maintain their same hashes but the rest will change.

### Changed

- Made it an error to attempt to merge two dupesets with different N files or tree types.
- Bumped .bigtree format version to reflect the breaking hash change.
- Improved logging functions.
- Added --verbose logging of all `insertDupeSet` calls.
- Added `Hash`es to `DupeSet`s
