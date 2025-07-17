# BigTrees Changelog

## Unreleased

### Fixed

- **Breaking:** Hashes of empty directories previously matched those of empty
files, which seems risky. Fixed by prepending a letter for tree type (`D`, `L`,
`B`, ...) to the content hashed in every case except `F` (file), so that files
maintain their same hashes but the rest will change.
- Made it an error to attempt to merge two dupesets with different N files or tree types.

### Changed

- Added hashes to DupeSets and dupes output comments for easier debugging.
- Added custom comments for the hashes of empty dirs, files, links, etc in dupes output.
- Added --verbose logging of all `insertDupeSet` calls.
- Added --verbose logging of dropped sets in `simplifyDupes`
