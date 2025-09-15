# Changelog for `gemssearch002`

All notable changes to this project are documented here.

Format: [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and adherence to the [PVP](https://pvp.haskell.org/).

Earlier entries before formal versioning are labeled as **"Pre-version phases"** and were not published releases.

## [Unreleased]
### Planned
- State interning architecture (see `docs/INTERNING_PLAN.md`).
- Potential transition to ST hash table + struct-of-arrays layout.

## [0.1.1.0] - 2025-09-14
### Added
- Archived profiling excerpt (`docs/profiles/profile-pre-interning-commit-4a7844b.prof`).
- Comprehensive interning/optimization plan (`docs/INTERNING_PLAN.md`).
### Changed
- Removed broad `INLINE` pragmas, yielding significant build and runtime improvements.
- Added command-line argument guard in `Main` (rejects unexpected args).
### Documentation
- Expanded internal design notes around hashing vs interning.
### Internal
- Housekeeping edits to build config / stylistic typo fixes.

## [0.1.0.0] - 2025-09-13
Initial formal version tag created immediately after integrating the second-generation bit‑packed solver. It matches (in functionality and behavior) the last commit before the wide removal of `INLINE` pragmas, and is the designated performance baseline for subsequent optimization experiments.

### Added
- Bit-packed board implementation (`TotM2`) second generation integrated.
- Dijkstra-based solver pipeline (`Dijk`, `SolveTotM2`).
- Test cases and movement spec scaffold.

### Changed
- Replaced earlier solver module (first generation) with optimized packed version.

---
## Pre-version Phases (Informal)

### Phase P4 – 2025-09-13
Final consolidation before establishing the baseline release: the original TotM module was replaced with its optimized bit‑packed successor and accompanying documentation (f1be4c6); obsolete references were cleaned out (e418e8a); internal AI/editor instructions were refreshed (803a5f4); and latest weekly puzzle data was incorporated (538355f). This phase stabilized the codebase for formal versioning.

### Phase P3 – 2025-09-11 to 2025-09-12
Rapid iteration on the second-generation bit‑packed engine: the dedicated branch was merged (62f486b); multiple indexing and layout fixes refined the packed representation (680f00e, b8840ff); a new `Main` structure and improved dimension handling plus diagnostics landed (594631d, c22d10d); a visualization aid was introduced (6bbbb84); movement edge cases (e.g., gem motion on impact) were addressed (6bce1ad, 9a37bcf); and minor micro-optimizations removed unnecessary strictness and forcing (`$!`) (1b36044, adfed3a).

### Phase P2 – 2025-09-09 to 2025-09-10
Focus on correctness and robustness: systematic resolution of physics and win‑condition test failures (4468ce0); refined bat collision handling aligned with an updated stepping API (3a2465f); repaired search logic producing losing branches (1f3b4f3); foundational Dijkstra fix (410e6a1); several critical stability patches (aeee37e); ensured zero‑step solutions include the target in reconstruction (133713e); and progressive enrichment of the test corpus with new and intentionally failing scenarios.

### Phase P1 – Legacy Prototype Era (Condensed)
Earlier web-oriented and prototype solver iterations predating the bit‑packed architecture. These commits established conceptual foundations but were superseded and are summarized here without granular detail.

---
## Reference

[Unreleased]: https://example.invalid/gemssearch002/compare/0.1.1.0...HEAD
[0.1.1.0]: https://example.invalid/gemssearch002/compare/0.1.0.0...0.1.1.0
[0.1.0.0]: https://example.invalid/gemssearch002/releases/tag/0.1.0.0

