# Solver Optimization & State Interning Plan

_Last updated: 2025-09-14_

_Authorship: Prepared with assistance from GPT-5 (design/analysis AI companion)._ 

This document captures the design rationale and concrete steps for future optimization work on the Gem Seeker solver, focusing on replacing repeated hashing of full game states with an **ID-based state interning architecture**, plus related data layout and strictness considerations.

The current solver already functions correctly and efficiently enough for its use cases. This plan is archival: implement only if further performance improvements are needed.

---
## 1. Motivation

Profiling showed hashing costs competing with actual simulation (gravity movement). The current architecture stores full game states in a `HashMap` plus a priority queue keyed by the state, requiring repeated hashing and allocation. By assigning **compact integer IDs** on first encounter (interning), all subsequent metadata operations work with small integers and primitive arrays.

Goals:
- Reduce per-relaxation overhead (metadata updates become array writes)
- Improve cache locality and reduce GC pressure
- Separate board storage from frequently mutated numeric metadata

Non-goals (initially):
### Historical Profiling Reference

An excerpted pre-interning profiling report (second-to-last commit prior to this plan’s drafting) is archived at:

`docs/profiles/profile-pre-interning-commit-4a7844b.prof`

Notable early hotspots (time %): `mixHash`, `hashInt`, generic `ghashWithSalt`, and several `moveGame` inner lambdas—supporting the shift away from repeated generic hashing overhead.

- Rolling/incremental hashing (complex; board mutations can touch many cells)
- Fully eliminating hashing (we still hash once per *candidate* state for table lookup)

---
## 2. High-Level Architecture (Interning Variant)

Data structures inside an `ST` search:

| Structure | Purpose |
|-----------|---------|
| `HashTable PackedGame Int` | Map canonical state → assigned ID |
| `costArr :: MVector s Int` | Best known cost (g-score) per ID |
| `parentArr :: MVector s Int` | Predecessor ID (−1 sentinel for start) |
| `moveArr :: MVector s Word8` | Encoded move from parent (0..3) |
| `boardArr :: MVector s PackedBoard` | Persistent board for reconstruction |
| `pq` | Priority queue of `(cost, id)`; duplicates allowed |

`PackedGame` (intern key) fields:
```haskell
data PackedGame = PackedGame
  { pgHash   :: !Int         -- cached hash
  , pgBoard  :: !IA          -- bit-packed board
  , pgTarget :: !(Int,Int)
  , pgGems   :: !Int
  }
```
`Eq` compares all fields; `Hashable` returns `pgHash` (may mix target/gems hash when computing).

### ID Assignment
1. Build successor board (immutable as now, or via a mutable temp structure if later optimized).
2. Compute its hash once; construct `PackedGame`.
3. Lookup in hash table:
   - Hit → reuse existing ID.
   - Miss → assign next ID; append default entries to arrays.
4. Relax cost: if `newCost < costArr[id]`, update arrays + push `(newCost, id)` onto PQ.

### PQ Strategy
Use a decrease-key-free approach (allow duplicates): on pop, discard if popped cost ≠ `costArr[id]`.

Recommended PQ choices:
- Keep `Data.IntPSQ` (works with Int keys) or
- Simpler binary heap specialized to `(Int, Int)` entries.

---
## 3. Array Layout Rationale

Use **Struct-of-Arrays (SoA)** for hot numeric metadata (cost, parent, move) to maximize cache density and enable tight loops. Keep boards separately (boxed vector) because they’re accessed only during expansion or reconstruction.

Advantages over Array-of-Struct (AoS):
- Fewer pointer dereferences during hot relaxation logic
- Better prefetching and reduced false sharing
- Lower GC overhead (primitive arrays are not scanned for pointers)

---
## 4. Implementation Phases

| Phase | Description | Risk | Can Revert Easily? |
|-------|-------------|------|--------------------|
| 0 | (Optional) Add expansion counter to current solver for baseline | None | Yes |
| 1 | Introduce `PackedGame`, hashing helper | Low | Yes |
| 2 | New module `DijkID.hs` implementing ID-based Dijkstra in `ST` | Medium | Yes (isolated) |
| 3 | Integrate via flag / alternate entry point; add CLI switch | Low | Yes |
| 4 | Benchmark both versions (median of N runs) | Low | N/A |
| 5 | (Optional) Replace PQ with simpler heap if profiling shows overhead | Low | Yes |
| 6 | (Optional) Optimize board construction/hash fusion | Medium | Yes |

Stop early if Phase 2–3 shows negligible speedup.

---
## 5. Hashing Strategy

Initial hash: word-wise mixing over packed board, then mix target + gem count.

Fast option (example skeleton):
```haskell
hashBoard :: IA -> Int
hashBoard = \(IA (UArray _ _ _ ba)) ->
  -- Interpret ByteArray as sequence of machine words when possible;
  -- fold with a mix function (e.g., 64-bit rotate & xor multiply).
```
(Exact low-level implementation deferred; ensure deterministic across architectures if reproducibility matters.)

No rolling hash initially: moves may slide multiple objects; complexity vs projected gain unfavorable.

---
## 6. Strictness and Laziness Notes

Global `Strict` is kept. All metadata arrays should remain strictly updated.

Areas considered for selective laziness:
- Path reconstruction: could lazily build path lists; not a priority unless memory profile demands.
- `_dijk'target` currently uses a lazy pattern `~`; can be made strict with no downside.

No other compelling laziness introductions identified; structural improvements dominate any micro laziness wins.

---
## 7. Vector vs Primitive Arrays

Start with `vector` for clarity:
- `Data.Vector.Unboxed.Mutable` for `Int` / `Word8` arrays.
- `Data.Vector.Mutable` for boards.

Migration to `primitive` / raw arrays only if profiling isolates vector overhead (unlikely).

Growth strategy: geometric (double capacity). `unsafeGrow` is O(n), amortized acceptable.

---
## 8. Move Encoding

```haskell
data Direction = DirUp | DirDown | DirLeft | DirRight

encodeDir :: Direction -> Word8
encodeDir DirUp=0; DirDown=1; DirLeft=2; DirRight=3

decodeDir :: Word8 -> Direction
```
Stored in `moveArr` for minimal footprint.

---
## 9. Potential Later Optimizations

| Idea | Trigger Condition |
|------|-------------------|
| Inline board hash into board construction | Profiling shows hashing still hot post-interning |
| Use `Linear` ST hash table backend | Collision or cache profile issues |
| Precompute & store popcount / heuristic | If introducing A* or pruning |
| Move to custom binary heap | If PQ operations dominate |

---
## 10. Validation / Benchmark Protocol

For both legacy (`dijk`) and interning (`dijkID`):
1. Warm run (discard).
2. Run each test case N=5 times, capture `real` time, compute median.
3. Record: expanded states, final cost, solution length.
4. Ensure equality of (solution length, cost) across versions.

Simple script stub (conceptual):
```
for case in cases/*.in; do
  stack exec solver -- --mode=legacy < $case
  stack exec solver -- --mode=intern < $case
done
```

---
## 11. Risks & Assumptions

*Assumptions*: Packed boards fit comfortably in memory for expected problem sizes; branching factor not extreme enough to explode state count beyond array growth feasibility.

*Risks*: Minimal—implementation complexity; accidental retaining of old board versions if interning API not carefully strict.

Mitigations:
- Force board hash & fields when constructing `PackedGame`.
- Use bang patterns on interning functions.

---
## 12. Declined / Deferred Ideas

| Idea | Rationale for Deferral |
|------|------------------------|
| Ord-based Map + OrdPSQ only | Likely higher comparison overhead; less clear win |
| Rolling/incremental hash | Variable-sized cascades reduce benefit; higher code complexity |
| Early removal of all hashing | Need a lookup anyway; hashing once per candidate is acceptable |
| Cross-language PQ (C++/Rust) | FFI + GC interaction + lost fusion/inlining outweigh gains |

---
## 13. Example Type Skeleton (Illustrative Only)

```haskell
module DijkID where

import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Mutable as B
import qualified Data.HashTable.ST.Linear as H

data PackedGame = PackedGame { pgHash :: !Int, pgBoard :: !IA
                             , pgTarget :: !(Int,Int), pgGems :: !Int }
  deriving (Eq)
instance Hashable PackedGame where
  hashWithSalt s pg = hashWithSalt s (pgHash pg) -- already mixed

data Result = Result
  { rTarget :: !(Maybe Int)
  , rBoards :: !(Vector IA)
  , rParents :: !(U.Vector Int)
  , rMoves :: !(U.Vector Word8)
  , rCosts :: !(U.Vector Int)
  }
```
(Full implementation intentionally omitted pending need.)

---
## 14. If Implementation Starts
Checklist to tick when coding begins:
- [ ] Implement `hashIA` (word-wise) & `mkPackedGame`
- [ ] Interning function: `intern :: PackedGame -> ST s Int`
- [ ] Priority queue wrapper on Int IDs
- [ ] Core loop (pop, expand, relax)
- [ ] Reconstruction using parent/move arrays
- [ ] Compatibility adapter to produce existing `(states, directions)` output
- [ ] Bench harness / mode flag

---
## 15. Closing
This plan captures the reasoning and structured steps for transitioning to an interning-based solver architecture. Keep it as a reference bank; only invest the engineering time if profiling after earlier improvements still shows hashing + map overhead dominating.

Feel free to update this document with empirical results once experimentation begins.
