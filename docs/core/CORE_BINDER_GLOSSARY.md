# GHC Core Binder Glossary

A quick reference for common short binder names and prefixes you will see in `-ddump-simpl` output (with most suppression flags enabled).

> NOTE: These stems are *conventions*, not a stable part of GHC's external interface. They are implementation artifacts aiding the compiler and dump readability.

## Table of Common Stems

| Stem / Pattern | Typical Expansion / Role | Where It Comes From | Allocation / Perf Implication | Ignore If… | Investigate If… | Example Snippet |
|----------------|--------------------------|---------------------|-------------------------------|------------|-----------------|-----------------|
| `ipv` | Internal pattern variable | Desugaring / Simplifier introduces neutral name | None (just a binder) | Always, unless it captures large closure | It closes over unexpected free vars | `case ipv of { I# x# -> … }` |
| `wild` | Wildcard / unused pattern placeholder | Lowering `_` or dead pattern var | None | Always | If it unexpectedly becomes used (rare) | `case wild of { }` (elided) |
| `d`, `$dEq`, `$dOrd` | Typeclass dictionary argument | Implicit parameter to polymorphic code | Pointer to dictionary record | If passed through untouched | If many appear in hot loop (specialize) | `\d -> case d of { … }` |
| `$fEqGame` | Dictionary *value* (instance) | Instance definition | Usually CAF / shared | If not very large | If it retains large data inadvertently | `$fEqGame = { == = … ; /= = … }` |
| `$ccompare`, `$c==` | Class method inside dictionary | Instance method binding | Small function | Usually | If it fails to inline in hot path | `$ccompare = \x y -> …` |
| `$p1Ord`, `$p2Eq` | Superclass projection(s) | Extracted from dictionary | Thunk or direct field | If not repeatedly re-extracted | If repeated extraction shows (force specialization) | `$p1Ord = case d of { MkOrd eq _ -> eq }` |
| `p`, `$p` (alone) | Projection (general) | Simplifier / specialization | None | Usually | If causing code duplication | |
| `ds`, `ds1`, `ds2` | "Desugared something" temporary | Desugaring complex syntax | None, may inline | Usually | If it hides a large expression reused – hoist manually | `let ds = <expr> in …` |
| `s`, `s1`, `s2` | State token for `ST` / `IO` | IO/ST sequencing | Unboxed token (no allocation) | Always | If you see it boxed (shouldn't) | `case f s of (# s', r #) -> …` |
| `$wfoo`, `$w…` | Worker function (worker/wrapper) | Worker/Wrapper transform | Often unboxed args/returns → perf win | Never remove manually | If missing and performance regressed | `$wmoveGame = \board# gems# … -> …` |
| `ww`, `ww1` | Unpacked field / intermediate from wrapper | Worker/Wrapper | Unboxed or raw field | Usually | If it remains boxed where unboxing expected | `case game of { Game ww ww1 … }` |
| `lvl`, `lvl1` | Floated (level) binding | Float-out pass | Potential CAF | If small | If it captures growing structure | `lvl = <pure expr>` |
| `eta`, `eta1` | Eta-expanded parameter | Eta-expansion step | None | Always | If it blocks further simplification | `foo = \eta -> …` |
| `go` | Local tail-recursive loop | Manual or created by transform | None (optimizable) | If simple | If recursion fails to fuse | `go acc x = …` |
| `j`, `j1` (with `join`) | Join point (control-flow only) | Simplifier join-point creation | No closure if used tail-only | Usually | If duplicated instead of joined | `join j x = … in case … of { … -> j v }` |
| `$sfoo`, `$s…` | Specialized version of polymorphic function | Specialisation pass | Eliminates dictionary indirections | Keep | If absent & dictionaries hot | `$sfoo = \x -> …` |
| `sat`, `sat_s3` | Saturated helper (pre-applied closure) | Simplifier saturation | Shared closure | If tiny | If retains large env unexpectedly | `sat = f bigCaptured` |
| `$krep`, `$tcGame` | Kind / type constructor info (if unsuppressed) | Type reflection machinery | Usually dead | If not referenced | If it drags type-level data into runtime | |
| `co`, `$co`, `$cobox` | Coercion evidence (suppressed by flags) | Type equality / newtype coercions | Erased at runtime | Always | If shows up after suppression (flag mis-set) | |
| `$dmfoo` | Default method (from class) | Class default method generation | Small wrapper | If unused | If instance didn’t override & slow | `$dmcompare = …` |
| `$trModule` | Module tracing metadata (if unsuppressed) | Debug / profiling info | CAF | Usually | If it prevents module GC | |

## Reading a Binding Quickly
1. Identify pattern: Does the name start with `$f`, `$c`, `$w`, `$s`, `$p`? That classifies it immediately.
2. Look at RHS shape: `\` = function, `case` chain = structural compare, `let` of tiny thunks = sharing.
3. Check for unboxed tuples `(# … #)`: indicates worker or primitive IO/ST sequencing.
4. Scan for dictionary re-extraction — repeated `case d of { … }` might signal specialization opportunity.

## When to Care vs Ignore
| Situation | Action |
|-----------|--------|
| Only binder names differ (no new cases/lets) | Ignore (alpha/float noise) |
| New `$s` specialization appears | Usually good (less dictionary indirection) |
| Worker `$wfoo` disappeared | Investigate strictness changes or lost unboxing |
| Large `lvl` captures data growing each iteration | Potential space leak |
| Repeated `$p1Eq` projections inside hot loop | Consider manual specialization / INLINE pragmas |

## Minimal Glossary (one-liners)
`ipv` internal pattern var; `wild` unused binding; `ds` desugared temp; `d` dictionary; `$fFoo` dictionary value; `$cbar` class method; `$p1Foo` superclass projection; `$wfoo` worker; `ww` unpacked field; `$sfoo` specialization; `lvl` floated pure binding; `eta` added parameter; `go` local loop; `j` join point; `sat` saturated helper.

## Suggested Future Enhancements
- Add annotated example of a full `Eq` instance Core.
- Add before/after snippet of worker/wrapper.
- Link to a local `grep` recipe for hot path extraction.

---
Generated as an internal aid for reading sanitized Core dumps. Feel free to expand with project-specific patterns.

---
Attribution: initial draft and explanations co-authored with GPT-5 automated assistant (session 2025-09-14).
