# Tree Borrows performance (`soteria-rust/lib/tree_borrows/`)

Soteria-rust models pointer aliasing with Tree Borrows. The hot module is
`raw.ml`: a per-memory-location **borrow tree** (`root`, a weak Patricia trie
`tag -> node`) plus a per-byte **state** (`tb_state`, a weak Patricia trie
`tag -> (protected * state)`). `borrow` adds tags to `root`; `access` updates
`tb_state` for every tag in `root` on each memory access; `compact` reclaims
dead tags. This file records what's been measured and changed here. For the
general profiling tooling (sample/perf/landmarks/memtrace/Z3) see
`ocaml-profiling.md`.

## How much is Tree Borrows actually worth?

Measured inclusive share of total time (sum of sample counts on every
`Raw$access_1212` frame ÷ main-thread samples, `sample`-based):
**`Raw.access` is ~5% on `btreeset_sort`** (the most realistic bench), ~2.4% on
the bushy reborrow tree, and ~22% only on the pathological all-live chain. So
`access` is a single-digit-% cost in realistic workloads — the chain just
over-represents it. The dominant TB cost in *tree-shaped reborrow* workloads is
instead `borrow`→`compact` (~53% inclusive there, of which the forced
`Gc.full_major` is ~58% of `borrow` and the `filter_map_no_share` rebuild the
rest); in `btreeset` even that is ~0 and the time is in the symex engine / value
layer / Z3. **Bottom line: there is a low ceiling on further TB work** — keep
this in mind before investing.

## Regression benches

Two shapes, because they stress different regimes (both 1-branch, concrete,
`PC 1: empty`, in `soteria-rust/test/cram/perf.t/`):

- `reborrow_chain.rs` — deep `&mut` reborrow chain; every tag stays live on the
  recursion stack → large tree, nothing reclaimable → **sensitive `access`
  sentinel** (−27% swing baseline→optimized).
- `reborrow_tree.rs` — balanced binary reborrow tree (`rec(&mut *r); rec(&mut
  *r)` per level); completed subtrees' tags **die**, so `compact` reclaims them
  and the live tree stays small → realistic, **`compact`-bound** (the `access`
  optimizations barely move it).

`writealot.rs`/`writealotloop.rs` are the clean isolators for the per-step
memory-model + monad cost: single path, no branching, no Z3, sub-1% variance.

## Realized optimizations (2026-06, all in `raw.ml`)

Each measured by clean back-to-back A/B (n=8, same build, only `raw.ml`).
All behaviour-identical (`dune test soteria-rust` unchanged).

1. **`access`: rebuild → sharing fold.** It used to rebuild the whole `tb_state`
   trie every access with `filter_map_no_share` (O(total borrows) allocation).
   Rewritten to fold over `root` with the prior `tb_state` as the accumulator —
   `add`/`remove` only on tags whose `(protected, state)` changes, returning the
   accumulator untouched otherwise → PatriciaTree structural sharing, allocation
   O(changed-per-access). **−13%** on the chain.

2. **`access`: fold + `find_opt` → single simultaneous walk.** Replace the
   `fold` over `root` + per-tag `find_opt tag st` with
   `Tag.WeakMap.WithForeign(Tag.WeakMap.BaseMap).update_multiple_from_foreign`,
   which walks `root` and `st` **together** in one pass (O(|root|+|st|), no
   `find_opt`), the closure receiving both the node and the current `st` value.
   Returning `old` unchanged shares; `None` drops. **−19% total** vs original.
   (`sample` had localized the cost to the `find_opt` into `st`,
   `PatriciaTree.find_opt`/`findint` ~425 samples — *not* iterating `root`, which
   is a cheap sequential trie walk.)

3. **`compact` backoff `1.5×` → `2×`.** When a `Gc.full_major` finds the borrow
   set still fully live, the next compaction threshold backs off; `known_size *
   3 / 2` → `known_size * 2`. **−14%** on the chain, **−2%** on btreeset; `3×`
   gave no further gain. Re-checked on the bushy bench: `1.5×` did *not* beat
   `2×` there either (0.588 vs 0.584, n=10), so `2×` stands for both shapes.

4. **`compact_threshold` 128 → 256.** On the bushy bench an inclusive profile
   showed `caml_finish_major_cycle` (the forced `Gc.full_major` in `compact`) at
   **~58%** — that single forced full-heap GC, fired every time the trie hits the
   threshold, is almost the entire `borrow` cost. The lever is *frequency*:
   `full_major` fires once per threshold-worth of borrows, and `access` walking a
   bigger trie is cheap, so letting the trie grow before compacting amortises the
   GC. Threshold sweep is a clean U-shape — 128:0.60, **256:0.52**, 512:0.54,
   1024:0.63 — so 256 is the floor (**−12%** on reborrow_tree), neutral on
   btreeset/chain, RSS unchanged (82 MB either way; the trie is tiny next to the
   symex heap).

## What was tried and rejected

- **Remove the forced `Gc.full_major`** (rely on natural GC): regression,
  reborrow_tree **0.6s → 1.7s**. On the small micro-bench heap, natural major
  cycles are too rare, so promoted dead tags never clear and the trie grows
  unbounded. The forced GC is load-bearing *here*. (For a *large* real heap the
  better fix is to skip the forced `full_major` when a natural major cycle
  already ran since the last compaction — but that can't be demonstrated on these
  small-heap benches, so it was not done. This is the most promising untried
  scalability lever.)
- **Sharing `filter_map` rebuild in `compact`** instead of `filter_map_no_share`:
  correct (it does drop dead-ephemeron leaves) but perf-neutral — in bulk-death
  workloads most paths have a dead leaf, so little is shareable. Kept as
  `filter_map_no_share`. (Note: it *must* be a full rebuild to drop the
  dead-ephemeron orphan `Branch` nodes; sharing all-live subtrees would keep
  them.)
- **`[@inline]`/`[@cold]` + flambda on the monad layer:** no effect — see
  `ocaml-profiling.md`.

## Gotchas

- **Load-bearing share branch (don't "simplify" it away):** the `access` closure
  has a branch `else if protected = protected0 && equal_state st' st0 then old`
  that returns the *existing* value unchanged for tags whose state didn't move.
  PatriciaTree's `update`/`insert`/`add` preserve physical equality only when the
  returned value is `==` the stored one, but `access` builds a *fresh*
  `(protected, st')` tuple every call, so returning `Some (protected, st')` for an
  unchanged tag still reallocates the leaf + the O(log n) spine. Dropping that
  branch measured **+18%** — *slower than the original `filter_map_no_share`
  baseline* — because every stored tag is rebuilt each access. Keep it.

## Hard constraints (why some redesigns are off the table)

- The **st/root split must stay**, **`borrow` must not propagate into `tb_state`**,
  and the **interface must not change**. `Tree` (root) and `State` (tb_state) are
  separate state monads threaded independently (`tree_borrows_intf.ml`).
- Consequence: any "iterate `st` alone with cached metadata" scheme (e.g. a
  "can't be protected" bit to avoid `root` lookups) needs `st` to be **dense**
  (hold every tag, not just deviations-from-default), which requires `borrow` to
  populate `tb_state` — a cross-component change that's forbidden. Without it,
  `access` must walk `root` every call to discover new borrows, so such a scheme
  *adds* a pass rather than removing one. The current single-pass
  `update_multiple_from_foreign` is near-optimal under these constraints.
- The tries must stay **persistent** (forked across symbolic branches — see the
  soundness invariant in `CLAUDE.md`), so a mutable weak `Hashtbl` is not an
  option; persistent weak Patricia + GC-based reclamation is essentially forced.
