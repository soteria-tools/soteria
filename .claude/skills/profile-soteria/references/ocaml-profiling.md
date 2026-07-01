# Profiling Soteria's OCaml + Z3 architecture

Concrete incantations for localizing a bottleneck in Soteria. Soteria is
OCaml 5.x (effects-based symbolic-execution monad) driving an external Z3
process over pipes, so the three things worth profiling are **OCaml CPU**,
**OCaml allocations/GC**, and **the Z3 subprocess boundary**. Living
document — add rows/recipes that worked.

Tree-Borrows-specific findings live in `tree-borrows.md`, and everything about
the **Z3 solver boundary** — spawn/round-trip instrumentation and solver-side
findings — in `z3-solver.md` (both kept separate because those areas accreted a
lot of detail).

## 0. First look — macOS `sample` (zero setup)

```bash
# while the experiment is running, sample by name or pid
sample soteria-c 5 -mayDie -file /tmp/soteria.sample.txt
sample <pid> 3       -file /tmp/soteria.sample.txt
```

Read the aggregated tree for the **syscall vs. compute split**:

- dominated by `read`/`__read`/`waitpid`/`select` → bottleneck is the Z3
  boundary or process lifecycle, see `z3-solver.md`;
- dominated by OCaml symbols (`caml_*`, `Soteria_*`, `camlZ3*`) → CPU/alloc
  in the engine, go to §1/§2.

This needs no build flags and is the fastest way to decide *which* of the
next sections to spend time in.

To turn a `sample` tree into an **inclusive % for one symbol**, sum the sample
counts on every frame for that symbol and divide by the main-thread total:

```bash
total=$(grep -m1 "main-thread" s.txt | grep -oE "^ *[0-9]+" | tr -d ' ')
grep -F 'Raw$access_1212' s.txt \
  | awk '{for(i=1;i<=NF;i++) if($i ~ /^caml/){print $(i-1); break}}' \
  | paste -sd+ - | bc        # ÷ $total = inclusive %
```

(Use `grep -F` — symbol names contain `$`, which is a regex anchor otherwise.)

## 1. OCaml CPU

Profile the **native** executable (Soteria builds native; `dune build` then
use the binary under `_build/default/...` or the installed exe — avoid
`dune exec` so dune isn't in the profile).

| Tool | Invocation | Good for / gotcha |
|---|---|---|
| `perf` (Linux) | `perf record -g --call-graph dwarf -- soteria-c capture-db <db> ...` then `perf report` | Native OCaml profiles like C with frame pointers/DWARF. Best general CPU view. |
| flamegraph | `perf script | stackcollapse-perf.pl | flamegraph.pl > cpu.svg` (or `inferno`) | One artifact for "where is wall-clock". |
| `landmarks` | `OCAML_LANDMARKS=on dune exec --instrument-with landmarks -- soteria-rust exec ...` (or `soteria-c ...`) — see "Landmarks" below | Precise: aggregates the full call graph (inclusive time, call counts, optionally allocations). High overhead, so use it for *attribution*, not for the wall-clock number. |
| `perf stat` | `perf stat -- soteria-c ...` | Quick IPC / branch-miss / context-switch read before deep diving. |
| `sample` (macOS) | see §0 | When `perf` isn't available (macOS) — coarser but enough to localize. |

Tips: keep optimization flags constant across the A/B; a `(dev)` vs
`(release)` dune profile difference will dwarf most code changes.

### Landmarks — precise, aggregated call-graph data

`landmarks` instruments the program itself and aggregates a lot of
information (per-node inclusive time, call counts, optionally bytes
allocated). It has **large overhead**, so use it to find *where* the cost is
concentrated, not to report the benchmark's wall-clock — get the timing
number from a clean `--benchmark` run, and the attribution from landmarks.

Because the instrumentation lives in the code, running it through
`dune exec` is fine here (unlike `perf`): dune/launcher time isn't attributed
to landmarks nodes.

```bash
# whole-program auto-instrumentation, console report
OCAML_LANDMARKS=on \
  dune exec --instrument-with landmarks -- soteria-rust exec <args...>
# (works the same for soteria-c, e.g. `soteria-c capture-db <db> ...`)
```

Pass options as a comma-separated list in the env var:
`OCAML_LANDMARKS="opt1=val1,opt2=val2,optWithNoParam"`. Recognised at
runtime when loading the instrumented program:

| Option | Argument | Effect |
|---|---|---|
| `format` | `textual` (default) \| `json` | Output format: console-friendly text, or a JSON encoding of the call graph (feed JSON to a viewer / diff two runs). |
| `threshold` | float `0.0`–`100.0` (default `1.0`) | In textual output, hide call-graph nodes below this % of their parent's time. `threshold=0.0` shows everything; raise it to cut noise. Ignored by non-text formats. |
| `output` | `stderr` (default) \| `stdout` \| `temporary` \| `temporary:<dir>` \| `<file>` | Where the profile is written. `temporary` writes to a temp file and prints its name on stderr; `<file>` is an explicit path. |
| `debug` | (none) | Verbose: trace on stderr each time a landmarks primitive is called. |
| `time` | (none) | Also collect `Sys.time` timestamps during profiling. |
| `allocation` | (none) | Also collect `Gc.allocated_bytes` per node — pairs CPU attribution with allocation attribution in one run. |
| `on` | (none) | Enable profiling (default; may be omitted). |
| `off` | (none) | Disable profiling. |

Useful combinations:

```bash
# JSON call graph to a file, nothing hidden — for diffing before/after
OCAML_LANDMARKS="format=json,threshold=0.0,output=/tmp/lm.json" \
  dune exec --instrument-with landmarks -- soteria-c capture-db <db> ...

# CPU + allocation attribution together, noisy nodes trimmed
OCAML_LANDMARKS="allocation,threshold=2.0" \
  dune exec --instrument-with landmarks -- soteria-rust exec <args...>
```

**Inclusive-time trap (important for the symex monad).** Landmarks reports
*inclusive* time. The symex engine is a continuation monad, and the codebase
is full of higher-order wrappers — `let x = some_wrapper @@ fun () -> ...`
(or `let@ () = some_wrapper in ...`). The wrapper's landmark encloses the
entire continuation, so it will show ~99% inclusive time even though it does
almost no work itself — it's just the frame the rest of the computation runs
inside. Don't read a fat node as "the bottleneck": follow the call graph
*down* until inclusive time actually splits across children (or drops into a
leaf doing real work), and prefer self/exclusive time and child deltas over
the inclusive figure of any `_wrapper`/`with_`/`run`-style function. This is
also why `perf`/`sample` (which sample the actual instruction pointer) are a
better first read for the CPS layer, with landmarks used to attribute within
a region you've already narrowed.

For finer (lower-overhead) control than whole-program auto-instrumentation,
annotate suspected regions with `[%landmark "name"]` / `Landmark.enter` /
`Landmark.exit` and build with the landmarks ppx; only annotated regions are
then aggregated.

## 2. OCaml allocations / GC

Allocation churn in the symbolic-execution monad, the value/encoding layer,
or s-expression construction shows up as GC time, not as an obvious hot
line. Measure it directly.

| Tool | Invocation | Notes |
|---|---|---|
| `memtrace` | link `memtrace`, `MEMTRACE=trace.ctf soteria-c ...`, view with `memtrace_viewer trace.ctf` | Sampled, low overhead, attributes bytes to allocation call stacks — the right tool for "why is GC hot". |
| `Gc.quick_stat` / `Gc.stat` | print `minor_words` / `major_collections` around a phase | Cheap before/after delta to confirm an alloc reduction without a full trace. |
| `OCAMLRUNPARAM` | `OCAMLRUNPARAM=v=0x400` (GC timing) or tune `s=`/`o=` to test the *hypothesis* that GC is the cost | If a bigger minor heap erases the regression, the fix is allocation reduction, not algorithmic. |

A useful experiment: rerun the benchmark with a much larger minor heap
(`OCAMLRUNPARAM=s=4M`). If wall-clock drops markedly, allocation pressure is
the bottleneck and §2-style work will pay; if not, look elsewhere.

## 3. Findings log — realized wins, levers, dead ends

Soteria-specific results below, kept so nobody re-runs a settled experiment.
(Z3 **solver-boundary** profiling — spawn counting, round-trip instrumentation —
and the solver-side findings now live in `z3-solver.md`.)

### Realized win: structural type hash, not polymorphic `Hashtbl.hash` (2026-06, soteria-rust)

`sample` on an array-heavy workload (a `[u32; N]` filled + RMW'd in a loop)
showed `caml_hash` as the single hottest leaf (~25% self-time). Attribution:
the Rust svalue extension's `hash_ty` (`soteria-rust/lib/svalue/ext.ml`) hashed
each element type of a `TTuple` with the **polymorphic `Hashtbl.hash`** over the
whole `Sv.ty` ADT. A large array is stored as one N-field tuple value, so every
hash-cons of that value walked all N element types through `caml_hash`. Fix:
expose the structural `hash_ty` the hash-cons table already uses from `Svalue`
(`soteria/lib/bv_values/svalue.ml`, parameterised by the extension's hasher) and
call it for embedded element types. `caml_hash` drops out of the profile;
~1.17x on a 2048-element array bench, 1.03–1.10x on smaller aggregates, cram
outputs unchanged. Commit `36d439217`. Lesson: any `Hashtbl.hash`/polymorphic
`compare`/`(=)` on a Charon-or-svalue ADT in a hot path is suspect — it walks
the whole structure in C; replace with a derived/structural function.

### Open lever (uncommitted): `Gc` `space_overhead`

Allocation/GC churn is the dominant self-time bucket on allocation-heavy rust
benches (`do_some_marking`/`caml_perform`/`pool_sweep`/`oldify`). A pure runtime
knob helps a little: `OCAMLRUNPARAM=o=240` (space_overhead 120→240) gave a
**consistent ~2–4%** (btreeset −2.9%, writealot −2–3%, heavy ~−2%) — and
isolation showed it's **entirely `o`, not `s`** (minor-heap size `s=4M`/`16M`/`64M`
did nothing; `s=64M` was worse). It's at the noise floor and trades heap memory
for CPU, so it's a maintainer policy call (a `Gc.set { (Gc.get ()) with
space_overhead = 240 }` at startup) rather than an autonomous commit. Recorded so
the next run doesn't re-sweep it: `o` ≈ +2–4% ceiling, `s` ≈ nothing.

### Within-noise (reverted): sharing in `Range_tree.map_leaves`

`map_leaves` (`soteria/lib/data/range_tree.ml`, used by every TB access via
`Rtree_block.Tree.map_leaves_tb`) unconditionally rebuilt every node
(`{t with node}` / `{t with children=Some(l,r)}`) even when `f` changed nothing.
Adding physical-eq sharing (return `t` when `node==t.node` / `l'==l && r'==r`),
plus making `map_leaves_tb` return the original node when `tb' == tb`, is a clean
rebuild→share in the style of the TB `access` wins — but measured **within noise**
on the TB sentinels (`reborrow_chain` 0.52→0.52, `reborrow_tree` 0.34→0.33,
btreeset 2.49→2.45). Reason: in the hot path the leaves being mapped are exactly
the accessed ones, whose `tb` state *does* change, so `tb' == tb` rarely holds and
little gets shared. Reverted per the within-noise rule.

### Realized win: homogeneous `TArray` type for arrays (2026-06, soteria-rust)

A `[T; N]` array was a `Tuple` value with a `TTuple` type carrying *one svty per
element* — so the array's **type** was O(N) to hash/compare. Hash-consing a value
re-hashes its type, so each element write into a large array paid an O(N) type
hash on top of the O(N) value copy → O(N²) per pass. Fix: a dedicated
`TArray of svty * int` (element type + length) svalue ext-type, O(1) regardless
of length, **plus a matching first-class `Array of sv array` value** (NOT a
re-typed `Tuple`: a value must be well-typed against its type or the SMT-LIB
encoding in `value_codec` mismatches). This means dedicated `as_array` /
`array_field_of` / `set_array_field` / `cast_array`, and *every generic
aggregate site that can see an array must dispatch to them*: store `Index`
navigation, `value_codec` encode/decode/validity/nondet/`ref_tys_in`, string
literals (`string_to_ptr`/`parse_string`), and the SIMD lane wrapper
(`simd_of_lanes`/`simd_lanes`/`raw_eq`). The cram suite is the gate that finds
missed sites (each surfaces as a `cast`/`as_tuple` failure on an array value).
`mk_array` takes the element type explicitly (callers read it off an element
when present, else derive it structurally via `value_codec.svty_of_ty`), so even
a length-0 array — whose element type is otherwise unobservable — stays correctly
typed rather than carrying a placeholder. **heavy 2048-RMW bench
4.58→~4.18s (~9%), memwrite ~7%**, cram unchanged. Commits `75e56911f` (type) +
the first-class-value follow-up. Scaling experiment that justified it: same
store count, halving per-store N (`N=2048,p=24` → `1024,48` → `512,96`) gave
4.57→3.99→3.61s, i.e. the O(N)-per-update was ~⅓ of the array bench.

**Next step (not yet done): tree-ify the array *value*.** The win above removes
only the O(N) *type* hash; the value copy + value hash are still O(N) per write.
Representing a large array as a balanced tree of hash-consed sub-arrays would
make updates O(log N) (rebuild only the path; root hash folds child tags). The
homogeneity of `TArray` already removes the type-nesting/distinguishability
blockers that sink a chunked-*tuple* (genuine nested tuples are
indistinguishable from chunk-trees; `impl.ml` SIMD matches exact tuple shapes).
Sound by construction as long as nodes are built immutably (never mutate a
shared node — the rule that sank the old `tb_state` array change). It's the
natural substrate for issue #383's "level 2".

### Dead end: guarding the logging closure allocation (PR #405, CLOSED)

Tempting and *almost* worth it, but already tried and rejected — do not
re-explore. `[%l.debug "fmt" args]` expands (see `soteria/ppx/logs.ml`) to
`L.debug (fun m -> m "fmt" args)`, allocating a closure on every call even when
the level is off (flambda is **off**, so it isn't sunk). PR #405 changed the ppx
to emit `if L.<level>_enabled () then L.debug (fun m -> ...)`. Microbenchmark:
4 words → 0, −40% CPU *on the closure in isolation*. But the real per-file
benches **regressed +0.8% to +2.3% across the board** (write-a-lot, ctpop,
btreeset, array_init, reborrow chain/tree): a minor-heap closure that's never
promoted is nearly free, while the `should_log` branch + `Config.get`
(`Write_once`) deref is a real cost paid on every hot-path log site even when
off. Net negative; the PR was approved-but-closed.

## What we already tried: compiler-level levers (flambda, `[@inline]`/`[@cold]`)

Settled finding (2026-06) so nobody re-runs this from scratch: **a flambda
switch and adding `[@inline]`/`[@cold]` to the monad layer do *not* speed up
the `soteria-rust` `perf.t` benchmarks.** Measured engine time ("done in", n=6,
`--profile release`) for base-compiler vs flambda vs flambda+annotations was
flat to within run-to-run noise (~2%) on `writealot`, `writealotloop`,
`btreeset_sort`, `ctpop` — if anything flambda was marginally *slower*.

Why: a `sample` of `ctpop` shows the hot regions are the **memory model and
persistent data structures**, not the monad — `PatriciaTree.filter_map_no_share`
(persistent-map rebuild that doesn't structurally share), `Tree_borrows`
(`Concrete.access`/`Raw.access`), `Range_tree.map_leaves`, `Tree_block`/
`Rtree_block`, plus `caml_runstack` (effects) and `caml_modify`/`caml_call_gc`/
`caml_alloc_small` (allocation/GC). `Monad.bind`/`Compo_res.bind`/`return`/`lift`
appear at **1–10 samples (noise)**. The combinators in `monad.ml`/
`state_monad.ml` are *already* `[@inline]`, so even the stock compiler's local
inlining covers the hot bind path; there is nothing left for flambda or extra
annotations to remove. `[@cold]` natively compiles on the 5.4.1 flambda switch
(no `ppx_cold` needed; survives `-warn-error +53`).

Confirmed at the assembly level (`-inlining-report -S` injected via
`(ocamlopt_flags (:standard -inlining-report -S))` on the two libs, then read
the `_build/.../native/*.s` and `*.inlining.org`): `[@inline]` *does* fire —
`Compo_res.bind`/the StateT `bind` leave **no standalone call symbol** in the
hot modules. But what `bind` inlines *to* is irreducible: e.g.
`Rustsymex.bind` compiles to `ldr x3,[x1]; blr x3` (an **indirect call** through
the runtime-built monadic value) plus a `sub x27,x27,#64` young-heap bump (a
**closure allocation** for the captured continuation). flambda can't optimize
either because the iterator/continuation are *data*, not statically-known
functions — the report literally says "not inlined because there was no useful
information" 36× in `Rustsymex`. The hot `.s` are dense with this:
`Symex`/`Rustsymex`/`Raw` carry ~107/73/47 indirect calls and ~155/145/76 inline
alloc sites each. So the cost is closure-alloc + indirect dispatch per monadic
step (CPS `Iter.t` + state-passing) and persistent-map rebuilds — none of which
inlining or flambda can touch.

Takeaway for the next attempt: the lever for `perf.t` is **structural** — cut
allocation/structural churn in the persistent maps and the Tree Borrows
state-rebuild, not compiler flags. The realized examples of that lever (the
`Tree_borrows.Raw.access`/`compact` rewrites) are written up in
`tree-borrows.md`.

The flambda switch built for this lives at `soteria-flambda`
(`ocaml-variants.5.4.1+options` + `ocaml-option-flambda`, rust-only deps:
`opam install ./soteria.opam ./soteria-rust.opam --deps-only`). `dune build`
all-packages fails there (no cerberus); build/install just `soteria-rust`.

## Build/test gates to keep green

A profiling change is only done when these still pass (run from repo root):

```bash
dune build
dune test soteria        # core library
dune test soteria-c      # C frontend end-to-end
```

plus the `report.json` finding-count gate from `SKILL.md` step 1.
