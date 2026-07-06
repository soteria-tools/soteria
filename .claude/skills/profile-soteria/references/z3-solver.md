# Profiling the Z3 solver boundary

Soteria (`soteria-rust` and `soteria-c`) drives an external **Z3 process** over
pipes through `Bv_solver.Z3_solver` (`soteria/lib/bv_values/bv_solver.ml`) — a
*stateless, caching* solver. Whether a given benchmark is Z3-bound or
OCaml-bound varies wildly per benchmark, so let a `sample`/`perf` profile decide
*before* spending time here (see `ocaml-profiling.md` §0 for the first-look
`sample` recipe and the syscall-vs-compute split).

**You are in the right file when** the profile is dominated by
`read`/`__read`/`waitpid`/`select`: the cost is *waiting on Z3*, not solving
math and not OCaml. Quantify the boundary instead of inferring it. (If instead
OCaml symbols — `caml_*`, `Soteria_*`, GC — dominate, Z3 instrumentation tells
you nothing; go back to `ocaml-profiling.md` §1/§2.)

## Count Z3 spawns per run (wrapper shim)

```sh
#!/bin/sh
# /tmp/z3trace.sh  — chmod +x, then put earlier on PATH or pass as solver path
echo "z3 spawn $(date +%s.%N)" >> /tmp/z3spawns.log
exec /absolute/path/to/real/z3 "$@"
```

```bash
: > /tmp/z3spawns.log
# point Soteria at the wrapper (PATH order, or its solver-path option),
# run one experiment, then:
wc -l /tmp/z3spawns.log     # spawns per analysis
```

This is how "3 spawns where 1 suffices" was found. A spawn count that scales
with work units (instead of being ~1) means a pooling/lifetime bug — usually
a bigger win than any solving micro-optimization.

## See the round-trips / wait time

| Tool | Invocation | Notes |
|---|---|---|
| `strace -c -f` (Linux) | `strace -c -f soteria-c capture-db <db> ...` | Per-syscall counts/time; confirms write/read round-trip volume to the Z3 pipes. |
| `dtruss` / `ktrace` (macOS) | `sudo dtruss -c soteria-c ...` | macOS equivalent; SIP may restrict — use `sample` as fallback. |
| SMT dump | Soteria's `--dump-smt-file` (if available) | Inspect the actual command stream: redundant declares/asserts, unnecessary `reset`, unbatched commands are visible structural wins. |

The reusable conclusion to aim for: **N round-trips/spawns per run at ~M ms
each**. Then the fix is one of: pool the process, pipeline fire-and-forget
commands and only block on the answers you need (`check`/`get-model`),
cache/dedupe encodings, or avoid a redundant `reset`.

But **`reset` is not free overhead** — read the next finding before "optimising"
it away.

## Finding (reverted, regression): `(reset)` per query → scoped `push`/`pop` (2026-06, soteria-rust)

**Tried and reverted — do not retry without solving the incrementality problem
below.** `check_sat_raw` issued a `(reset)` before **every** non-cached query,
then re-declared the query's vars, asserted the full constraint, and checked. On
`ctpop.rs` the SMT dump (`--dump-smt=...`) showed one `(reset)` per `(check-sat)`
(995 of each) and `sample` showed ~95% of wall-clock blocked in `read`, i.e.
Z3-bound; the cost there wasn't *solving* (one 8-bit var) but Z3 re-initialising
its solver object on each `(reset)`. Swapping `(reset)` for an `Intf.push z3 1` …
`Intf.pop z3 1` pair around the per-query declares+assert cut that re-init:
**ctpop 4.93 s → 0.52 s** locally.

**Why it was reverted:** that local benchmark was misleading. `(reset)` lets Z3
run its **non-incremental** tactic pipeline (aggressive bit-blasting /
preprocessing); `push`/`pop` forces Z3's **incremental** online SMT core, which
is dramatically slower for the hard goals these benchmarks actually hit —
bit-blasted floating-point (FPA) and bitvector-heavy queries. CI on the full PR
exposed it: **Collections-C `capture-db` 9.96 s → 44.39 s (+346%, ≈4.4×)** and
**6 kani FP-intrinsic tests (`trunc`/`floor`/`ceil`/`round`/`round_ties_even`/
FastMath `mul`) went `pass → timeout`** (3 s limit). A minimal A/B confirmed it
in isolation: a symbolic `f64` through `trunc`/`floor`/`ceil` took **4.65 s with
push/pop vs 0.28 s with reset** — a ~16× regression from this one hunk. The
trivial-query win (ctpop) does not come close to paying for the FP/bitvector
timeouts.

Transferable lesson: **`reset` vs `push`/`pop` is not a free re-init
optimisation — it changes Z3's solving mode.** A per-query `(reset)`/full
re-declare looks wasteful in an SMT dump, but for FPA/bitvector goals the
non-incremental tactic path it enables is often the reason Z3 terminates at all.
Before touching it, gate on a *solver-heavy* and an *FP-heavy* benchmark (e.g.
Collections-C + the kani rounding intrinsics), not just a re-init-bound one like
ctpop. (Separately: `(set-option :produce-models true)` is sent on every init
but `get_model` is never called outside `tests/bv_fuzz`; dropping it shaved ~3%
on ctpop but was left in to avoid a latent `None`-model footgun.)

Confirmed by Z3 maintainers — this is documented Z3 behaviour, not a
soteria-specific quirk:

- **Why incremental is a different, weaker solver.** Christoph Wintersteiger:
  *"The 'one-shot' solver translates everything into bit-vectors, bit-blasts,
  then runs a specialized SAT-only solver. The incremental version uses a
  completely different (SMT) solver which supports theory combination, supports
  incrementality, and which is lazy in terms of float-to-bv conversion and
  bit-blasting, and uses different heuristics."*
  ([Z3 #1459](https://github.com/Z3Prover/z3/issues/1459)) — same issue reports
  push/pop turning *instant* into 14 s, and 2 s into >6 min, on small QF_FP
  goals.
- **The lost preprocessing is variable elimination.** Nikolaj Bjørner:
  *"It is solved mainly due to pre-processing that eliminates variables from
  equations. This is not available in incremental mode for this version … the
  more common shortcoming of incremental mode that it doesn't include useful
  variable elimination."* His recommendation is literally what `reset` does:
  *"instead of using incremental mode, create a solver for every new query."*
  ([Z3 #7441](https://github.com/Z3Prover/z3/issues/7441))
- **The tactic dispatch.** For `QF_BV`, Z3's `qfbv` tactic picks the fast
  one-shot `sat` solver or the slow fully-featured `smt` solver based on
  features; *"If you don't specify any logic, Z3 will often run the slowest
  possible solver because it has to expect anything."*
  ([Z3 #248](https://github.com/Z3Prover/z3/issues/248))
- **The "throw it away periodically" pattern** (relevant if a future re-init
  optimisation is attempted): some users *"push/pop for some time, but after
  some number of queries they throw it all away and re-build all the
  assertions"* — i.e. amortise re-init without staying incremental forever
  ([Z3 #1459](https://github.com/Z3Prover/z3/issues/1459)).
