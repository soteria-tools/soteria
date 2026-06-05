---
name: profile-soteria
description: >
  Profile and speed up Soteria (the OCaml symbolic-execution engine and the
  soteria-c / soteria-rust tools) without changing analysis results. Use this
  whenever the user wants to make Soteria faster, investigate why an analysis
  or a benchmark is slow, cut wall-clock / memory / allocations, chase a
  performance regression, or optimize a hot path in the OCaml code or the SMT
  solver interaction — even if they only say "this is slow", "why does
  capture-db take so long", or "can we speed up the Collections-C run". Covers
  the experiments.py benchmark harness, OCaml-native profiling, and the Z3
  subprocess boundary.
---

# Profiling & speeding up Soteria

Soteria is an OCaml symbolic-execution engine; `soteria-c` / `soteria-rust`
drive a frontend and an external **Z3 process** over pipes. The cost lives in
one of two layers and **which one varies wildly by benchmark** — do not
assume:

- **OCaml-bound:** many benchmarks spend ~99% of wall-clock *inside Soteria*
  — path exploration in the symex monad, the value/encoding layer,
  Tree Borrows, allocation/GC — with Z3 a rounding error. Here, time spent
  staring at the solver interaction is wasted.
- **Z3-bound:** other benchmarks are dominated by round-trips with Z3 or by
  process startup/churn, where the OCaml is idle waiting on a pipe.

Performance work here fails in two specific ways:

1. **Optimizing the wrong layer.** Assuming "it's the solver" (or "it's the
   OCaml") instead of letting a profile say which. The split is
   benchmark-specific; guessing wastes effort on cold code.
2. **Getting a faster but *different* answer.** A run that finishes quicker
   but changes the bug report is a regression, not a win.

The defense for both: **measure against the real benchmark, pair every speed
number with a correctness gate, profile to localize before changing, and
change one thing at a time.**

## The loop

### 1. Use the experiments harness as the benchmark + gate

The reproducible workload is `soteria-c/scripts/experiments.py`. It runs
`soteria-c capture-db` on a fixed project and writes `report.json` and
`stats.json`.

```bash
# baseline run on one experiment (defaults to the `run` subcommand)
python3 soteria-c/scripts/experiments.py --experiment Collections-C
```

- **Primary metric**: the `Took N seconds` it prints (end-to-end
  wall-clock).
- **Correctness gate** — the analysis result must not change. Check the
  finding count in `report.json` before and after every change:

  ```bash
  python3 -c "import json,sys; d=json.load(open(sys.argv[1])); \
    print('report entries:', len(d) if isinstance(d,list) else d)" \
    <experiments-results>/Collections-C/report.json
  ```

  Treat the count (and ideally a diff of the report) as a gate you check
  *every time* you check speed — never "I'll verify at the end". A faster
  run with a different `report.json` is a regression to be reverted.

Pick an experiment whose runtime is dominated by the path you're changing;
`--experiment X` selects one, omit it for all. `--solver-timeout` (ms) and
`compare` subcommand exist for deeper work.

### 2. Record the baseline with hyperfine

`experiments.py` has built-in hyperfine support — pass `--benchmark` and it
wraps the `soteria-c` invocation in `hyperfine ... --warmup 1 --runs 10 -i`,
giving you mean ± σ and min/max instead of one noisy sample:

```bash
python3 soteria-c/scripts/experiments.py --experiment Collections-C --benchmark
```

(Requires `hyperfine` on `PATH`.) For an ad-hoc A/B between two builds or
branches, call hyperfine directly on the resolved `soteria-c capture-db`
command and read off the ratio with its uncertainty. Always report
best/median **and** the spread; if run-to-run spread exceeds the improvement
you expect, reduce variance (more runs, quieter machine, larger experiment)
before trusting any delta. See `references/measurement-pitfalls.md`.

### 3. Profile to localize — don't guess

Profile the actual benchmark run and produce a *localized* claim ("X% of
wall-clock is in Y"). `references/ocaml-profiling.md` has the concrete
incantations; the short version:

- **First look, zero setup (macOS):** `sample` the live process —
  `sample soteria-c 5 -mayDie -file /tmp/s.txt`. The first question this
  answers is *which layer*: dominated by OCaml symbols (`caml_*`,
  `Soteria_*`, GC) → it's compute-bound, profile the engine (next two
  bullets); dominated by `read`/`waitpid`/`select` → it's waiting on Z3,
  go to the boundary. Decide this before anything else — the two paths
  share almost no work.
- **OCaml CPU:** build the native exe and `perf record -g` it (Linux); or
  `landmarks` for a precise aggregated call graph —
  `OCAML_LANDMARKS=on dune exec --instrument-with landmarks -- soteria-c ...`
  (high overhead, so use it for attribution, not for the wall-clock number;
  options, JSON/allocation modes, and the *inclusive-time wrapper trap* —
  CPS higher-order wrappers like `let@ () = some_wrapper in ...` show ~99%
  inclusive time without doing the work — in
  `references/ocaml-profiling.md`).
- **OCaml allocations:** `memtrace` + `memtrace_viewer` — allocation churn
  in the symex monad / value layer is a common, non-obvious cost.

Look past hot loops. The biggest Soteria wins are **structural**, and the
profile points at them if you read it as "why is the program in this state
so much". Which structural problems are even possible depends on the layer
the profile pointed at:

- **OCaml-bound (the common case for many benchmarks):** combinatorial path
  blow-up / insufficient merging in the symex monad; redundant
  re-analysis or re-encoding of the same terms; quadratic behaviour in a
  state/heap representation; allocation pressure in the value/encoding layer
  or Tree Borrows triggering GC. Here Z3 instrumentation tells you nothing —
  the lever is the engine.
- **Z3-bound:** synchronous round-trips where the cost is *waiting*, not
  solving (batch / pipeline fire-and-forget commands; only block on the
  answers you need); a Z3 process created per unit of work instead of
  pooled/reused; redundant `reset`/re-declaration.

When the profile implicates **OCaml**, attribute it precisely: `landmarks`
for an aggregated call graph (with `allocation` for byte attribution),
`memtrace` for who allocates, the `OCAMLRUNPARAM` minor-heap experiment to
confirm GC is the cost. When it implicates **Z3**, instrument the boundary
directly instead of inferring it: shim the solver binary with a wrapper
earlier on `PATH` that logs a line then `exec`s the real one, and count
spawns per benchmark run. Either way the goal is the same — a quantified,
localized claim ("70% of minor allocations are in `Value.encode`",
"we spawn Z3 N times when 1 would do"), not a feeling.
(`references/ocaml-profiling.md` has both the OCaml and the Z3 recipes.)

### 4. One hypothesis, one change

State it against the localized finding — OCaml-bound, e.g. "merge these
paths so we stop re-exploring the shared prefix", "memoize this encoding",
"avoid the quadratic rebuild in the heap representation"; or Z3-bound, e.g.
"pool the solver so the ~N per-run startups disappear", "pipeline
declare/assert/push so we stop paying a round-trip each". Make the smallest
change that tests exactly that.

### 5. Re-measure with the same harness and gate

Rerun step 2 (`--benchmark`) **and** the `report.json` gate. Decide on
evidence:

- **Faster and report unchanged** → keep it; record the delta and *why* it
  worked.
- **Within noise** → revert; complexity with no payoff also pollutes the
  next measurement.
- **Faster but report changed / tests fail** → revert. This is exactly what
  the gate exists to catch; do not defer it.

Also keep the project's own checks green — `dune build`, `dune test soteria`,
`dune test soteria-c` — before declaring a win.

### 6. Sanity-check what actually ran

Soteria-specific traps that invent or hide results:

- **`dune exec` overhead.** `experiments.py` runs `dune exec -- soteria-c`
  by default, which can re-link/rebuild. For startup-sensitive numbers pass
  `--use-installed-exe` (and `dune build` first) so you measure the analyzer,
  not dune.
- **The Z3 on `PATH` may be a shim.** A pyenv/asdf shim adds tens of ms per
  spawn — devastating when spawn count is the thing you changed. Resolve it
  (`readlink -f "$(command -v z3)"`, `z3 --version`) and benchmark the real
  binary (or pass an explicit solver path) so spawn-count changes aren't
  drowned out.
- **Dev vs. release build, warm vs. cold caches** — keep them constant
  across the A/B. `references/measurement-pitfalls.md` lists the rest.

## Worked example (the pattern to imitate)

This is **one** path through the loop — a Z3-bound benchmark — not the
typical profile; plenty of benchmarks are ~99% OCaml and would never reach
the solver bullets. It's here for the *method*, not as a default diagnosis.

The Collections-C benchmark looked I/O-bound; `sample` showed `read`/
`waitpid` dominating, not OCaml. A `z3` wrapper script showed **3 Z3
processes spawned per analysis** where 1 should suffice. Root cause was
structural, not a hot loop: a pooled-solver resource leaked on a non-local
exit, so the pool kept respawning. Fixing the leak (3→1 spawns) plus
pipelining the fire-and-forget SMT commands cut wall-clock with
`report.json` entries unchanged.

The transferable method (identical for an OCaml-bound benchmark, only the
tools change): the profile said *which layer*, boundary/region
instrumentation *quantified* it, the fix was *structural*, and the
report-count gate proved correctness was preserved. For a compute-bound run
the same story reads: `sample` shows `caml_*`/`Soteria_*` dominating →
`landmarks`/`memtrace` localize it to a function/allocation site → the fix
is merging/memoization/representation, not a solver tweak → same gate.

## Reporting back

Give the decision-relevant story: the experiment + metric used and the
`report.json` gate; baseline → result (best/median, spread, delta); the
localized bottleneck the profile revealed; what was changed, kept, reverted
and why.

## Extending this skill

This skill should accrete Soteria-specific performance knowledge. When you
find a profiler incantation that worked on the OCaml code, a new
measurement trap, or a benchmark that isolates a subsystem well, add it to
`references/` rather than keeping it in your head.

- `references/ocaml-profiling.md` — concrete CPU / allocation / Z3-boundary
  profiling for Soteria's OCaml + subprocess architecture, plus the settled
  flambda / `[@inline]` findings.
- `references/tree-borrows.md` — Tree Borrows (`soteria-rust/lib/tree_borrows/`)
  specific findings: what `Raw.access`/`compact`/`borrow` actually cost, the
  realized optimizations and their benches, and the hard constraints.
- `references/measurement-pitfalls.md` — what makes these benchmarks lie and
  how to get a number you can trust.
