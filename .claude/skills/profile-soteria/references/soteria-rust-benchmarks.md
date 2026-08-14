# soteria-rust benchmarks & cross-commit A/B

`experiments.py` (see `SKILL.md`) is the soteria-**c** harness. The
soteria-**rust** benchmarks are a separate thing: `scripts/benchmarks.json`
lists them, `scripts/run_benchmarks.py` runs them, and CI publishes the
results. This file covers how to reproduce one locally and how to A/B it
across commits — which is harder than it looks, because the exe does not run
outside `dune exec` without help and because the frontend is pinned per
commit.

## What the benchmark actually measures

For every `rust_files` entry, `run_benchmarks.py` does:

```
soteria-rust compile <file>                      # not timed
soteria-rust exec <file> --no-compile            # timed, hyperfine
```

with `HYPERFINE_WARMUP = 1`, `HYPERFINE_RUNS = 10`, `-i`. So **frontend
(Charon/Obol) time is excluded** and only the OCaml engine + Z3 are timed.
Reproduce that exact split locally; timing `exec` without `--no-compile`
folds in a ~0.2 s cache-hit (or ~2.4 s cold) compile and hides the signal.

The benches live in `soteria-rust/test/cram/perf.t/`. `btreeset_sort.rs` is
the most realistic one (~2.9 s, 75 branches, 2203 allocations); the others
are 0.3–1.9 s and mostly 1–3 branches.

## Running the built exe outside `dune exec`

`CLAUDE.md` says to invoke the tools via `dune exec` so plugin/runtime sites
resolve. That's correct but useless for benchmarking — `dune exec` re-checks
the build on every invocation. Running `_build/default/soteria-rust/bin/soteria_rust.exe`
directly fails with:

```
Compiling... errored
error: Compilation error
Couldn't find plugin directory
```

because the dune *site* for plugins isn't resolvable from a bare exe. Fix it
with the documented escape hatch (`--plugins` / `SOTERIA_RUST_PLUGINS`):

```bash
R=/path/to/soteria
env TERM=dumb SOTERIA_OFFLINE=1 \
    SOTERIA_RUST_PLUGINS=$R/_build/default/soteria-rust/plugins \
    $R/_build/default/soteria-rust/bin/soteria_rust.exe exec --no-compile bench.rs
```

Notes:

- Copying the exe somewhere else does **not** help; the site lookup is the
  problem, not the path.
- In a *fresh worktree* `_build/default/soteria-rust/plugins` is not
  populated by building the exe alone. Run `dune build @install` and use
  `_build/install/default/share/soteria-rust/plugins` instead.
- Each plugin site needs a one-time `SOTERIA_OFFLINE=0 … build-plugins`
  (~7 s) before the first `compile`.
- **Never benchmark inside `soteria-rust/test/cram/perf.t/`.** The cram
  setup runs with `SOTERIA_RUST_CLEANUP=1`, and `cleanup.sh` does
  `rm -rf *.ullbc *.crate target/ Cargo.lock`. Copy the `.rs` to a scratch
  dir and work there.
- The cram env is `HIDE_UNSTABLE=1 SOTERIA_RUST_CLEANUP=1 SHOW_PCS=1
  SOTERIA_OFFLINE=1 TERM=dumb`. `HIDE_UNSTABLE=1` replaces timings with
  `<time>`, so drop it when you want numbers.

## The workload gate: `--stats stdout`

Pair every timing with `--stats stdout`. It prints exactly the invariants you
need to prove an optimization (or a regression) didn't change what ran:

```
• Z3 check-sat calls: 118
• branch_on: branches 0.01% of calls (74 of 1134097)
• Steps: 58107
• Function calls: 17187
• Load accesses: 179452 (92.4% through store)
• Allocations: 2203
• SAT checks: 245 (0 unknowns)
• SAT solving time: 0.64s (21.89%)
• Branches: 74 (0 unexplored)
```

This is the soteria-rust analogue of soteria-c's `report.json` count, and it
is strictly more informative: identical `Steps` / `Function calls` /
`Load accesses` / `Allocations` / `SAT checks` / `Branches` across two builds
proves a timing delta is *pure per-operation cost*, not extra paths or extra
solver work. `SAT solving time` immediately tells you the OCaml/Z3 split
(≈78% / 22% on `btreeset_sort`).

`soteria-rust/test/cram/perf.t/run.t` is a second, coarser gate: it records
branch counts and full path conditions, so `git diff main -- perf.t/run.t`
being empty means the benchmark's symbolic behaviour is unchanged.

## Cross-commit A/B: the frontend is pinned per commit

`scripts/versions.json` pins `CHARON_COMMIT_HASH` and `OBOL_COMMIT_HASH` per
commit, and **both matter**:

- **Obol is the default frontend** (`--frontend` defaults to `obol`), so it
  produces the `.ullbc`. Different Obol versions emit different output —
  recompile the `.ullbc` when you change it. Point at a specific build with
  `SOTERIA_OBOL_PATH`. Obol builds standalone (`make build` in a worktree,
  ~30 s warm) and needs no opam changes.
- **charon-ml is an opam dependency**, so a commit only builds against *its*
  pinned Charon. Building across a Charon bump fails with e.g.
  `Error: Unbound constructor TAdtId`.

There is no way around the charon-ml pin: a `.ullbc` from an older Obol is
rejected by a newer engine (`Failed to parse ULLBC … unknown enum variant
tag: 4`), so you cannot hold the frontend fixed and swap only the engine.

Repinning charon-ml to an older commit, using a local worktree so no network
fetch is needed:

```bash
git -C ../charon worktree add /tmp/charon-old <OLD_CHARON_HASH>
opam pin add -y --no-action charon.~dev              /tmp/charon-old
opam pin add -y --no-action name_matcher_parser.~dev /tmp/charon-old
opam install -y charon name_matcher_parser
```

**Write the restore script before you pin** — this mutates the shared switch
and the repo will not build against the new Charon until you undo it:

```bash
opam pin add -y --no-action charon.~dev \
  "git+https://github.com/soteria-tools/charon#<NEW_CHARON_HASH>"
opam pin add -y --no-action name_matcher_parser.~dev \
  "git+https://github.com/soteria-tools/charon#<NEW_CHARON_HASH>"
opam install -y charon name_matcher_parser
```

Both installs are fast (charon-ml only, seconds), so the cost is the soteria
rebuild in the worktree, not opam.

Also check the *installed* frontend binaries against the pins before trusting
a number — `../obol/bin/obol` and `../charon` are often a commit or two ahead
of `versions.json`.

## Reading the CI benchmark comment

CI posts a "Benchmark comparison vs `main`" table on PRs. **It is not a
same-run A/B.** `benchmarks.yml` measures the PR now, then pulls the main
baseline from published history:

```
git show origin/gh-pages:dev/bench/data.js
```

so the two sides ran at different times on `[self-hosted, linux]` runners.
Before chasing a red Δ, pull that history and look at main's own scatter for
the same benchmark:

```bash
git fetch --depth=50 origin gh-pages
git show origin/gh-pages:dev/bench/data.js | sed 's/^window.BENCHMARK_DATA = //' \
  | python3 -c "import json,sys
d=json.load(sys.stdin)
for e in d['entries']['Soteria benchmarks'][-12:]:
    for b in e['benches']:
        if 'btreeset' in b['name']:
            print(e['commit']['timestamp'][:16], e['commit']['id'][:8], b['value'], b.get('range'))"
```

Two things to check there: whether the baseline point is representative of
main's recent band, and whether the run carries a blown-up `range` (one
historical point reads `3.162 ± 0.667` — a visibly disturbed run). A single
red benchmark while every other benchmark in the same run is flat argues
*against* a globally slow runner, but it still isn't a controlled A/B —
reproduce locally before believing it.

## Findings log

### `btreeset_sort` +15% on `miri-fixes` (Aug 2026) — Linux-only, unexplained

CI reported `btreeset sort (size 4)` at 2.895 s (main) → 3.328 s (PR, +15.0%)
while all nine other benchmarks were flat, and **reproduced it on a re-run**
(3.363 s, +16.2%) — so it is real on `[self-hosted, linux]` x86_64, not a
runner artifact. It does **not** reproduce on macOS/arm64 under any
configuration tried. A controlled local A/B (same machine, back-to-back,
hyperfine 12 runs each, each side with its own pinned Obol and plugin site)
measured:

| build | mean | σ |
|---|---:|---:|
| main `8d04c1ff3` | 2.927 s | ±0.028 |
| HEAD `14ec2d4b0` | 2.958 s | ±0.035 |

**1.01 ± 0.02×**, and `--stats` was byte-identical on both (Steps 58107,
Function calls 17187, Load accesses 179452, Allocations 2203, SAT checks 245,
Branches 74). Ruled out along the way, each with a measurement:

- **Core library** — `soteria/lib/` was untouched by the branch except a
  2-line zero/one fix on the `>256`-bit path (dead for this bench).
- **The Charon/Obol bumps** — Obol rebuilt at each pinned commit and the
  `.ullbc` regenerated; no change in stats or time. The bench's `.ullbc` size
  moved 0.36% across the bump.
- **The re-enabled strong-protector-on-free scan** (`Raw.strong_protector_exists`,
  restored on that branch, commented out on main) — the only addition
  proportional to alloc/free count, and `btreeset_sort` is the only
  alloc-heavy bench, so it was the leading structural hypothesis. Stubbing it
  to `false`: 2.925 s vs 2.926 s. **Zero cost.** A per-`free` walk of the weak
  tag map does not show up at this scale.
- **Build profile** — CI's `make ocaml` is a plain `dune build`, i.e. the same
  dev profile as a local build. Not a release/dev mismatch.
- **Z3 version** — the local Z3 (4.12.5) is much slower than a recent one on
  this workload (`sat-time` 0.66 s vs 0.22 s on 4.16.0, `exec` 2.97 s vs
  2.51 s), so it is worth pinning deliberately. But the main-vs-HEAD ratio is
  1.00× under 4.12.5 and 1.02× under 4.16.0 — the Z3 version changes the
  baseline, not the gap.
- **A worse AST from the Charon/Obol bump** — `--output-crate` dumps the
  pretty-printed crate; main 30 622 lines / 1 018 592 B vs HEAD 30 818 /
  1 025 086 B (+0.6%). The whole diff is cosmetic: tuple types now print with
  explicit generic args (`(_, _)::<usize, Option::<usize>>` for
  `(usize, Option<usize>)`) and a few extra parens, from Charon's "Stop
  removing tuple trait clauses". Same `.ullbc` size to within 0.4%.
- **Allocation / GC volume** (`OCAMLRUNPARAM=v=0x400`) — HEAD allocates
  *fewer* words (1.734 G vs 1.763 G), does fewer minor collections (6439 vs
  6549), and has the same top heap (5.84 M vs 5.85 M words).

So by every measure reachable on macOS/arm64 — wall clock under two Z3s,
every execution counter, AST size, allocation volume, GC collections, heap
size — HEAD is equal to or marginally *cheaper* than main, yet Linux CI
reproducibly shows +16%. **Next step must be a measurement on Linux x86_64:
run both sides there with `--stats stdout` and compare `soteria.sat-time`
against total — that single split says whether the extra ~0.47 s is Z3 or
OCaml and halves the search space.**

**Lesson: on a cross-run CI comparison, reproduce with a same-machine
back-to-back A/B plus a `--stats` diff before spending time on hypotheses —
identical stats mean you are looking for per-operation cost or an environment
difference, not a code path. And when the counters, the AST and the GC
numbers all say "no change", stop hypothesising on the wrong machine and go
measure on the one that shows the regression.**
