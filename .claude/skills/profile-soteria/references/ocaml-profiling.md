# Profiling Soteria's OCaml + Z3 architecture

Concrete incantations for localizing a bottleneck in Soteria. Soteria is
OCaml 5.x (effects-based symbolic-execution monad) driving an external Z3
process over pipes, so the three things worth profiling are **OCaml CPU**,
**OCaml allocations/GC**, and **the Z3 subprocess boundary**. Living
document — add rows/recipes that worked.

## 0. First look — macOS `sample` (zero setup)

```bash
# while the experiment is running, sample by name or pid
sample soteria-c 5 -mayDie -file /tmp/soteria.sample.txt
sample <pid> 3       -file /tmp/soteria.sample.txt
```

Read the aggregated tree for the **syscall vs. compute split**:

- dominated by `read`/`__read`/`waitpid`/`select` → bottleneck is the Z3
  boundary or process lifecycle, go to §3;
- dominated by OCaml symbols (`caml_*`, `Soteria_*`, `camlZ3*`) → CPU/alloc
  in the engine, go to §1/§2.

This needs no build flags and is the fastest way to decide *which* of the
next sections to spend time in.

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

## 3. The Z3 subprocess boundary

If §0 showed `read`/`waitpid` dominance, the cost is *waiting on Z3*, not
solving math, and not OCaml. Quantify the boundary instead of inferring it.

### Count Z3 spawns per run (wrapper shim)

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

### See the round-trips / wait time

| Tool | Invocation | Notes |
|---|---|---|
| `strace -c -f` (Linux) | `strace -c -f soteria-c capture-db <db> ...` | Per-syscall counts/time; confirms write/read round-trip volume to the Z3 pipes. |
| `dtruss` / `ktrace` (macOS) | `sudo dtruss -c soteria-c ...` | macOS equivalent; SIP may restrict — use `sample` as fallback. |
| SMT dump | Soteria's `--dump-smt-file` (if available) | Inspect the actual command stream: redundant declares/asserts, unnecessary `reset`, unbatched commands are visible structural wins. |

The reusable conclusion to aim for: **N round-trips/spawns per run at ~M ms
each**. Then the fix is one of: pool the process, pipeline fire-and-forget
commands and only block on the answers you need (`check`/`get-model`),
cache/dedupe encodings, or avoid a redundant `reset`.

## Build/test gates to keep green

A profiling change is only done when these still pass (run from repo root):

```bash
dune build
dune test soteria        # core library
dune test soteria-c      # C frontend end-to-end
```

plus the `report.json` finding-count gate from `SKILL.md` step 1.
