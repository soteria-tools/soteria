# Measurement pitfalls (Soteria)

A benchmark or profile is only useful if it reflects what you think it does.
These are the Soteria-specific ways it doesn't, and how to get a number you
can trust. Add rows as you hit new ones.

## What actually ran

- **`dune exec` is in the loop.** `experiments.py` defaults to
  `dune exec -- soteria-c`, which can re-check/re-link before running. For
  anything startup- or wall-clock-sensitive, `dune build` first and pass
  `--use-installed-exe` so you measure the analyzer, not the build system.
  (For pure in-analysis hot-path work the dune overhead is a constant and
  matters less, but keep it constant across the A/B either way.)
- **The `z3` on `PATH` may be a shim.** pyenv/asdf/rbenv-style shims resolve
  + re-exec and add tens of ms *per spawn* — catastrophic precisely when
  spawn count is what you changed. Always
  `readlink -f "$(command -v z3)"` and `z3 --version`; benchmark the
  resolved binary or pass an explicit solver path.
- **dev vs. release dune profile**, sanitizers, or a different OCaml switch
  swing results more than most code changes. Confirm the profile and switch
  are identical on both sides of a comparison.
- **An attached profiler distorts timing.** Get absolute numbers with
  `--benchmark` (clean), use `perf`/`memtrace`/`landmarks` only to localize.

## How fast it really is

- **One run is noise.** Use `experiments.py --benchmark` (hyperfine,
  `--warmup 1 --runs 10`) or call hyperfine directly; report best/median and
  spread. Laptop thermal/turbo and background load move wall-clock by
  double digits.
- **Variance > effect.** SMT solving time is itself variable. If the
  run-to-run spread is wider than your expected gain, you cannot see the
  gain — increase runs, pick a larger/longer experiment, quiet the machine,
  or pin CPU frequency before trusting a delta.
- **Cold vs. warm.** First run pays cold file cache and (with `dune exec`)
  a possible rebuild. hyperfine's warmup handles this for the steady-state
  number; be explicit about which you're reporting.
- **Too-small experiment.** If an experiment finishes in well under a
  second, fixed overhead (process start, db parse) dominates and the
  subsystem you changed is invisible. Choose one whose runtime is dominated
  by the path under test.

## Correctness, not just speed

- The gate is `report.json`: the finding count (and ideally a structural
  diff) must be unchanged. Check it on **every** timed comparison, not at
  the end — a faster run with a different report is a regression. Wire it
  next to the timing so it can't be skipped.
- An optimization can be correct on the benchmark project but wrong in
  general (dropped path, weakened solver query, unsound simplification).
  Keep `dune test soteria` / `dune test soteria-c` green in addition to the
  per-experiment report gate; they exercise inputs the benchmark doesn't.
- Changing `--solver-timeout` changes results, not just speed — never use it
  to "make the benchmark faster". It's a knob for investigation only, held
  constant across any A/B.

## Trustworthy-number checklist

Before believing a delta: same exe path (resolved), same dune profile/switch,
same `z3` (resolved, not a shim), `--benchmark` with warmup and ≥10 runs,
spread reported and smaller than the effect, and `report.json` count
unchanged with the project test suites green.
