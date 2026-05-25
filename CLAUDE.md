This file provides guidance to AI Agents when working with code in this repository.

## Agent Behaviour

This codebase is extremely sensitive to correctness.
AI Agents are *never allowed* to push code on their own without manual human review and full validation.

All code will be reviewed by a human. To facilitate this:
- Keep the changes minimal
- Do not modify code that is unrelated to your task
- Do not add comments unless they add value to the codebase


## What is Soteria

Soteria is an OCaml library for writing symbolic execution engines / bug-finding tools, plus two tools built on top of it:
- **soteria-c**: symbolic execution for C programs (via the Cerberus/AIL frontend)
- **soteria-rust**: symbolic execution for Rust programs (via Charon/Obol MIR frontends)

## Commands

```bash
dune build              # Build everything
dune test soteria       # Run core library tests only (no frontend needed)
dune test soteria-c     # Run soteria-c tests only
dune test               # All tests (requires Obol and Charon frontends)
dune fmt                # Format code (ocamlformat)
dune build @fmt         # Check formatting without modifying
dune build @doc         # Build documentation
```

To run a specific cram test:

```bash
dune test soteria-c/test/cram/<test_name>.t
```

To run with slow/very-slow tests:

```bash
dune build @slow-tests
dune build @very-slow-tests
```

## Development Setup

See [CONTRIBUTING.md](./CONTRIBUTING.md).

## Architecture

### Core library (`soteria/lib/`)

Organized as qualified submodules (`include_subdirs qualified`). Key subsystems:

- **`symex/`**: The generic symbolic execution monad — path exploration, branching, path condition tracking
- **`sym_states/`**: Symbolic separation-logic compatible state management, memory model abstraction
- **`solvers/`**: SMT solver integration (Z3)
- **`bv_values/`**: Bitvector value abstraction for bit-accurate analysis
- **`logic/`**: Modules to facilitate the substitution and usage of symbolic booleans.
- **`data/`**: Shared symbolic data structures (e.g. `S_map`, `Range_tree`)
- **`soteria_std/`**: Utility modules extending the OCaml stdlib
- **`logs/`**: Execution trace logging and interactive HTML viewer

The PPX extension `ppx_symex` (in `soteria/ppx/`) provides syntax sugar for the symbolic execution monad.

### Language-specific tools

**soteria-c** (`soteria-c/lib/`):
- `interp.ml`: Core C interpreter over AIL (Abstract Intermediate Language from Cerberus)
- `csymex.ml`: Instantiates the symbolic execution monad for C
- `state.ml` / `bi_state.ml`: C-specific memory state
- `driver.ml`: Entry point logic

**soteria-rust** (`soteria-rust/lib/`):
- `interp.ml`: Core Rust MIR interpreter
- `rustsymex.ml`: Rust symbolic execution instantiation
- `layout.ml`: Memory layout for Rust types
- `value_codec.ml`: Encoding/decoding of Rust values
- `tree_borrows/`: Implementation of Tree Borrows (ownership/aliasing model)
- `analyses/`: Analysis-specific logic
- `builtins/`: Stubs for standard library functions

**soteria-linear** (`soteria-linear/`): Tiny example programming language, called Linear, used as an example.

### Testing

Tests use two frameworks:
- **Alcotest**: Unit tests, run via `dune test`
- **Cram**: End-to-end snapshot tests in `soteria-c/test/cram/` and `soteria-rust/test/cram/`; expected output is stored in `.t` files and diffed against actual output

### Version management

`scripts/versions.json` is the single source of truth for tool versions (OCaml, Obol, Charon, etc.). Use `./scripts/versionsync.py` to propagate changes — do not edit version strings in individual files by hand.

## Soundness Invariants

- **No mutable state threaded through symex/analysis state.** Symbolic execution forks and resumes execution states, so any in-place mutation of a value reachable from a saved state leaks across paths: one branch mutates it and a *different* branch later observes the mutation. Representations carried in symex/analysis state must be persistent/immutable (e.g. PatriciaTree maps/sets, functional records). Mutation is acceptable **only** for a *global, reusable, path-independent cache* that memoizes a pure function. When optimizing data structures here, reach for persistent structures, not `array`s/`ref`s in the state. (A TB `tb_state` bucketing change used a mutable `array` index and was reverted for this reason — it only appeared to work on a concrete benchmark that never branches symbolically.)

## Code Style

- Formatted with ocamlformat
- `snake_case` for values/functions, `Module_name` for modules and variant constructors
- Prefer `Result` types for recoverable errors; exceptions for genuine invariant violations
- Modules use `include_subdirs qualified` — subdirectory name becomes a module prefix
