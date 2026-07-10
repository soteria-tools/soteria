# Soteria PHP design and implementation roadmap

Status: proposal

## Overview

Soteria PHP will be a symbolic execution and bug-finding tool for PHP programs,
built on the Soteria symbolic execution library. It should be implemented as a
new language frontend and semantic model, rather than as an extension of either
Soteria C or Soteria Rust.

The intended architecture is:

```text
PHP source
    |
    v
PHP parser and lowering sidecar
    |
    v
Versioned Soteria PHP IR
    |
    v
OCaml symbolic interpreter
    |
    +--> persistent PHP state
    |
    v
Soteria symbolic execution and Z3
    |
    v
Diagnostics, counterexamples, and execution statistics
```

Most of the engineering effort will be in defining PHP semantics precisely,
especially dynamic type coercion, arrays, references, objects, lvalue behavior,
and errors. The Dune and CLI integration is comparatively small.

## Goals

Soteria PHP should eventually be able to:

- Symbolically execute selected PHP scripts and functions.
- Find failed assertions, uncaught exceptions, type errors, and selected classes
  of application bugs.
- Generate useful source-level diagnostics and concrete counterexamples.
- Distinguish verified paths, failing paths, infeasible paths, fuel exhaustion,
  and unsupported behavior.
- Model enough PHP builtins and runtime behavior to analyse realistic Composer
  projects incrementally.
- Reuse Soteria's path exploration, solver integration, logging, statistics,
  fuel management, and symbolic state abstractions.

## Initial non-goals

The first implementation should not attempt to cover all of PHP. In particular,
the following should remain explicitly unsupported until the scalar and state
models are stable:

- `eval` and runtime-generated PHP source.
- Reflection and dynamic modification of program structure.
- Fibers, parallel execution, and asynchronous extension internals.
- Native PHP extensions and arbitrary resources.
- Fully symbolic unbounded strings.
- Web-server integration and complete request lifecycle emulation.
- Full compatibility with every historical PHP version.

Unsupported constructs must produce an explicit give-up result. They must never
be treated as successful verification.

## Target language and runtime

Before implementation begins, the project must select:

- One PHP 8.x language version.
- One target integer width, initially 64-bit.
- A policy for strict and weak typing modes.
- A reference PHP binary used as the concrete semantics oracle.
- A pinned version of the source parser.

The selected versions should be recorded in `scripts/versions.json` and
propagated through `scripts/versionsync.py`. Generated version strings should not
be edited by hand.

The initial language contract should be written as a support matrix. Each
language construct should be classified as supported, partially supported,
modelled by a stub, or unsupported.

## Frontend

### Recommended parser

The initial frontend should use
[nikic/PHP-Parser](https://github.com/nikic/PHP-Parser). It supports PHP 7 and
PHP 8 syntax, retains source locations, provides name-resolution facilities, and
can serialize syntax trees as JSON.

The parser should run in a small PHP sidecar, for example:

```text
soteria-php/frontend/
  composer.json
  composer.lock
  bin/
    lower.php
```

The sidecar should:

1. Parse a source file using the selected PHP version.
2. Preserve filenames and precise source ranges.
3. Resolve namespace-qualified names where possible.
4. Validate or reject syntax outside the supported language contract.
5. Lower the parser AST into a Soteria-owned intermediate representation.
6. Emit the intermediate representation as versioned JSON.

The OCaml interpreter should not consume PHP-Parser's raw JSON representation.
A stable Soteria PHP IR keeps parser-specific details out of the interpreter and
allows the parser dependency to be upgraded independently.

### Intermediate representation

The IR should normalize syntax while retaining source locations. It should make
the following concepts explicit:

- Variable reads and lvalue evaluation.
- Ordinary assignment and assignment by reference.
- Short-circuiting boolean operations.
- Function, method, and callable invocation.
- Loop exits and their target depth.
- Returns and throws.
- Array key/value entries and unpacking.
- Namespace-resolved function and class names.
- Function and class declaration metadata.

The first version may use structured control flow. A later version can lower to
a control-flow graph if this materially simplifies `goto`, exception edges,
generators, or `finally` execution.

The JSON format should carry an explicit schema version, and the OCaml decoder
should reject incompatible versions with a frontend error.

### Alternative: Zend opcodes

Consuming Zend opcodes could provide a representation closer to PHP's runtime
behavior, but it would tightly couple Soteria PHP to PHP internals and normally
require a C extension or another native helper. This is worth investigating
after the AST-based interpreter works, but should not block the first version.

## Repository structure

The proposed initial layout is:

```text
soteria-php/
  lib/
    php_ir.ml
    frontend.ml
    value.ml
    coercion.ml
    state_intf.ml
    state.ml
    phpsymex.ml
    interp.ml
    builtins.ml
    error.ml
    config.ml
    driver.ml
    dune
  bin/
    soteria_php.ml
    dune
  frontend/
    composer.json
    composer.lock
    bin/
      lower.php
  test/
    unit/
    cram/
  docs/
    roadmap.md
```

Repository integration will require:

- A `soteria-php` package stanza in `dune-project`.
- A `soteria-php.opam.template` file.
- A public `soteria-php` library and executable.
- A `dune test soteria-php` test target.
- Formatting and documentation coverage.
- Packaging rules in the `Makefile` once standalone distribution is required.
- Build, package, smoke-test, and release workflow updates.
- README documentation and an explicit limitations section.

During early development, it is reasonable to require a compatible system PHP
binary. Bundling PHP into release archives should be considered separately
because it changes the size, licensing review, platform support, and security
update responsibilities of the distribution.

## Symbolic execution instantiation

Soteria PHP should instantiate the bitvector-based symbolic value layer and Z3,
following the overall pattern used by `soteria-c/lib/csymex.ml` and
`soteria-rust/lib/rustsymex.ml`.

The language-specific symbolic monad should carry source and call-trace
information so that errors can be reported at the PHP operation that triggered
them. It should expose helpers for:

- Running a PHP entry point with fuel and execution mode settings.
- Attaching source locations to operations.
- Reporting PHP errors and uncaught exceptions.
- Recording unsupported constructs and give-up reasons.
- Creating symbolic inputs.
- Branching over symbolic PHP types when necessary.

The first version should use whole-program execution. Compositional analysis and
bi-abductive summaries should only be added after the ordinary state semantics
are reliable.

## PHP value model

An initial value representation could be:

```ocaml
type t =
  | Undef
  | Null
  | Bool of sbool
  | Int of sint
  | Float of sfloat
  | String of php_string
  | Array of php_array
  | Object of object_id
```

Resources, enums, and specialized callables can be added when their semantics
are needed.

### Scalars

- PHP integers should initially be signed 64-bit bitvectors.
- PHP floats should use the existing IEEE-754 symbolic float support.
- Boolean values should use Soteria symbolic booleans directly.
- `null` and undefined values must remain distinct because reads, coercions, and
  diagnostics treat them differently.
- Integer overflow behavior must match the selected PHP runtime. If it cannot be
  modelled initially, the overflowing branch must give up rather than continuing
  with bitvector wraparound.

### Dynamic types

The outer PHP value constructor may initially remain concrete. A symbolic input
of type `mixed` can branch over the permitted constructors, with a symbolic
payload for each branch. This is simpler and easier to validate than introducing
a symbolic tagged-union encoding immediately.

Type predicates such as `is_int` and type-dependent operators must constrain or
branch consistently with this representation.

### Strings

The first milestone should support concrete strings. The next step should use a
bounded representation consisting of a symbolic length and a fixed maximum
number of symbolic bytes. This can support equality, concatenation, indexing,
length, and selected parsing operations for small strings.

Numeric-string conversion, loose comparison, Unicode-sensitive functions, and
regular expressions should be treated as separate semantic work. If broad
symbolic string support becomes a central requirement, extending the solver
value layer with an SMT string theory may be preferable to accumulating bounded
ad hoc models.

### Coercions and operators

PHP coercion rules should be centralized in `coercion.ml`, not distributed
throughout the interpreter. This module should define:

- Conversion to boolean, integer, float, string, and array where supported.
- Numeric-string classification.
- Arithmetic promotion and overflow behavior.
- Strict and loose equality.
- Ordering and comparison behavior.
- Array-key normalization.
- Error, warning, and exception behavior for invalid operands.

Every rule should have differential tests against the pinned PHP runtime.

## Persistent PHP state

The PHP state is the most soundness-sensitive component. A possible structure is:

```ocaml
type t = {
  scopes : scope Scope_map.t;
  cells : value Cell_map.t;
  objects : object_state Object_map.t;
  globals : cell_id String_map.t;
  next_scope : int;
  next_cell : int;
  next_object : int;
}
```

All maps and records reachable from symbolic execution state must be persistent
and immutable. A symbolic branch must never observe an in-place mutation made by
another branch. Mutable OCaml arrays, hash tables, and refs must not be threaded
through the saved PHP state.

### Variables and references

Variables should resolve to cells rather than storing values directly. This
allows ordinary reads and writes to share a common model with PHP references:

```php
$a = 1;
$b =& $a;
$b = 2;
```

After the reference assignment, both names resolve to the same cell.

The interpreter should implement a first-class lvalue or place abstraction for:

- Local and global variables.
- Array elements.
- Object properties.
- Static properties.
- Destructuring targets.

Evaluation of an lvalue must preserve PHP's evaluation order and distinguish
reading a value from obtaining a writable or referenceable location.

### Arrays

PHP arrays are ordered maps whose keys are integers or strings after coercion.
The semantic representation should include:

- A persistent ordered map of normalized keys to element bindings.
- An explicit next integer key for append operations.
- Element bindings that either contain ordinary values or refer to cells.
- Insertion order.

An ordinary array assignment should behave as a value copy. A persistent map
makes that copy cheap. References stored inside the array must retain their
aliases when the outer array is copied.

Symbolic array keys require careful lookup. Soteria's symbolic map abstractions
can branch over equality with existing keys and the possibility of a fresh key.
The implementation must also model PHP key coercion before lookup.

### Objects

Objects should be represented by stable object identifiers. The state maps each
identifier to its class and persistent property store. Assigning an object value
copies its handle, not its property store.

The initial object milestone should support declared properties, construction,
property reads and writes, instance methods, and visibility. Later work can add
inheritance, interfaces, traits, static state, magic methods, cloning,
serialization, and dynamic properties.

## Interpreter

The interpreter should follow the general monadic organization of
`soteria-linear/semantic/interp.ml`, adapted to PHP's dynamic values and richer
state.

Implementation order:

1. Literals and scalar variables.
2. Lvalue resolution and ordinary assignment.
3. Unary and binary operators.
4. Short-circuit boolean operations.
5. Conditionals and symbolic branching.
6. Loops with step and branching fuel.
7. Functions, arguments, returns, and local scopes.
8. Arrays and array indexing.
9. References and assignment by reference.
10. `foreach`, including by-reference iteration.
11. Exceptions, catches, and `finally`.
12. Objects, properties, and methods.
13. Closures, generators, includes, and autoloading.

Control flow should be represented explicitly, for example:

```ocaml
type control =
  | Normal
  | Return of Value.t
  | Break of int
  | Continue of int
  | Throw of Value.t
```

These are language-level results, not OCaml exceptions. OCaml exceptions should
remain reserved for genuine interpreter invariant violations.

Every interpreted expression or statement should consume appropriate fuel and
run with its source location attached to the symbolic execution trace.

## Builtins and symbolic test API

The first version should recognize a small set of Soteria-specific functions:

```php
<?php

$x = Soteria\symbolic_int();
Soteria\assume($x >= 0);
Soteria\assert($x + 1 > $x);
```

Likely initial functions include:

- `Soteria\symbolic_bool()`
- `Soteria\symbolic_int()`
- `Soteria\symbolic_float()`
- `Soteria\assume(bool $condition)`
- `Soteria\assert(bool $condition)`
- `Soteria\expect_fail()`

Attributes such as `#[Soteria\Test]` can later identify function or method entry
points.

PHP builtins should be registered in a table that distinguishes:

- Precisely modelled builtins.
- Conservative summaries.
- Concrete-only implementations.
- Unsupported builtins.

Models for filesystem, environment, database, network, time, and randomness
should be added only with an explicit analysis contract. They must not silently
consult or mutate ambient host state during symbolic execution.

## CLI and diagnostics

The initial CLI should support commands such as:

```bash
dune exec -- soteria-php exec example.php
dune exec -- soteria-php exec example.php --function test_sort
TERM=dumb dune exec -- soteria-php --help
```

Diagnostics should distinguish:

- Failed Soteria assertions.
- Uncaught PHP exceptions and errors.
- PHP warnings selected by configuration as bugs.
- Unsupported language constructs or builtins.
- Frontend and parse failures.
- Fuel exhaustion and incomplete exploration.

Each diagnostic should include the entry point, source range, relevant call
trace, path condition where useful, and a concrete model for symbolic inputs
when one is available.

## Testing strategy

Correctness should be established in layers.

### Unit tests

Unit tests should cover:

- Value construction and printing.
- Every scalar coercion.
- Strict and loose comparisons.
- Arithmetic promotion and overflow.
- Array-key coercion and insertion order.
- Lvalue reads and writes.
- Reference creation, rebinding, and unsetting.
- Array copy behavior with referenced and unreferenced elements.
- Object handle identity and property updates.

### Differential tests

Concrete programs and expressions should be executed by both Soteria PHP and the
pinned PHP runtime. The tests should compare:

- Resulting values and types.
- Warnings, errors, and exceptions.
- Final observable variable and array state.
- Evaluation order where it is externally visible.

Generated differential tests will be particularly valuable for operator and
coercion tables.

### Symbolic tests

Symbolic tests should cover:

- Feasible and infeasible branches.
- Counterexample generation.
- Mixed-type inputs.
- Symbolic array indexes.
- Loop and branching fuel.
- Unsupported branches mixed with successful and failing branches.

Branch-isolation regression tests are mandatory. For example, one branch should
modify an array, object, or aliased cell while another branch proves that it
still sees its own original persistent state.

### Cram and conformance tests

Cram tests should exercise the CLI, diagnostics, exit codes, stable output, and
source locations. Once the core is stable, selected upstream PHP `.phpt` tests
can be adapted into a conformance suite.

The standard validation sequence should include:

```bash
dune build
dune test soteria-php
dune build @fmt
dune build @doc
```

Full `dune test` should be run when the required language frontends are
available.

## Milestones

### M0: frontend spike

Deliverables:

- Pinned PHP and PHP-Parser versions.
- Parser sidecar and versioned IR schema.
- OCaml IR decoder.
- Source location preservation.
- `soteria-php parse file.php` command.
- Parser and lowering tests.

Exit criterion: supported example files produce deterministic, validated IR,
and unsupported or malformed input produces a source-level frontend error.

### M1: scalar symbolic executor

Deliverables:

- `Phpsymex` instantiation.
- `null`, boolean, integer, float, and concrete string values.
- Variables, assignment, operators, conditionals, loops, and functions.
- Symbolic boolean and integer inputs.
- `assume` and `assert`.
- Basic diagnostics, models, and fuel reporting.

Exit criterion: differential scalar tests agree with PHP, symbolic branch tests
produce expected models, and unsupported constructs cannot be mistaken for
verification success.

### M2: PHP core state semantics

Deliverables:

- General lvalue abstraction.
- Ordered arrays and array-key coercion.
- Assignment by reference and aliased cells.
- `foreach`, including by-reference cases.
- Undefined variable and offset behavior.
- Exceptions, catches, and `finally`.

Exit criterion: array and reference conformance tests pass, including mandatory
branch-isolation tests.

### M3: object model

Deliverables:

- Classes, constructors, properties, methods, and visibility.
- Inheritance, interfaces, and traits.
- Static properties and methods.
- Closures and callable values.
- Selected magic methods and common builtins.

Exit criterion: small object-oriented programs can be analysed without semantic
shortcuts that break object identity or property isolation.

### M4: project analysis

Deliverables:

- Multi-file programs, `include`, and `require`.
- Composer autoloading.
- Entry-point discovery and attributes.
- Superglobal input models.
- Configurable filesystem, environment, database, and network summaries.
- Function and method summaries where sound.
- Benchmarks on representative PHP packages.

Exit criterion: selected real Composer projects can be analysed with documented
coverage and give-up statistics.

### M5: distribution

Deliverables:

- Standalone packaging policy for the PHP frontend runtime.
- macOS and Linux CI packages.
- Package smoke tests.
- Release workflow integration.
- User documentation, support matrix, and limitations.

## Suggested pull request sequence

Changes should remain small and independently reviewable.

1. Package skeleton, frontend sidecar, IR schema, and parse-only CLI.
2. Scalar value representation and differential coercion test harness.
3. Symbolic monad instantiation, diagnostics, and assertion API.
4. Scalar expression and statement interpreter.
5. Functions and scopes.
6. Persistent arrays and lvalue abstraction.
7. References and aliasing tests.
8. Exceptions and structured control flow.
9. Object identity and properties.
10. Packaging and CI integration.

Each pull request should include focused tests and should not combine repository
packaging work with substantial semantic changes.

## Major risks

### Semantic breadth

PHP has many context-dependent conversions and historical edge cases. The
mitigation is to pin one version, centralize conversions, and use continuous
differential testing.

### State aliasing

References, array elements, and object handles make an apparently simple store
model unsound. The mitigation is a cell-based persistent state, a first-class
lvalue abstraction, and branch-isolation tests from the beginning.

### Symbolic strings

Real PHP applications are string-heavy, while bounded string models can become
expensive or incomplete. The mitigation is to start with concrete strings,
measure required operations on target projects, and make a deliberate decision
between bounded byte strings and solver-level string support.

### Builtin and environment coverage

Application code depends heavily on extensions and ambient state. The mitigation
is an explicit builtin registry, conservative summaries, configurable environment
models, and visible give-up statistics.

### Frontend drift

Parser upgrades can change AST structure. The mitigation is a versioned,
Soteria-owned IR and a pinned parser dependency.

## Effort estimate

These are order-of-magnitude estimates rather than delivery commitments:

- Frontend and scalar MVP: approximately four to eight engineer-weeks.
- Arrays, references, functions, and exceptions: an additional two to four
  engineer-months.
- Objects, common builtins, Composer projects, and useful framework coverage:
  approximately six to twelve or more engineer-months in total.

The effort depends most strongly on the required PHP compatibility level and
whether symbolic strings and external environment models are part of the first
useful release.

## Recommended first deliverable

**Status:** Complete — implemented and validated on 10 July 2026.

The first implementation pull request should contain only:

- The `soteria-php` package skeleton.
- Pinned frontend versions.
- The PHP-Parser sidecar.
- A small versioned PHP IR.
- An OCaml decoder.
- A `soteria-php parse` command.
- Frontend unit and cram tests.

This establishes and validates the frontend boundary before the project commits
to a PHP state representation or interpreter architecture.
