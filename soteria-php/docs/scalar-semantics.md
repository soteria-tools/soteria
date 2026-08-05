# Soteria PHP scalar semantics

This document defines the PHP 8.4.19 operand contract for the scalar kinds
`null`, `bool`, `int`, `float`, and concrete `string`. `S` means supported and
`G` means the path explicitly gives up under the condition in the notes. Arrays,
objects, and undefined values are outside this scalar matrix.

## Casts

| Source | `(bool)` | `(int)` | `(float)` | `(string)` |
| --- | --- | --- | --- | --- |
| `null` | S | S | S | S |
| `bool` | S | S | S | S |
| `int` | S | S | S | S |
| `float` | S | S | S | S |
| `string` | S | S | S | S |

Concrete casts follow PHP 8.4.19, including float-to-integer wrapping,
numeric-prefix casts, `NAN` and infinity, and PHP float formatting. A symbolic
boolean, integer, or float gives up only when conversion requires producing a
concrete string. Symbolic float-to-integer conversion also gives up.

## Unary operators

| Operand | `!` | unary `+` | unary `-` |
| --- | --- | --- | --- |
| `null` | S | S | S |
| `bool` | S | S | S |
| `int` | S | S | S |
| `float` | S | S | S |
| `string` | S | S | S |

Unary numeric operators use the centralized numeric-string classifier.
Well-formed numeric strings are supported. Leading-numeric strings emit PHP's
non-numeric-value warning and use their numeric prefix. Non-numeric strings
raise a catchable PHP `TypeError`.
Negating the minimum integer promotes the result to float.

## Arithmetic operators

The same matrix applies independently to `+`, `-`, `*`, and `/`.

| Left / right | `null` | `bool` | `int` | `float` | `string` |
| --- | --- | --- | --- | --- | --- |
| `null` | S | S | S | S | S |
| `bool` | S | S | S | S | S |
| `int` | S | S | S | S | S |
| `float` | S | S | S | S | S |
| `string` | S | S | S | S | S |

Numeric and leading-numeric strings are supported as described above;
non-numeric strings raise a catchable PHP `TypeError`. Integer `+`, `-`, and `*` preserve an
integer result when it fits and promote to float on overflow. Division always
returns float and division by zero raises a catchable `DivisionByZeroError`.

The concatenation matrix is:

| Left / right | `null` | `bool` | `int` | `float` | `string` |
| --- | --- | --- | --- | --- | --- |
| `null` | S | S | S | S | S |
| `bool` | S | S | S | S | S |
| `int` | S | S | S | S | S |
| `float` | S | S | S | S | S |
| `string` | S | S | S | S | S |

These cells require concrete scalar values. Concatenation gives up when a
symbolic scalar would have to be materialized as a concrete string.

## Equality and ordering

The following matrix applies to `==`, `!=`, `<`, `<=`, `>`, and `>=`.

| Left / right | `null` | `bool` | `int` | `float` | `string` |
| --- | --- | --- | --- | --- | --- |
| `null` | S | S | S | S | S |
| `bool` | S | S | S | S | S |
| `int` | S | S | S | S | S |
| `float` | S | S | S | S | S |
| `string` | S | S | S | S | S |

The comparison order is PHP's boolean/null precedence, the special null/string
rule, numeric comparison for numeric pairs and numeric strings, and bytewise
lexical comparison otherwise. Mixed integer/float comparison uses PHP's
binary64 promotion, including its precision loss. Two integer-syntax numeric
strings are compared without first truncating them to 64 bits. Any comparison
involving `NAN` is false after numeric promotion; boolean precedence still
applies before numeric comparison.

Concrete scalar cells in this matrix are covered differentially against PHP
8.4.19. Symbolic booleans, same-kind numeric values, and numeric strings are
supported. A symbolic number compared with a non-numeric string gives up because
PHP requires converting that number to a concrete string.

Strict `===` and `!==` use the following scalar matrix and never coerce:

| Left / right | `null` | `bool` | `int` | `float` | `string` |
| --- | --- | --- | --- | --- | --- |
| `null` | S | S | S | S | S |
| `bool` | S | S | S | S | S |
| `int` | S | S | S | S | S |
| `float` | S | S | S | S | S |
| `string` | S | S | S | S | S |

Strict equality also remains supported for every other current value kind.
