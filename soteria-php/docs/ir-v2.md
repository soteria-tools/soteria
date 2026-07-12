# Soteria PHP IR version 2

Version 2 is historical. The current frontend emits version 11, documented in
`ir-v11.md`.

Version 2 extends the scalar-only version 1 format with executable scalar
expressions and structured statements. The program envelope and source-location
format are unchanged, but `schema_version` is `2`.

Expression kinds are:

| Kind | Additional fields |
| --- | --- |
| `null` | None |
| `bool` | `value` |
| `int` | Decimal-string `value` |
| `float` | Decimal-string `value` |
| `string` | `value` |
| `variable` | `name` |
| `assign` | `variable`, `value` |
| `unary` | `operator`, `operand` |
| `binary` | `operator`, `left`, `right` |
| `cast` | `type`, `expression` |
| `call` | Resolved `name`, `arguments` |

Unary operators are `boolean_not`, `numeric_identity`, and
`numeric_negation`. Binary operators are `add`, `subtract`, `multiply`,
`divide`, `concat`, `identical`, `not_identical`, `equal`, `not_equal`, the
four ordering comparisons, `boolean_and`, and `boolean_or`. Cast types are
`bool`, `int`, `float`, and `string`.

Statement kinds are `expression`, `echo`, `if`, `while`, and `nop`. An `if`
contains a `condition` and `then` and `else` statement arrays. A `while`
contains a `condition` and a `body` statement array.

Every nested expression and statement carries its own source location. The
OCaml decoder recursively validates that all locations use the program's
`source_file`, and rejects unknown fields, node kinds, operators, and casts.
