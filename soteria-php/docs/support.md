# Soteria PHP support matrix

Soteria PHP targets PHP 8.4.19 with 64-bit integers. PHP 8.4.19 is the concrete
semantics oracle, while the parser sidecar may run on any PHP version accepted
by its Composer manifest. The frontend uses nikic/PHP-Parser 5.8.0 and emits
Soteria PHP IR schema version 1.

The initial parse-only skeleton supports this deliberately small source subset:

| Construct | Status | Notes |
| --- | --- | --- |
| `null`, boolean, integer, and float literals | Supported | Integers must fit in a signed 64-bit value; floats must be finite. |
| String literals | Partially supported | UTF-8 string values only. |
| Expression statements | Supported | The expression itself must be supported. |
| `echo` | Supported | Every argument must be a supported expression. |
| Empty statements | Supported | Lowered to an explicit `nop` statement. |
| All other expressions and statements | Unsupported | The frontend exits with a source-level error. |

Parsing support does not imply execution support. This deliverable only lowers
source to IR and validates that IR in OCaml; it does not interpret PHP code.
