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

The scalar semantic layer represents `null`, booleans, signed 64-bit integers,
IEEE-754 binary64 floats, and concrete strings. Boolean, integer, and float
payloads may be symbolic; the outer PHP type remains concrete. An internal
undefined value is kept distinct from `null` for later variable-read
diagnostics.

Explicit scalar casts to boolean, integer, float, and string are implemented and
differentially tested against the pinned PHP runtime with 64-bit integers and
the default `precision=14` float-to-string setting. Conversions that require a
symbolic string result, and symbolic float-to-integer conversions whose range is
not yet known, return an explicit unsupported error. Reading an undefined value
is also an explicit error until interpreter diagnostics are introduced.
