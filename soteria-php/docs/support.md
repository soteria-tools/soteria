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

Parsing support does not imply source execution support. The frontend currently
lowers source to IR and validates that IR in OCaml, but the scalar expression
and statement interpreter has not been implemented yet.

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

The symbolic runtime is instantiated with Soteria's bitvector value layer and
Z3. It currently exposes the following case-insensitive intrinsic functions to
the future interpreter:

| Function | Status | Notes |
| --- | --- | --- |
| `Soteria\symbolic_bool()` | Supported | Returns a fresh symbolic boolean. |
| `Soteria\symbolic_int()` | Supported | Returns a fresh symbolic signed 64-bit integer. |
| `Soteria\symbolic_float()` | Supported | Returns a fresh symbolic binary64 float. |
| `Soteria\assume(bool)` | Supported | Restricts the current path; currently requires a boolean argument. |
| `Soteria\assert(bool)` | Supported | Produces distinct success and failure paths; currently requires a boolean argument. |
| `Soteria\expect_fail()` | Unsupported | Requires entry-point result handling that is not implemented yet. |

Builtin arity and type errors and failed assertions retain their source
location and call trace. Unsupported builtin names explicitly give up the
current path. These intrinsics are unit-testable through the OCaml runtime, but
cannot be invoked from PHP source until function calls and the interpreter are
added to the IR.
