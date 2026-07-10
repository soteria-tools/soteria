# Soteria PHP support matrix

Soteria PHP targets PHP 8.4.19 with 64-bit integers. PHP 8.4.19 is the concrete
semantics oracle, while the parser sidecar may run on any PHP version accepted
by its Composer manifest. The frontend uses nikic/PHP-Parser 5.8.0 and emits
Soteria PHP IR schema version 2.

The scalar interpreter supports this deliberately bounded source subset:

| Construct | Status | Notes |
| --- | --- | --- |
| `null`, boolean, integer, and float literals | Supported | Integers must fit in a signed 64-bit value; floats must be finite. |
| String literals | Partially supported | UTF-8 string values only. |
| Scalar variables and ordinary assignment | Supported | Variables are stored in a persistent map. Dynamic variables and non-variable assignment targets are unsupported. |
| Boolean, integer, float, and string casts | Partially supported | Conversions requiring a symbolic string, and symbolic float-to-integer conversions, explicitly give up. |
| Unary `!`, `+`, and `-` | Partially supported | Numeric unary operators currently require an integer or float operand. Integer-negation overflow explicitly gives up. |
| Integer and float `+`, `-`, `*`, and `/` | Partially supported | Mixed integer/float arithmetic is supported. Integer overflow explicitly gives up instead of wrapping. Other PHP numeric coercions are not yet supported. |
| `.` concatenation | Partially supported | Supported when both operands can be converted to concrete strings. |
| `===` and `!==` | Supported | Supported for all current scalar value kinds, including symbolic payloads. |
| `==` and `!=` | Partially supported | Supported for numeric pairs and boolean/null pairs. Numeric-string and other mixed-type rules are not yet supported. |
| `<`, `<=`, `>`, and `>=` | Partially supported | Numeric operands only. |
| `&&`, `||`, `and`, and `or` | Supported | Right-hand evaluation is correctly short-circuited and may branch symbolically. |
| Intrinsic function calls | Supported | The callee must be a statically named Soteria intrinsic and arguments must be positional, non-reference, and non-unpacked. User functions are the next roadmap part. |
| Expression statements, `echo`, `if`, `else`, and `while` | Supported | `elseif`, loop-control statements, and all other statements remain unsupported. |
| Undefined variable reads | Unsupported | The execution path explicitly gives up. Undefined-variable warning semantics are planned with the core state milestone. |

Every interpreted expression and statement consumes step fuel. The command-line
driver also sets finite branching fuel, so unbounded loops and path explosion
cannot be reported as successful verification. `--step-fuel`,
`--branching-fuel`, and `--infinite-fuel` configure these limits; exhausting a
finite limit produces an explicit incomplete result.

The symbolic runtime recognizes these case-insensitive intrinsic functions:

| Function | Status | Notes |
| --- | --- | --- |
| `Soteria\symbolic_bool()` | Supported | Returns a fresh symbolic boolean. |
| `Soteria\symbolic_int()` | Supported | Returns a fresh symbolic signed 64-bit integer. |
| `Soteria\symbolic_float()` | Supported | Returns a fresh symbolic binary64 float. |
| `Soteria\assume(bool)` | Supported | Restricts the current path and currently requires a boolean argument. |
| `Soteria\assert(bool)` | Supported | Produces distinct success and failure paths and currently requires a boolean argument. |
| `Soteria\expect_fail()` | Unsupported | Requires entry-point result handling that is not implemented yet. |

Builtin arity and type errors, failed assertions, and division by zero retain
their source location and call trace. Unsupported builtin names and unsupported
semantic cases explicitly give up the current path. The `exec` command returns
status 1 when a definite failure is found, status 2 for frontend errors, and
status 3 when exploration is incomplete.
