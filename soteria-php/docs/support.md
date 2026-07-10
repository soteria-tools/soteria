# Soteria PHP support matrix

Soteria PHP targets PHP 8.4.19 with 64-bit integers. PHP 8.4.19 is the concrete
semantics oracle, while the parser sidecar may run on any PHP version accepted
by its Composer manifest. The frontend uses nikic/PHP-Parser 5.8.0 and emits
Soteria PHP IR schema version 7.

The interpreter supports this deliberately bounded source subset:

The detailed cast and operand-kind tables are in
[`scalar-semantics.md`](scalar-semantics.md).

| Construct | Status | Notes |
| --- | --- | --- |
| `null`, boolean, integer, and float literals | Supported | Integers must fit in a signed 64-bit value; floats must be finite. |
| String literals | Partially supported | UTF-8 string values only. |
| Variables, assignment, and references | Supported | Scopes map variables to immutable cells. Ordinary and reference assignment targets may be variables, nested array elements, or declared object properties. Reference assignment aliases these bindings through persistent cells. Dynamic variables and other lvalues remain unsupported. |
| Array literals | Partially supported | Keyed and unkeyed items are evaluated in PHP order. Unpacking and items by reference remain unsupported. |
| Array keys | Partially supported | Concrete null, boolean, integer, float, and string keys use PHP key normalization. Float-to-integer deprecation notices are not yet reported. Arrays are rejected as keys. |
| Array reads, writes, and append | Partially supported | Nested array lvalues, null/undefined autovivification, insertion order, negative next-integer keys, and append exhaustion are modelled. Missing-offset warning/value behavior remains unsupported. |
| Symbolic array keys | Partially supported | Symbolic integer and boolean keys branch against existing integer keys. A feasible fresh-key read or write explicitly gives up because persistent symbolic-key insertion is not implemented. |
| Array assignment and copying | Supported | Arrays are immutable values, so ordinary assignment copies the value while updates rebuild only the affected path. Array elements promoted to references retain their aliases in later copies; earlier copies remain independent. Mandatory branch-isolation tests cover ordinary and referenced cells. |
| Boolean, integer, float, and string casts | Partially supported | All concrete scalar casts are supported. Array-to-boolean, integer, and float conversion is supported. Array-to-string conversion, conversions requiring a symbolic string, and symbolic float-to-integer conversions explicitly give up. |
| Unary `!`, `+`, and `-` | Partially supported | Numeric unary operators support null, booleans, integers, floats, and well-formed concrete numeric strings. Integer-negation overflow promotes to float. Leading-numeric strings explicitly give up until warning events are modelled; invalid operand types give up until their `TypeError` is catchable. |
| `+`, `-`, `*`, and `/` | Partially supported | Scalar weak numeric coercion, integer/float promotion, integer-overflow promotion, division's always-float result, and array union with `+` are supported. Leading-numeric strings and invalid operand types explicitly give up pending warning and catchable-error support. |
| `.` concatenation | Partially supported | Supported when both operands can be converted to concrete strings. |
| `===` and `!==` | Supported | Supported for all current value kinds, including ordered recursive arrays, symbolic scalar payloads, and stable object identity. |
| `==` and `!=` | Partially supported | Supported for every scalar pair, including boolean/null precedence, integer/float promotion, numeric strings, ordinary strings, and `NAN`. Array and object loose equality remain unsupported. |
| `<`, `<=`, `>`, and `>=` | Partially supported | Supported for every scalar pair with PHP 8.4.19 coercion rules. Array and object ordering remain unsupported. |
| `&&`, `||`, `and`, and `or` | Supported | Right-hand evaluation is correctly short-circuited and may branch symbolically. |
| Function calls | Partially supported | The callee must be a statically named user function or Soteria intrinsic. Arguments must be positional, non-reference, and non-unpacked. |
| Named functions and returns | Partially supported | Top-level declarations, required untyped by-value parameters, case-insensitive calls, early returns, fallthrough-to-`null`, and recursion are supported. Extra arguments are evaluated and ignored as in PHP. Parameter defaults and types, return types, references, variadics, named arguments, attributes, nested declarations, and closures remain unsupported. |
| Function-local variables | Supported | Calls use a fresh persistent local scope initialized with the parameters. Assignments do not modify or leak into the caller's scope. Output remains visible across calls. PHP `global` and static local variables remain unsupported. |
| Expression statements, `echo`, `if`, `else`, `while`, `return`, `break`, `continue`, and `unset` | Supported | `return` is supported in function bodies. `break` and `continue` use positive static depths and may target enclosing `while` loops. `unset` removes variable, nested array-element, or declared object-property bindings without destroying aliased cells. `elseif` and other loop forms remain unsupported. |
| `throw`, `try`, multi-catch, and `finally` | Partially supported | Explicit throws propagate through expressions and function calls. Catch order and the supported subset of the built-in PHP throwable hierarchy are modelled, catch variables retain stable object identity, and finally runs for every structured completion. Non-object and non-`Throwable` object throws become catchable `Error` objects. Existing interpreter errors such as division by zero are not yet catchable. |
| Class declarations | Partially supported | Ordinary named classes containing only public, untyped, non-static properties are supported. Property defaults may use supported scalar literals, nested array literals, and numeric unary signs. Inheritance, interfaces, traits, methods, attributes, property hooks, typed properties, and other class or property modifiers remain unsupported. |
| `new` | Partially supported | Statically named supported user classes and built-in throwable classes may be constructed. User classes currently have no constructors; arguments are evaluated and ignored as PHP does for a class without a constructor. Throwable constructors accept no arguments or one concrete or coercible message argument. Dynamic class names remain unsupported. |
| Object identity and properties | Partially supported | Each construction creates a stable object handle and a persistent property store. Assignment copies the handle, so aliases share updates while separate objects remain independent. Declared property reads, writes, references, `unset`, nested array access, and branch isolation are supported. Property names must be static; dynamic and undeclared properties, methods, cloning, and serialization remain unsupported. |
| Undefined variable, array-offset, and property reads | Unsupported | The execution path explicitly gives up. PHP warning and resulting-value semantics are not implemented yet. |

Throwable construction and catch inheritance currently cover `Throwable`,
`Exception`, `LogicException`, `InvalidArgumentException`, `DomainException`,
`LengthException`, `OutOfRangeException`, `RuntimeException`,
`OutOfBoundsException`, `OverflowException`, `RangeException`,
`UnderflowException`, `UnexpectedValueException`, `Error`, `ArithmeticError`,
`DivisionByZeroError`, `AssertionError`, `TypeError`, `ArgumentCountError`, and
`ValueError`. `Throwable` itself is not constructible.

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

Builtin and user-function arity errors, failed assertions, division by zero,
and uncaught explicit exceptions retain their source location and call trace.
Failures reached in a user function also identify the call site. Unsupported
function names, unsupported builtins, and unsupported semantic cases explicitly
give up the current path. The `exec` command returns status 1 when a definite
failure is found, status 2 for frontend errors, and status 3 when exploration is
incomplete.
