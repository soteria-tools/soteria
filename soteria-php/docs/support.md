# Soteria PHP support matrix

Soteria PHP targets PHP 8.4.19 with 64-bit integers. PHP 8.4.19 is the concrete
semantics oracle, while the parser sidecar may run on any PHP version accepted
by its Composer manifest. The frontend uses nikic/PHP-Parser 5.8.0 and emits
Soteria PHP IR schema version 15.

Standalone macOS arm64 and Linux x86_64 packages bundle Z3 and the locked
PHP-Parser dependencies. They deliberately do not bundle PHP: a 64-bit system
PHP runtime version 8.2 or newer must be available as `php` on `PATH`. Composer
is needed for source development, but not for a prebuilt package.

The interpreter supports this deliberately bounded source subset:

The detailed cast and operand-kind tables are in
[`scalar-semantics.md`](scalar-semantics.md).

| Construct | Status | Notes |
| --- | --- | --- |
| `null`, boolean, integer, and float literals | Supported | Integers must fit in a signed 64-bit value; floats must be finite. |
| String literals | Partially supported | UTF-8 string values only. |
| Variables, assignment, and references | Supported | Scopes map variables to immutable cells. Ordinary and reference assignment targets may be variables, nested array elements, or declared object properties. Reference assignment aliases these bindings through persistent cells. Dynamic variables and other lvalues remain unsupported. |
| Array literals | Partially supported | Keyed and unkeyed items are evaluated in PHP order. Unpacking and items by reference remain unsupported. |
| Array keys | Partially supported | Concrete null, boolean, integer, float, and string keys use PHP key normalization. Lossy float-to-integer keys emit PHP's deprecation event. Arrays raise a catchable `TypeError`. |
| Array reads, writes, and append | Partially supported | Nested array lvalues, null/undefined and false autovivification, insertion order, negative next-integer keys, append exhaustion, missing-offset warnings, and invalid-container behavior are modelled. String offsets remain unsupported. |
| Symbolic array keys | Partially supported | Symbolic integer and boolean keys branch against concrete and symbolic integer keys. Feasible fresh keys are retained persistently for reads and writes. Appending after a symbolic integer-key insertion and array union involving arrays with symbolic-key history remain unsupported because their resulting keys can themselves require symbolic branching. |
| Array assignment and copying | Supported | Arrays are immutable values, so ordinary assignment copies the value while updates rebuild only the affected path. Array elements promoted to references retain their aliases in later copies; earlier copies remain independent. Mandatory branch-isolation tests cover ordinary and referenced cells. |
| `foreach` | Partially supported | By-value array iteration uses an insertion-order snapshot. By-reference array iteration promotes visited entries to persistent cells, traverses live additions and removals, and preserves the lingering value alias. Optional key targets, nested key/value lvalues, structured loop control, copied references, and symbolic branch isolation are supported. Object iteration remains unsupported. |
| Boolean, integer, float, and string casts | Partially supported | All concrete scalar casts are supported. Array-to-boolean, integer, and float conversion is supported. Array-to-string conversion, conversions requiring a symbolic string, and symbolic float-to-integer conversions explicitly give up. |
| Unary `!`, `+`, and `-` | Partially supported | Numeric unary operators support null, booleans, integers, floats, and concrete numeric strings. Integer-negation overflow promotes to float. Leading-numeric strings emit a warning; invalid operand types raise a catchable `TypeError`. |
| `+`, `-`, `*`, and `/` | Partially supported | Scalar weak numeric coercion, integer/float promotion, integer-overflow promotion, division's always-float result, and array union with `+` are supported. Leading-numeric strings emit a warning, invalid operand combinations raise `TypeError`, and division by zero raises `DivisionByZeroError`. |
| `.` concatenation | Partially supported | Supported when both operands can be converted to concrete strings. |
| `===` and `!==` | Supported | Supported for all current value kinds, including ordered recursive arrays, symbolic scalar payloads, and stable object identity. |
| `==` and `!=` | Partially supported | Supported for every scalar pair, including boolean/null precedence, integer/float promotion, numeric strings, ordinary strings, and `NAN`. Array and object loose equality remain unsupported. |
| `<`, `<=`, `>`, and `>=` | Partially supported | Supported for every scalar pair with PHP 8.4.19 coercion rules. Array and object ordering remain unsupported. |
| `&&`, `||`, `and`, and `or` | Supported | Right-hand evaluation is correctly short-circuited and may branch symbolically. |
| Function calls | Partially supported | Statically named user functions and Soteria intrinsics are supported, as are calls through supported first-class callable and closure values. Arguments must be positional, non-reference, and non-unpacked. String and array callable syntax remains unsupported. |
| Named functions and returns | Partially supported | Top-level declarations, required untyped by-value parameters, case-insensitive calls, early returns, fallthrough-to-`null`, recursion, and first-class named-function callables are supported. Extra arguments are evaluated and ignored as in PHP. Parameter defaults and types, return types, references, variadics, named arguments, attributes, and nested declarations remain unsupported. |
| Function entry points | Partially supported | `exec --function NAME` selects a named function case-insensitively and skips top-level executable statements. Entry-point functions must currently have no parameters. |
| Function-local variables | Supported | Calls use a fresh persistent local scope initialized with the parameters. Assignments do not modify or leak into the caller's scope. Output remains visible across calls. PHP `global` and static local variables remain unsupported. |
| Expression statements, `echo`, `if`, `else`, `while`, `return`, `break`, `continue`, and `unset` | Supported | `return` is supported in function bodies. `break` and `continue` use positive static depths and may target enclosing loops. `unset` removes variable, nested array-element, or declared object-property bindings without destroying aliased cells. `elseif` and other loop forms remain unsupported. |
| `throw`, `try`, multi-catch, and `finally` | Partially supported | Explicit throws and modelled runtime errors propagate through expressions and function calls. Catch order and the supported subset of the built-in PHP throwable hierarchy are modelled, catch variables retain stable object identity, and finally runs for every structured completion. Non-object and non-`Throwable` object throws become catchable `Error` objects. |
| Class declarations | Partially supported | Ordinary named classes may extend one supported class and implement supported interfaces. Class and interface graphs are validated case-insensitively and cycles are rejected. Public, protected, and private untyped instance and static properties and methods are supported, including inherited layout, overriding, constructor inheritance, and built-in throwable subclasses. Property defaults may use supported scalar literals, nested array literals, and numeric unary signs. Attributes, property hooks, typed properties, and abstract, final, or readonly members remain unsupported. |
| Interfaces | Partially supported | Named interfaces, interface inheritance, class implementation, inherited implementations, public untyped instance and static method obligations, and internal object-type checks are supported. The current method subset requires matching required-parameter counts and staticness. Interface constants, properties, types, variance, and direct `instanceof` expressions remain unsupported. |
| Traits | Partially supported | Named traits may contribute supported instance or static properties and methods to classes or other traits. Method precedence with `insteadof`, named and unqualified aliases, and alias visibility changes are supported deterministically. Unresolved method conflicts are rejected. Duplicate trait properties are conservatively rejected, including PHP-compatible identical declarations that are not yet compared semantically. |
| `new` | Partially supported | Statically named supported user classes and built-in throwable classes may be constructed; interfaces and traits cannot be instantiated. Declared inherited property initialization precedes a supported inherited or overriding `__construct` call, whose visibility, required arguments, `$this` binding, mutation, and thrown control are modelled. Inaccessible constructor arguments are not evaluated. Arguments to classes without a constructor are evaluated and ignored as in PHP. Throwable constructors and inherited built-in throwable constructors accept no arguments or one concrete or coercible message argument. Dynamic class names remain unsupported. |
| Method calls | Partially supported | Statically named public, protected, and private instance and static methods use case-insensitive inherited lookup. Static calls support named classes, `self`, `parent`, and late-static `static`; missing or inaccessible static methods dispatch through a supported `__callStatic`. Static property lookup shares inherited declaring-class slots and respects redeclarations. Receivers or classes and visibility are checked before arguments are evaluated. Dynamic method names, named or reference arguments, and unpacking remain unsupported. |
| Callable values and closures | Partially supported | PHP first-class callable syntax is supported for named functions, static methods, and bound object methods, with visibility checked when the callable is created. Objects with a supported `__invoke` method can be invoked directly. Anonymous functions support required untyped by-value parameters, explicit `use` captures by value or reference, lexical `$this` and class scope, returns, recursion through captured references, and stable callable identity. Static closures, arrow functions, by-reference returns, typed/default/variadic parameters, and string or array callable syntax remain unsupported. |
| Static state | Supported | Declared static property cells are persistent, initialized once per execution, shared through inheritance unless redeclared, referenceable, visibility-checked, and isolated across symbolic branches. Dynamic static properties and unsetting static properties are rejected with catchable errors. |
| Object identity and properties | Partially supported | Each construction creates a stable object handle and a persistent inherited property store. Declared cells are keyed by declaring class and source name, so parent and child private properties remain distinct. Assignment copies the handle, so aliases share updates while separate objects remain independent. Cloning supported user objects creates a fresh handle and shallow-copies the property store: ordinary property cells are separated, arrays retain value-copy behavior, nested object handles remain shared, and live property references retain their aliases. Visibility is enforced across related and unrelated classes with catchable `Error` values for reads, writes, references, `unset`, cloning, and nested access. Property-assignment right-hand sides run before an access error, matching PHP. Static-name dynamic-property writes emit PHP's deprecation event; dynamic property names and serialization remain unsupported. Built-in throwable objects remain uncloneable. |
| Selected magic methods | Partially supported | Supported `__clone` methods must be non-static and take no arguments; inherited lookup and public, protected, or private visibility are enforced before the shallow clone is made, and the method runs on the fresh object. Public, non-static `__get`, `__set`, `__isset`, `__unset`, `__call`, and `__toString` methods use their required arities. Public non-static `__invoke` methods use the ordinary supported method-parameter rules, and public static `__callStatic` methods accept the missing method name and ordered argument array. `isset` supports ordered variable, array-element, object-property, and static-property lvalues without undefined-read events. Magic property references, indirect modification of overloaded properties, magic first-class callables, recursion suppression, serialization hooks, and dynamic member names remain unsupported. |
| Object builtins | Partially supported | `get_class`, `is_a`, `property_exists`, and `method_exists` support object values and concrete class/member strings. `is_a` supports its optional concrete `allow_string` flag and implicit `Stringable` conformance through `__toString`. Property and method queries do not invoke magic hooks. Symbolic names and autoloading remain unsupported. |
| Undefined variable, array-offset, and property reads | Supported | PHP 8.4 warning events are retained with their source and call trace, and execution continues with `null`. Nested reads retain each warning in evaluation order. |

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

Runtime events are stored persistently in each symbolic state, including their
severity, source location, and call trace. `--runtime-events conservative` is
the default: warnings and error events are reported as bugs, while notices and
deprecations remain diagnostics. `--runtime-events report` retains every event
as a non-failing diagnostic, and `--runtime-events ignore` suppresses them.

The interpreter recognizes these case-insensitive intrinsic and object
functions:

| Function | Status | Notes |
| --- | --- | --- |
| `Soteria\symbolic_bool()` | Supported | Returns a fresh symbolic boolean. |
| `Soteria\symbolic_int()` | Supported | Returns a fresh symbolic signed 64-bit integer. |
| `Soteria\symbolic_float()` | Supported | Returns a fresh symbolic binary64 float. |
| `Soteria\assume(bool)` | Supported | Restricts the current path and currently requires a boolean argument. |
| `Soteria\assert(bool)` | Supported | Produces distinct success and failure paths and currently requires a boolean argument. |
| `Soteria\expect_fail()` | Supported | Marks the current entry point as expected to find a definite failure. Finding a failure succeeds, finding none fails, and incomplete exploration remains incomplete. |
| `get_class(object)` | Supported | Returns the runtime class name for a modelled object. |
| `is_a(object|string, string, bool = false)` | Partially supported | Uses the modelled class/interface graph and requires concrete strings and a concrete optional flag. |
| `property_exists(object|string, string)` | Partially supported | Checks declared and object-local dynamic properties without invoking magic hooks. Built-in throwable properties are not exposed. |
| `method_exists(object|string, string)` | Partially supported | Checks modelled declared methods case-insensitively without invoking `__call`. Built-in throwable methods are not exposed. |

Builtin and user-function arity errors, failed assertions, division by zero,
invalid operand and offset errors, and uncaught explicit exceptions retain
their source location and call trace. PHP runtime errors first follow the
ordinary `throw` path so matching `catch` and `finally` blocks execute; only an
uncaught throwable becomes a final failure.
Failures reached in a user function also identify the call site. Failing paths
print deterministic PHP-level models for symbolic boolean, integer, and float
inputs when Z3 supplies a model. Symbolic inputs are named `$input0`, `$input1`,
and so on in creation order. Unsupported
function names, unsupported builtins, and unsupported semantic cases explicitly
give up the current path. The `exec` command returns status 1 when a definite
failure is found, status 2 for frontend or entry-point errors, and status 3 when
exploration is incomplete.
