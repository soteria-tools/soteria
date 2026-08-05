# Soteria PHP IR schema version 13

Schema version 13 extends version 12 with static members, callable values, and
closures. Every node continues to carry the source `location` used by earlier
schemas.

Class property and method `modifiers` contain one visibility modifier followed
by an optional `"static"` modifier. Static property lvalues use:

```json
{"kind":"static_property","class":"Counter","name":"value","location":{}}
```

Static reads use the existing `property_get` expression with a static-property
target. Static calls and first-class callables use the following expression
kinds:

```text
static_method_call      { class, method, arguments }
function_callable       { name }
static_method_callable  { class, method }
object_method_callable  { object, method }
invoke                  { callee, arguments }
```

The callable nodes correspond to PHP's first-class callable syntax such as
`function_name(...)`, `ClassName::method(...)`, and `$object->method(...)`.
`invoke` represents a call through an expression such as `$callable($argument)`.

A closure expression has this shape:

```json
{
  "kind": "closure",
  "parameters": [{"name":"argument","location":{}}],
  "captures": [
    {"name":"value","by_reference":false,"location":{}},
    {"name":"shared","by_reference":true,"location":{}}
  ],
  "body": [],
  "location": {}
}
```

Parameters remain required, untyped, positional, and by-value. Closure captures
are variables and explicitly record whether PHP `use` captured their value or
their cell. Closure bodies use the ordinary statement schema and may return.

Version 13 is intentionally incompatible with version 12. Decoders reject all
other schema versions rather than guessing at missing member or capture
semantics.
