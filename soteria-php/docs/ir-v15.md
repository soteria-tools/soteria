# Soteria PHP IR schema version 15

Schema version 15 extends version 14 with the `clone` expression. Every node
continues to carry the source `location` used by earlier schemas.

`clone` retains the operand that is evaluated before PHP's object and
`__clone` checks:

```json
{
  "kind": "clone",
  "expression": {
    "kind": "variable",
    "name": "box",
    "location": {}
  },
  "location": {}
}
```

The dedicated node lets the interpreter allocate fresh object identity while
preserving PHP's shallow property-copy and reference behavior. If the runtime
class declares or inherits a supported `__clone` method, it runs with `$this`
bound to the newly allocated object.

Version 15 is intentionally incompatible with version 14. Decoders reject all
other schema versions rather than treating clone as ordinary object assignment.
