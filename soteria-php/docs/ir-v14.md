# Soteria PHP IR schema version 14

Schema version 14 extends version 13 with the `isset` expression. Every node
continues to carry the source `location` used by earlier schemas.

`isset` retains its ordered, non-empty list of lvalue targets:

```json
{
  "kind": "isset",
  "targets": [
    {"kind":"variable","name":"value","location":{}},
    {
      "kind":"object_property",
      "object":{"kind":"variable","name":"box","location":{}},
      "name":"item",
      "location":{}
    }
  ],
  "location": {}
}
```

Targets are tested from left to right and evaluation stops at the first unset
target. The dedicated node lets the interpreter suppress ordinary undefined-read
events and invoke `__isset` for inaccessible or missing object properties.

Version 14 is intentionally incompatible with version 13. Decoders reject all
other schema versions rather than guessing at missing `isset` semantics.
