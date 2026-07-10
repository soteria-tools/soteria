# Soteria PHP IR version 5

Version 5 is historical. The current frontend emits version 6, documented in
`ir-v6.md`.

Version 5 extends version 4 with assignment by reference and `unset`. The
program envelope uses `schema_version` `5`; all version 4 constructs retain
their existing representation.

Assignment by reference uses two lvalues. The `target` binding is replaced with
an alias of the `source` binding:

```json
{
  "kind": "assign_reference",
  "target": {
    "kind": "variable",
    "name": "alias",
    "location": {}
  },
  "source": {
    "kind": "array_element",
    "array": {
      "kind": "variable",
      "name": "values",
      "location": {}
    },
    "key": {
      "kind": "int",
      "value": "0",
      "location": {}
    },
    "location": {}
  },
  "location": {}
}
```

Both lvalues may contain append elements. This represents forms such as
`$values[] =& $value` and preserves PHP's left-to-right lvalue evaluation
order. The initial version 5 grammar still excludes dynamic variables, object
and static properties, destructuring, references returned by functions, and
other referenceable expressions.

An `unset` statement contains one or more non-append lvalues:

```json
{
  "kind": "unset",
  "targets": [
    {
      "kind": "variable",
      "name": "alias",
      "location": {}
    }
  ],
  "location": {}
}
```

Targets retain source order. Unsetting a binding does not destroy a cell that
is still reachable through another variable or array element.
