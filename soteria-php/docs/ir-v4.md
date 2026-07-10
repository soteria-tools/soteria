# Soteria PHP IR version 4

Version 4 is historical. The current frontend emits version 7, documented in
`ir-v7.md`.

Version 4 extends version 3 with array literals, array-element reads, and a
first-class lvalue representation. The program envelope uses `schema_version`
`4`; functions, statements, scalar expressions, and source locations otherwise
retain their version 3 representation.

Assignment no longer names a variable directly. Its `target` is an lvalue:

```json
{
  "kind": "assign",
  "target": {
    "kind": "variable",
    "name": "value",
    "location": {}
  },
  "value": {
    "kind": "int",
    "value": "1",
    "location": {}
  },
  "location": {}
}
```

An array-element lvalue recursively identifies its containing lvalue and has
either an expression key or JSON `null` for append:

```json
{
  "kind": "array_element",
  "array": {
    "kind": "variable",
    "name": "values",
    "location": {}
  },
  "key": {
    "kind": "string",
    "value": "name",
    "location": {}
  },
  "location": {}
}
```

This recursive form represents nested targets such as `$values["name"][0]`.
Append lvalues are accepted only as assignment targets; the decoder rejects an
`array_get` whose target contains a JSON `null` key.

Array-element reads use an `array_get` expression with a non-append `target`:

```json
{
  "kind": "array_get",
  "target": {
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

The initial version 4 lvalue grammar deliberately permits only variables and
array elements rooted in variables. Array access on temporary expressions,
dynamic variables, destructuring targets, object and static properties, and
references remain unsupported.

An `array` expression contains an ordered `items` array. Each item has an
optional expression `key`, a `value` expression, and its own `location`:

```json
{
  "kind": "array",
  "items": [
    {
      "key": null,
      "value": {
        "kind": "string",
        "value": "first",
        "location": {}
      },
      "location": {}
    }
  ],
  "location": {}
}
```

JSON `null` keys request the next PHP integer key. Array unpacking, array items
by reference, and omitted destructuring items are rejected by the frontend.
All lvalues, items, keys, and values carry source locations, and the decoder
validates those locations against the program's `source_file`.
