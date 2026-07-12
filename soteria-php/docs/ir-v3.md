# Soteria PHP IR version 3

Version 3 is historical. The current frontend emits version 11, documented in
`ir-v11.md`.

Version 3 extends version 2 with top-level named functions, parameters, and
returns. The program object adds a required `functions` array and uses
`schema_version` `3`:

```json
{
  "schema_version": 3,
  "target_php_version": "8.4.19",
  "source_file": "example.php",
  "functions": [],
  "statements": []
}
```

Each function has a namespace-resolved `name`, a `parameters` array, a `body`
statement array, and a `location`. Each parameter has a `name` and `location`.
Functions are separate from executable statements because supported top-level
PHP function declarations are available before control reaches their source
position.

Location objects are abbreviated as `{}` in this example.

```json
{
  "name": "add",
  "parameters": [
    { "name": "left", "location": {} },
    { "name": "right", "location": {} }
  ],
  "body": [],
  "location": {}
}
```

The new `return` statement has an `expression` field containing either an
expression or JSON `null`, plus a `location`. Returns are valid only within a
function body. All version 2 expression and statement kinds remain unchanged.

Version 3 deliberately represents only required, untyped, by-value parameters.
Default values, declared parameter and return types, references, variadics,
named arguments, attributes, nested or conditional function declarations, and
closures remain unsupported and are rejected by the frontend.

The OCaml decoder validates locations throughout function declarations and
bodies, rejects returns in top-level statements, and continues to reject
unknown fields, node kinds, operators, and casts.
