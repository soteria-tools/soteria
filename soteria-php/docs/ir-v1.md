# Soteria PHP IR version 1

Version 1 is the historical parse-only format. The current frontend emits
version 2, documented in `ir-v2.md`.

The frontend emits one JSON program object:

```json
{
  "schema_version": 1,
  "target_php_version": "8.4.19",
  "source_file": "example.php",
  "statements": []
}
```

Every statement and expression has a `location`. Lines and columns are
one-based, byte offsets are zero-based, and ranges are half-open:

```json
{
  "file": "example.php",
  "start": { "line": 2, "column": 1, "offset": 6 },
  "end": { "line": 2, "column": 9, "offset": 14 }
}
```

Version 1 statement kinds are `expression`, `echo`, and `nop`. Version 1
expression kinds are the scalar literals `null`, `bool`, `int`, `float`, and
`string`. Integer and float payloads are decimal strings so their JSON transport
does not depend on a consumer's numeric width or JSON number implementation.

The OCaml decoder rejects unknown fields, unknown node kinds, incompatible
schema or target PHP versions, invalid scalar payloads, and inconsistent source
filenames. Extending or changing the format incompatibly requires a new schema
version.
