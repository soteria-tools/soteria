# Soteria PHP IR version 9

Version 9 is historical. The current frontend emits version 10, documented in
`ir-v10.md`.

Version 9 extends version 8 with by-reference `foreach`. The program envelope
uses `schema_version` `9`; all version 8 constructs retain their existing
representation.

A `foreach` statement now includes a required `by_reference` boolean alongside
the iterable expression, optional key lvalue, value lvalue, and body:

```json
{
  "kind": "foreach",
  "iterable": {
    "kind": "variable",
    "name": "items",
    "location": {}
  },
  "key": null,
  "value": {
    "kind": "variable",
    "name": "value",
    "location": {}
  },
  "by_reference": true,
  "body": [],
  "location": {}
}
```

When `by_reference` is `false`, version 8 snapshot semantics are unchanged.
When it is `true`, array entries are promoted to persistent cells as they are
visited and the value target is rebound to each cell. The source array is
traversed live, so entries appended during the loop are visited and entries
removed before their turn are skipped. The final value-target binding remains
an alias to the last visited entry after the loop.
