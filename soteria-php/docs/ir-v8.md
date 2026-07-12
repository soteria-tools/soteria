# Soteria PHP IR version 8

Version 8 extends version 7 with by-value `foreach`. The program envelope uses
`schema_version` `8`; all version 7 constructs retain their existing
representation.

A `foreach` statement contains the iterable expression, an optional key lvalue,
a value lvalue, and its body:

```json
{
  "kind": "foreach",
  "iterable": {
    "kind": "variable",
    "name": "items",
    "location": {}
  },
  "key": {
    "kind": "variable",
    "name": "key",
    "location": {}
  },
  "value": {
    "kind": "variable",
    "name": "value",
    "location": {}
  },
  "body": [],
  "location": {}
}
```

`key` is JSON `null` when the source loop omits a key target. Key and value
targets use the existing lvalue representation and may include nested array or
object-property targets. Array append lvalues are allowed for these write
targets.

The loop is an enclosing loop for the existing positive-depth `break` and
`continue` statements. The iterable is evaluated once. Array entries are
visited in insertion order using a snapshot of the array structure; later
structural mutations do not alter the pending entries, while reference entries
continue to name their persistent cells.

Version 8 does not encode by-reference iteration. The frontend rejects
`foreach ($items as &$value)` explicitly; that form requires a later schema
version.
