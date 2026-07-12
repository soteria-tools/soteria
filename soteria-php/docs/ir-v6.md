# Soteria PHP IR version 6

Version 6 is historical. The current frontend emits version 10, documented in
`ir-v10.md`.

Version 6 extends version 5 with structured loop control, exception handling,
throw expressions, and statically named object construction. The program
envelope uses `schema_version` `6`; all version 5 constructs retain their
existing representation.

`break` and `continue` statements contain a positive, normalized `depth`:

```json
{
  "kind": "continue",
  "depth": 2,
  "location": {}
}
```

The decoder rejects a depth greater than the number of lexically enclosing
`while` loops. Other loop forms remain outside the version 6 grammar.

A `throw` is an expression so that abrupt completion propagates through nested
expression evaluation and function calls:

```json
{
  "kind": "throw",
  "expression": {
    "kind": "new",
    "class": "RuntimeException",
    "arguments": [
      { "kind": "string", "value": "boom", "location": {} }
    ],
    "location": {}
  },
  "location": {}
}
```

`new` contains a namespace-resolved class name and an ordered argument list.
The version 6 interpreter implements this general IR node only for its
documented subset of built-in `Throwable` classes, with zero arguments or one
message argument. General class construction is reserved for the object-model
milestone.

A `try` statement contains its body, ordered catch clauses, and either a
statement list or JSON `null` for `finally`:

```json
{
  "kind": "try",
  "body": [],
  "catches": [
    {
      "types": ["LogicException", "RuntimeException"],
      "variable": "exception",
      "body": [],
      "location": {}
    }
  ],
  "finally": [],
  "location": {}
}
```

Catch type arrays preserve multi-catch source order. `variable` is JSON `null`
for a catch without a binding. A try statement must contain at least one catch
or a finally block. Returns, loop exits, and throws all run the finally block;
an abrupt completion from finally replaces the pending completion.
