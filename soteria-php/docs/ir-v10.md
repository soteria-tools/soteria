# Soteria PHP IR version 10

Version 10 extends version 9 with ordered class methods and statically named
instance method calls. The program envelope uses `schema_version` `10`; all
version 9 constructs retain their existing representation.

Every class now has a required `methods` array. Each method records its name,
parameters, body, normalized modifiers, and source location:

```json
{
  "name": "setValue",
  "parameters": [
    {
      "name": "value",
      "location": {}
    }
  ],
  "body": [],
  "modifiers": ["public"],
  "location": {}
}
```

Version 10 supports public, non-static methods only, so `public` is currently
the sole method modifier. Methods without an explicit visibility modifier are
normalized to public.

A statically named instance method call is represented as:

```json
{
  "kind": "method_call",
  "object": {
    "kind": "variable",
    "name": "object",
    "location": {}
  },
  "method": "setValue",
  "arguments": [],
  "location": {}
}
```

The object expression is evaluated before method lookup and arguments. Method
calls use the receiver's runtime class, bind the receiver as `$this` in a fresh
call scope, and resolve method names case-insensitively.
