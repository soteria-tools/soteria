# Soteria PHP IR version 7

Version 7 is historical. The current frontend emits version 10, documented in
`ir-v10.md`.

Version 7 extends version 6 with named class declarations, declared instance
properties, and object-property lvalues. The program envelope uses
`schema_version` `7`, adds a required `classes` array, and retains all version 6
constructs in their existing representation.

A class declaration has a namespace-resolved name and an ordered list of
declared properties:

```json
{
  "name": "App\\Box",
  "properties": [
    {
      "name": "value",
      "default": {
        "kind": "int",
        "value": "1",
        "location": {}
      },
      "location": {}
    }
  ],
  "location": {}
}
```

`default` is JSON `null` when an untyped property has no explicit initializer.
An explicit PHP `null` initializer is represented by the ordinary `null`
expression object. A version 7 frontend accepts ordinary named classes whose
members are public, untyped, non-static properties. Property defaults are
limited to supported scalar literals, nested array literals, and numeric unary
signs. Inheritance, interfaces, traits, methods, attributes, property hooks,
typed properties, and class or property modifiers outside this subset are
rejected during lowering.

An object-property lvalue recursively identifies the lvalue containing the
object and uses a statically named property:

```json
{
  "kind": "object_property",
  "object": {
    "kind": "variable",
    "name": "box",
    "location": {}
  },
  "name": "value",
  "location": {}
}
```

This form composes with array-element lvalues and may be used by ordinary
assignment, assignment by reference, and `unset`. A property read wraps a
non-append property lvalue in a `property_get` expression:

```json
{
  "kind": "property_get",
  "target": {
    "kind": "object_property",
    "object": {
      "kind": "variable",
      "name": "box",
      "location": {}
    },
    "name": "value",
    "location": {}
  },
  "location": {}
}
```

Object construction continues to use the version 6 `new` expression. A user
class in this subset has no constructor, so arguments are evaluated in order
and otherwise ignored, matching PHP. Each construction creates a fresh stable
object identity and a fresh persistent property store. Assignment copies the
object handle; it does not copy the property store.
