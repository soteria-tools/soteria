# Soteria PHP IR version 11

Version 11 extends version 10 with property and method visibility. The program
envelope uses `schema_version` `11`; all version 10 constructs retain their
existing representation.

Every declared property now has a required `modifiers` array containing exactly
one normalized visibility modifier:

```json
{
  "name": "value",
  "default": null,
  "modifiers": ["private"],
  "location": {}
}
```

Method `modifiers` likewise accepts exactly one of `public`, `protected`, or
`private`. Members without an explicit visibility modifier are normalized to
public. Static, abstract, final, readonly, typed, and hooked members remain
outside this schema version.

The interpreter associates every declared property cell with both its declaring
class and source name. It carries the declaring class as the active class scope
while executing a method. Public members are accessible from every scope;
protected and private members are accessible from their declaring class. The
additional protected access granted through inheritance is deferred until class
inheritance is represented in the IR.

Invalid member access raises a catchable PHP `Error`. The object expression is
evaluated before checking access. For property assignment, the right-hand side
is evaluated before the access error is raised; inaccessible method and
constructor arguments are not evaluated.
