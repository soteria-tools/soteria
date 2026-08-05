# Soteria PHP IR schema version 16

Schema version 16 extends version 15 with multi-file source metadata and
supported declaration attributes. Every node continues to carry its original
source location.

`source_file` remains the analysis root. `source_files` lists that root followed
by every statically included or Composer-autoloaded PHP file represented in the
program:

```json
{
  "source_file": "app.php",
  "source_files": ["app.php", "src/Service.php"]
}
```

The frontend resolves the supported leading `include` and `require` bootstrap
before emitting IR. Included and autoloaded files must contain declarations and
further leading includes only, so their declarations can be merged without
approximating PHP's runtime declaration visibility. Include operations therefore
do not appear as executable IR nodes in this schema.

Function and method declarations now carry an `attributes` array. The only
supported attribute is the argument-free `#[Soteria\Test]` entry-point marker:

```json
{
  "name": "checks_invariant",
  "parameters": [],
  "body": [],
  "attributes": [
    {
      "name": "Soteria\\Test",
      "location": {}
    }
  ],
  "location": {}
}
```

Version 16 is intentionally incompatible with version 15. Decoders reject all
other schema versions and reject locations whose files are absent from
`source_files`.
