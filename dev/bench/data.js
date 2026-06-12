window.BENCHMARK_DATA = {
  "lastUpdate": 1781281210291,
  "repoUrl": "https://github.com/soteria-tools/soteria",
  "entries": {
    "Soteria benchmarks": [
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "1c1070cf6140b9dd1e2c7fa8eb51f4f14440e7ea",
          "message": "Update to all workflows + benchmarks (#352)\n\n* Update to all workflows + benchmarks\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* fix-bench\n\n* install dune version first to avoid dumb reinstall of everything\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* make it run\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* relevant benchmarks\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* schema for benchmarks\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* fix branch call percentage\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* benchmarks\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Fix tests\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* For PRs, just push a table to the comments, nothing on the website\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* fix benchmark script to show diff properly\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-05-25T08:49:09Z",
          "tree_id": "59ebc08b1bc2bade99d06b367b589c91ef100e08",
          "url": "https://github.com/soteria-tools/soteria/commit/1c1070cf6140b9dd1e2c7fa8eb51f4f14440e7ea"
        },
        "date": 1779700391699,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07763321158,
            "range": "± 0.0022",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 2.3827436683200003,
            "range": "± 0.0154",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.89841091718,
            "range": "± 0.0107",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.28085986254,
            "range": "± 0.0084",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.6721822496399996,
            "range": "± 0.0379",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.87182900501648,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "796e29d955ad228128dcb0b6983b46a623e7689d",
          "message": "Agent files + AI Policy (#347)\n\n* CLAUDE.md\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Add skill for profiling\n\n* AI Policy\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* license\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* gitignore\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* CLAUDE: Keep changes minimal\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Update CLAUDE.md\n\nCo-authored-by: opale <opale.sjostedt@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: opale <opale.sjostedt@gmail.com>",
          "timestamp": "2026-05-25T09:57:27Z",
          "tree_id": "8816f82f92a00dc32973274db4297cfb572b75b3",
          "url": "https://github.com/soteria-tools/soteria/commit/796e29d955ad228128dcb0b6983b46a623e7689d"
        },
        "date": 1779704005236,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07757038022,
            "range": "± 0.0021",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 2.38750320368,
            "range": "± 0.0076",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.90162289932,
            "range": "± 0.0094",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.28179865158000006,
            "range": "± 0.004",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.68722576662,
            "range": "± 0.0288",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.762987945985515,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "bce47f692472d16763e6f03b717d4bea62216643",
          "message": "Drastically improve performance of Rust WPST (#349)\n\n* Make the input of `Map.Key.distinct` lazy, speeding up wpst by ~45% on cases with a lot of allocation\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* minor optimisation (2%)\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* crate: memoize get_adt / get_trait_impl / get_trait_decl\n\nThese ran a full Charon-AST substitution visitor on every call to apply\ngeneric args, recomputed for identical refs throughout interpretation\n(~5% of samples on the tokio --no-compile run; the GC-pressure\nexperiment ruled out allocation, so this recomputed CPU is real).\n\nAdd a per-crate decl cache installed by with_crate (available exactly\nwherever these are callable, since they all get_crate ()). Pure function\nof the immutable crate + the decl ref, so memoizing by the ref cannot\nchange results -- same reasoning as Layout.Session's layout cache.\n\nOutput byte-identical, cram + soteria + soteria-c green; ~1.04x on the\nbenchmark.\n\n* Only perform Stats effects if Config.output_stats is `Some _`\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* micro optimise\n\n* Rename `Key.distinct` -> `Key.distinct_seq`\n\n* ???\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Fix overwritten distinct_seq function\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* fix tests after merging main\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: N1ark <opale.sjostedt@gmail.com>",
          "timestamp": "2026-05-25T10:13:23Z",
          "tree_id": "23891ce09f5f206fe660df1131978ae7e8ffdc28",
          "url": "https://github.com/soteria-tools/soteria/commit/bce47f692472d16763e6f03b717d4bea62216643"
        },
        "date": 1779704891494,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07902581794000002,
            "range": "± 0.0019",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.98547319848,
            "range": "± 0.0109",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.85585460032,
            "range": "± 0.0108",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.24536114222000008,
            "range": "± 0.0045",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.30027946964,
            "range": "± 0.033",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.679007767990697,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "035d37887b87d6bdcb76ca809a93e89bae883a1c",
          "message": "Minor doc improvements (#355)\n\n* Add missing code formatting on links\n\n* Fix some broken links\n\n* Reformat .mld files (max col = 80!)\n\n* Add CI step to make sure mld files are formatted\n\n* minor: remove trailing `.` in CI output\n\n* re-format\n\n* whitespace\n\n* ... formatting?\n\n* ~~very~~ parametric",
          "timestamp": "2026-05-25T12:55:19Z",
          "tree_id": "a94d1e9dc1c898c212bb9e741226cdec58a97355",
          "url": "https://github.com/soteria-tools/soteria/commit/035d37887b87d6bdcb76ca809a93e89bae883a1c"
        },
        "date": 1779714515400,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.0797010953,
            "range": "± 0.004",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.98175352674,
            "range": "± 0.0097",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8548734561,
            "range": "± 0.0125",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.24685573912,
            "range": "± 0.0044",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.3071739005799996,
            "range": "± 0.039",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.781565395998769,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1acb32890c4aaadc7b719cafacfbfa5cef7eda4c",
          "message": "BtreeSet sort benchmark + trivial inequalities (#356)\n\n* btreeset_sort perf test\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* add it to benchmarks\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Trivial truthines of inequalities.\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Opale's comments\n\nCo-authored-by: N1ark <opale.sjostedt@gmail.com>\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: N1ark <opale.sjostedt@gmail.com>",
          "timestamp": "2026-05-25T17:59:58Z",
          "tree_id": "f508648f9181a23b874335546031b7c4630fa5e2",
          "url": "https://github.com/soteria-tools/soteria/commit/1acb32890c4aaadc7b719cafacfbfa5cef7eda4c"
        },
        "date": 1779732912795,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07900557242,
            "range": "± 0.0022",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.97816865092,
            "range": "± 0.01",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8503823429000001,
            "range": "± 0.0116",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.2420915451,
            "range": "± 0.0032",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.90169984512,
            "range": "± 0.0237",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.28718126018,
            "range": "± 0.0355",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.58647286699852,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0c8229b3139d620335b8e18a29ff23edbfb91b82",
          "message": "Weak Tree Borrows (#357)\n\n* Weakly referenced pointer tags !\n\n* minor optim\n\n* Remove `UB` state, use an exception\n\n* Better Rust `Call_graph`\n\n* Update writealotloop.rs\n\n* Less GC cleanups\n\n* Create compare_landmarks.py\n\n* Move GC cleanup to tag creation\n\n* reset node map at GC boundary !\n\n* final attempt\n\n* Revert \"final attempt\"\n\nThis reverts commit 37ecbdfcc62d94555547c1c23ddc76251d38dfc6.\n\n* Call me ratatouille the way im always cooking\n\n* Update raw.ml\n\n* Update raw.ml\n\n* Keep track of `known_size`\n\n* Resizable compact thershold !\n\n* Delete compare_landmarks.py\n\n* factor out the `compact` code\n\n* inline",
          "timestamp": "2026-05-27T16:45:09Z",
          "tree_id": "066c5fae0a042a7179439b2abd2169249cb12aad",
          "url": "https://github.com/soteria-tools/soteria/commit/0c8229b3139d620335b8e18a29ff23edbfb91b82"
        },
        "date": 1779901238229,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08186097789999999,
            "range": "± 0.0077",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.60968420724,
            "range": "± 0.0119",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.89271253452,
            "range": "± 0.0136",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.24718004946,
            "range": "± 0.0033",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.9201989588,
            "range": "± 0.0608",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.32188418934,
            "range": "± 0.0356",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.818454856984317,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "919823a852f74c5df3f665f8237a5ef61e22e67b",
          "message": "Update the README.md (#358)\n\n* Update README.md\n\n* Update README.md\n\n* center stuff\n\n* Update README.md\n\n* Update README.md\n\n* Update README.md\n\n* Update README.md",
          "timestamp": "2026-05-27T17:52:29Z",
          "tree_id": "894597abf00b134096efb2836d8e0c181c09b555",
          "url": "https://github.com/soteria-tools/soteria/commit/919823a852f74c5df3f665f8237a5ef61e22e67b"
        },
        "date": 1779905400166,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08190774696,
            "range": "± 0.0076",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.61021212628,
            "range": "± 0.0146",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8971920621200002,
            "range": "± 0.0073",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.24360055631999997,
            "range": "± 0.0024",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.890714196120001,
            "range": "± 0.0487",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.3139297710000006,
            "range": "± 0.0412",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.873503129929304,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "8d0c0dd2c44bd4eb1ea34049373a7bb4b66d999a",
          "message": "Don't hardcode name of the repo in actions (#361)",
          "timestamp": "2026-05-28T14:10:51Z",
          "tree_id": "66f6553ba0975249c9709ecb5cab6519cc38c17a",
          "url": "https://github.com/soteria-tools/soteria/commit/8d0c0dd2c44bd4eb1ea34049373a7bb4b66d999a"
        },
        "date": 1779979984599,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07828744182000001,
            "range": "± 0.0024",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.6107941336,
            "range": "± 0.0098",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.89772864408,
            "range": "± 0.0212",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.24279650467999997,
            "range": "± 0.0038",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.8729035471,
            "range": "± 0.0838",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.3234484337,
            "range": "± 0.0228",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.857336894026957,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "184fa00876da75c3dc6c36660682787b9b343201",
          "message": "More performance improvements (#360)\n\n* Cache masks\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Don't throw an effect when value is already a concrete boolean\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* cache zeros and ones\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Don't throw flamegraph effects if they're not enabled\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Domain local storage to cache crate stuff\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Optimise if%sat information tracking\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Zeros and ones are cached so they come left in expressions, promote tests\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* simplify\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-05-28T14:57:45Z",
          "tree_id": "1b99eb4dbf972af9ddfd3d6b3421fffb9cc5bd0b",
          "url": "https://github.com/soteria-tools/soteria/commit/184fa00876da75c3dc6c36660682787b9b343201"
        },
        "date": 1779981267680,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07795116236,
            "range": "± 0.0021",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.5573845076,
            "range": "± 0.0074",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.85760233932,
            "range": "± 0.0099",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.21275034062000003,
            "range": "± 0.0032",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.218485337159999,
            "range": "± 0.0669",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9703423238,
            "range": "± 0.0248",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.855916576925665,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "55fa5c99921cdbdc7d24cc1c9321d745403bee9b",
          "message": "Fix nondet_raw for unions (#362)\n\n* printer fix\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* drive-by: improve printing when cleaning rust files\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* only use cache if less than 265\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Fix union nondet\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* cleanup crate files as well\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* mask cache\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* add regression test\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* oops\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-05-29T15:53:39Z",
          "tree_id": "e1b45db9c29837337747a493e969ec24bf613159",
          "url": "https://github.com/soteria-tools/soteria/commit/55fa5c99921cdbdc7d24cc1c9321d745403bee9b"
        },
        "date": 1780070676128,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.0791825439,
            "range": "± 0.0026",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.56589654636,
            "range": "± 0.0092",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8614671701799999,
            "range": "± 0.01",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.21161119286000002,
            "range": "± 0.0023",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.25596636778,
            "range": "± 0.056",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9612227264600002,
            "range": "± 0.0172",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.856619941070676,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1284b2020a5ef92340cc084d13ed8c9c8bf4b723",
          "message": "Minor helpers (#364)\n\n* factor out cleanup from `exec_fun`\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* helpers\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-02T22:09:34Z",
          "tree_id": "07b0791efe5110bb45cb42220bccd0da0e434f7e",
          "url": "https://github.com/soteria-tools/soteria/commit/1284b2020a5ef92340cc084d13ed8c9c8bf4b723"
        },
        "date": 1780439110853,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08019595970000001,
            "range": "± 0.0074",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.56525596294,
            "range": "± 0.0071",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8703578724000003,
            "range": "± 0.0113",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.21397813156,
            "range": "± 0.0037",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.3126365042399994,
            "range": "± 0.0653",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9610946232400004,
            "range": "± 0.0307",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.866082086926326,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6c67c719c78882a8c7877c059155f1b4f43c8fa5",
          "message": "Bump Charon (#363)\n\n* Bump Charon\n\n* minor fixes\n\n* minor fixes to improve Charon support\n\n* dawg\n\n* Cache stub lookups\n\n* Revert \"Cache stub lookups\"\n\nThis reverts commit fa9be67ca0e166795cb882ccafef23bc2fb72490.\n\n* minor guaranteed improvement",
          "timestamp": "2026-06-03T10:43:57Z",
          "tree_id": "b8a35566ca7b5139d63948f791f94441d6d2160c",
          "url": "https://github.com/soteria-tools/soteria/commit/6c67c719c78882a8c7877c059155f1b4f43c8fa5"
        },
        "date": 1780485380504,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08115670182,
            "range": "± 0.0019",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.5840161312600001,
            "range": "± 0.0045",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8887559492200001,
            "range": "± 0.0149",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22229221744000002,
            "range": "± 0.0025",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.82859007128,
            "range": "± 0.0545",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9464446885799997,
            "range": "± 0.0267",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.867492355871946,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c151bbcd98f1570def9d7da4cf5012f5fbbc0237",
          "message": "Faster CI (#365)\n\n* Only wait for Ubuntu to be done to run PR commands\n\n* Try making `test-lib` faster\n\n* Fixup the test-lib\n\nCo-authored-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* setup-dune v2 🤦‍♂️\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* update actions versions\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* display: short\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-03T13:51:38Z",
          "tree_id": "8a284108f80ff1477e0e9f51e19084bda43cd22a",
          "url": "https://github.com/soteria-tools/soteria/commit/c151bbcd98f1570def9d7da4cf5012f5fbbc0237"
        },
        "date": 1780496143719,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08093267652000001,
            "range": "± 0.0023",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.58204885328,
            "range": "± 0.0062",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8949418894200005,
            "range": "± 0.0124",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22292018750000003,
            "range": "± 0.0037",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.844865756600001,
            "range": "± 0.0387",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9561474824399998,
            "range": "± 0.0242",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.744375874986872,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9ae2fa719468ab35509772fc289a23c33241e420",
          "message": "Automatic action pinning (#367)\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-03T19:52:42Z",
          "tree_id": "0ada97394aca5f83cecec95f67252d323405ce7d",
          "url": "https://github.com/soteria-tools/soteria/commit/9ae2fa719468ab35509772fc289a23c33241e420"
        },
        "date": 1780517406565,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07925323156,
            "range": "± 0.0007",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.5868575976200001,
            "range": "± 0.0043",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.9016211463000006,
            "range": "± 0.0241",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22287094582000005,
            "range": "± 0.0031",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.86522193568,
            "range": "± 0.0384",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.94691155106,
            "range": "± 0.0303",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.702021068893373,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "1dfec10bb3c40231f2e40f9a442caa27cb49b393",
          "message": "Cleaner transmutes (#366)\n\n* Factor out `check_validity`\n\n* Add `State.transmute`\n\n* Tests :)\n\n* transmute fast path\n\n* Revert \"transmute fast path\"\n\nThis reverts commit 9866ff2e7456903c24f844ba76ea66d791c86b2f.\n\n* An `open` never hurts\n\n* Fix bug in transmute",
          "timestamp": "2026-06-04T16:20:50Z",
          "tree_id": "3e54b185694f0fae54931a841fa64cd3bc319673",
          "url": "https://github.com/soteria-tools/soteria/commit/1dfec10bb3c40231f2e40f9a442caa27cb49b393"
        },
        "date": 1780591126769,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08090569054,
            "range": "± 0.002",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.5846825986799999,
            "range": "± 0.0051",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.9069453598800004,
            "range": "± 0.0119",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22219427364,
            "range": "± 0.0024",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.74640076066,
            "range": "± 0.0521",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9601054134400004,
            "range": "± 0.0179",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.841945405118167,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "733547f5834001b62d7cb42e62867a7ad92e2fb6",
          "message": "Detect writes to read-only memory (#368)\n\n* Add wrong test\n\n* Simplify leak checks\n\n* Remove/merge ref_tys_in and update_ref_tys_in\n\n* Properly detect writes to const memory\n\n* promote tests + minor optim",
          "timestamp": "2026-06-05T14:01:48Z",
          "tree_id": "a66bd123e6c4323c55f84e3ce3cfdf0f42c7ac96",
          "url": "https://github.com/soteria-tools/soteria/commit/733547f5834001b62d7cb42e62867a7ad92e2fb6"
        },
        "date": 1780669536535,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08077915584000002,
            "range": "± 0.0017",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.58791915352,
            "range": "± 0.006",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8917436256000002,
            "range": "± 0.0169",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22184303344000003,
            "range": "± 0.0024",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.739409996659998,
            "range": "± 0.0595",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.96702352272,
            "range": "± 0.0273",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.559385573957115,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ff2529e7d331dee52c94c7d8bf6ae93505df9e10",
          "message": "Implement the `caller_location` intrinsic (#370)\n\nImplement `caller_location` intrinsic",
          "timestamp": "2026-06-05T14:37:45Z",
          "tree_id": "5d27e1a3451602e6384734319d7de2f1ddc65350",
          "url": "https://github.com/soteria-tools/soteria/commit/ff2529e7d331dee52c94c7d8bf6ae93505df9e10"
        },
        "date": 1780671257626,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.0813526115,
            "range": "± 0.002",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.58293084036,
            "range": "± 0.0058",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.88684066516,
            "range": "± 0.0149",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22399905722000005,
            "range": "± 0.003",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.73750695864,
            "range": "± 0.0556",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9712506766800004,
            "range": "± 0.0262",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 11.035852757981047,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e3a7be359f5d4a4c864a6f557d5eb807ce8ad847",
          "message": "Bump Charon and Obol (#369)\n\n* Bump Charon and Obol\n\n* promote tests\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-05T14:48:17Z",
          "tree_id": "c2081b54a1517295396b53c26fa426f3527f3835",
          "url": "https://github.com/soteria-tools/soteria/commit/e3a7be359f5d4a4c864a6f557d5eb807ce8ad847"
        },
        "date": 1780672874478,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08128274204,
            "range": "± 0.0018",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.5898526938800001,
            "range": "± 0.0054",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.88703479482,
            "range": "± 0.0138",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.224247551,
            "range": "± 0.0036",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.674862675339999,
            "range": "± 0.0537",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.96047198156,
            "range": "± 0.0151",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.828531790990382,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a84938c5df0c946698e41fac8d4750ddd9e9da63",
          "message": "Utilities in tree_state.ml (#371)\n\nutilities in tree_state.ml\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-05T16:39:40Z",
          "tree_id": "5845ab80d90732b835fd6b883729dd598081ecef",
          "url": "https://github.com/soteria-tools/soteria/commit/a84938c5df0c946698e41fac8d4750ddd9e9da63"
        },
        "date": 1780678591107,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.10916612860000001,
            "range": "± 0.0292",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.89270827018,
            "range": "± 0.2915",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 2.2588759550799997,
            "range": "± 0.4909",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.24675174046,
            "range": "± 0.0339",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 7.2682293608999995,
            "range": "± 0.8422",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.96797784512,
            "range": "± 0.0569",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 10.032648183999981,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "80d2e462d334d9736b9eb60d2e54dcf92681f299",
          "message": "Iarray helpers (#372)\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-08T15:51:43Z",
          "tree_id": "22ff783f03f69a6ad6c97138d5d2ba3c09703c8a",
          "url": "https://github.com/soteria-tools/soteria/commit/80d2e462d334d9736b9eb60d2e54dcf92681f299"
        },
        "date": 1780934965934,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08104372572,
            "range": "± 0.0019",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.5852662231200001,
            "range": "± 0.0066",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 2.4693535261600004,
            "range": "± 0.4921",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22464577500000002,
            "range": "± 0.0093",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 7.099559678,
            "range": "± 0.7315",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.97337971368,
            "range": "± 0.0374",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 15.384265647997381,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "8c2f6974abb5d6352373c9efc1fc12be9b77f01f",
          "message": "Factor out logic of python scripts into soteria_utils.py (#373)\n\nfactor out logic of python scripts into soteria_utils.py\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-08T21:17:48Z",
          "tree_id": "2a9a3d7a40619c16aa51338be7cf168a55eadf0b",
          "url": "https://github.com/soteria-tools/soteria/commit/8c2f6974abb5d6352373c9efc1fc12be9b77f01f"
        },
        "date": 1780954366755,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08014656658,
            "range": "± 0.0025",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.5731718947600001,
            "range": "± 0.0097",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8851251537200002,
            "range": "± 0.0144",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.21966799046000002,
            "range": "± 0.0028",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.630362080439999,
            "range": "± 0.039",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.92357578362,
            "range": "± 0.0323",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.93679752299795,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "78c565539991f75e6ed0acd102fd9759cd8df402",
          "message": "Add `--offline` to Soteria Rust (#375)\n\n* Add `--offline` to Soteria Rust\n\n* Run soteria rust tests offline\n\n* whatever",
          "timestamp": "2026-06-10T13:19:20Z",
          "tree_id": "36a8fd21d383d6e753b4e4dc9bcdfa347f8af9fd",
          "url": "https://github.com/soteria-tools/soteria/commit/78c565539991f75e6ed0acd102fd9759cd8df402"
        },
        "date": 1781098560888,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08060060308000001,
            "range": "± 0.0014",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.59463587988,
            "range": "± 0.0052",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.9188985373199998,
            "range": "± 0.0168",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22326083574000002,
            "range": "± 0.0026",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.715940859220001,
            "range": "± 0.0344",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.91708219942,
            "range": "± 0.0187",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.746439278998878,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "8c5c2bf777be5ef34ab5b3462ffb6ea2467b2228",
          "message": "Fix Unsoundness in Interval Analysis (#377)\n\nfix unsoundness in interval solver\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-10T15:21:03Z",
          "tree_id": "a3c014c4bbc4909e812556073b195b4d21c2253d",
          "url": "https://github.com/soteria-tools/soteria/commit/8c5c2bf777be5ef34ab5b3462ffb6ea2467b2228"
        },
        "date": 1781106060197,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08005382124,
            "range": "± 0.0014",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.5921669120599999,
            "range": "± 0.0041",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8952986801600002,
            "range": "± 0.0184",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22280976280000003,
            "range": "± 0.0031",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 6.68108994454,
            "range": "± 0.035",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9325928219999997,
            "range": "± 0.0339",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.76795479300199,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "b9ec0a9365609f12ac592e940abcfb90c3dcabfd",
          "message": "Rust: Disjoint exposed addresses (#380)\n\n* update Claude.md\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Exposed addresses are disjoint\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-11T20:56:37Z",
          "tree_id": "cc488741b4789f8ea9a43b9a32ee210bc32a60f1",
          "url": "https://github.com/soteria-tools/soteria/commit/b9ec0a9365609f12ac592e940abcfb90c3dcabfd"
        },
        "date": 1781212522380,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08163023076000002,
            "range": "± 0.0018",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.59285841858,
            "range": "± 0.0071",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8922528627,
            "range": "± 0.0098",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22460559352000004,
            "range": "± 0.0022",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 7.334873698020002,
            "range": "± 0.0232",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9085731389800002,
            "range": "± 0.0188",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.627857301034965,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "opale.sjostedt@gmail.com",
            "name": "opale",
            "username": "N1ark"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e7abf7f8b9252008b6319c20aeacf993d6a38b18",
          "message": "Bump Charon and Obol (#381)\n\n* Bump Charon and Obol\n\n* Fix ref/ptr constants\n\n* Claude says this will fix CI ?",
          "timestamp": "2026-06-11T21:02:55Z",
          "tree_id": "8db24576753f920a414a6d1e776c16a072d4ab74",
          "url": "https://github.com/soteria-tools/soteria/commit/e7abf7f8b9252008b6319c20aeacf993d6a38b18"
        },
        "date": 1781213812563,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07995715846000002,
            "range": "± 0.0011",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.58789027674,
            "range": "± 0.0042",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.8942043001799997,
            "range": "± 0.0163",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.22305163802,
            "range": "± 0.0027",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 7.256610628019999,
            "range": "± 0.057",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9109202170799997,
            "range": "± 0.0273",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.896290545002557,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2c470d26bee3bbcdbfb0d8dedd6fcf672e7f3595",
          "message": "Optimise struct and array access in the store (#374)\n\n* various helpers\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* store optimisation\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Add tests\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Use validity checks\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* add tests that no allocation is happening\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Revert \"Use validity checks\"\n\nThis reverts commit aac1368a79def33f8144424f25dbc4a5f1f43869.\n\n* lazily load discriminants from the store\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* more tests, we're getting quicker!!\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* optimise metadata projections\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Add `List.update_at`\n\n* Remove zst_value, much cleaner!\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* avoid `option`s when not needed\n\n* Fix box allocations test\n\n* zst_dangling in Sptr, and make it \"dangling_if_zst\"\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* cleanup dangling_if_zst PR to fit in the codebase\n\n* simplify a bit\n\n* remember origin of store pointers; simplifies code\n\n* Remove `Dead` cases, delegate to `resolve_place`\n\n* Simplify things a bunch\n\n* clean a bit\n\n* Fix checking for uninhabited pointees\n\n* final touch ups\n\n* Fix metadata projection in the store\n\n* Fix metadata optimisation in the store\n\n* extract resolve_place_lazy out of try_lazy 💔\n\n* tests\n\n* rerun tests\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: N1ark <opale.sjostedt@gmail.com>",
          "timestamp": "2026-06-12T09:37:50Z",
          "tree_id": "58370e6133de14e0a6dcb86546a2ddbfe695bd90",
          "url": "https://github.com/soteria-tools/soteria/commit/2c470d26bee3bbcdbfb0d8dedd6fcf672e7f3595"
        },
        "date": 1781258116166,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08054683978,
            "range": "± 0.0007",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.35357013536000004,
            "range": "± 0.0034",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6543821505800005,
            "range": "± 0.0203",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.10754288544000004,
            "range": "± 0.0011",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 4.34522024094,
            "range": "± 0.0249",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9016778897000002,
            "range": "± 0.0184",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 10.001670294906944,
            "unit": "s"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "sachaayoun@gmail.com",
            "name": "Sacha Ayoun",
            "username": "giltho"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d61524085759458d22a7c94e2bb7a69bec82de00",
          "message": "Don't decay pointer for null checks in niche optims (#382)\n\n* Don't decay pointer for null checks in niche optims\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* add test that decays indeed don't happen\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Don't format .t files\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* aggregate statistics over all executions\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Opale's comments\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* update tests\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-12T16:00:33Z",
          "tree_id": "534ea3a12a93ce6ecf8ec6bca4ef88c8b4c89e6b",
          "url": "https://github.com/soteria-tools/soteria/commit/d61524085759458d22a7c94e2bb7a69bec82de00"
        },
        "date": 1781281207677,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08090264123999999,
            "range": "± 0.0015",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.35489642792000003,
            "range": "± 0.0034",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6598154267200003,
            "range": "± 0.0149",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.11015558552,
            "range": "± 0.0033",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 4.3644283474200005,
            "range": "± 0.0286",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.90822243216,
            "range": "± 0.0228",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.764614545973018,
            "unit": "s"
          }
        ]
      }
    ]
  }
}