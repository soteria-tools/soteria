window.BENCHMARK_DATA = {
  "lastUpdate": 1782206209441,
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
          "id": "90e60f51bcff1b9f0b4bbb5951ab69bf19e4be2a",
          "message": "Fix benchmark's version of Obol in CI (#393)\n\nSync benchmarks.yml obol hash via versionsync",
          "timestamp": "2026-06-13T17:50:30Z",
          "tree_id": "8b338d22a6be31ce3ea5eccfe7a36e12cd7fa64a",
          "url": "https://github.com/soteria-tools/soteria/commit/90e60f51bcff1b9f0b4bbb5951ab69bf19e4be2a"
        },
        "date": 1781374139295,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08182354010000001,
            "range": "± 0.0016",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.35379630298,
            "range": "± 0.0051",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6529096082600003,
            "range": "± 0.0104",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.10916740384000001,
            "range": "± 0.0015",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 4.341446335419999,
            "range": "± 0.0447",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.8963194847,
            "range": "± 0.0219",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.723480546032079,
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
          "id": "9833e920b5656a75756ba1ed1bb9c6b82a933378",
          "message": "Use the Obol version from the PR in benchmarks (#395)",
          "timestamp": "2026-06-13T19:54:51+01:00",
          "tree_id": "5c92d763853a9cbc3b28cfdb06608d30167a9b2f",
          "url": "https://github.com/soteria-tools/soteria/commit/9833e920b5656a75756ba1ed1bb9c6b82a933378"
        },
        "date": 1781377383335,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08113994682,
            "range": "± 0.0026",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.35679199295999997,
            "range": "± 0.0037",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6579404534200002,
            "range": "± 0.016",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.11075143822,
            "range": "± 0.0024",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 4.31649033806,
            "range": "± 0.0182",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.89779752798,
            "range": "± 0.0195",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.836936132982373,
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
          "id": "7973d66cb331cf9756e7801e82b44812f8a366fd",
          "message": "Don't decay on `check_overlap` for pointers with != provenance (#384)\n\nDon't decay when using copy_nonoverlapping on pointers with different provenance\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-13T22:00:16Z",
          "tree_id": "96405f40aaa9ec25bed27ec024d6ff11949f2abb",
          "url": "https://github.com/soteria-tools/soteria/commit/7973d66cb331cf9756e7801e82b44812f8a366fd"
        },
        "date": 1781389021304,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07978487368,
            "range": "± 0.0015",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.35209669668,
            "range": "± 0.004",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.66965482224,
            "range": "± 0.0274",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.10881280446000001,
            "range": "± 0.0027",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.44231524824,
            "range": "± 0.0257",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9061286173599996,
            "range": "± 0.0201",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.812607476022094,
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
          "id": "98ccb58dbed5d3b27a09c5bca91cad00dcf52b3d",
          "message": "Don't decay on pointer equality if it can be avoided (#385)\n\n* optimise pointer equality\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* fix the issue\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Opale's comments\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* promote test\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* optimise case where one of the pointers is null\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-14T11:36:24Z",
          "tree_id": "72592fa3762b3209ad849522272f54da3ab2c9a3",
          "url": "https://github.com/soteria-tools/soteria/commit/98ccb58dbed5d3b27a09c5bca91cad00dcf52b3d"
        },
        "date": 1781438114593,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07926494412000003,
            "range": "± 0.0013",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.35140056044,
            "range": "± 0.0051",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6473295652800002,
            "range": "± 0.014",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.10835691917999998,
            "range": "± 0.0023",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.4369795437599997,
            "range": "± 0.0458",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.92348178698,
            "range": "± 0.0204",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.733944016043097,
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
          "id": "eabd02b4d0b4751fdb17e10bb521e8392df19651",
          "message": "Improve diagnostics the tool gives (#389)\n\n* Improve diagnostics in a few places\n\n* more message improvements\n\n* fix messages and cram test (hilarious)\n\n* Cuter diagnostics with `Unimplemented`\n\n* minor\n\n* comment\n\nCo-Authored-By: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nCo-authored-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-14T11:59:54Z",
          "tree_id": "b3e7bbced9e3a1bfe9d27f131d9b21a86769ff4e",
          "url": "https://github.com/soteria-tools/soteria/commit/eabd02b4d0b4751fdb17e10bb521e8392df19651"
        },
        "date": 1781439800854,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08154168888,
            "range": "± 0.0025",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 0.34614840776,
            "range": "± 0.0034",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.65025250572,
            "range": "± 0.0156",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.10807002638,
            "range": "± 0.0022",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.40230775842,
            "range": "± 0.039",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.92531900492,
            "range": "± 0.0282",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.75002915400546,
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
          "id": "4c8b459ddcd9517ed30de85c8d9c5a8e995dad16",
          "message": "Conformance benchmark in CI (#392)\n\n* tweak scripts\n\n* Add conformance suite to benchmark (Kani/Miri)\n\n* commit the csv results of the conformance test\n\n* Track changes in conformance test in CI\n\n* Fix Obol to have Miri in the toolchain\n\n* Bump benchmarks to take >1s\n\n* mb",
          "timestamp": "2026-06-14T12:23:53Z",
          "tree_id": "96056e253d7f9b9bbedf11ae3ad2749db215d43e",
          "url": "https://github.com/soteria-tools/soteria/commit/4c8b459ddcd9517ed30de85c8d9c5a8e995dad16"
        },
        "date": 1781441519087,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.07961236728000001,
            "range": "± 0.0019",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.1288893502200001,
            "range": "± 0.0096",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6512541627800001,
            "range": "± 0.0118",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.39936377542000007,
            "range": "± 0.0042",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.4016032873200004,
            "range": "± 0.0279",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9028874734399999,
            "range": "± 0.0073",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.833253738004714,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 368,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 8,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 118.4122,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 456,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 100,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 42,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 11,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 111.0342,
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
          "id": "db31eab773d734f9f464ce49b881e2b114ca58db",
          "message": "Support calls to `dyn FnOnce` (#390)\n\noh that's it\n\nCo-authored-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-14T12:54:36Z",
          "tree_id": "11b92926ec83e7a619454ebd310381759e03c121",
          "url": "https://github.com/soteria-tools/soteria/commit/db31eab773d734f9f464ce49b881e2b114ca58db"
        },
        "date": 1781442986406,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08224853862,
            "range": "± 0.0018",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.08292655208,
            "range": "± 0.0137",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6296202407600002,
            "range": "± 0.011",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.39895909322000006,
            "range": "± 0.0049",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.42469579148,
            "range": "± 0.0827",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.7225304368400001,
            "range": "± 0.0039",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.42154613130000007,
            "range": "± 0.0116",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9659031939799998,
            "range": "± 0.0794",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.89795180899091,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.0171,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 457,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 100,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 41,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 11,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 111.5719,
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
          "id": "d6cffa205786a44d4cc11f6d64e34ccf07a46564",
          "message": "`not_impl` receives an fmt instead of a string (#397)\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-14T13:48:51Z",
          "tree_id": "5b7f16c2314b866d426e0c9a82b59b8566458c85",
          "url": "https://github.com/soteria-tools/soteria/commit/d6cffa205786a44d4cc11f6d64e34ccf07a46564"
        },
        "date": 1781446222319,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.0809506872,
            "range": "± 0.002",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.06326003978,
            "range": "± 0.0071",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6319763720399998,
            "range": "± 0.0215",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.39755954884,
            "range": "± 0.0056",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.4251592316600004,
            "range": "± 0.0772",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.7133214421200001,
            "range": "± 0.0046",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.4135875603,
            "range": "± 0.0053",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.91799214718,
            "range": "± 0.021",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.85773815610446,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 118.4043,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 457,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 100,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 41,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 11,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 110.5266,
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
          "id": "e883e196719a6240218db4643a337cbebf119278",
          "message": "bump obol to use the `reconstruct_ptr_checks` pass (#398)\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-14T14:07:22Z",
          "tree_id": "bee54827cd949940fff2d18640a8338c7e46da59",
          "url": "https://github.com/soteria-tools/soteria/commit/e883e196719a6240218db4643a337cbebf119278"
        },
        "date": 1781447690329,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08122231656000001,
            "range": "± 0.0013",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.06075948228,
            "range": "± 0.0192",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6292532953399999,
            "range": "± 0.0133",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.39833406092,
            "range": "± 0.0056",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6439353972600004,
            "range": "± 0.0182",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.7140850247999999,
            "range": "± 0.0045",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.41537683484,
            "range": "± 0.0044",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9109566697999998,
            "range": "± 0.0191",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.93506033893209,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 118.4714,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 457,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 100,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 41,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 11,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 110.5318,
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
          "id": "638b578acabee9fe21d3bea8f5ec2ddb4602bcf3",
          "message": "Minor fixes and cleaning up (#400)\n\n* don't raise a UB for dangling pointer on miss\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* prints\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* producing a bound produces a consistent assumption\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* produce fixes for offset\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-15T10:36:07Z",
          "tree_id": "6aeb5d40251ee307699b5b8fa6159dc3cbc973a0",
          "url": "https://github.com/soteria-tools/soteria/commit/638b578acabee9fe21d3bea8f5ec2ddb4602bcf3"
        },
        "date": 1781521063662,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08092837556000002,
            "range": "± 0.0024",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.0899053484799999,
            "range": "± 0.0158",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6307505864000003,
            "range": "± 0.0133",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.39669465114,
            "range": "± 0.006",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6474250045400005,
            "range": "± 0.0154",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.7166485374,
            "range": "± 0.006",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.42441019898,
            "range": "± 0.0066",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9149617060800002,
            "range": "± 0.0196",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.921140952967107,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 118.3431,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 457,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 100,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 41,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 11,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 110.5755,
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
          "id": "f97f32d7d6038fcfdf3d3d8fc43ff2e291f06db2",
          "message": "Fix Z3 leaks (#401)\n\n* prevent z3 solvers from leaking\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* move soteria_smt inside of soteria/lib, extract the subprocess abstraction\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* add tests for subprocess\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* oops\n\nCo-authored-by: opale <opale.sjostedt@gmail.com>\n\n* oops again\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: opale <opale.sjostedt@gmail.com>",
          "timestamp": "2026-06-15T11:09:32Z",
          "tree_id": "ff359d71ad9842af812774921018d1d26a2112f3",
          "url": "https://github.com/soteria-tools/soteria/commit/f97f32d7d6038fcfdf3d3d8fc43ff2e291f06db2"
        },
        "date": 1781523055952,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08174445674000003,
            "range": "± 0.0016",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.09874660516,
            "range": "± 0.0231",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6938221336800001,
            "range": "± 0.0187",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.40729595266,
            "range": "± 0.0074",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 4.0303306907400005,
            "range": "± 0.4806",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.7152471428,
            "range": "± 0.0073",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.42800836158000005,
            "range": "± 0.0045",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 2.1273114003599995,
            "range": "± 0.2766",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 14.22526629792992,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 120.1254,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 457,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 100,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 41,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 11,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 111.8517,
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
          "id": "4daba4cb97258da4fa3ffad6dcc2fd6d5c24a3c7",
          "message": "Implement `L.failwith`, use it everywhere (#402)\n\n* Use L.failwith everywhere\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* add callstack to failure log\n\nCo-authored-by: opale <opale.sjostedt@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: opale <opale.sjostedt@gmail.com>",
          "timestamp": "2026-06-15T11:23:49Z",
          "tree_id": "634098380dab7ac77928cfe4a6720a668de79c4f",
          "url": "https://github.com/soteria-tools/soteria/commit/4daba4cb97258da4fa3ffad6dcc2fd6d5c24a3c7"
        },
        "date": 1781524171324,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 0.08106421666,
            "range": "± 0.0017",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.08113771304,
            "range": "± 0.0142",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6295299432799997,
            "range": "± 0.0131",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.39473892126,
            "range": "± 0.0046",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6501469928600003,
            "range": "± 0.0517",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.71738635786,
            "range": "± 0.0035",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.42604331724,
            "range": "± 0.0044",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9080905728000002,
            "range": "± 0.0168",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 10.040484800934792,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 371,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 17,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 122.0459,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 457,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 100,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 41,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 11,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 117.745,
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
          "id": "57e21c1971bdee01e59d7d0e7f04263e8165ea7f",
          "message": "Support more atomics, some externs, fix benchmark (#403)\n\n* Fix plugins breaking\n\n* Fix/update some stubs\n\n* Resolve `Pointee::Metadata` builtin trait type\n\n* Support thread locals on Linux! re-run tests\n\n* Implement all remaining `atomic_` intrinsics\n\n* fix `Sptr` considering wrapping offset checked\n\n* minor fix to benchmarks workflow\n\n* this took ages to compile ?????\n\n* oops\n\n* tests\n\n* Update run.t",
          "timestamp": "2026-06-15T17:23:33Z",
          "tree_id": "aa4b4ca5b8bbc6034808ac395c608df54e818f6f",
          "url": "https://github.com/soteria-tools/soteria/commit/57e21c1971bdee01e59d7d0e7f04263e8165ea7f"
        },
        "date": 1781545842549,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 4.57955050194,
            "range": "± 0.0482",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.10973305118,
            "range": "± 0.0073",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.633621053,
            "range": "± 0.0153",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.40077522004,
            "range": "± 0.0054",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.67320927106,
            "range": "± 0.0353",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.7383905244200001,
            "range": "± 0.0237",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.4305881059200001,
            "range": "± 0.0046",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9288699240399996,
            "range": "± 0.0315",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.884891076944768,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 118.8502,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 105.6089,
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
          "id": "3cb990fc2d894f5467efce48315c4a475292d6cc",
          "message": "Fix `Bv_values` checked flag (#404)\n\n* Add test\n\n* Fix unsoundness in `Bv_values` checked ops\n\n* Add checked flag to neg\n\n* Revert \"Add checked flag to neg\"\n\nThis reverts commit c95403132ec5270cb995e7b7d8807d76b143bf77.\n\n* Make `Sptr.offset` simpler + reductions\n\n* tweak write a lot to not take 4s\n\n* tweak ptr offsets to prefer unsigned\n\n* add reductions to reclaim PC\n\n* Reapply \"Add checked flag to neg\"\n\nThis reverts commit b3e5f9793fac516a8810502032c959c4fe693cb1.\n\n* try improving reductions\n\n* Reduce signed comparisons better\n\n* Add some more reductions...\n\n* minor: gate reductions\n\n* okayyy okay\n\n* Fix a wrong reduction\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\nCo-authored-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-16T15:04:36Z",
          "tree_id": "6f1b0b1f6a6fdc6ef813798bc4304dde15e8e0cc",
          "url": "https://github.com/soteria-tools/soteria/commit/3cb990fc2d894f5467efce48315c4a475292d6cc"
        },
        "date": 1781623787182,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.32777405234,
            "range": "± 0.0267",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.1024798161799998,
            "range": "± 0.0057",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.62103306188,
            "range": "± 0.0101",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.4015371418600001,
            "range": "± 0.0056",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6328467840399994,
            "range": "± 0.0312",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.73048191508,
            "range": "± 0.0048",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.42506445226,
            "range": "± 0.0046",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.90803220586,
            "range": "± 0.0266",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.795160409994423,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.9142,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 105.426,
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
          "id": "ecc876c6f83e745abe25b5bde19092fddff9bca7",
          "message": "Fix crashes (#407)\n\n* Don't crash when iterating over an empty value\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Improve reading of Any values\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* more prints\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-16T17:03:19Z",
          "tree_id": "63a498d0be7a6edf58af0411794185ae120245bb",
          "url": "https://github.com/soteria-tools/soteria/commit/ecc876c6f83e745abe25b5bde19092fddff9bca7"
        },
        "date": 1781630942310,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.3072475297000006,
            "range": "± 0.0136",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.0847306064000002,
            "range": "± 0.0143",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.62221109232,
            "range": "± 0.0115",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.3967079655,
            "range": "± 0.0051",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6107278005000003,
            "range": "± 0.0204",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.7170945799000001,
            "range": "± 0.0058",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.42594966768000003,
            "range": "± 0.0051",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9194460234400001,
            "range": "± 0.0205",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 10.013432317064144,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.4448,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 104.5656,
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
          "id": "ebe1e19d0b72847b3c345fb13c59821842ea475d",
          "message": "Add --list-tests to `soteria-rust exec` (#408)\n\n* add --list-tests\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* Oops promote test\n\n* add compile\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-16T20:10:28Z",
          "tree_id": "d35f403b3b19884d6d6987cded8fae19c77b7912",
          "url": "https://github.com/soteria-tools/soteria/commit/ebe1e19d0b72847b3c345fb13c59821842ea475d"
        },
        "date": 1781642031408,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.3019125186,
            "range": "± 0.0292",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.10599047166,
            "range": "± 0.0166",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.62019506336,
            "range": "± 0.0175",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.40255417794000004,
            "range": "± 0.0088",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6429532569400003,
            "range": "± 0.0309",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.711275724,
            "range": "± 0.0072",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.4336585143200001,
            "range": "± 0.0054",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9203203933400002,
            "range": "± 0.0336",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 10.001477677025832,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.7221,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 105.4979,
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
          "id": "ed80ab7c5d413461a1dbac19b7505479291f97f3",
          "message": "Update readme for clearer cargo-soteria instructions (#409)\n\nUpdate readme for people coming from Cargo\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-17T18:28:08Z",
          "tree_id": "625771633ba779bf0f4e6bd32ca3d2d9a0f013e9",
          "url": "https://github.com/soteria-tools/soteria/commit/ed80ab7c5d413461a1dbac19b7505479291f97f3"
        },
        "date": 1781722159935,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.3310511086,
            "range": "± 0.0344",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.1026746636599998,
            "range": "± 0.018",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6236536903,
            "range": "± 0.0108",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.40343091440000006,
            "range": "± 0.0057",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6591097507400008,
            "range": "± 0.0489",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.70765379326,
            "range": "± 0.0057",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.43193758712,
            "range": "± 0.0047",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9066454479800001,
            "range": "± 0.0297",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 10.038119934033602,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.6719,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 104.9234,
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
          "id": "c22c425a531b60b9e46351560c11154135c7f841",
          "message": "Fix writealot + keep track if it changes (#415)\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-18T13:45:01Z",
          "tree_id": "6abf84f62dfca48ce726024bca8b649c4310fc82",
          "url": "https://github.com/soteria-tools/soteria/commit/c22c425a531b60b9e46351560c11154135c7f841"
        },
        "date": 1781791775538,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.358430061020001,
            "range": "± 0.039",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.1014262119,
            "range": "± 0.0179",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.63171091902,
            "range": "± 0.0136",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.40051900638000004,
            "range": "± 0.0057",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6767592389200003,
            "range": "± 0.0293",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.7108827337000001,
            "range": "± 0.0067",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.43382697420000005,
            "range": "± 0.0062",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.92295165678,
            "range": "± 0.0269",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.934100121026859,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.5256,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 104.9524,
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
          "id": "23761acd6b7aed482652df8721df92050608f894",
          "message": "Allocation-free hashing for Bv_values (#416)\n\n* allocation-free hashing\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n* derive operator hashes\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>\n\n---------\n\nSigned-off-by: Sacha Ayoun <sachaayoun@gmail.com>",
          "timestamp": "2026-06-19T12:57:37Z",
          "tree_id": "3ab5abc921b568c8777827d6089ee454fd32ae7d",
          "url": "https://github.com/soteria-tools/soteria/commit/23761acd6b7aed482652df8721df92050608f894"
        },
        "date": 1781875310038,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.3160312862800003,
            "range": "± 0.051",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.0975024794199997,
            "range": "± 0.0092",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6302750280400002,
            "range": "± 0.0159",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.40025744440000005,
            "range": "± 0.008",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.6333686058800003,
            "range": "± 0.0274",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.71937355562,
            "range": "± 0.0043",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.42509566548000005,
            "range": "± 0.0043",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.90122483436,
            "range": "± 0.0307",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.959439008962363,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 120.0323,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 105.5388,
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
          "id": "a3f80a64fd8a78fa1db138628bece7ea05ec269d",
          "message": "Fixes for `HashTable` (#406)\n\n* Fix global allocations being mismatched\n\n* Fix stubs.py\n\n* Generate and implement some easy SIMD instructions\n\n* Stub hashing to support hashtables!\n\n* Add a UX warning",
          "timestamp": "2026-06-20T14:08:48Z",
          "tree_id": "b33c7616d56bd8d3380ffd7ba55fcdff0d96e7b2",
          "url": "https://github.com/soteria-tools/soteria/commit/a3f80a64fd8a78fa1db138628bece7ea05ec269d"
        },
        "date": 1781966092429,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.2618206196000004,
            "range": "± 0.0327",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.0884363884,
            "range": "± 0.0162",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6224041657999997,
            "range": "± 0.0162",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.39904683416,
            "range": "± 0.0051",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.64126367724,
            "range": "± 0.0192",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.72382305284,
            "range": "± 0.0047",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.42255140991999995,
            "range": "± 0.0063",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.8875430868600003,
            "range": "± 0.0212",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.84072664892301,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.5168,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 105.1136,
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
          "id": "b153718f819f114dffcf8eb21a916cd2e6964261",
          "message": "Make `Bv_values` a functor (#422)\n\n* Make `Bv_values` a functor\n\n* Make Soteria Rust and Soteria C use the functor\n\n* Add `'ghost` to never mix instantiations of Svalue\n\n* Tweak signature of `mk`\n\n* Document the interface a bit\n\n* Avoid the awkward `Solver_value` thing\n\n* Use memoized encoder for extension values",
          "timestamp": "2026-06-22T22:53:56Z",
          "tree_id": "2bc5e5eaed9b68be773b57bc40399ed3c7c96aa9",
          "url": "https://github.com/soteria-tools/soteria/commit/b153718f819f114dffcf8eb21a916cd2e6964261"
        },
        "date": 1782170121924,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.37654851698,
            "range": "± 0.0225",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.1082500182600001,
            "range": "± 0.0122",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.63000321982,
            "range": "± 0.0134",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.40337855351999996,
            "range": "± 0.0043",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.64991625702,
            "range": "± 0.0251",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.71449387608,
            "range": "± 0.0051",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.423426801,
            "range": "± 0.0069",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.97107626552,
            "range": "± 0.0272",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.892853029072285,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.2375,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 101,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 40,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 104.3808,
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
          "id": "99712cb3ac01613892869c761f8ee6de6c4a22f3",
          "message": "Tidy Soteria Rust (#425)\n\n* utilities\n\n* Remove  `Rust_val.flatten`, match on types\n\n* Split off enum case in Store\n\n* Use `DecayMapS`\n\n* simplify unreachable cases\n\n* type-oriented unsizing rather than structural\n\n* tweak `check_non_dangling_untyped`\n\n* Add `transmute_raw` for raw bytes constants\n\n* remove Value_codec.size_of, make blocks sized\n\n* handle `dyn_metadata` in `AggregatedRawPtr`\n\n* function pointers can also appear in binops\n\n* Review comment",
          "timestamp": "2026-06-23T08:46:06Z",
          "tree_id": "db9637b21d41bea0a4ac9ef86cc7effde09b9f37",
          "url": "https://github.com/soteria-tools/soteria/commit/99712cb3ac01613892869c761f8ee6de6c4a22f3"
        },
        "date": 1782206207313,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "rust-file: write-a-lot",
            "value": 2.3480701627400005,
            "range": "± 0.0203",
            "unit": "s"
          },
          {
            "name": "rust-file: write-a-lot (for loop)",
            "value": 1.1218912214199999,
            "range": "± 0.0107",
            "unit": "s"
          },
          {
            "name": "rust-file: ctpop",
            "value": 1.6446169543,
            "range": "± 0.0152",
            "unit": "s"
          },
          {
            "name": "rust-file: array_init (rust)",
            "value": 0.41186245362000007,
            "range": "± 0.0024",
            "unit": "s"
          },
          {
            "name": "rust-file: btreeset sort (size 4)",
            "value": 3.7072960706800004,
            "range": "± 0.0334",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (chain)",
            "value": 0.72019601832,
            "range": "± 0.0061",
            "unit": "s"
          },
          {
            "name": "rust-file: reborrow (tree)",
            "value": 0.43290984812,
            "range": "± 0.0033",
            "unit": "s"
          },
          {
            "name": "c: array_init (c)",
            "value": 1.9539884715,
            "range": "± 0.0253",
            "unit": "s"
          },
          {
            "name": "c-capture-db: Collections-C",
            "value": 9.91118234093301,
            "unit": "s"
          },
          {
            "name": "conformance-kani: passed",
            "value": 372,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: failed",
            "value": 25,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: unsupported",
            "value": 4,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: timed out",
            "value": 16,
            "unit": "tests"
          },
          {
            "name": "conformance-kani: total time",
            "value": 119.5167,
            "unit": "s"
          },
          {
            "name": "conformance-miri: passed",
            "value": 459,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: failed",
            "value": 100,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: unsupported",
            "value": 41,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: timed out",
            "value": 9,
            "unit": "tests"
          },
          {
            "name": "conformance-miri: total time",
            "value": 104.9482,
            "unit": "s"
          }
        ]
      }
    ]
  }
}