window.BENCHMARK_DATA = {
  "lastUpdate": 1780070677467,
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
      }
    ]
  }
}