window.BENCHMARK_DATA = {
  "lastUpdate": 1779704893782,
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
      }
    ]
  }
}