window.BENCHMARK_DATA = {
  "lastUpdate": 1779700392953,
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
      }
    ]
  }
}