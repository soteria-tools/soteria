Test printing stats to stdout (ensure this looks somewhat nice)
  $ ./print_stats.exe
  Statistics:
  • Z3 check-sat calls: 8
  • branch_on: branches 100% of calls (4 of 4)
  • Execution time: <time>
  • Give up reasons: 
    - give up reason
  • Misses without fix: 
    - miss no fix
    - other miss no fix
  • SAT checks: 12 (0 unknowns)
  • SAT solving time: <time> (<%>)
  • Branches: 4 (0 unexplored)
  
  
  {
    "solvers.z3.check_sats": 8,
    "soteria.branch-on-calls": 4,
    "soteria.exec-time": "<float>",
    "soteria.give-up-reasons": [ "give up reason" ],
    "soteria.branch-on-branched": 4,
    "soteria.miss-without-fix": [ "miss no fix", "other miss no fix" ],
    "soteria.sat-checks": 12,
    "soteria.sat-time": "<float>",
    "soteria.branches": 4
  }
  
  Serialization round-trip successful.
