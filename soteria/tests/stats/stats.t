Test printing stats to stdout (ensure this looks somewhat nice)
  $ ./print_stats.exe
  Statistics:
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
    "soteria.exec-time": "<float>",
    "soteria.give-up-reasons": { "give up reason": [ "RawJson", null ] },
    "soteria.miss-without-fix": [ "miss no fix", "other miss no fix" ],
    "soteria.sat-checks": 12,
    "soteria.sat-time": "<float>",
    "soteria.branches": 4
  }
  
  Serialization round-trip successful.
