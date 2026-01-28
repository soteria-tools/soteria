Test printing stats to stdout (ensure this looks somewhat nice)
  $ ./print_stats.exe
  Statistics:
  • Execution time: <time>
  • Give up reasons: 
    - give up reason
  • Misses without fix: 
    - miss no fix
    - other miss no fix
  • SAT checks: 12
  • SAT solving time: <time>
  • Branches: 4
  
  
  {
    "soteria.exec-time": 0.0073359012603759766,
    "soteria.give-up-reasons": { "give up reason": [ "RawJson", null ] },
    "soteria.miss-without-fix": [ "miss no fix", "other miss no fix" ],
    "soteria.sat-checks": 12,
    "soteria.sat-time": 0.004536151885986328,
    "soteria.branches": 4
  }
  
  Serialization round-trip successful.
