  $ ./run_twice.exe
  Number of outcomes for process_one: 0
  Number of outcomes for process_two: 1
  
  
  Branches: {true, [(0 == V|1|); (0 == V|1|)]}
  Branches: {true, [(0 == V|1|); (0 == V|1|)]}

  $ ./deep_give_up.exe
  Csymex.run ~mode:UX complex_process:
    [(Ok: (V|1| + V|2|), [(V|2| < V|1|); (1 <= V|1|)]);
     (Ok: (V|1| - V|2|), [(V|2| < V|1|); (V|1| <= 0)]);
     (Error: okkk, [(V|1| <= V|2|); (V|1| != V|2|)])]
  
  Csymex.Result.run ~mode:UX complex_process:
    [(Ok: (V|1| + V|2|), [(V|2| < V|1|); (1 <= V|1|)]);
     (Ok: (V|1| - V|2|), [(V|2| < V|1|); (V|1| <= 0)]);
     (Error: okkk, [(V|1| <= V|2|); (V|1| != V|2|)])]
  
  Csymex.run ~mode:OX complex_process;; EXPECTING EXCEPTION -- Caught Gave_up in OX: x == y
  
  Csymex.Result.run ~mode:OX complex_process:
    [(Ok: (V|1| + V|2|), [(V|2| < V|1|); (1 <= V|1|)]);
     (Ok: (V|1| - V|2|), [(V|2| < V|1|); (V|1| <= 0)]);
     (Error: Gave up: x == y, [(V|1| <= V|2|); (V|1| == V|2|)]);
     (Error: okkk, [(V|1| <= V|2|); (V|1| != V|2|)])]
  
This test is considered successful if the out of each process is a single branch containing 42.
  $ ./if_sure.exe
  if_true: [42]
  if_false: [42]
  if_maybe: [42]
  if_guranteed: [42]

Absolute value in a simple language, should return two cases: positive and strictly negative, both return true.
  $ ./simple_lang.exe
  Results: [(Ok: true, [(0 <= V|1|)]); (Ok: true, [(V|1| <= -1)])]
