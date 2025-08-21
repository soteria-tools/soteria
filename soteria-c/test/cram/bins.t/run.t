  $ ../bins/run_twice.exe
  Number of outcomes for process_one: 0
  Number of outcomes for process_two: 1
  
  
  Branches: {true, [(0 == V|1|); (0 == V|1|)]}
  Branches: {true, [(0 == V|1|); (0 == V|1|)]}

  $ ../bins/deep_give_up.exe
  In UX with Csymex.run: [(Ok: (V|1| + V|2|), [(V|2| < V|1|); (1 <= V|1|)]);
                          (Ok: (V|1| - V|2|), [(V|2| < V|1|); (V|1| <= 0)]);
                          (Error: okkk, [(V|1| <= V|2|); (V|1| != V|2|)])]
  
  In UX with Csymex.Result.run: [(Ok: (V|1| + V|2|),
                                  [(V|2| < V|1|); (1 <= V|1|)]);
                                 (Ok: (V|1| - V|2|),
                                  [(V|2| < V|1|); (V|1| <= 0)]);
                                 (Error: okkk,
                                  [(V|1| <= V|2|); (V|1| != V|2|)])]
  
  Trying to run in OX with Csymex.run, expecting to catch an exception -- Caught Gave_up in OX: x == y
  
  In UX with Csymex.Result.run: [(Ok: (V|1| + V|2|),
                                  [(V|2| < V|1|); (1 <= V|1|)]);
                                 (Ok: (V|1| - V|2|),
                                  [(V|2| < V|1|); (V|1| <= 0)]);
                                 (Error: Gave up: x == y,
                                  [(V|1| <= V|2|); (V|1| == V|2|)]);
                                 (Error: okkk,
                                  [(V|1| <= V|2|); (V|1| != V|2|)])]
  
