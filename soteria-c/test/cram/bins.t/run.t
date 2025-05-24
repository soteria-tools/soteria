  $ ../bins/batch.exe
  (declare-datatype Ptr ((mk-ptr (loc Int) (ofs Int)))) ; -> success
  (declare-datatype Opt (par (P) ((mk-some (opt-unwrap P)) (none)))) ; -> success
  (push 1) ; -> success
  (pop 1) ; -> success
  (push 1) ; -> success
  (declare-fun |0| () Int) ; -> success
  (assert (<= 0 |0|)) ; -> success
  (assert (< |0| 10)) ; -> success
  (assert (< |0| 0)) ; -> success
  (check-sat) ; -> unsat

  $ ../bins/run_twice.exe
  Number of outcomes for process_one: 0
  Number of outcomes for process_two: 1
  
  
  Branches: {true, [(1 != V|0|); (0 == V|0|)]}
  Branches: {true, [(1 != V|0|); (0 == V|0|)]}
