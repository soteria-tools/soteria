  $ bfa-c gen-summary load.c "f"
  bfa-c: [DEBUG] SMT:SEND: (set-option :print-success true)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (set-option :produce-models true)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (declare-datatype Ptr ((mk-ptr (loc Int) (ofs Int))))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (declare-datatype Opt (par (P) ((mk-some (opt-unwrap P)) (none))))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (push 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] Resetting solver: 0
  bfa-c: [DEBUG] SMT:SEND: (declare-fun |0| () Int)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (declare-fun |1| () Int)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [INFO] Executing function f_1
  bfa-c: [DEBUG] SMT:SEND: (declare-fun |2| () Int)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert true)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (not (= |2| 0)))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] About to execute action: store at &(V|2|, 0) (load.c:1:1-4:2 (cursor: 1:5 - 1:6))
                 HEAP:
                   {V|2| ->
                      ┌───────────┬──────┐
                      │[0, 8[     │[8; ∞[│
                      ├───────────┼──────┤
                      │Uninit Tot.│OOB   │
                      └───────────┴──────┘}
  bfa-c: [DEBUG] SMT:SEND: (push 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (= |2| 0))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (check-sat)
  bfa-c: [DEBUG] SMT:RECV: unsat
  bfa-c: [DEBUG] SMT:SEND: (pop 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert true)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] Executing statement: {
    return rvalue(*rvalue(x));
  }
  bfa-c: [DEBUG] Executing statement: return rvalue(*rvalue(x));
  bfa-c: [DEBUG] About to execute action: load at &(V|2|, 0) (load.c:3:11-12)
                 HEAP:
                   {V|2| ->
                      ┌───────────────────────────┬──────┐
                      │[0, 8[                     │[8; ∞[│
                      ├───────────────────────────┼──────┤
                      │&(V|0|, V|1|) : signed int*│OOB   │
                      └───────────────────────────┴──────┘}
  bfa-c: [DEBUG] SMT:SEND: (push 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (= |2| 0))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (check-sat)
  bfa-c: [DEBUG] SMT:RECV: unsat
  bfa-c: [DEBUG] SMT:SEND: (pop 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert true)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] About to execute action: load at &(V|0|, V|1|) (load.c:3:10-12 (cursor: 3:10))
                 HEAP:
                   {V|2| ->
                      ┌───────────────────────────┬──────┐
                      │[0, 8[                     │[8; ∞[│
                      ├───────────────────────────┼──────┤
                      │&(V|0|, V|1|) : signed int*│OOB   │
                      └───────────────────────────┴──────┘}
  bfa-c: [DEBUG] SMT:SEND: (push 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (= |0| 0))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (check-sat)
  bfa-c: [DEBUG] SMT:RECV: sat
  bfa-c: [DEBUG] SMT:SEND: (pop 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (not (= |0| 0)))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (check-sat)
  bfa-c: [DEBUG] SMT:RECV: sat
  bfa-c: [DEBUG] SMT:SEND: (declare-fun |3| () Int)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (<= (- 2147483648) |3|))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (<= |3| 2147483647))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] About to execute action: load at &(V|0|, V|1|) (unknown location)
                 HEAP:
                   {V|0| ->
                      ┌─────────────────────┐
                      │[V|1|, (V|1| Plus 4)[│
                      ├─────────────────────┤
                      │V|3| : signed int    │
                      └─────────────────────┘;
                    V|2| ->
                      ┌───────────────────────────┬──────┐
                      │[0, 8[                     │[8; ∞[│
                      ├───────────────────────────┼──────┤
                      │&(V|0|, V|1|) : signed int*│OOB   │
                      └───────────────────────────┴──────┘}
  bfa-c: [DEBUG] SMT:SEND: (push 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (= |0| 0))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (check-sat)
  bfa-c: [DEBUG] SMT:RECV: unsat
  bfa-c: [DEBUG] SMT:SEND: (pop 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert true)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (push 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (< |1| |1|))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (check-sat)
  bfa-c: [DEBUG] SMT:RECV: unsat
  bfa-c: [DEBUG] SMT:SEND: (pop 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (not (< |1| |1|)))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (push 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (> (+ |1| 4) (+ |1| 4)))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (check-sat)
  bfa-c: [DEBUG] SMT:RECV: unsat
  bfa-c: [DEBUG] SMT:SEND: (pop 1)
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [DEBUG] SMT:SEND: (assert (not (> (+ |1| 4) (+ |1| 4))))
  bfa-c: [DEBUG] SMT:RECV: success
  bfa-c: [INFO] Returning: V|3|
  { args = [&(V|0|, V|1|)]; pre = []; pc = [(V|0| Eq 0); Not((V|2| Eq 0))];
    post =
    [(V|2|,
      [TypedVal {offset = 0; ty = signed int*; v = &(V|0|, V|1|)}; (Bound 8)])];
    ret = (Error NullDereference at load.c:3:10-12 (cursor: 3:10)) }
  { args = [&(V|0|, V|1|)];
    pre = [[(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])]];
    pc =
    [Not(((V|1| Plus 4) Gt (V|1| Plus 4))); Not((V|1| Lt V|1|));
      (V|3| Leq 2147483647); (-2147483648 Leq V|3|); Not((V|0| Eq 0));
      Not((V|2| Eq 0))];
    post =
    [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}]);
     (V|2|, Freed)];
    ret = (Ok V|3|) }
