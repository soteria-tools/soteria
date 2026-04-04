Reject invalid type name
  $ ../test.sh invalid_non_t.ml
  File "invalid_non_t.ml", line 3, characters 5-10:
  3 | type state = { heap : Heap.t option } [@@deriving sym_state { symex = Symex }]
           ^^^^^
  Error: [@deriving sym_state] only supports type named 't'
  [1]
