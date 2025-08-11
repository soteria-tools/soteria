type 'a t = {
  mutable exec_time : float;
  give_up_reasons : string Dynarray.t;
  mutable branch_number : int;
  user : 'a;
}
[@@deriving yojson]

let create ~user () =
  {
    exec_time = 0.0;
    give_up_reasons = Dynarray.create ();
    branch_number = 1;
    user;
  }

let set_exec_time t time = t.exec_time <- time
let push_give_up_reason t reason = Dynarray.add_last t.give_up_reasons reason
