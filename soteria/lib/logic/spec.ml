module M (Symex : Symex.S) = struct
  open Symex

  type ('asrt, 'err) t = {
    params : Value.Syn.t list;
    pre : 'asrt;
    post : 'asrt;
    ret : (Value.Syn.t, 'err) Stdlib.Result.t;
  }

  (** Executes a specification given an initial substitution. *)
  let execute
      ~(consume :
         'asrt -> 'state option -> ('state option, 'fix list) Consumer.t)
      ~(produce : 'asrt -> 'state option -> 'state option Producer.t)
      (spec : ('asrt, 'err) t) (args : 'a Value.t list) (state : 'state option)
      :
      ( 'b Value.t * 'state option,
        ('err, cons_fail) Either.t,
        'fix list )
      Result.t =
    let open Symex.Syntax in
    let** frame, subst =
      let consumer =
        let open Consumer.Syntax in
        let mappings = List.combine spec.params args in
        let* () =
          Consumer.fold_list ~init:()
            ~f:(fun () (param, arg) -> Consumer.learn_eq param arg)
            mappings
        in
        consume spec.pre state
      in
      let result =
        Consumer.run_consumer ~subst:Value.Syn.Subst.empty consumer
      in
      Result.map_error result Either.right
    in
    let* state, subst =
      Producer.run_producer ~subst (produce spec.post frame)
    in
    let** ret =
      match spec.ret with
      | Ok e ->
          let subst_val = Producer.apply_subst Fun.id e in
          let* v, _subst = Producer.run_producer ~subst subst_val in
          Result.ok v
      | Error err -> Result.error (Either.left err)
    in
    Result.ok (ret, state)
end
