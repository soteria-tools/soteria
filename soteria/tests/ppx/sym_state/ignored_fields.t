Ignored field usage
  $ ../test.sh ignored_fields.ml
  open Prelude
  
  type t = {
    my_int : Excl_int.t option;
        [@sym_state.ignore
          {
            empty = None;
            pp = Format.pp_print_option Excl_int.pp;
            is_empty = Option.is_none;
          }]
  }
  [@@deriving sym_state { symex = Symex }]
  
  include struct
    [@@@ocaml.warning "-60"]
  
    let _ = fun (_ : t) -> ()
  
    module SM =
      Soteria.Sym_states.State_monad.Make
        (Symex)
        (struct
          type nonrec t = t option
        end)
  
    let pp fmt x =
      Format.fprintf fmt "@[<2>{ ";
      Format.fprintf fmt "@[%s =@ " "my_int";
      (Format.pp_print_option Excl_int.pp) fmt x.my_int;
      Format.fprintf fmt "@]";
      Format.fprintf fmt "@ }@]"
  
    let _ = pp
    let show x = Format.asprintf "%a" pp x
    let _ = show
  
    type syn = |
  
    let pp_syn _ _ = ()
    let _ = pp_syn
    let show_syn s = Format.asprintf "%a" pp_syn s
    let _ = show_syn
    let of_opt = function None -> { my_int = None } | Some v -> v
    let _ = of_opt
  
    let to_opt = function
      | { my_int } when Option.is_none my_int -> None
      | t -> Some t
  
    let _ = to_opt
    let empty = None
    let _ = empty
    let to_syn (_ : t) : syn list = []
    let _ = to_syn
    let ins_outs (syn : syn) = match syn with _ -> .
    let _ = ins_outs
  
    let with_my_int_sym f =
      let open SM.Syntax in
      let* st_opt = SM.get_state () in
      let st = of_opt st_opt in
      let { my_int } = st in
      let**^ res, my_int = f my_int in
      let+ () = SM.set_state (to_opt { my_int }) in
      Soteria.Symex.Compo_res.Ok res
  
    let _ = with_my_int_sym
    let produce (syn : syn) () = match syn with _ -> .
    let _ = produce
    let consume (syn : syn) () = match syn with _ -> .
    let _ = consume
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
