Context field usage
  $ ../test.sh context.ml
  open Prelude
  
  type t = {
    my_int : Excl_int.t option;
    my_super_int : Excl_int_in_int.t option;
        [@sym_state.context { field = my_int }]
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
      (match x.my_int with
      | None -> Format.pp_print_string fmt "empty"
      | Some v -> Excl_int.pp fmt v);
      Format.fprintf fmt "@]";
      Format.fprintf fmt ";@ ";
      Format.fprintf fmt "@[%s =@ " "my_super_int";
      (match x.my_super_int with
      | None -> Format.pp_print_string fmt "empty"
      | Some v -> Excl_int_in_int.pp fmt v);
      Format.fprintf fmt "@]";
      Format.fprintf fmt "@ }@]"
  
    let _ = pp
    let show x = Format.asprintf "%a" pp x
    let _ = show
  
    type syn =
      | Ser_my_int of Excl_int.syn
      | Ser_my_super_int of Excl_int_in_int.syn
  
    let pp_syn ft (s : syn) =
      match s with
      | Ser_my_int v -> Fmt.pf ft "(@[<2>%s@ %a@])" "Ser_my_int" Excl_int.pp_syn v
      | Ser_my_super_int v ->
          Fmt.pf ft "(@[<2>%s@ %a@])" "Ser_my_super_int" Excl_int_in_int.pp_syn v
      | _ -> .
  
    let _ = pp_syn
    let show_syn s = Format.asprintf "%a" pp_syn s
    let _ = show_syn
  
    let of_opt = function
      | None -> { my_int = None; my_super_int = None }
      | Some v -> v
  
    let _ = of_opt
  
    let to_opt = function
      | { my_int = None; my_super_int = None } -> None
      | t -> Some t
  
    let _ = to_opt
    let empty = None
    let _ = empty
  
    let to_syn (st : t) : syn list =
      List.map
        (fun v -> Ser_my_int v)
        (Option.fold ~none:[] ~some:Excl_int.to_syn st.my_int)
      @ List.map
          (fun v -> Ser_my_super_int v)
          (Option.fold ~none:[] ~some:Excl_int_in_int.to_syn st.my_super_int)
  
    let _ = to_syn
  
    let ins_outs (syn : syn) =
      match syn with
      | Ser_my_int v -> Excl_int.ins_outs v
      | Ser_my_super_int v -> Excl_int_in_int.ins_outs v
      | _ -> .
  
    let _ = ins_outs
    let lift_my_int_fixes = List.map (fun v -> Ser_my_int v)
    let _ = lift_my_int_fixes
    let lift_my_super_int_fixes = List.map (fun v -> Ser_my_super_int v)
    let _ = lift_my_super_int_fixes
  
    let with_my_int_sym f =
      let open SM.Syntax in
      let* st_opt = SM.get_state () in
      let st = of_opt st_opt in
      let { my_int; _ } = st in
      let*^ res, my_int = f my_int in
      let+ () = SM.set_state (to_opt { st with my_int }) in
      res
  
    let _ = with_my_int_sym
  
    let with_my_super_int_sym f =
      let open SM.Syntax in
      let* st_opt = SM.get_state () in
      let st = of_opt st_opt in
      let { my_super_int; my_int } = st in
      let*^ (res, my_super_int), my_int =
        Excl_int.SM.run_with_state ~state:my_int (f my_super_int)
      in
      let+ () = SM.set_state (to_opt { my_super_int; my_int }) in
      res
  
    let _ = with_my_super_int_sym
  
    let with_my_int f =
      SM.Result.map_missing lift_my_int_fixes (with_my_int_sym f)
  
    let _ = with_my_int
  
    let with_my_super_int f =
      SM.Result.map_missing lift_my_super_int_fixes (with_my_super_int_sym f)
  
    let _ = with_my_super_int
  
    let produce (syn : syn) (st : t option) : t option SM.Symex.Producer.t =
      let open SM.Symex.Producer.Syntax in
      let st = of_opt st in
      match syn with
      | Ser_my_int v ->
          let+ my_int = Excl_int.produce v st.my_int in
          to_opt { st with my_int }
      | Ser_my_super_int v ->
          let+ my_super_int, my_int =
            Excl_int.SM.Producer.run_with_state ~state:st.my_int
              (Excl_int_in_int.produce v st.my_super_int)
          in
          to_opt { my_super_int; my_int }
      | _ -> .
  
    let _ = produce
  
    let consume (syn : syn) (st : t option) :
        (t option, syn list) SM.Symex.Consumer.t =
      let open SM.Symex.Consumer.Syntax in
      let st = of_opt st in
      match syn with
      | Ser_my_int v ->
          let+ my_int =
            let+? fixes = Excl_int.consume v st.my_int in
            lift_my_int_fixes fixes
          in
          to_opt { st with my_int }
      | Ser_my_super_int v ->
          let+ my_super_int, my_int =
            let+? fixes =
              Excl_int.SM.Consumer.run_with_state ~state:st.my_int
                (Excl_int_in_int.consume v st.my_super_int)
            in
            lift_my_super_int_fixes fixes
          in
          to_opt { my_super_int; my_int }
      | _ -> .
  
    let _ = consume
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
