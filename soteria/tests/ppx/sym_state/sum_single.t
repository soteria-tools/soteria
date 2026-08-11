Single-variant sum compiles without unused-match warnings
  $ ../test.sh sum_single.ml
  open Prelude
  
  type t = Only of Excl_int.t [@@deriving sym_state { symex = Symex }]
  
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
      match x with
      | Only v ->
          Format.fprintf fmt "@[<2>%s@ " "Only";
          Excl_int.pp fmt v;
          Format.fprintf fmt "@]"
  
    let _ = pp
    let show x = Format.asprintf "%a" pp x
    let _ = show
  
    type syn = Ser_Only of Excl_int.syn
  
    let pp_syn ft (s : syn) =
      match s with
      | Ser_Only v -> Fmt.pf ft "(@[<2>%s@ %a@])" "Ser_Only" Excl_int.pp_syn v
      | _ -> .
  
    let _ = pp_syn
    let show_syn s = Format.asprintf "%a" pp_syn s
    let _ = show_syn
    let empty = None
    let _ = empty
  
    let to_syn (st : t) : syn list =
      match st with Only x -> List.map (fun v -> Ser_Only v) (Excl_int.to_syn x)
  
    let _ = to_syn
  
    let ins_outs (syn : syn) =
      match syn with Ser_Only v -> Excl_int.ins_outs v | _ -> .
  
    let _ = ins_outs
    let lift_Only_fixes = List.map (fun v -> Ser_Only v)
    let _ = lift_Only_fixes
  
    let produce (syn : syn) (st : t option) : t option SM.Symex.Producer.t =
      let open SM.Symex.Producer in
      let open SM.Symex.Producer.Syntax in
      match syn with
      | Ser_Only v -> (
          let* inner =
            match st with None -> return None | Some (Only x) -> return (Some x)
          in
          let+ inner' = Excl_int.produce v inner in
          match inner' with None -> None | Some y -> Some (Only y))
      | _ -> .
  
    let _ = produce
  
    let consume (syn : syn) (st : t option) :
        (t option, syn list) SM.Symex.Consumer.t =
      let open SM.Symex.Consumer in
      let open SM.Symex.Consumer.Syntax in
      match syn with
      | Ser_Only v -> (
          let* inner =
            match st with None -> ok None | Some (Only x) -> ok (Some x)
          in
          let+ inner' =
            let+? fixes = Excl_int.consume v inner in
            lift_Only_fixes fixes
          in
          match inner' with None -> None | Some y -> Some (Only y))
      | _ -> .
  
    let _ = consume
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
