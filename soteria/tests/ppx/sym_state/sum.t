Sum (variant) memory model
  $ ../test.sh sum.ml
  open Prelude
  
  type t = Cell of Excl_int.t | Map of Heap.t
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
      match x with
      | Cell v ->
          Format.fprintf fmt "@[<2>%s@ " "Cell";
          Excl_int.pp fmt v;
          Format.fprintf fmt "@]"
      | Map v ->
          Format.fprintf fmt "@[<2>%s@ " "Map";
          Heap.pp fmt v;
          Format.fprintf fmt "@]"
  
    let _ = pp
    let show x = Format.asprintf "%a" pp x
    let _ = show
  
    type syn = Ser_Cell of Excl_int.syn | Ser_Map of Heap.syn
  
    let pp_syn ft (s : syn) =
      match s with
      | Ser_Cell v -> Fmt.pf ft "(@[<2>%s@ %a@])" "Ser_Cell" Excl_int.pp_syn v
      | Ser_Map v -> Fmt.pf ft "(@[<2>%s@ %a@])" "Ser_Map" Heap.pp_syn v
      | _ -> .
  
    let _ = pp_syn
    let show_syn s = Format.asprintf "%a" pp_syn s
    let _ = show_syn
    let empty = None
    let _ = empty
  
    let to_syn (st : t) : syn list =
      match st with
      | Cell x -> List.map (fun v -> Ser_Cell v) (Excl_int.to_syn x)
      | Map x -> List.map (fun v -> Ser_Map v) (Heap.to_syn x)
  
    let _ = to_syn
  
    let ins_outs (syn : syn) =
      match syn with
      | Ser_Cell v -> Excl_int.ins_outs v
      | Ser_Map v -> Heap.ins_outs v
      | _ -> .
  
    let _ = ins_outs
    let lift_Cell_fixes = List.map (fun v -> Ser_Cell v)
    let _ = lift_Cell_fixes
    let lift_Map_fixes = List.map (fun v -> Ser_Map v)
    let _ = lift_Map_fixes
  
    let produce (syn : syn) (st : t option) : t option SM.Symex.Producer.t =
      let open SM.Symex.Producer in
      let open SM.Symex.Producer.Syntax in
      match syn with
      | Ser_Cell v -> (
          let* inner =
            match st with
            | None -> return None
            | Some (Cell x) -> return (Some x)
            | Some (Map _) -> vanish ()
          in
          let+ inner' = Excl_int.produce v inner in
          match inner' with None -> None | Some y -> Some (Cell y))
      | Ser_Map v -> (
          let* inner =
            match st with
            | None -> return None
            | Some (Map x) -> return (Some x)
            | Some (Cell _) -> vanish ()
          in
          let+ inner' = Heap.produce v inner in
          match inner' with None -> None | Some y -> Some (Map y))
      | _ -> .
  
    let _ = produce
  
    let consume (syn : syn) (st : t option) :
        (t option, syn list) SM.Symex.Consumer.t =
      let open SM.Symex.Consumer in
      let open SM.Symex.Consumer.Syntax in
      match syn with
      | Ser_Cell v -> (
          let* inner =
            match st with
            | None -> ok None
            | Some (Cell x) -> ok (Some x)
            | Some (Map _) -> lfail (SM.Symex.Value.of_bool false)
          in
          let+ inner' =
            let+? fixes = Excl_int.consume v inner in
            lift_Cell_fixes fixes
          in
          match inner' with None -> None | Some y -> Some (Cell y))
      | Ser_Map v -> (
          let* inner =
            match st with
            | None -> ok None
            | Some (Map x) -> ok (Some x)
            | Some (Cell _) -> lfail (SM.Symex.Value.of_bool false)
          in
          let+ inner' =
            let+? fixes = Heap.consume v inner in
            lift_Map_fixes fixes
          in
          match inner' with None -> None | Some y -> Some (Map y))
      | _ -> .
  
    let _ = consume
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
