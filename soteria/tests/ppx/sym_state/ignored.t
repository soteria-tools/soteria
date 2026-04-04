Ignored field usage
  $ ../test.sh ignored.ml
  open Prelude
  
  type t = { heap : Heap.t option; steps : int [@sym_state.ignore { empty = 0 }] }
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
      Format.fprintf fmt "@[%s =@ " "heap";
      (match x.heap with
      | None -> Format.pp_print_string fmt "empty"
      | Some v -> Heap.pp fmt v);
      Format.fprintf fmt "@]";
      Format.fprintf fmt ";@ ";
      Format.fprintf fmt "@[%s =@ <ignored>@]" "steps";
      Format.fprintf fmt "@ }@]"
  
    let _ = pp
    let show x = Format.asprintf "%a" pp x
    let _ = show
  
    type syn = Ser_heap of Heap.syn
  
    let pp_syn ft (s : syn) =
      match s with
      | Ser_heap v -> Fmt.pf ft "(@[<2>%s@ %a@])" "Ser_heap" Heap.pp_syn v
      | _ -> .
  
    let _ = pp_syn
    let show_syn s = Format.asprintf "%a" pp_syn s
    let _ = show_syn
    let of_opt = function None -> { heap = None; steps = 0 } | Some v -> v
    let _ = of_opt
  
    let to_opt = function
      | { heap = None; steps } when steps = 0 -> None
      | t -> Some t
  
    let _ = to_opt
    let empty = None
    let _ = empty
  
    let to_syn (st : t) : syn list =
      List.map
        (fun v -> Ser_heap v)
        (Option.fold ~none:[] ~some:Heap.to_syn st.heap)
  
    let _ = to_syn
  
    let ins_outs (syn : syn) =
      match syn with Ser_heap v -> Heap.ins_outs v | _ -> .
  
    let _ = ins_outs
    let lift_heap_fixes = List.map (fun v -> Ser_heap v)
    let _ = lift_heap_fixes
  
    let with_heap_sym f =
      let open SM.Syntax in
      let* st_opt = SM.get_state () in
      let st = of_opt st_opt in
      let { heap; _ } = st in
      let*^ res, heap = f heap in
      let+ () = SM.set_state (to_opt { st with heap }) in
      res
  
    let _ = with_heap_sym
  
    let with_steps_sym f =
      let open SM.Syntax in
      let* st_opt = SM.get_state () in
      let st = of_opt st_opt in
      let { steps; _ } = st in
      let**^ res, steps = f steps in
      let+ () = SM.set_state (to_opt { st with steps }) in
      Soteria.Symex.Compo_res.Ok res
  
    let _ = with_steps_sym
    let with_heap f = SM.Result.map_missing (with_heap_sym f) lift_heap_fixes
    let _ = with_heap
  
    let produce (syn : syn) (st : t option) : t option SM.Symex.Producer.t =
      let open SM.Symex.Producer.Syntax in
      let st = of_opt st in
      match syn with
      | Ser_heap v ->
          let+ heap = Heap.produce v st.heap in
          to_opt { st with heap }
      | _ -> .
  
    let _ = produce
  
    let consume (syn : syn) (st : t option) :
        (t option, syn list) SM.Symex.Consumer.t =
      let open SM.Symex.Consumer.Syntax in
      let st = of_opt st in
      match syn with
      | Ser_heap v ->
          let+ heap =
            let+? fixes = Heap.consume v st.heap in
            lift_heap_fixes fixes
          in
          to_opt { st with heap }
      | _ -> .
  
    let _ = consume
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
