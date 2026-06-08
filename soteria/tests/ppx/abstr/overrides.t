  $ ../test.sh overrides.ml
  open Prelude
  
  type ('a, 'b) abstr_raw = { a : 'a; b : 'b }
  type t = (S_int.t, Typed.T.sint Typed.t) abstr_raw
  type syn = (S_int.syn, Typed.Expr.t) abstr_raw
  
  let pp fmt (x : t) =
    Format.fprintf fmt "@[<2>{ ";
    Format.fprintf fmt "@[%s =@ " "a";
    S_int.pp fmt x.a;
    Format.fprintf fmt "@]";
    Format.fprintf fmt ";@ ";
    Format.fprintf fmt "@[%s =@ " "b";
    Typed.ppa fmt x.b;
    Format.fprintf fmt "@]";
    Format.fprintf fmt "@ }@]"
  
  let show x = Format.asprintf "%a" pp x
  
  let pp_syn fmt (x : syn) =
    Format.fprintf fmt "@[<2>{ ";
    Format.fprintf fmt "@[%s =@ " "a";
    S_int.pp_syn fmt x.a;
    Format.fprintf fmt "@]";
    Format.fprintf fmt ";@ ";
    Format.fprintf fmt "@[%s =@ " "b";
    Typed.Expr.pp fmt x.b;
    Format.fprintf fmt "@]";
    Format.fprintf fmt "@ }@]"
  
  let show_syn x = Format.asprintf "%a" pp_syn x
  
  let fresh () =
    let open Symex.Syntax in
    let* a = S_int.fresh () in
    let* b = Symex.nondet Typed.t_int in
    Symex.return { a; b }
  
  let to_syn (x : t) : syn = { a = S_int.to_syn x.a; b = Typed.Expr.of_value x.b }
  
  let subst sub (x : syn) : t =
    { a = S_int.subst sub x.a; b = Typed.Expr.subst sub x.b }
  
  let learn_eq (s : syn) (st : t) =
    let open Symex.Consumer.Syntax in
    let* () = S_int.learn_eq s.a st.a in
    let* () = Symex.Consumer.learn_eq s.b st.b in
    Symex.Consumer.ok ()
  
  let exprs_syn (s : syn) = S_int.exprs_syn s.a @ (fun _ -> []) s.b
  Success ✅
