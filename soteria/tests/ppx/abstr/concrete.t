  $ ../test.sh concrete.ml
  open Prelude
  
  type ('off, 'addr) abstr_raw = { off : 'off; addr : 'addr; exposed : bool }
  type t = (S_int.t, Typed.T.sint Typed.t) abstr_raw
  type syn = (S_int.syn, Typed.Expr.t) abstr_raw
  
  let pp fmt (x : t) =
    Format.fprintf fmt "@[<2>{ ";
    Format.fprintf fmt "@[%s =@ " "off";
    S_int.pp fmt x.off;
    Format.fprintf fmt "@]";
    Format.fprintf fmt ";@ ";
    Format.fprintf fmt "@[%s =@ " "addr";
    Typed.ppa fmt x.addr;
    Format.fprintf fmt "@]";
    Format.fprintf fmt ";@ ";
    Format.fprintf fmt "@[%s =@ " "exposed";
    Format.pp_print_bool fmt x.exposed;
    Format.fprintf fmt "@]";
    Format.fprintf fmt "@ }@]"
  
  let show x = Format.asprintf "%a" pp x
  
  let pp_syn fmt (x : syn) =
    Format.fprintf fmt "@[<2>{ ";
    Format.fprintf fmt "@[%s =@ " "off";
    S_int.pp_syn fmt x.off;
    Format.fprintf fmt "@]";
    Format.fprintf fmt ";@ ";
    Format.fprintf fmt "@[%s =@ " "addr";
    Typed.Expr.pp fmt x.addr;
    Format.fprintf fmt "@]";
    Format.fprintf fmt ";@ ";
    Format.fprintf fmt "@[%s =@ " "exposed";
    Format.pp_print_bool fmt x.exposed;
    Format.fprintf fmt "@]";
    Format.fprintf fmt "@ }@]"
  
  let show_syn x = Format.asprintf "%a" pp_syn x
  let fresh () = failwith "[@@deriving abstr]: no fresh for this type"
  
  let to_syn (x : t) : syn =
    {
      off = S_int.to_syn x.off;
      addr = Typed.Expr.of_value x.addr;
      exposed = x.exposed;
    }
  
  let subst sub (x : syn) : t =
    {
      off = S_int.subst sub x.off;
      addr = Typed.Expr.subst sub x.addr;
      exposed = x.exposed;
    }
  
  let learn_eq (s : syn) (st : t) =
    let open Symex.Consumer.Syntax in
    let* () = S_int.learn_eq s.off st.off in
    let* () = Symex.Consumer.learn_eq s.addr st.addr in
    let* () =
      if s.exposed = st.exposed then Symex.Consumer.ok ()
      else Symex.Consumer.lfail (Typed.of_bool false)
    in
    Symex.Consumer.ok ()
  
  let exprs_syn (s : syn) = S_int.exprs_syn s.off @ [ s.addr ]
  Success ✅
