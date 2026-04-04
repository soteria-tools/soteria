module Syntaxes = struct
  let ( let@ ) = ( @@ )
end

module LocCtx = struct
  open Ppxlib
  open Ast_builder.Default

  type _ Effect.t += Get_loc : location Effect.t

  let with_loc (loc : location) f =
    let open Effect.Deep in
    try f () with effect Get_loc, k -> continue k loc

  let get_loc () = Effect.perform Get_loc
  let wloc x = { loc = get_loc (); txt = x }

  (* Override anything we need *)
  let constructor_declaration ~name ~args =
    constructor_declaration ~loc:(get_loc ()) ~name:(wloc name) ~args

  let estring x = estring ~loc:(get_loc ()) x
  let evar x = evar ~loc:(get_loc ()) x
  let pexp_construct x y = pexp_construct ~loc:(get_loc ()) x y
  let pexp_field x y = pexp_field ~loc:(get_loc ()) x y
  let pexp_ident x = pexp_ident ~loc:(get_loc ()) x
  let pexp_match x y = pexp_match ~loc:(get_loc ()) x y
  let pexp_record x y = pexp_record ~loc:(get_loc ()) x y
  let pmod_ident x = pmod_ident ~loc:(get_loc ()) x
  let ppat_any () = ppat_any ~loc:(get_loc ())
  let ppat_construct x y = ppat_construct ~loc:(get_loc ()) x y
  let ppat_record x y = ppat_record ~loc:(get_loc ()) x y

  let ppat_tuple pats =
    match pats with
    | [] -> ppat_any ()
    | [ pat ] -> pat
    | _ -> ppat_tuple ~loc:(get_loc ()) pats

  let ppat_var x = ppat_var ~loc:(get_loc ()) x
  let pstr_type x y = pstr_type ~loc:(get_loc ()) x y
  let ptyp_constr x y = ptyp_constr ~loc:(get_loc ()) x y
  let pvar x = pvar ~loc:(get_loc ()) x

  let type_declaration ~name ~params ~cstrs ~kind ~private_ ~manifest =
    type_declaration ~loc:(get_loc ()) ~name:(wloc name) ~params ~cstrs ~kind
      ~private_ ~manifest
end
