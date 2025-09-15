open Ppxlib

module If_sat = struct
  module Branch_names = struct
    let then_name = "lname"
    let else_name = "rname"
    let branch_name = "name"

    let attribute_expr (attr : attribute) =
      match attr.attr_payload with
      | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> expr
      | _ ->
          Location.raise_errorf ~loc:attr.attr_name.loc
            "Invalid branch name attribute payload"
  end

  module Extension_name = struct
    type t = Sat | Sat1 | Sure

    let to_string = function Sat -> "sat" | Sat1 -> "sat1" | Sure -> "sure"
  end

  let get_attr ~name expr =
    List.find_opt (fun attr -> attr.attr_name.txt = name) expr.pexp_attributes

  let override_attr_opt ~name expr (new_attr : attribute option) =
    match new_attr with
    | None -> expr
    | Some new_attr -> (
        let existing_attr = get_attr ~name expr in
        match existing_attr with
        | Some _ ->
            Location.raise_errorf ~loc:new_attr.attr_name.loc
              "Branch name attribute specified multiple times"
        | None ->
            let new_attr =
              {
                new_attr with
                attr_name = { new_attr.attr_name with txt = name };
              }
            in
            { expr with pexp_attributes = new_attr :: expr.pexp_attributes })

  let associated_fn ~loc = function
    | Extension_name.Sat -> [%expr Symex_syntax.branch_on]
    | Sat1 -> [%expr Symex_syntax.branch_on_take_one]
    | Sure -> [%expr Symex_syntax.if_sure]

  let expand_if ~loc ~ext guard then_ else_ =
    let associated_fn = associated_fn ~loc ext in
    let lname =
      get_attr ~name:Branch_names.branch_name then_
      |> Option.fold ~some:Branch_names.attribute_expr
           ~none:[%expr Stdlib.String.cat "Left branch at " __LOC__]
    in
    let rname =
      get_attr ~name:Branch_names.branch_name else_
      |> Option.fold ~some:Branch_names.attribute_expr
           ~none:[%expr Stdlib.String.cat "Right branch at " __LOC__]
    in
    [%expr
      [%e associated_fn] [%e guard] ~left_branch_name:[%e lname]
        ~right_branch_name:[%e rname]
        ~then_:(fun () -> [%e then_])
        ~else_:(fun () -> [%e else_])]

  let expand ~ext expr =
    let loc = { expr.pexp_loc with loc_ghost = true } in
    let expansion =
      match expr with
      | [%expr if [%e? guard] then [%e? then_] else [%e? else_]] as whole_expr
        ->
          let then_ =
            get_attr ~name:Branch_names.then_name whole_expr
            |> override_attr_opt ~name:Branch_names.branch_name then_
          in
          let else_ =
            get_attr ~name:Branch_names.else_name whole_expr
            |> override_attr_opt ~name:Branch_names.branch_name else_
          in

          expand_if ~loc ~ext guard then_ else_
      | [%expr if [%e? _] then [%e? _]] ->
          Location.raise_errorf ~loc "'if%%%s' must include an else branch"
            (Extension_name.to_string ext)
      | _ -> Location.raise_errorf ~loc "%%sat can only be used with 'if'"
    in
    {
      expansion with
      pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes;
    }
end

module Sym_constants = struct
  let rewriter loc s =
    let i = int_of_string s in
    let ei = Ast_builder.Default.eint ~loc i in
    if i = 0 then [%expr Sym_int_syntax.zero ()]
    else if i = 1 then [%expr Sym_int_syntax.one ()]
    else [%expr Sym_int_syntax.mk_nonzero [%e ei]]

  let suffix = 's'
end
