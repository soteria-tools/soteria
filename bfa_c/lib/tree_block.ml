open Csymex.Syntax
open Typed
open Typed.Infix
open Typed.Syntax
open Csymex
module Ctype = Cerb_frontend.Ctype

module MemVal = struct
  type t = { value : T.cval Typed.t; ty : Ctype.ctype [@printer Fmt_ail.pp_ty] }
  [@@deriving make, show { with_path = false }]

  let any_of_type (Ctype.Ctype (_, kty) as ty) =
    match kty with
    | Basic (Integer int_ty) ->
        let* constrs =
          Csymex.of_opt_not_impl ~msg:"Int constraints"
            (Layout.int_constraints int_ty)
        in
        let+ value = Typed.nondet ~constrs Typed.t_int in
        { value :> T.cval Typed.t; ty }
    | _ -> Fmt.kstr not_impl "Nondet of type %a" Fmt_ail.pp_ty ty
end

module MUMemVal = struct
  (* The svalue must be of type Int option, where the int is a valid representative of a type *)
  type t = {
    value : T.cval T.sopt Typed.t;
    ty : Ctype.ctype; [@printer Fmt_ail.pp_ty]
  }
  [@@deriving make, show { with_path = false }]

  let unwrap_or ~err v =
    (* What I would like to be writing:
       match%sat v with
       | Some v -> Result.ok v
       | _ -> Result.error err
    *)
    if%sat Typed.SOption.is_some v then Result.ok (Typed.SOption.unwrap v)
    else Result.error err
end

module Node = struct
  type qty = Partially | Totally [@@deriving show { with_path = false }]

  let merge_qty left right =
    match (left, right) with Totally, Totally -> Totally | _, _ -> Partially

  type mem_val =
    | Zeros
    | Uninit of qty
    | Lazy
    | Init of MemVal.t
    | MaybeUninit of MUMemVal.t
  [@@deriving show { with_path = false }]

  (* Later we could add arbitrary annotations to the mem_val variant.
     For example, we can keep track of read-only or tainted bytes.
     i.e. MemVal { v: mem_val; annot: 'a }.
     Quite probably, Node module can be parametrized by a module
     capturing the annotations behavior in analysis. *)
  type t = NotOwned of qty | Owned of mem_val
  [@@deriving show { with_path = false }]

  let is_fully_owned = function NotOwned _ -> false | Owned _ -> true

  let merge ~left ~right =
    match (left, right) with
    | NotOwned Totally, NotOwned Totally -> return (NotOwned Totally)
    | NotOwned _, _ | _, NotOwned _ -> return (NotOwned Partially)
    | Owned Zeros, Owned Zeros -> return (Owned Zeros)
    | Owned (Uninit lq), Owned (Uninit rq) ->
        let qty = merge_qty lq rq in
        return (Owned (Uninit qty))
    | Owned _, Owned _ -> return (Owned Lazy)

  let split ~range:(_, _) ~at:_ node =
    match node with
    | NotOwned Totally -> return (NotOwned Totally, NotOwned Totally)
    | Owned (Uninit Totally) ->
        return (Owned (Uninit Totally), Owned (Uninit Totally))
    | Owned Zeros -> return (Owned Zeros, Owned Zeros)
    | Owned (Init _ | MaybeUninit _) -> Fmt.kstr not_impl "Splitting %a" pp node
    | NotOwned Partially | Owned (Uninit Partially) | Owned Lazy ->
        failwith "Should never split an intermediate node"

  let uninit = Owned (Uninit Totally)

  let decode ~ty t : ([> T.sint ] Typed.t, 'err) Csymex.Result.t =
    match t with
    | NotOwned _ -> Result.error `MissingResource
    | Owned (Uninit _) -> Result.error `UninitializedMemoryAccess
    | Owned Zeros ->
        if Layout.is_int ty then Result.ok 0s
        else Fmt.kstr not_impl "Float zeros"
    | Owned Lazy ->
        Fmt.kstr not_impl "Lazy memory access, cannot decode %a" pp t
    | Owned (Init { value; ty = tyw }) ->
        if Ctype.ctypeEqual ty tyw then Result.ok value
        else
          Fmt.kstr not_impl "Type mismatch when decoding value: %a vs %a"
            Fmt_ail.pp_ty ty Fmt_ail.pp_ty tyw
    | Owned (MaybeUninit { value; ty = tyw }) ->
        if Ctype.ctypeEqual ty tyw then
          MUMemVal.unwrap_or ~err:`UninitializedMemoryAccess value
        else
          Fmt.kstr not_impl
            "Type mismatch when decoding maybe_uninit value: %a vs %a"
            Fmt_ail.pp_ty ty Fmt_ail.pp_ty tyw
end

module Tree = struct
  type t = { node : Node.t; range : Range.t; children : (t * t) option }
  [@@deriving show { with_path = false }, make]

  let is_empty { node; _ } =
    match node with NotOwned Totally -> true | _ -> false

  let uninit range = make ~node:Node.uninit ~range ?children:None ()
  let zeros range = make ~node:(Node.Owned Node.Zeros) ~range ?children:None ()
  let not_owned range = make ~node:(NotOwned Totally) ~range ?children:None ()

  let sval_leaf ~range ~value ~ty =
    make ~node:(Owned (Init { value; ty })) ~range ?children:None ()

  let of_children_s ~left ~right =
    let range = (fst left.range, snd right.range) in
    let+ node = Node.merge ~left:left.node ~right:right.node in
    let children =
      match node with
      | Node.NotOwned Totally | Owned (Zeros | Uninit Totally) -> None
      | _ -> Some (left, right)
    in
    { range; children; node }

  let of_children _ ~left ~right = of_children_s ~left ~right

  let with_children t ~left ~right =
    return { t with children = Some (left, right) }

  let rec split ~range t : (Node.t * t * t) Csymex.t =
    (* this function splits a tree and returns the node in the given range *)
    (* We're assuming that range is inside old_span *)
    let old_span = t.range in
    let ol, oh = old_span in
    let nl, nh = range in
    if%sat ol #== nl then
      let at = nh in
      let+ left_node, right_node = Node.split ~range:old_span ~at t.node in
      let left_span, right_span = Range.split_at old_span at in
      let left = make ~node:left_node ~range:left_span () in
      let right = make ~node:right_node ~range:right_span () in
      (left_node, left, right)
    else
      if%sat oh #== nh then
        let+ left_node, right_node = Node.split ~range:old_span ~at:nl t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~range:left_span () in
        let right = make ~node:right_node ~range:right_span () in
        (right_node, left, right)
      else
        (* We're first splitting on the left then splitting again on the right *)
        let* left_node, right_node = Node.split ~range:old_span ~at:nl t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~range:left_span () in
        let full_right = make ~node:right_node ~range:right_span () in
        let* node, right_left, right_right = split ~range full_right in
        let+ right =
          with_children full_right ~left:right_left ~right:right_right
        in
        (node, left, right)

  let extend_if_needed t range =
    let rl, rh = range in
    let sl, sh = t.range in
    let* t_with_left =
      if%sat rl #< sl then
        let new_left_tree = make ~node:(NotOwned Totally) ~range:(rl, sl) () in
        let children = (new_left_tree, t) in
        let qty = if is_empty t then Node.Totally else Partially in
        return (make ~node:(NotOwned qty) ~range:(rl, sh) ~children ())
      else return t
    in
    let sl, _ = t_with_left.range in
    let* result =
      if%sat rh #> sh then
        let new_right_tree = make ~node:(NotOwned Totally) ~range:(sh, rh) () in
        let children = (t_with_left, new_right_tree) in
        let qty = if is_empty t_with_left then Node.Totally else Partially in
        return (make ~node:(NotOwned qty) ~range:(sl, rh) ~children ())
      else return t_with_left
    in
    return result

  let rec extract (t : t) (range : Range.t) : (t * t option) Csymex.t =
    (* First result is the extracted tree, second is the remain *)
    if%sat Range.sem_eq range t.range then return (t, None)
    else
      let left, right = Option.get t.children in
      if%sat Range.is_inside range left.range then
        let* extracted, new_left = extract left range in
        let+ new_self =
          match new_left with
          | Some left -> of_children_s ~right ~left
          | None -> return right
        in
        (extracted, Some new_self)
      else
        let* extracted, new_right = extract right range in
        let+ new_self =
          match new_right with
          | Some right -> of_children_s ~right ~left
          | None -> return left
        in
        (extracted, Some new_self)

  let rec add_to_the_right t addition : t Csymex.t =
    match t.children with
    | None -> of_children_s ~left:t ~right:addition
    | Some (left, right) ->
        let* new_right = add_to_the_right right addition in
        of_children_s ~left ~right:new_right

  let rec add_to_the_left t addition : t Csymex.t =
    match t.children with
    | None -> of_children_s ~left:addition ~right:t
    | Some (left, right) ->
        let* new_left = add_to_the_left left addition in
        of_children_s ~left:new_left ~right

  let frame_range (t : t) ~(replace_node : t -> t) ~rebuild_parent
      (range : Range.t) : (t * t) Csymex.t =
    let rec frame_inside ~(replace_node : t -> t) ~rebuild_parent (t : t)
        (range : Range.t) : (t * t) Csymex.t =
      if%sat Range.sem_eq range t.range then
        let new_tree = replace_node t in
        return (t, new_tree)
      else
        match t.children with
        | Some (left, right) ->
            let _, mid = left.range in
            if%sat Range.strictly_inside mid range then
              let l, h = range in
              let upper_range = (mid, h) in
              let dont_replace_node t = t in
              if%sat
                (* High-range already good *)
                Range.sem_eq upper_range right.range
              then
                (* Rearrange left*)
                let lower_range = (l, mid) in
                let* _, left =
                  frame_inside ~replace_node:dont_replace_node
                    ~rebuild_parent:with_children left lower_range
                in
                let* extracted, left_opt = extract left lower_range in
                let* right = add_to_the_left right extracted in
                let* new_self =
                  of_children_s ~left:(Option.get left_opt) ~right
                in
                frame_inside ~replace_node ~rebuild_parent new_self range
              else
                let* _, right =
                  frame_inside ~replace_node:dont_replace_node
                    ~rebuild_parent:with_children right upper_range
                in
                let* extracted, right_opt = extract right upper_range in
                let* left = add_to_the_right left extracted in
                let* new_self =
                  of_children_s ~left ~right:(Option.get right_opt)
                in
                frame_inside ~replace_node ~rebuild_parent new_self range
            else
              if%sat Range.is_inside range left.range then
                let* node, left =
                  frame_inside ~replace_node ~rebuild_parent left range
                in
                let+ new_parent = rebuild_parent t ~left ~right in
                (node, new_parent)
              else
                (* Range is necessarily inside of right *)
                let* node, right =
                  frame_inside ~replace_node ~rebuild_parent right range
                in
                let+ new_parent = rebuild_parent t ~left ~right in
                (node, new_parent)
        | None ->
            let* _, left, right = split ~range t in
            let* new_self = with_children t ~left ~right in
            frame_inside ~replace_node ~rebuild_parent new_self range
    in
    let* root = extend_if_needed t range in
    frame_inside ~replace_node ~rebuild_parent root range

  let load (ofs : [< T.sint ] Typed.t) (ty : Ctype.ctype) (t : t) :
      (T.cval Typed.t * t, 'err) Result.t =
    let* range = Range.of_low_and_type ofs ty in
    let replace_node node = node in
    let rebuild_parent = with_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ sval = Node.decode ~ty framed.node in
    ((sval :> T.cval Typed.t), tree)

  let store (low : [< T.sint ] Typed.t) (ty : Ctype.ctype)
      (sval : [< T.cval ] Typed.t) (t : t) : (unit * t, 'err) Result.t =
    let* range = Range.of_low_and_type low ty in
    let replace_node _ = sval_leaf ~range ~value:sval ~ty in
    let rebuild_parent = of_children in
    let* node, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ () =
      match node.node with
      | NotOwned Totally -> Result.error `MissingResource
      | NotOwned Partially -> Result.error `MissingResource
      | _ -> Result.ok ()
    in
    ((), tree)
end

type t = { root : Tree.t; bound : T.sint Typed.t option }
[@@deriving show { with_path = false }]

let with_bound_check (t : t) (ofs : [< T.sint ] Typed.t) f =
  let** () =
    match t.bound with
    | None -> Result.ok ()
    | Some bound ->
        if%sat ofs #> bound then (
          L.debug (fun m ->
              m "Out of bounds access: %a > %a" Typed.ppa ofs Typed.ppa bound);
          Result.error `OutOfBounds)
        else Result.ok ()
  in
  let++ res, root = f () in
  (res, { t with root })

let is_exclusively_owned t =
  match t.bound with
  | None -> return Typed.v_false
  | Some bound ->
      let Tree.{ range = low, high; node; _ } = t.root in
      if Node.is_fully_owned node then return low #== 0s #&& (high #== bound)
      else return Typed.v_false

let load (ofs : [< T.sint ] Typed.t) (ty : Ctype.ctype) (t : t) :
    ([> T.sint ] Typed.t * t, 'err) Result.t =
  let* size = Layout.size_of_s ty in
  with_bound_check t ofs #+ size @@ fun () -> Tree.load ofs ty t.root

let store ofs ty sval t =
  let* size = Layout.size_of_s ty in
  with_bound_check t ofs #+ size @@ fun () -> Tree.store ofs ty sval t.root

let alloc size = { root = Tree.uninit (0s, size); bound = Some size }
