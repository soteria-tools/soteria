module Hint = Hashtbl.Make (Int)

module List_ex = struct
  let combine_opt l1 l2 =
    try Some (List.combine l1 l2) with Invalid_argument _ -> None

  let rec combine3 l1 l2 l3 =
    match (l1, l2, l3) with
    | a :: l1, b :: l2, c :: l3 -> (a, b, c) :: combine3 l1 l2 l3
    | [], [], [] -> []
    | _ -> raise (Invalid_argument "combine3")

  let combine3_opt l1 l2 l3 =
    try Some (combine3 l1 l2 l3) with Invalid_argument _ -> None

  (* An iteration over the cross-product of l with itself,
      excluding pairs (x, x) of the same element.
     For instance, [self_cross_product [1; 2; 3]] will iterate over [(1, 2), (1, 2), (2, 3)] *)
  let iter_self_cross_product l f =
    let rec aux = function
      | [] -> ()
      | x :: l ->
          List.iter (fun y -> f (x, y)) l;
          aux l
    in
    aux l
end

module Syntax = struct
  let ( << ) f g x = f (g x)
end
