module type S = sig
  type 'a t

  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module List = struct
  type 'a t = 'a list

  let fold t ~init ~f = List.fold_left f init t
end

module Option = struct
  type 'a t = 'a option

  let fold t ~init ~f = match t with None -> init | Some x -> f init x
end

module Iter = struct
  type 'a t = 'a Iter.t

  let fold t ~init ~f = Iter.fold f init t
end

module Seq = struct
  type 'a t = 'a Seq.t

  let fold t ~init ~f = Seq.fold_left f init t
end
