module type Base = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module type S = sig
  include Base

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module Extend (Base : Base) : S with type 'a t := 'a Base.t = struct
  include Base

  module Syntax = struct
    let ( let* ) = Base.bind
    let ( let+ ) = Base.map
  end
end

module type Base2 = sig
  type ('a, 'b) t

  val ok : 'a -> ('a, 'b) t
  val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val map : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  val error : 'b -> ('a, 'b) t
  val bind_error : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val map_error : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
end

module type S2 = sig
  include Base2

  module Syntax : sig
    val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
    val ( let/ ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
    val ( let- ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  end
end

module Extend2 (Base : Base2) : S2 with type ('a, 'b) t := ('a, 'b) Base.t =
struct
  include Base

  module Syntax = struct
    let ( let* ) = Base.bind
    let ( let+ ) = Base.map
    let ( let/ ) = Base.bind_error
    let ( let- ) = Base.map_error
  end
end

module Id = Extend (struct
  type 'a t = 'a

  let[@inline] return x = x
  let[@inline] bind x f = f x
  let[@inline] map x f = f x
end)
