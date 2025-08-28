module type S = sig
  type 'a t

  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module List : S with type 'a t = 'a list
module Option : S with type 'a t = 'a option
module Iter : S with type 'a t = 'a Iter.t
module Seq : S with type 'a t = 'a Seq.t
