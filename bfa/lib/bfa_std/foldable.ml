module type S = sig
  type 'a t

  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end
