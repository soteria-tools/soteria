module List : Monad.Syntax with type 'a t := 'a list

module Option : sig
  include Monad.Syntax with type 'a t := 'a option

  val ( let/ ) : 'a option -> (unit -> 'a option) -> 'a option
  val ( let- ) : 'a option -> (unit -> 'a) -> 'a
end

module Result : Monad.Syntax2 with type ('a, 'b) t := ('a, 'b) result

module FunctionWrap : sig
  val ( let@ ) : ('a -> 'b) -> 'a -> 'b
end
