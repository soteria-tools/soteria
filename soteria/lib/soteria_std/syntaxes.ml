module List = Monad.ListM.Syntax

module Option = struct
  include Monad.OptionM.Syntax

  let ( let/ ) x f = match x with Some _ as x -> x | None -> f ()
  let ( let- ) x f = match x with Some x -> x | None -> f ()
end

module Result = Monad.ResultM.Syntax

module FunctionWrap = struct
  let ( let@ ) = Stdlib.( @@ )
  let ( let@@ ) f x = f (x ())
end
