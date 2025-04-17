module List = Monad.ListM.Syntax

module Option = struct
  include Monad.OptionM.Syntax

  let ( let/ ) x f = match x with Some _ as x -> x | None -> f ()
end

module Result = Monad.ResultM.Syntax
