  $ ../test.sh ignored.ml
  open Prelude
  
  module Struct = struct
    type t = { foo : Foo.t; [@reversible.ignore] bar : Bar.t }
    [@@deriving reversible]
  
    include struct
      let _ = fun (_ : t) -> ()
      let init () = { foo = Foo.init (); bar = Bar.init () }
      let _ = init
      let save state = Bar.save state.bar
      let _ = save
      let backtrack_n state n = Bar.backtrack_n state.bar n
      let _ = backtrack_n
      let reset state = Bar.reset state.bar
      let _ = reset
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
  
  module Tuple = struct
    type t = (Foo.t[@reversible.ignore]) * Bar.t [@@deriving reversible]
  
    include struct
      let _ = fun (_ : t) -> ()
      let init () = (Foo.init (), Bar.init ())
      let _ = init
      let save (_, x1) = Bar.save x1
      let _ = save
      let backtrack_n (_, x1) n = Bar.backtrack_n x1 n
      let _ = backtrack_n
      let reset (_, x1) = Bar.reset x1
      let _ = reset
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
  Success ✅
