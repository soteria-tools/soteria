  $ ../test.sh all_ignored.ml
  open Prelude
  
  module Struct = struct
    type t = { foo : Foo.t; [@ignore] bar : Bar.t [@ignore] }
    [@@deriving reversible]
  
    include struct
      let _ = fun (_ : t) -> ()
      let init () = { foo = Foo.init (); bar = Bar.init () }
      let _ = init
      let save _ = ()
      let _ = save
      let backtrack_n _ _ = ()
      let _ = backtrack_n
      let reset _ = ()
      let _ = reset
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
  
  module Tuple = struct
    type t = (Foo.t[@ignore]) * (Bar.t[@ignore]) [@@deriving reversible]
  
    include struct
      let _ = fun (_ : t) -> ()
      let init () = (Foo.init (), Bar.init ())
      let _ = init
      let save _ = ()
      let _ = save
      let backtrack_n _ _ = ()
      let _ = backtrack_n
      let reset _ = ()
      let _ = reset
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
  Success ✅
