  $ ../test.sh simple.ml
  open Prelude
  
  module Struct = struct
    type t = { foo : Foo.t; bar : Bar.t } [@@deriving reversible]
  
    include struct
      let _ = fun (_ : t) -> ()
      let init () = { foo = Foo.init (); bar = Bar.init () }
      let _ = init
  
      let save state =
        Foo.save state.foo;
        Bar.save state.bar
  
      let _ = save
  
      let backtrack_n state n =
        Foo.backtrack_n state.foo n;
        Bar.backtrack_n state.bar n
  
      let _ = backtrack_n
  
      let reset state =
        Foo.reset state.foo;
        Bar.reset state.bar
  
      let _ = reset
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
  
  module Tuple = struct
    type t = Foo.t * Bar.t [@@deriving reversible]
  
    include struct
      let _ = fun (_ : t) -> ()
      let init () = (Foo.init (), Bar.init ())
      let _ = init
  
      let save (x0, x1) =
        Foo.save x0;
        Bar.save x1
  
      let _ = save
  
      let backtrack_n (x0, x1) n =
        Foo.backtrack_n x0 n;
        Bar.backtrack_n x1 n
  
      let _ = backtrack_n
  
      let reset (x0, x1) =
        Foo.reset x0;
        Bar.reset x1
  
      let _ = reset
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
  Success ✅
