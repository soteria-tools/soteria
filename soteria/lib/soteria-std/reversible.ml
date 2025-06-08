module type Mutable = sig
  type t

  val init : unit -> t
  val backtrack_n : t -> int -> unit
  val save : t -> unit
  val reset : t -> unit
end

module Make_mutable (M : sig
  type t

  val default : t
end) : sig
  include Mutable with type t = M.t Dynarray.t

  val set_default : M.t -> unit
  val wrap : (M.t -> 'a * M.t) -> t -> 'a
  val wrap_read : (M.t -> 'a) -> t -> 'a
end = struct
  type t = M.t Dynarray.t

  let default = ref M.default

  let init () =
    let d = Dynarray.create () in
    Dynarray.add_last d !default;
    d

  let set_default v = default := v

  let backtrack_n d n =
    let len = Dynarray.length d in
    Dynarray.truncate d (len - n)

  let save d = Dynarray.add_last d (Dynarray.get_last d)

  let reset d =
    Dynarray.clear d;
    Dynarray.add_last d !default

  let wrap (f : M.t -> 'a * M.t) d =
    let e = Dynarray.pop_last d in
    let a, e' = f e in
    Dynarray.add_last d e';
    a

  let wrap_read (f : M.t -> 'a) d =
    let e = Dynarray.get_last d in
    f e
end

module type In_place = sig
  val backtrack_n : int -> unit
  val save : unit -> unit
  val reset : unit -> unit
end

module Mutable_to_in_place (M : Mutable) = struct
  let state = lazy (M.init ())
  let save () = M.save (Lazy.force state)
  let backtrack_n n = M.backtrack_n (Lazy.force state) n
  let reset () = M.reset (Lazy.force state)
  let wrap (f : M.t -> 'a) () : 'a = f (Lazy.force state)
end

module Make_in_place (M : sig
  type t

  val default : t
end) =
struct
  module Mutable = Make_mutable (M)
  include Mutable_to_in_place (Mutable)

  let set_default = Mutable.set_default
  let wrap_read f () = wrap (Mutable.wrap_read f) ()
  let wrap f () = wrap (Mutable.wrap f) ()
end

module type Immutable = sig
  type t

  val init : t
  val backtrack_n : t -> int -> t
  val save : t -> t
  val reset : t -> t
end
