module type Printable = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type String_encodable = sig
  type t

  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module type Yojsonable = sig
  type t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

(** A foldable interface for uniform iteration over different container types.*)
module type Foldable = sig
  type 'a t

  (** Fold over the elements of the data structure. *)
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end
