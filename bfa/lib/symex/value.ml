module type S = sig
  type t
  type ty

  val not : t -> t
  val sem_eq : t -> t -> t
  val pp : t Fmt.t
end
