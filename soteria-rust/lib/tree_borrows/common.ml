type access = Read | Write

and state =
  | Reserved of bool
  | Unique
  | Frozen
  | ReservedIM
  | Cell
  | Disabled
  | UB

and protector = Strong | Weak
and locality = Local | Foreign [@@deriving show { with_path = false }, eq]
