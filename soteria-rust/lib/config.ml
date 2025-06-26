type t = {
  no_compile : bool; [@default false]
  cleanup : bool; [@default false]
  ignore_leaks : bool; [@default false]
  ignore_aliasing : bool; [@default false]
  with_kani : bool; [@default false]
  with_miri : bool; [@default false]
}
[@@deriving make]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config
