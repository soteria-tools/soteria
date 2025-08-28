include Stdlib.Result

let of_opt ~err = function Some v -> Ok v | None -> Error err
let get_or ~err = function Ok v -> v | Error e -> err e
let get_or_raise ex = function Ok v -> v | Error msg -> raise (ex msg)
