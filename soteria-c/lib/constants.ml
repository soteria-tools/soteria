let string_to_char = function
  | "\\0" -> Some 0
  | "\\n" -> Some (Char.code '\n')
  | "\\t" -> Some (Char.code '\t')
  | "\\r" -> Some (Char.code '\r')
  | c when String.length c = 1 -> Some (Char.code c.[0])
  | _ -> None
