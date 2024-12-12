module List = Monad.Extend (struct
  type 'a t = 'a list

  let bind x f = List.concat_map f x
  let return x = [ x ]
  let map x f = List.map f x
end)

module List = List.Syntax

module Option = Monad.Extend (struct
  type 'a t = 'a option

  let bind = Option.bind
  let map x f = Option.map f x
  let return x = Some x
end)

module Option = Option.Syntax
