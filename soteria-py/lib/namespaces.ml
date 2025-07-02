module Scope = struct
  include Soteria_symex.Var.Map

  type nonrec t = Pyval.t t
end

module Builtin_scope = struct end
module Scope_chain = struct end
