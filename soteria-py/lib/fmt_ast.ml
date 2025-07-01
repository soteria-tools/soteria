open PyreAst.Concrete

let pp_of_sexp to_sexp =
 fun ft e -> Sexplib0.Sexp.pp_hum_indent 2 ft (to_sexp e)

let pp_constant = pp_of_sexp Constant.sexp_of_t
let pp_expr = pp_of_sexp Expression.sexp_of_t
let pp_stmt = pp_of_sexp Statement.sexp_of_t
let pp_module = pp_of_sexp Module.sexp_of_t
