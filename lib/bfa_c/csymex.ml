include Bfa_symex.Symex.M (Z3solver)

let not_impl ?source_loc ?loc what =
  let pp_source_loc ft sl =
    Fmt.pf ft "%s@\n" (Cerb_location.location_to_string sl)
  in
  L.info (fun m ->
      m "%aMISSING FEATURE, VANISHING%a@\n%s" (Fmt.option pp_source_loc)
        source_loc
        Fmt.(option (parens string))
        loc what);
  vanish ()
