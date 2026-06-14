open Soteria.Logs.Printers

(** A structured description of an unsupported feature, produced by [not_impl].

    It is serialised to JSON so it can travel through the symbolic execution
    engine's string-based [give_up] mechanism, and reconstructed when reporting
    results to give a nicer, actionable diagnostic. *)
type t = {
  desc : string;  (** What isn't supported. *)
  tip : (string * string option) option;
      (** An optional hint on how to work around the limitation; the second
          component is an optional command to suggest alongside the tip. *)
  issue : int option;  (** A relevant GitHub issue number, if any. *)
}
[@@deriving yojson]

(* The description is rendered as a standalone sentence, so we capitalise it
   here rather than asking every call site to. *)
let make ?tip ?issue desc = { desc = String.capitalize_ascii desc; tip; issue }
let repo_issues = "https://github.com/soteria-tools/soteria/issues"
let pp_repo_issues ft = (pp_clr `Cyan) ft repo_issues
let pp_repo_issue ft num = (pp_clr `Cyan) ft (Fmt.str "%s/%d" repo_issues num)

(** Serialise to a compact JSON string, for use as a [give_up] reason. *)
let to_string t = Yojson.Safe.to_string (to_yojson t)

(** Try to reconstruct a [t] from a [give_up] reason, constructing a new
    objecting from the string if conversion fails. *)
let of_string str =
  match Yojson.Safe.from_string str with
  | json -> ( match of_yojson json with Ok t -> t | Error _ -> make str)
  | exception _ -> make str

let pp_tip ft (tip, cmd) =
  Fmt.pf ft "%a %s" (pp_clr `Yellow) "tip:" tip;
  match cmd with
  | Some cmd -> Fmt.pf ft "@.     %a" (pp_style `Bold) cmd
  | None -> ()

let pp_issue ft issue =
  match issue with
  | Some n -> Fmt.pf ft "This is tracked at %a" pp_repo_issue n
  | None ->
      Fmt.pf ft
        "If you think Soteria should handle this, please consider opening an \
         issue at %t"
        pp_repo_issues

(** Full rendering: the description followed by the actionable block. Intended
    to be printed directly (e.g. with [Fmt.pr]), not wrapped in a diagnostic. *)
let pp ft { desc; tip; issue } =
  let open Fmt in
  pf ft "%s@.@.%a%a" desc (option (pp_tip ++ any "@.@.")) tip pp_issue issue
