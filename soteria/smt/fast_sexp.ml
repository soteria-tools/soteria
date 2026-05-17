(** Fast s-expression type, serializer, and buffered reader.

    This module is [include]d by {!Soteria_smt}; do not use it directly. *)

(** {1 S-expressions} *)

type sexp = Atom of string | List of sexp list

let atom f : sexp = Atom f
let list (xs : sexp list) : sexp = List xs
let is_atom = function Atom _ -> true | List _ -> false
let to_list = function Atom _ -> None | List xs -> Some xs

(** Serialize an s-expression into [buf], using the minimal whitespace the
    solver needs (a single space between list elements). *)
let rec write_buf buf = function
  | Atom s -> Buffer.add_string buf s
  | List [] -> Buffer.add_string buf "()"
  | List (x :: xs) ->
      Buffer.add_char buf '(';
      write_buf buf x;
      List.iter
        (fun e ->
          Buffer.add_char buf ' ';
          write_buf buf e)
        xs;
      Buffer.add_char buf ')'

let to_string (s : sexp) : string =
  let buf = Buffer.create 64 in
  write_buf buf s;
  Buffer.contents buf

let pp_sexp fmt (s : sexp) = Format.pp_print_string fmt (to_string s)

let output_sexp (oc : out_channel) (s : sexp) =
  let buf = Buffer.create 256 in
  write_buf buf s;
  Buffer.output_buffer oc buf

(** {1 Exceptions} *)

exception UnexpectedSolverResponse of sexp

let () =
  Printexc.register_printer (function
    | UnexpectedSolverResponse s ->
        Some (Printf.sprintf "UnexpectedSolverResponse(%s)" (to_string s))
    | _ -> None)

(** {1 Buffered reader / response parser} *)

module Reader = struct
  type t = {
    ic : in_channel;
    err : in_channel;
    mutable buf : bytes;
    mutable pos : int;
    mutable len : int;
    atom_buf : Buffer.t;
  }

  let create ic err =
    {
      ic;
      err;
      buf = Bytes.create 65536;
      pos = 0;
      len = 0;
      atom_buf = Buffer.create 64;
    }

  (* Refill the buffer; returns false at EOF. *)
  let refill r =
    r.pos <- 0;
    r.len <- input r.ic r.buf 0 (Bytes.length r.buf);
    r.len > 0

  let rec peek r =
    if r.pos < r.len then Some (Bytes.unsafe_get r.buf r.pos)
    else if refill r then peek r
    else None

  let[@inline] advance r = r.pos <- r.pos + 1
  let[@inline] is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

  (* [;] ends a plain atom: in SMT-LIB it always starts a line comment, so a
     simple (unquoted) symbol can never contain one. *)
  let[@inline] is_delim = function
    | ' ' | '\t' | '\n' | '\r' | '(' | ')' | '|' | '"' | ';' -> true
    | _ -> false

  let rec skip_ws r =
    match peek r with
    | Some c when is_ws c ->
        advance r;
        skip_ws r
    | Some ';' ->
        (* line comment *)
        let rec drop () =
          match peek r with
          | Some '\n' ->
              advance r;
              skip_ws r
          | Some _ ->
              advance r;
              drop ()
          | None -> ()
        in
        drop ()
    | _ -> ()

  (* Read a delimited token (verbatim, keeping any quoting). *)
  let read_plain r =
    Buffer.clear r.atom_buf;
    let rec loop () =
      match peek r with
      | Some c when not (is_delim c) ->
          Buffer.add_char r.atom_buf c;
          advance r;
          loop ()
      | _ -> Buffer.contents r.atom_buf
    in
    loop ()

  let read_quoted r =
    (* assumes leading '|' already at peek *)
    Buffer.clear r.atom_buf;
    advance r;
    Buffer.add_char r.atom_buf '|';
    let rec loop () =
      match peek r with
      | Some '|' ->
          advance r;
          Buffer.add_char r.atom_buf '|';
          Buffer.contents r.atom_buf
      | Some c ->
          advance r;
          Buffer.add_char r.atom_buf c;
          loop ()
      | None -> Buffer.contents r.atom_buf
    in
    loop ()

  let read_string r =
    (* assumes leading '"' already at peek; "" is an escaped quote *)
    Buffer.clear r.atom_buf;
    advance r;
    Buffer.add_char r.atom_buf '"';
    let rec loop () =
      match peek r with
      | Some '"' -> (
          advance r;
          match peek r with
          | Some '"' ->
              advance r;
              Buffer.add_string r.atom_buf "\"\"";
              loop ()
          | _ ->
              Buffer.add_char r.atom_buf '"';
              Buffer.contents r.atom_buf)
      | Some c ->
          advance r;
          Buffer.add_char r.atom_buf c;
          loop ()
      | None -> Buffer.contents r.atom_buf
    in
    loop ()

  (* Interned hot atoms so check / ack_command do not allocate. *)
  let a_success = Atom "success"
  let a_sat = Atom "sat"
  let a_unsat = Atom "unsat"
  let a_unknown = Atom "unknown"

  let intern = function
    | "success" -> a_success
    | "sat" -> a_sat
    | "unsat" -> a_unsat
    | "unknown" -> a_unknown
    | s -> Atom s

  let read_all_err r = In_channel.input_all r.err

  let rec read_sexp r : sexp =
    skip_ws r;
    match peek r with
    | None -> Atom (read_all_err r)
    | Some '(' ->
        advance r;
        read_list r []
    | Some ')' -> raise (UnexpectedSolverResponse (Atom ")"))
    | Some '|' -> Atom (read_quoted r)
    | Some '"' -> Atom (read_string r)
    | Some _ -> intern (read_plain r)

  and read_list r acc =
    skip_ws r;
    match peek r with
    | Some ')' ->
        advance r;
        List (List.rev acc)
    | None -> Atom (read_all_err r)
    | Some _ ->
        let e = read_sexp r in
        read_list r (e :: acc)
end
