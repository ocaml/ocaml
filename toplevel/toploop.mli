(* The interactive toplevel loop *)

val loop: unit -> unit

(* Interface with toplevel directives *)

type directive_fun =
    Directive_none of (unit -> unit)
  | Directive_string of (string -> unit)
  | Directive_int of (int -> unit)
  | Directive_ident of (Longident.t -> unit)

val directive_table: (string, directive_fun) Hashtbl.t
        (* Table of known directives, with their execution function *)
val execute_phrase: Parsetree.toplevel_phrase -> unit
        (* Execute the given toplevel phrase *)
val print_exception_outcome: exn -> unit
        (* Print an exception resulting from the evaluation of user code. *)
val toplevel_env: Env.t ref
        (* Typing environment for the toplevel *)
