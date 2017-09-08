(* Auxiliary functions for parsing *)

type error =
    Unbound of string
  | Undefined_continuation of int
  | Wrong_stack_at_poptrap of (int * int option)
  | Inconsistent_stacks of int

exception Error of error

let tbl_ident = (Hashtbl.create 57 : (string, Ident.t) Hashtbl.t)
let tbl_label = (Hashtbl.create 57 : (string, int) Hashtbl.t)

let ident_name s =
  match String.index s '/' with
  | exception Not_found -> s
  | n -> String.sub s 0 n

let bind_ident s =
  let id = Ident.create (ident_name s) in
  Hashtbl.add tbl_ident s id;
  id

let find_ident s =
  try
    Hashtbl.find tbl_ident s
  with Not_found ->
    raise(Error(Unbound s))

let unbind_ident id =
  Hashtbl.remove tbl_ident (Ident.name id)

let find_label s =
  try
    Hashtbl.find tbl_label s
  with Not_found ->
    let lbl = Lambda.next_raise_count () in
    Hashtbl.add tbl_label s lbl;
    lbl

let report_error = function
    Unbound s ->
      prerr_string "Unbound identifier "; prerr_string s; prerr_endline "."
  | Undefined_continuation i ->
      prerr_string "Exit "; prerr_int i; prerr_endline " with no matching handler."
  | Wrong_stack_at_poptrap (i, None) ->
      prerr_string "Poptrap "; prerr_int i; prerr_endline " on empty stack."
  | Wrong_stack_at_poptrap (i, Some j) ->
      prerr_string "Poptrap "; prerr_int i;
      prerr_string " with stack top "; prerr_int j;
      prerr_endline "."
  | Inconsistent_stacks i ->
      prerr_string "Exit "; prerr_int i;
      prerr_endline " and its handler have incompatible stacks."

let debuginfo ?(loc=Location.symbol_rloc ()) () =
  Debuginfo.(from_location loc)

let nodebuginfo () =
  Debuginfo.(from_location Location.none)

let adjust_traps exit_stack handler_stack
  : int list =
  let rec diff_stack exit_stack handler_stack =
    match exit_stack, handler_stack with
    | stack, [] -> stack
    | conte :: stacke, conth :: stackh
      when conte = conth -> diff_stack stacke stackh
    | _, _ ->
      raise (Error (Inconsistent_stacks 0))
  in
  let diff_stack =
    List.rev (diff_stack (List.rev exit_stack) (List.rev handler_stack))
  in
  diff_stack

let rec adjust_traps_expr env stack (expr : Cmm.expression)
  : Cmm.expression * int list =
  match expr with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _ | Cblockheader _ | Cvar _ ->
    expr, stack
  | Clet (id, def, body) ->
    let def, stack = adjust_traps_expr env stack def in
    let body, stack = adjust_traps_expr env stack body in
    Clet (id, def, body), stack
  | Cassign (id, expr) ->
    let expr, stack = adjust_traps_expr env stack expr in
    Cassign (id, expr), stack
  | Ctuple exprs ->
    Ctuple (adjust_traps_expr_list env stack exprs), stack
  | Cop (op, exprs, dbg) ->
    Cop (op, adjust_traps_expr_list env stack exprs, dbg), stack
  | Csequence (expr1, expr2) ->
    let expr1, stack = adjust_traps_expr env stack expr1 in
    let expr2, stack = adjust_traps_expr env stack expr2 in
    Csequence (expr1, expr2), stack
  | Cifthenelse (cond, ifso, ifnot) ->
    let cond, stack = adjust_traps_expr env stack cond in
    let ifso, stack_so = adjust_traps_expr env stack ifso in
    let ifnot, stack_not = adjust_traps_expr env stack ifnot in
    assert (stack_so = stack_not);
    Cifthenelse (cond, ifso, ifnot), stack_so
  | Cswitch (expr, ints, exprs, dbg) ->
    let expr, stack = adjust_traps_expr env stack expr in
    let exprs =
      Array.of_list (adjust_traps_expr_list env stack (Array.to_list exprs))
    in
    Cswitch (expr, ints, exprs, dbg), stack
  | Cloop expr ->
    let expr, stack = adjust_traps_expr env stack expr in
    Cloop expr, stack
  | Ccatch (kind, handlers, body) ->
    let body_env =
      (List.map (fun (cont, _, _) -> (cont, stack)) handlers) @ env
    in
    let handlers_env = match kind with
      | Normal Recursive -> body_env
      | Normal Nonrecursive | Exn_handler -> env
    in
    let handlers =
      List.map (fun (cont, params, body) ->
          (cont, params, fst (adjust_traps_expr handlers_env stack body)))
        handlers
    in
    let body, stack = adjust_traps_expr body_env stack body in
    Ccatch (kind, handlers, body), stack
  | Cexit (cont, exprs, _) ->
    let exprs = adjust_traps_expr_list env stack exprs in
    let handler_stack =
      match List.assoc cont env with
      | stack -> stack
      | exception Not_found -> raise (Error (Undefined_continuation cont))
    in
    match adjust_traps stack handler_stack with
    | [] -> Cexit (cont, exprs, No_action), stack
    | to_pop -> Cexit (cont, exprs, Pop to_pop), stack
    | exception (Error (Inconsistent_stacks _)) ->
      raise (Error (Inconsistent_stacks cont))

and adjust_traps_expr_list env stack (exprs : Cmm.expression list) =
  match exprs with
  | [] -> []
  | expr :: rest ->
    let expr, _stack = adjust_traps_expr env stack expr in
    expr :: (adjust_traps_expr_list env stack rest)

let adjust_traps_at_exit (phrase: Cmm.phrase) : Cmm.phrase =
  match phrase with
  | Cfunction fundecl ->
    let fun_body, _stack = adjust_traps_expr [] [] fundecl.fun_body in
    Cfunction { fundecl with fun_body; }
  | Cdata data -> phrase
