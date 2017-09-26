(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2016 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "-40"]

module Int = Numbers.Int

(* Check a number of continuation-related invariants *)

module Env : sig
  type t

  val init : unit -> t

  val check_trap_action : t -> Clambda.trap_action -> unit

  val handler : t -> kind:Clambda.catch_kind -> cont:int -> arg_num:int -> t

  val jump : t -> cont:int -> arg_num:int -> unit

  val report : Format.formatter -> bool
end = struct
  type handler_kind = Static | Exception

  type t = {
    bound_handlers : (handler_kind * int) Int.Map.t;
  }

  type error =
    | Unbound_handler of { cont: int }
    | Mismatching_kind of
        { cont: int; handler_kind: handler_kind; kind: handler_kind; }
    | Multiple_handlers of { cont: int; }
    | Wrong_arguments_number of
        { cont: int; handler_args: int; jump_args: int; }
    | Bad_exception_handler of { cont: int; args: int; }

  module Error = struct
    type t = error

    let compare x y =
      match x, y with
      | Mismatching_kind {cont=cont1;_}, Mismatching_kind {cont=cont2;_} ->
          (* Only report mismatching kinds once per continuation *)
          cont2 - cont1
      | _, _ -> Pervasives.compare x y
  end

  module ErrorSet = Set.Make(Error)

  type persistent_state = {
    mutable all_handlers : Int.Set.t;
    mutable errors : ErrorSet.t;
  }

  let state = {
    all_handlers = Int.Set.empty;
    errors = ErrorSet.empty;
  }

  let record_error error =
    state.errors <- ErrorSet.add error state.errors

  let unbound_handler cont =
    record_error (Unbound_handler { cont; })

  let mismatch cont handler_kind kind =
    record_error (Mismatching_kind { cont; handler_kind; kind; })

  let multiple_handler cont =
    record_error (Multiple_handlers { cont; })

  let wrong_arguments cont handler_args jump_args =
    record_error (Wrong_arguments_number { cont; handler_args; jump_args; })

  let bad_exception_handler cont args =
    record_error (Bad_exception_handler { cont; args; })

  let check_trap_action t (ta: Clambda.trap_action) =
    let check_trap cont =
      match Int.Map.find cont t.bound_handlers with
      | Exception, _ -> ()
      | Static, _ -> mismatch cont Static Exception
      | exception Not_found -> unbound_handler cont
    in
    match ta with
    | No_action -> ()
    | Pop cl | Push cl -> List.iter check_trap cl

  let init () =
    state.all_handlers <- Int.Set.empty;
    state.errors <- ErrorSet.empty;
    {
      bound_handlers = Int.Map.empty;
    }

  let handler t ~kind ~cont ~arg_num =
    if Int.Set.mem cont state.all_handlers then multiple_handler cont;
    state.all_handlers <- Int.Set.add cont state.all_handlers;
    let kind = match (kind : Clambda.catch_kind) with
      | Normal _ -> Static
      | Exn_handler ->
        if arg_num <> 1 then bad_exception_handler cont arg_num;
        Exception
    in
    let bound_handlers = Int.Map.add cont (kind, arg_num) t.bound_handlers in
    { bound_handlers; }

  let jump t ~cont ~arg_num =
    match Int.Map.find cont t.bound_handlers with
    | Static, handler_args ->
      if arg_num <> handler_args then
        wrong_arguments cont handler_args arg_num
    | Exception, _ -> mismatch cont Exception Static
    | exception Not_found -> unbound_handler cont

  let print_handler_kind ppf kind =
    let str = match kind with
      | Static -> "static"
      | Exception -> "exception"
    in
    Format.fprintf ppf "%s" str

  let print_error ppf error =
    match error with
    | Unbound_handler { cont } ->
      if Int.Set.mem cont state.all_handlers then
        Format.fprintf ppf
          "Continuation %d was used outside the scope of its handler"
          cont
      else
        Format.fprintf ppf
          "Continuation %d was used but never bound"
          cont
    | Mismatching_kind { cont; handler_kind; kind } ->
      Format.fprintf ppf
        "Continuation %d was declared as %a but used as %a"
        cont
        print_handler_kind handler_kind
        print_handler_kind kind
    | Multiple_handlers { cont; } ->
      Format.fprintf ppf
        "Continuation %d was declared in more than one handler"
        cont
    | Wrong_arguments_number { cont; handler_args; jump_args } ->
      Format.fprintf ppf
        "Continuation %d was declared with %d arguments but called with %d"
        cont
        handler_args
        jump_args
    | Bad_exception_handler { cont; args } ->
      Format.fprintf ppf
        "Continuation %d was declared as an exception handler with %d arguments"
        cont
        args

  let print_error_newline ppf error =
    Format.fprintf ppf "%a@." print_error error

  let report ppf =
    if ErrorSet.is_empty state.errors then false
    else begin
      ErrorSet.iter (fun err -> print_error_newline ppf err) state.errors;
      true
    end
end

let rec check env (expr : Cmm.expression) =
  match expr with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _ | Cblockheader _ | Cvar _ ->
    ()
  | Clet (_, expr, body) ->
    check env expr;
    check env body
  | Cassign (_, expr) ->
    check env expr
  | Ctuple exprs ->
    List.iter (check env) exprs
  | Cop (_, args, _) ->
    List.iter (check env) args
  | Csequence (expr1, expr2) ->
    check env expr1;
    check env expr2
  | Cifthenelse (test, ifso, ifnot) ->
    check env test;
    check env ifso;
    check env ifnot
  | Cswitch (body, _, branches, _) ->
    check env body;
    Array.iter (check env) branches
  | Cloop expr ->
    check env expr
  | Ccatch (kind, handlers, body) ->
    let env_extended =
      List.fold_left
        (fun env (cont, args, _) ->
           Env.handler env ~kind ~cont ~arg_num:(List.length args))
        env
        handlers
    in
    check env_extended body;
    let env_handler =
      match kind with
      | Normal Recursive -> env_extended
      | Normal Nonrecursive | Exn_handler -> env
    in
    List.iter (fun (_, _, handler) -> check env_handler handler) handlers
  | Cexit (cont, args, ta) ->
    Env.jump env ~cont ~arg_num:(List.length args);
    Env.check_trap_action env ta

let run ppf (fundecl : Cmm.fundecl) =
  let env = Env.init () in
  check env fundecl.fun_body;
  Env.report ppf
