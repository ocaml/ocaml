(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Entry points in the parser *)

(* Skip tokens to the end of the phrase *)

let last_token = ref Parser.EOF

let token lexbuf =
  let token = Lexer.token lexbuf in
  last_token := token;
  token

let rec skip_phrase lexbuf =
  match token lexbuf with
  | Parser.SEMISEMI | Parser.EOF -> ()
  | _ -> skip_phrase lexbuf
  | exception (Lexer.Error (Lexer.Unterminated_comment _, _)
              | Lexer.Error (Lexer.Unterminated_string, _)
              | Lexer.Error (Lexer.Reserved_sequence _, _)
              | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
              | Lexer.Error (Lexer.Illegal_character _, _)) ->
      skip_phrase lexbuf

let maybe_skip_phrase lexbuf =
  match !last_token with
  | Parser.SEMISEMI | Parser.EOF -> ()
  | _ -> skip_phrase lexbuf

let get_triple lexbuf =
  let token = Lexer.token lexbuf in
  let {Lexing. lex_start_p; lex_curr_p; _} = lexbuf in
  (token, lex_start_p, lex_curr_p)

let rec parse_loop lexbuf last_token last_input checkpoint =
  match checkpoint with
  | Parser.MenhirInterpreter.InputNeeded env ->
     let triple = get_triple lexbuf in
     parse_loop lexbuf triple env
       (Parser.MenhirInterpreter.offer checkpoint triple)
  | Parser.MenhirInterpreter.HandlingError _ ->
     let _, loc_start, loc_end = last_token in
     let loc = {Location. loc_start; loc_end; loc_ghost = false} in
     begin match Parse_errors.error_messages last_input last_token with
     | Some msg ->
        raise (Syntaxerr.Error (Syntaxerr.Custom (loc, msg)))
     | None ->
        raise (Syntaxerr.Error (Syntaxerr.Other loc))
     end
  | Parser.MenhirInterpreter.Accepted ast -> ast
  | _ ->
     parse_loop lexbuf last_token last_input
       (Parser.MenhirInterpreter.resume ~strategy:`Simplified checkpoint)

let run_parser lexbuf checkpoint =
  match checkpoint with
  | Parser.MenhirInterpreter.InputNeeded env ->
     let triple = get_triple lexbuf in
     parse_loop lexbuf triple env
       (Parser.MenhirInterpreter.offer checkpoint triple)
  | _ -> assert false

let wrap entrypoint lexbuf : 'a =
  try
    Docstrings.init ();
    let keyword_edition =
      Clflags.(Option.map parse_keyword_edition !keyword_edition)
    in
    Lexer.init ?keyword_edition ();
    let ast = run_parser lexbuf (entrypoint lexbuf.Lexing.lex_curr_p) in
    Parsing.clear_parser();
    Docstrings.warn_bad_docstrings ();
    last_token := Parser.EOF;
    ast
  with
  | Lexer.Error(Lexer.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//"->
      skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err
    when !Location.input_name = "//toplevel//" ->
      maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))

let implementation = wrap Parser.Incremental.implementation
and interface = wrap Parser.Incremental.interface
and toplevel_phrase = wrap Parser.Incremental.toplevel_phrase
and use_file = wrap Parser.Incremental.use_file
and core_type = wrap Parser.Incremental.parse_core_type
and expression = wrap Parser.Incremental.parse_expression
and pattern = wrap Parser.Incremental.parse_pattern
let module_type = wrap Parser.Incremental.parse_module_type
let module_expr = wrap Parser.Incremental.parse_module_expr

let longident = wrap Parser.Incremental.parse_any_longident
let val_ident = wrap Parser.Incremental.parse_val_longident
let constr_ident= wrap Parser.Incremental.parse_constr_longident
let extended_module_path = wrap Parser.Incremental.parse_mod_ext_longident
let simple_module_path = wrap Parser.Incremental.parse_mod_longident
let type_ident = wrap Parser.Incremental.parse_mty_longident

(* Error reporting for Syntaxerr *)
(* The code has been moved here so that one can reuse Pprintast.tyvar *)

module Style = Misc.Style

let prepare_error err =
  let open Syntaxerr in
  match err with
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
      Location.errorf
        ~loc:closing_loc
        "Syntax error: %a expected" Style.inline_code closing
        ~sub:[
          Location.msg ~loc:opening_loc
            "This %a might be unmatched" Style.inline_code opening
        ]

  | Expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %a expected."
        Style.inline_code nonterm
  | Not_expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %a not expected."
        Style.inline_code nonterm
  | Applicative_path loc ->
      Location.errorf ~loc
        "Syntax error: applicative paths of the form %a \
         are not supported when the option %a is set."
        Style.inline_code "F(X).t"
        Style.inline_code "-no-app-func"
  | Variable_in_scope (loc, var) ->
      Location.errorf ~loc
        "In this scoped type, variable %a \
         is reserved for the local type %a."
        (Style.as_inline_code Pprintast.Doc.tyvar) var
        Style.inline_code var
  | Other loc ->
      Location.error ~loc "Syntax error"
  | Custom (loc, msg) ->
      Location.error ~loc msg
  | Ill_formed_ast (loc, s) ->
      Location.errorf ~loc
        "broken invariant in parsetree: %s" s
  | Invalid_package_type (loc, ipt) ->
      let invalid ppf ipt = match ipt with
        | Syntaxerr.Parameterized_types ->
            Format_doc.fprintf ppf "parametrized types are not supported"
        | Constrained_types ->
            Format_doc.fprintf ppf "constrained types are not supported"
        | Private_types ->
            Format_doc.fprintf ppf  "private types are not supported"
        | Not_with_type ->
            Format_doc.fprintf ppf "only %a constraints are supported"
              Style.inline_code "with type t ="
        | Neither_identifier_nor_with_type ->
            Format_doc.fprintf ppf
              "only module type identifier and %a constraints are supported"
              Style.inline_code "with type"
      in
      Location.errorf ~loc "Syntax error: invalid package type: %a" invalid ipt
  | Removed_string_set loc ->
      Location.errorf ~loc
        "Syntax error: strings are immutable,@ there@ is@ no@ assignment@ \
         syntax@ for@ them."
        ~sub:[
          Location.msg
            "@{<hint>Hint@}: Mutable sequences of bytes are available in \
             the %a module."
            Style.inline_code "Bytes";
          Location.msg
            "@{<hint>Hint@}: Did you mean to use %a?"
            Style.inline_code "Bytes.set"
        ]

let () =
  Location.register_error_of_exn
    (function
      | Syntaxerr.Error err -> Some (prepare_error err)
      | _ -> None
    )
