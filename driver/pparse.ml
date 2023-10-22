(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt       *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

(* Optionally preprocess a source file *)

let call_external_preprocessor sourcefile pp =
      let tmpfile = Filename.temp_file "ocamlpp" "" in
      let comm = Printf.sprintf "%s %s > %s"
                                pp (Filename.quote sourcefile) tmpfile
      in
      if Ccomp.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        raise (Error (CannotRun comm));
      end;
      tmpfile

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      Profile.record "-pp"
        (call_external_preprocessor sourcefile) pp


let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile

type ('a, 'b) ast_kind =
| Structure : (Parsetree.structure, Parsetree.implementation) ast_kind
| Signature : (Parsetree.signature, Parsetree.interface) ast_kind

let magic_of_kind : type a b . (a, b) ast_kind -> string = function
  | Structure -> Config.ast_impl_magic_number
  | Signature -> Config.ast_intf_magic_number

(* Note: some of the functions here should go to Ast_mapper instead,
   which would encapsulate the "binary AST" protocol. *)

let write_ast (type a b) (kind : (a, b) ast_kind) fn (ast : a) =
  let oc = open_out_bin fn in
  output_string oc (magic_of_kind kind);
  output_value oc (!Location.input_name : string);
  output_value oc (ast : a);
  close_out oc

let apply_rewriter kind fn_in ppx =
  let magic = magic_of_kind kind in
  let fn_out = Filename.temp_file "camlppx" "" in
  let comm =
    Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out)
  in
  let ok = Ccomp.command comm = 0 in
  Misc.remove_file fn_in;
  if not ok then begin
    Misc.remove_file fn_out;
    raise (Error (CannotRun comm));
  end;
  if not (Sys.file_exists fn_out) then
    raise (Error (WrongMagic comm));
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer =
    try really_input_string ic (String.length magic) with End_of_file -> "" in
  close_in ic;
  if buffer <> magic then begin
    Misc.remove_file fn_out;
    raise (Error (WrongMagic comm));
  end;
  fn_out

let read_ast (type a b) (kind : (a, b) ast_kind) fn : a =
  let ic = open_in_bin fn in
  Misc.try_finally
    ~always:(fun () -> close_in ic; Misc.remove_file fn)
    (fun () ->
       let magic = magic_of_kind kind in
       let buffer = really_input_string ic (String.length magic) in
       assert(buffer = magic); (* already checked by apply_rewriter *)
       Location.input_name := (input_value ic : string);
       (input_value ic : a)
    )

let rewrite kind ppxs ast =
  let fn = Filename.temp_file "camlppx" "" in
  write_ast kind fn ast;
  let fn = List.fold_left (apply_rewriter kind) fn (List.rev ppxs) in
  read_ast kind fn

let apply_rewriters_str ?(restore = true) ~tool_name ast =
  match !Clflags.all_ppx with
  | [] -> ast
  | ppxs ->
      let ast =
        ast
        |> Ast_mapper.add_ppx_context_str ~tool_name
        |> rewrite Structure ppxs
        |> Ast_mapper.drop_ppx_context_str ~restore
      in
      Ast_invariants.structure ast; ast

let apply_rewriters_sig ?(restore = true) ~tool_name ast =
  match !Clflags.all_ppx with
  | [] -> ast
  | ppxs ->
      let ast =
        ast
        |> Ast_mapper.add_ppx_context_sig ~tool_name
        |> rewrite Signature ppxs
        |> Ast_mapper.drop_ppx_context_sig ~restore
      in
      Ast_invariants.signature ast; ast

let apply_rewriters ?restore ~tool_name
    (type a b) (kind : (a, b) ast_kind) (ast : a) : a =
  match kind with
  | Structure ->
      apply_rewriters_str ?restore ~tool_name ast
  | Signature ->
      apply_rewriters_sig ?restore ~tool_name ast

(* Parse a file or get a dumped syntax tree from it *)

exception Outdated_version

let open_and_check_magic inputfile ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = really_input_string ic (String.length ast_magic) in
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        Misc.fatal_error "OCaml and preprocessor have incompatible versions"
    | _ -> false
  in
  (ic, is_ast_file)

let wrap_with_location (type a b) (kind : (a, b) ast_kind) (ast : a) ~file : b =
  let loc = Location.in_file file in
  match kind with
  | Structure -> { Parsetree.pimpl_structure = ast; pimpl_loc = loc }
  | Signature -> { Parsetree.pintf_signature = ast; pintf_loc = loc }

let map_ast (type a b) (kind : (a, b) ast_kind) (ast : b) ~(f:a -> a) : b =
  match kind with
  | Structure -> { ast with pimpl_structure = f ast.pimpl_structure }
  | Signature -> { ast with pintf_signature = f ast.pintf_signature }

let file_aux ~tool_name inputfile (type a b) parse_fun (invariant_fun : b -> unit)
             (kind : (a, b) ast_kind) : b =
  let ast =
    let ast_magic = magic_of_kind kind in
    let (ic, is_ast_file) = open_and_check_magic inputfile ast_magic in
    let close_ic () = close_in ic in
    if is_ast_file then begin
      let ast =
        Fun.protect ~finally:close_ic @@ fun () ->
        let file = (input_value ic : string) in
        Location.input_name := file;
        if !Clflags.unsafe then
          Location.prerr_warning (Location.in_file !Location.input_name)
            Warnings.Unsafe_array_syntax_without_parsing;
        wrap_with_location kind (input_value ic : a) ~file
      in
      if !Clflags.all_ppx = [] then invariant_fun ast;
      (* if all_ppx <> [], invariant_fun will be called by apply_rewriters *)
      ast
    end else begin
      let source =
        (* We read the whole source file at once. This guarantees that all
           input is in the lexing buffer and can be reused by error printers
           to quote source code at specific locations -- see #12238 and the
           Location.lines_around* functions. *)
        Fun.protect ~finally:close_ic @@ fun () ->
        seek_in ic 0;
        In_channel.input_all ic
      in
      let lexbuf = Lexing.from_string source in
      Location.init lexbuf inputfile;
      Location.input_lexbuf := Some lexbuf;
      Profile.record_call "parser" (fun () -> parse_fun lexbuf)
    end
  in
  Profile.record_call "-ppx" (fun () ->
      map_ast kind ast ~f:(apply_rewriters ~restore:false ~tool_name kind)
    )

let file ~tool_name inputfile parse_fun ast_kind =
  file_aux ~tool_name inputfile parse_fun ignore ast_kind

let report_error ppf = function
  | CannotRun cmd ->
      fprintf ppf "Error while running external preprocessor@.\
                   Command line: %s@." cmd
  | WrongMagic cmd ->
      fprintf ppf "External preprocessor does not produce a valid file@.\
                   Command line: %s@." cmd

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let parse_file ~tool_name invariant_fun parse kind sourcefile =
  Location.input_name := sourcefile;
  let inputfile = preprocess sourcefile in
  Misc.try_finally
    (fun () ->
       Profile.record_call "parsing" @@ fun () ->
       file_aux ~tool_name inputfile parse invariant_fun kind)
    ~always:(fun () -> remove_preprocessed inputfile)

let parse_implementation ~tool_name sourcefile =
  parse_file ~tool_name Ast_invariants.implementation
    Parse.implementation Structure sourcefile

let parse_interface ~tool_name sourcefile =
  parse_file ~tool_name Ast_invariants.interface
    Parse.interface Signature sourcefile
