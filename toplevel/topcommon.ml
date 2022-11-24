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

(* Definitions for the interactive toplevel loop that are common between
   bytecode and native *)

open Format
open Parsetree
open Outcometree
open Ast_helper

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print_loc
let print_error = Location.print_report
let print_warning = Location.print_warning
let input_name = Location.input_name

let parse_mod_use_file name lb =
  let modname =
    String.capitalize_ascii
      (Filename.remove_extension (Filename.basename name))
  in
  let items =
    List.concat
      (List.map
         (function Ptop_def s -> s | Ptop_dir _ -> [])
         (!parse_use_file lb))
  in
  [ Ptop_def
      [ Str.module_
          (Mb.mk
             (Location.mknoloc (Some modname))
             (Mod.structure items)
          )
       ]
   ]

(* Hooks for printing *)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_out_value = Oprint.out_value
let print_out_type = Oprint.out_type
let print_out_class_type = Oprint.out_class_type
let print_out_module_type = Oprint.out_module_type
let print_out_type_extension = Oprint.out_type_extension
let print_out_sig_item = Oprint.out_sig_item
let print_out_signature = Oprint.out_signature
let print_out_phrase = Oprint.out_phrase

let find_eval_phrase str =
  let open Typedtree in
  match str.str_items with
  | [ { str_desc = Tstr_eval (e, attrs) ; str_loc = loc } ]
  | [ { str_desc = Tstr_value (Asttypes.Nonrecursive,
                                [{ vb_expr = e
                                 ; vb_pat = { pat_desc = Tpat_any; _ }
                                 ; vb_attributes = attrs }])
      ; str_loc = loc }
    ] ->
      Some (e, attrs, loc)
  | _ -> None

(* The current typing environment for the toplevel *)

let toplevel_env = ref Env.empty

let backtrace = ref None

(* Generic evaluator and printer *)

exception Undefined_global of string

module type EVAL_BASE = sig

  (* Return the value referred to by a base ident.
     @raise [Undefined_global] if not found *)
  val eval_ident: Ident.t -> Obj.t

end

module MakeEvalPrinter (E: EVAL_BASE) = struct

  let rec eval_address = function
    | Env.Aident id -> E.eval_ident id
    | Env.Adot(p, pos) -> Obj.field (eval_address p) pos

  let eval_path find env path =
    match find path env with
    | addr -> eval_address addr
    | exception Not_found ->
        Misc.fatal_error ("Cannot find address for: " ^ (Path.name path))

  let eval_module_path env path =
    eval_path Env.find_module_address env path

  let eval_value_path env path =
    eval_path Env.find_value_address env path

  let eval_extension_path env path =
    eval_path Env.find_constructor_address env path

  let eval_class_path env path =
    eval_path Env.find_class_address env path


  module Printer = Genprintval.Make(Obj)(struct
      type valu = Obj.t
      exception Error
      let eval_address addr =
        try eval_address addr
        with Undefined_global _ ->
          raise Error
      let same_value v1 v2 = (v1 == v2)
    end)

  let print_untyped_exception ppf obj =
    !print_out_value ppf (Printer.outval_of_untyped_exception obj)
  let outval_of_value env obj ty =
    Printer.outval_of_value !max_printer_steps !max_printer_depth
      (fun _ _ _ -> None) env obj ty
  let print_value env obj ppf ty =
    !print_out_value ppf (outval_of_value env obj ty)

  (* Print an exception produced by an evaluation *)

  let print_out_exception ppf exn outv =
    !print_out_phrase ppf (Ophr_exception (exn, outv))

  let print_exception_outcome ppf exn =
    if exn = Out_of_memory then Gc.full_major ();
    let outv = outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn in
    print_out_exception ppf exn outv;
    if Printexc.backtrace_status ()
    then
      match !backtrace with
      | None -> ()
      | Some b ->
          print_string b;
          backtrace := None

  type ('a, 'b) gen_printer = ('a, 'b) Genprintval.gen_printer =
    | Zero of 'b
    | Succ of ('a -> ('a, 'b) gen_printer)

  let install_printer = Printer.install_printer
  let install_generic_printer = Printer.install_generic_printer
  let install_generic_printer' = Printer.install_generic_printer'
  let remove_printer = Printer.remove_printer

end


(* Hook for initialization *)

let toplevel_startup_hook = ref (fun () -> ())

type event = ..
type event +=
  | Startup
  | After_setup

let hooks = ref []

let add_hook f = hooks := f :: !hooks

let () =
  add_hook (function
      | Startup -> !toplevel_startup_hook ()
      | _ -> ())

let run_hooks hook = List.iter (fun f -> f hook) !hooks

(* Helpers for execution *)

type evaluation_outcome = Result of Obj.t | Exception of exn

let record_backtrace () =
  if Printexc.backtrace_status ()
  then backtrace := Some (Printexc.get_backtrace ())

let preprocess_phrase ppf phr =
  let phr =
    match phr with
    | Ptop_def str ->
        let str =
          Pparse.apply_rewriters_str ~restore:true ~tool_name:"ocaml" str
        in
        Ptop_def str
    | phr -> phr
  in
  if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
  if !Clflags.dump_source then Pprintast.top_phrase ppf phr;
  phr

(* Phrase buffer that stores the last toplevel phrase (see
   [Location.input_phrase_buffer]). *)
let phrase_buffer = Buffer.create 1024

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false

let read_input_default prompt buffer len =
  output_string stdout prompt; flush stdout;
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = input_char stdin in
      Bytes.set buffer !i c;
      (* Also populate the phrase buffer as new characters are added. *)
      Buffer.add_char phrase_buffer c;
      incr i;
      if c = '\n' then raise Exit;
    done;
    (!i, false)
  with
  | End_of_file ->
      (!i, true)
  | Exit ->
      (!i, false)

let read_interactive_input = ref read_input_default

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !Clflags.noprompt then ""
      else if !first_line then "# "
      else if !Clflags.nopromptcont then ""
      else if Lexer.in_comment () then "* "
      else "  "
    in
    first_line := false;
    let (len, eof) = !read_interactive_input prompt buffer len in
    if eof then begin
      Location.echo_eof ();
      if len > 0 then got_eof := true;
      len
    end else
      len
  end

let set_paths ?(auto_include=Compmisc.auto_include) () =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  let expand = Misc.expand_directory Config.standard_library in
  let current_load_path = Load_path.get_paths () in
  let load_path = List.concat [
      [ "" ];
      List.map expand (List.rev !Compenv.first_include_dirs);
      List.map expand (List.rev !Clflags.include_dirs);
      List.map expand (List.rev !Compenv.last_include_dirs);
      current_load_path;
      [expand "+camlp4"];
    ]
  in
  Load_path.init ~auto_include load_path;
  Dll.add_path load_path

let update_search_path_from_env () =
  let extra_paths =
    let env = Sys.getenv_opt "OCAMLTOP_INCLUDE_PATH" in
    Option.fold ~none:[] ~some:Misc.split_path_contents env
  in
  Clflags.include_dirs := List.rev_append extra_paths !Clflags.include_dirs

let initialize_toplevel_env () =
  toplevel_env := Compmisc.initial_env()

external caml_sys_modify_argv : string array -> unit =
  "caml_sys_modify_argv"

let override_sys_argv new_argv =
  caml_sys_modify_argv new_argv;
  Arg.current := 0

let is_command_like_name s =
  not (String.length s = 0
       || s.[0] = '-'
       || Filename.basename s <> s
       || Filename.extension s <> "")

(* The table of toplevel directives.
   Filled by functions from module topdirs. *)

type directive_fun =
  | Directive_none of (unit -> unit)
  | Directive_string of (string -> unit)
  | Directive_int of (int -> unit)
  | Directive_ident of (Longident.t -> unit)
  | Directive_bool of (bool -> unit)

type directive_info = {
  section: string;
  doc: string;
}

let directive_table = (Hashtbl.create 23 : (string, directive_fun) Hashtbl.t)

let directive_info_table =
  (Hashtbl.create 23 : (string, directive_info) Hashtbl.t)

let add_directive name dir_fun dir_info =
  Hashtbl.add directive_table name dir_fun;
  Hashtbl.add directive_info_table name dir_info

let get_directive name =
  Hashtbl.find_opt directive_table name

let get_directive_info name =
  Hashtbl.find_opt directive_info_table name

let all_directive_names () =
  Hashtbl.fold (fun dir _ acc -> dir::acc) directive_table []

let try_run_directive ppf dir_name pdir_arg =
  begin match get_directive dir_name with
  | None ->
      fprintf ppf "Unknown directive `%s'." dir_name;
      let directives = all_directive_names () in
      Misc.did_you_mean ppf
        (fun () -> Misc.spellcheck directives dir_name);
      fprintf ppf "@.";
      false
  | Some d ->
      match d, pdir_arg with
      | Directive_none f, None -> f (); true
      | Directive_string f, Some {pdira_desc = Pdir_string s} -> f s; true
      | Directive_int f, Some {pdira_desc = Pdir_int (n,None) } ->
         begin match Misc.Int_literal_converter.int n with
         | n -> f n; true
         | exception _ ->
           fprintf ppf "Integer literal exceeds the range of \
                        representable integers for directive `%s'.@."
                   dir_name;
           false
         end
      | Directive_int _, Some {pdira_desc = Pdir_int (_, Some _)} ->
          fprintf ppf "Wrong integer literal for directive `%s'.@."
            dir_name;
          false
      | Directive_ident f, Some {pdira_desc = Pdir_ident lid} -> f lid; true
      | Directive_bool f, Some {pdira_desc = Pdir_bool b} -> f b; true
      | _ ->
          let dir_type = match d with
          | Directive_none _   -> "no argument"
          | Directive_string _ -> "a `string' literal"
          | Directive_int _    -> "an `int' literal"
          | Directive_ident _  -> "an identifier"
          | Directive_bool _   -> "a `bool' literal"
          in
          let arg_type = match pdir_arg with
          | None                              -> "no argument"
          | Some {pdira_desc = Pdir_string _} -> "a `string' literal"
          | Some {pdira_desc = Pdir_int _}    -> "an `int' literal"
          | Some {pdira_desc = Pdir_ident _}  -> "an identifier"
          | Some {pdira_desc = Pdir_bool _}   -> "a `bool' literal"
          in
          fprintf ppf "Directive `%s' expects %s, got %s.@."
            dir_name dir_type arg_type;
          false
  end
