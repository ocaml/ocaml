(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type directive_fun =
   | Directive_none of (unit -> unit)
   | Directive_string of (string -> unit)
   | Directive_int of (int -> unit)
   | Directive_ident of (Longident.t -> unit)
   | Directive_bool of (bool -> unit)

(* Return the value referred to by a path *)

let eval_path_ref : (Path.t -> Obj.t) ref =
  ref (fun _ -> failwith "eval_path_ref undefined")
let set_eval_path = (:=) eval_path_ref

let eval_path env path =
  !eval_path_ref (Env.normalize_path (Some Location.none) env path)

(* To print values *)

module EvalPath = struct
  type valu = Obj.t
  exception Error
  let eval_path env p = try eval_path env p with _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
end

module Printer = Genprintval.Make(Obj)(EvalPath)

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

let print_untyped_exception ppf obj =
  !print_out_value ppf (Printer.outval_of_untyped_exception obj)
let outval_of_value env obj ty =
  Printer.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty
let print_value env obj ppf ty =
  !print_out_value ppf (outval_of_value env obj ty)

type ('a, 'b) gen_printer = ('a, 'b) Genprintval.gen_printer =
  | Zero of 'b
  | Succ of ('a -> ('a, 'b) gen_printer)

let install_printer = Printer.install_printer
let install_generic_printer = Printer.install_generic_printer
let install_generic_printer' = Printer.install_generic_printer'
let remove_printer = Printer.remove_printer


(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print_error (* FIXME change back to print *)
let print_error = Location.print_error
let print_warning = Location.print_warning
let input_name = Location.input_name

let parse_mod_use_file name lb =
  let open Parsetree in
  let modname =
    String.capitalize_ascii (Filename.chop_extension (Filename.basename name))
  in
  let items =
    List.concat
      (List.map
         (function Ptop_def s -> s | Ptop_dir _ -> [])
         (!parse_use_file lb))
  in
  [ Ptop_def
      Ast_helper.[
        Str.module_
          (Mb.mk (Location.mknoloc modname) (Mod.structure items))
      ]
   ]

(* Hooks for initialization *)

let toplevel_startup_hook = ref (fun () -> ())

(* Print the outcome of an evaluation *)

let pr_item toplevel_value =
  let open Types in
  Printtyp.print_items
    (fun env -> function
      | Sig_value(id, {val_kind = Val_reg; val_type}) ->
          Some (outval_of_value env (toplevel_value id) val_type)
      | _ -> None
    )

(* The current typing environment for the toplevel *)

let toplevel_env = ref Env.empty

(* Print an exception produced by an evaluation *)

let print_out_exception ppf exn outv =
  !print_out_phrase ppf (Outcometree.Ophr_exception (exn, outv))

let print_exception_outcome ppf exn =
  if exn = Out_of_memory then Gc.full_major ();
  let outv = outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn in
  print_out_exception ppf exn outv


(* The table of toplevel directives.
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

(* Temporary assignment to a reference *)

let protect r newval body =
  let oldval = !r in
  try
    r := newval;
    let res = body() in
    r := oldval;
    res
  with x ->
    r := oldval;
    raise x

(* Execute a toplevel phrase *)

let execute_phrase_ref :
  (bool -> Format.formatter -> Parsetree.toplevel_phrase -> bool) ref
  =
  ref (fun _ _ _ -> failwith "Toploop.execute_phrase not implemented")

let set_execute_phrase = (:=) execute_phrase_ref

let execute_phrase print_outcome ppf phr =
  !execute_phrase_ref print_outcome ppf phr

(* Execute a toplevel directive *)

let execute_topdir ppf dir_name dir_arg =
  let open Parsetree in
  match Hashtbl.find directive_table dir_name, dir_arg with
  | exception Not_found ->
      Format.fprintf ppf "Unknown directive `%s'.@." dir_name;
      false
  | Directive_none f, Pdir_none -> f (); true
  | Directive_string f, Pdir_string s -> f s; true
  | Directive_int f, Pdir_int n -> f n; true
  | Directive_ident f, Pdir_ident lid -> f lid; true
  | Directive_bool f, Pdir_bool b -> f b; true
  | _ ->
      Format.fprintf ppf "Wrong type of argument for directive `%s'.@."
        dir_name;
      false

(* Read and execute commands from a file, or from stdin if [name] is "". *)

let use_print_results = ref true

let preprocess_phrase ppf phr =
  let phr =
    let open Parsetree in
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

let use_file ppf wrap_mod name =
  try
    let (filename, ic, must_close) =
      if name = "" then
        ("(stdin)", stdin, false)
      else begin
        let filename = Misc.find_in_path !Config.load_path name in
        let ic = open_in_bin filename in
        (filename, ic, true)
      end
    in
    let lb = Lexing.from_channel ic in
    Location.init lb filename;
    (* Skip initial #! line if any *)
    Lexer.skip_sharp_bang lb;
    let success =
      protect Location.input_name filename (fun () ->
        try
          List.iter
            (fun ph ->
              let ph = preprocess_phrase ppf ph in
              if not (execute_phrase !use_print_results ppf ph) then raise Exit)
            (if wrap_mod then
               parse_mod_use_file name lb
             else
               !parse_use_file lb);
          true
        with
        | Exit -> false
        | Sys.Break -> Format.fprintf ppf "Interrupted.@."; false
        | x -> Location.report_exception ppf x; false) in
    if must_close then close_in ic;
    success
  with Not_found -> Format.fprintf ppf "Cannot find file %s.@." name; false

let mod_use_file ppf name = use_file ppf true name
let use_file ppf name = use_file ppf false name

let use_silently ppf name =
  protect use_print_results false (fun () -> use_file ppf name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let read_input_default prompt buffer len =
  output_string Pervasives.stdout prompt; flush Pervasives.stdout;
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = input_char Pervasives.stdin in
      Bytes.set buffer !i c;
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

exception PPerror

(* Toplevel initialization. *)

let load_ocamlinit ppf =
  if !Clflags.noinit then ()
  else match !Clflags.init_file with
  | Some f -> if Sys.file_exists f then ignore (use_silently ppf f)
              else Format.fprintf ppf "Init file not found: \"%s\".@." f
  | None ->
     if Sys.file_exists ".ocamlinit" then ignore (use_silently ppf ".ocamlinit")
     else try
       let home_init = Filename.concat (Sys.getenv "HOME") ".ocamlinit" in
       if Sys.file_exists home_init then ignore (use_silently ppf home_init)
     with Not_found -> ()
;;

let initialize_toplevel_env () =
  toplevel_env := Compmisc.initial_env()

(* The interactive loop *)

let loop_no_header ppf =
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Location.init lb "//toplevel//";
  Location.input_name := "//toplevel//";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  load_ocamlinit ppf;
  while true do
    let snap = Btype.snapshot () in
    try
      Lexing.flush_input lb;
      Location.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      let phr = preprocess_phrase ppf phr  in
      Env.reset_cache_toplevel ();
      if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
      if !Clflags.dump_source then Pprintast.top_phrase ppf phr;
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> exit 0
    | Sys.Break -> Format.fprintf ppf "Interrupted.@."; Btype.backtrack snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack snap
  done

(* Execute a script.  If [name] is "", read the script from stdin. *)

let run_script ppf name args =
  let len = Array.length args in
  if Array.length Sys.argv < len then invalid_arg "Toploop.run_script";
  Array.blit args 0 Sys.argv 0 len;
  Obj.truncate (Obj.repr Sys.argv) len;
  Arg.current := 0;
  Compmisc.init_path ~dir:(Filename.dirname name) true;
                   (* Note: would use [Filename.abspath] here, if we had it. *)
  toplevel_env := Compmisc.initial_env();
  Sys.interactive := false;
  let explicit_name =
    (* Prevent use_silently from searching in the path. *)
    if Filename.is_implicit name
    then Filename.concat Filename.current_dir_name name
    else name
  in
  use_silently ppf explicit_name
