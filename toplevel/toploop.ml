(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The interactive toplevel loop *)

open Path
open Lexing
open Format
open Config
open Misc
open Parsetree
open Types
open Typedtree

type directive_fun =
   | Directive_none of (unit -> unit)
   | Directive_string of (string -> unit)
   | Directive_int of (int -> unit)
   | Directive_ident of (Longident.t -> unit)
   | Directive_bool of (bool -> unit)

(* The table of toplevel value bindings and its accessors *)

let toplevel_value_bindings =
  (Hashtbl.create 37 : (string, Obj.t) Hashtbl.t)

let getvalue name =
  try
    Hashtbl.find toplevel_value_bindings name
  with Not_found ->
    fatal_error (name ^ " unbound at toplevel")

let setvalue name v =
  Hashtbl.replace toplevel_value_bindings name v

(* Return the value referred to by a path *)

let rec eval_path = function
  | Pident id ->
      if Ident.persistent id || Ident.global id then
        Symtable.get_global_value id
      else begin
        let name = Ident.name id in
        try
          Hashtbl.find toplevel_value_bindings name
        with Not_found ->
          raise (Symtable.Error(Symtable.Undefined_global name))
      end
  | Pdot(p, s, pos) ->
      Obj.field (eval_path p) pos
  | Papply(p1, p2) ->
      fatal_error "Toploop.eval_path"

(* To print values *)

module EvalPath = struct
  type value = Obj.t
  exception Error
  let eval_path p = try eval_path p with Symtable.Error _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
end

module Printer = Genprintval.Make(Obj)(EvalPath)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_untyped_exception = Printer.print_untyped_exception
let print_value env obj ty =
  Printer.print_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> true) env obj ty

let install_printer = Printer.install_printer
let remove_printer = Printer.remove_printer

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print
let print_warning = Location.print_warning
let input_name = Location.input_name

(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)
type evaluation_outcome = Result of Obj.t | Exception of exn

let load_lambda ppf lam =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then
    fprintf ppf "%a%a@."
    Printinstr.instrlist init_code
    Printinstr.instrlist fun_code;
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.update_global_table();
  try
    may_trace := true;
    let retval = (Meta.reify_bytecode code code_size) () in
    may_trace := false;
    if can_free then Meta.static_free code;
    Result retval
  with x ->
    may_trace := false;
    if can_free then Meta.static_free code;
    Symtable.restore_state initial_symtable;
    Exception x

(* Print the outcome of an evaluation *)

let pr_item env ppf = function
  | Tsig_value(id, decl) :: rem ->
      begin match decl.val_kind with
      | Val_prim _ ->
          fprintf ppf "@[<2>%a@]"
          (Printtyp.value_description id) decl
      | _ ->
          fprintf ppf "@[<2>%a =@ %a@]"
          (Printtyp.value_description id) decl
          (print_value env (getvalue (Translmod.toplevel_name id)))
          decl.val_type
      end;
      rem
  | Tsig_type(id, decl) :: rem ->
      fprintf ppf "@[%a@]"
      (Printtyp.type_declaration id) decl;
      rem
  | Tsig_exception(id, decl) :: rem ->
      fprintf ppf "@[%a@]"
      (Printtyp.exception_declaration id) decl;
      rem
  | Tsig_module(id, mty) :: rem ->
      fprintf ppf "@[<2>module %a :@ %a@]"
      Printtyp.ident id
      Printtyp.modtype mty;
      rem
  | Tsig_modtype(id, decl) :: rem ->
      fprintf ppf "@[%a@]" 
      (Printtyp.modtype_declaration id) decl;
      rem
  | Tsig_class(id, decl) :: cltydecl :: tydecl1 :: tydecl2 :: rem ->
      fprintf ppf "@[%a@]"
      (Printtyp.class_declaration id) decl;
      rem
  | Tsig_cltype(id, decl) :: tydecl1 :: tydecl2 :: rem ->
      fprintf ppf "@[%a@]" 
      (Printtyp.cltype_declaration id) decl;
      rem
  | _ -> []

let rec print_items env ppf = function
  | [] -> ()
  | items ->
     match pr_item env ppf items with
     | [] -> ()
     | items -> fprintf ppf "@ %a" (print_items env) items;;

(* The current typing environment for the toplevel *)

let toplevel_env = ref Env.empty

(* Print an exception produced by an evaluation *)

let print_exception_outcome ppf = function
  | Sys.Break ->
      fprintf ppf "Interrupted.@."
  | Out_of_memory ->
      Gc.full_major();
      fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | exn ->
      fprintf ppf "@[Uncaught exception:@ %a.@]@."
              (print_value !toplevel_env (Obj.repr exn)) Predef.type_exn

(* The table of toplevel directives. 
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let (str, sg, newenv) = Typemod.type_structure !toplevel_env sstr in
      let lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      let res = load_lambda ppf lam in
      begin match res with
      | Result v ->
          if print_outcome then begin
            match str with
            | [Tstr_eval exp] ->
                fprintf ppf "@[- : %a@ =@ %a@]@."
                Printtyp.type_scheme exp.exp_type
                (print_value newenv v) exp.exp_type
            | [] -> ()
            | _ ->
                fprintf ppf "@[<v>%a@]@."
                (print_items newenv) sg
          end;
          toplevel_env := newenv;
          true
      | Exception exn ->
          print_exception_outcome ppf exn;
          false
      end
  | Ptop_dir(dir_name, dir_arg) ->
      try
        match (Hashtbl.find directive_table dir_name, dir_arg) with
        | (Directive_none f, Pdir_none) -> f (); true
        | (Directive_string f, Pdir_string s) -> f s; true
        | (Directive_int f, Pdir_int n) -> f n; true
        | (Directive_ident f, Pdir_ident lid) -> f lid; true
        | (Directive_bool f, Pdir_bool b) -> f b; true
        | (_, _) ->
            fprintf ppf "Wrong type of argument for directive `%s'.@." dir_name;
            false
      with Not_found ->
        fprintf ppf "Unknown directive `%s'.@." dir_name;
        false

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

(* Read and execute commands from a file *)

let use_print_results = ref true

let use_file ppf name =
  try
    let filename = find_in_path !Config.load_path name in
    let ic = open_in_bin filename in
    let lb = Lexing.from_channel ic in
    (* Skip initial #! line if any *)
    let buffer = String.create 2 in
    if input ic buffer 0 2 = 2 && buffer = "#!"
    then ignore(input_line ic)
    else seek_in ic 0;
    let success =
      protect Location.input_name filename (fun () ->
        try
          List.iter
            (fun ph ->
              if !Clflags.dump_parsetree then Printast.top_phrase ppf ph;
              if not (execute_phrase !use_print_results ppf ph) then raise Exit)
            (!parse_use_file lb);
          true
        with
        | Exit -> false
        | Sys.Break -> fprintf ppf "Interrupted.@."; false
        | x -> Errors.report_error ppf x; false) in
    close_in ic;
    success
  with Not_found -> fprintf ppf "Cannot find file %s.@." name; false

let use_silently ppf name =
  protect use_print_results false (fun () -> use_file ppf name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    output_string stdout (if !first_line then "# " else "  "); flush stdout;
    first_line := false;
    let i = ref 0 in
    try
      while true do
        if !i >= len then raise Exit;
        let c = input_char stdin in
        buffer.[!i] <- c;
        incr i;
        if c = '\n' then raise Exit;
      done;
      !i
    with
    | End_of_file ->
        Location.echo_eof ();
        if !i > 0 then (got_eof := true; !i) else 0
    | Exit -> !i
  end

(* Discard everything already in a lexer buffer *)

let empty_lexbuf lb =
  let l = String.length lb.lex_buffer in
  lb.lex_abs_pos <- (-l);
  lb.lex_curr_pos <- l

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let _ =
  Sys.interactive := true;
  Symtable.init_toplevel();
  Clflags.thread_safe := true;
  Compile.init_path()

let load_ocamlinit ppf =
  if Sys.file_exists ".ocamlinit" then ignore(use_silently ppf ".ocamlinit")

(* The interactive loop *)

exception PPerror

let loop ppf =
  fprintf ppf "        Objective Caml version %s@.@." Config.version;
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
  toplevel_env := Compile.initial_env();
  let lb = Lexing.from_function refill_lexbuf in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  load_ocamlinit ppf;
  while true do
    try
      empty_lexbuf lb;
      Location.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> exit 0
    | Sys.Break -> fprintf ppf "Interrupted.@."
    | PPerror -> ()
    | x -> Errors.report_error ppf x
  done

(* Execute a script *)

let run_script ppf name args =
  let rec find n =
    if n >= Array.length args then invalid_arg "Toploop.run_script";
    if args.(n) = name then n else find (n+1) 
  in
  let pos = find 0 in
  let len = Array.length args - pos in
  if Array.length Sys.argv < len then invalid_arg "Toploop.run_script";
  Array.blit args pos Sys.argv 0 len;
  Obj.truncate (Obj.repr Sys.argv) len;
  Arg.current := 0;
  Compile.init_path();
  toplevel_env := Compile.initial_env();
  use_silently ppf name
