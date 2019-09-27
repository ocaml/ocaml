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

(* The interactive toplevel loop *)

open Format
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Ast_helper

type res = Ok of Obj.t | Err of string
type evaluation_outcome = Result of Obj.t | Exception of exn

let _dummy = (Ok (Obj.magic 0), Err "")

external ndl_run_toplevel: string -> string -> res
  = "caml_natdynlink_run_toplevel"

let global_symbol id =
  let sym = Compilenv.symbol_for_global id in
  match Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym with
  | None ->
    fatal_error ("Opttoploop.global_symbol " ^ (Ident.unique_name id))
  | Some obj -> obj

let need_symbol sym =
  Option.is_none (Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym)

let dll_run dll entry =
  match (try Result (Obj.magic (ndl_run_toplevel dll entry))
         with exn -> Exception exn)
  with
    | Exception _ as r -> r
    | Result r ->
        match Obj.magic r with
          | Ok x -> Result x
          | Err s -> fatal_error ("Opttoploop.dll_run " ^ s)


type directive_fun =
   | Directive_none of (unit -> unit)
   | Directive_string of (string -> unit)
   | Directive_int of (int -> unit)
   | Directive_ident of (Longident.t -> unit)
   | Directive_bool of (bool -> unit)


let remembered = ref Ident.empty

let rec remember phrase_name i = function
  | [] -> ()
  | Sig_value  (id, _, _) :: rest
  | Sig_module (id, _, _, _, _) :: rest
  | Sig_typext (id, _, _, _) :: rest
  | Sig_class  (id, _, _, _) :: rest ->
      remembered := Ident.add id (phrase_name, i) !remembered;
      remember phrase_name (succ i) rest
  | _ :: rest -> remember phrase_name i rest

let toplevel_value id =
  try Ident.find_same id !remembered
  with _ -> Misc.fatal_error @@ "Unknown ident: " ^ Ident.unique_name id

let close_phrase lam =
  let open Lambda in
  Ident.Set.fold (fun id l ->
    let glb, pos = toplevel_value id in
    let glob =
      Lprim (Pfield pos,
             [Lprim (Pgetglobal glb, [], Location.none)],
             Location.none)
    in
    Llet(Strict, Pgenval, id, glob, l)
  ) (free_variables lam) lam

let toplevel_value id =
  let glob, pos =
    if Config.flambda then toplevel_value id else Translmod.nat_toplevel_name id
  in
  (Obj.magic (global_symbol glob)).(pos)

(* Return the value referred to by a path *)

let rec eval_address = function
  | Env.Aident id ->
      if Ident.persistent id || Ident.global id
      then global_symbol id
      else toplevel_value id
  | Env.Adot(a, pos) ->
      Obj.field (eval_address a) pos

let eval_path find env path =
  match find path env with
  | addr -> eval_address addr
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))

let eval_module_path env path =
  eval_path Env.find_module_address env path

let eval_value_path env path =
  eval_path Env.find_value_address env path

let eval_extension_path env path =
  eval_path Env.find_constructor_address env path

let eval_class_path env path =
  eval_path Env.find_class_address env path

(* To print values *)

module EvalPath = struct
  type valu = Obj.t
  exception Error
  let eval_address addr =
    try eval_address addr with _ -> raise Error
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
             (Location.mknoloc modname)
             (Mod.structure items)
          )
       ]
   ]

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

(* Load in-core and execute a lambda term *)

let phrase_seqid = ref 0
let phrase_name = ref "TOP"

(* CR-soon trefis for mshinwell: copy/pasted from Optmain. Should it be shared
   or?
   mshinwell: It should be shared, but after 4.03. *)
module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end
let backend = (module Backend : Backend_intf.S)

let load_lambda ppf ~module_ident ~required_globals lam size =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;

  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ !phrase_name) ext_dll
  in
  let fn = Filename.chop_extension dll in
  if not Config.flambda then
    Asmgen.compile_implementation_clambda
      ~toplevel:need_symbol fn ~backend ~ppf_dump:ppf
      { Lambda.code=slam ; main_module_block_size=size;
        module_ident; required_globals }
  else
    Asmgen.compile_implementation_flambda
      ~required_globals ~backend ~toplevel:need_symbol fn ~ppf_dump:ppf
      (Flambda_middle_end.middle_end ~ppf_dump:ppf ~prefixname:fn ~backend ~size
         ~module_ident ~module_initializer:slam ~filename:"toplevel");
  Asmlink.call_linker_shared [fn ^ ext_obj] dll;
  Sys.remove (fn ^ ext_obj);

  let dll =
    if Filename.is_implicit dll
    then Filename.concat (Sys.getcwd ()) dll
    else dll in
  let res = dll_run dll !phrase_name in
  (try Sys.remove dll with Sys_error _ -> ());
  (* note: under windows, cannot remove a loaded dll
     (should remember the handles, close them in at_exit, and then remove
     files) *)
  res

(* Print the outcome of an evaluation *)

let pr_item =
  Printtyp.print_items
    (fun env -> function
      | Sig_value(id, {val_kind = Val_reg; val_type}, _) ->
          Some (outval_of_value env (toplevel_value id) val_type)
      | _ -> None
    )

(* The current typing environment for the toplevel *)

let toplevel_env = ref Env.empty

(* Print an exception produced by an evaluation *)

let print_out_exception ppf exn outv =
  !print_out_phrase ppf (Ophr_exception (exn, outv))

let print_exception_outcome ppf exn =
  if exn = Out_of_memory then Gc.full_major ();
  let outv = outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn in
  print_out_exception ppf exn outv

(* The table of toplevel directives.
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      incr phrase_seqid;
      phrase_name := Printf.sprintf "TOP%i" !phrase_seqid;
      Compilenv.reset ?packname:None !phrase_name;
      Typecore.reset_delayed_checks ();
      let sstr, rewritten =
        match sstr with
        | [ { pstr_desc = Pstr_eval (e, attrs) ; pstr_loc = loc } ]
        | [ { pstr_desc = Pstr_value (Asttypes.Nonrecursive,
                                      [{ pvb_expr = e
                                       ; pvb_pat = { ppat_desc = Ppat_any ; _ }
                                       ; pvb_attributes = attrs
                                       ; _ }])
            ; pstr_loc = loc }
          ] ->
            let pat = Ast_helper.Pat.var (Location.mknoloc "_$") in
            let vb = Ast_helper.Vb.mk ~loc ~attrs pat e in
            [ Ast_helper.Str.value ~loc Asttypes.Nonrecursive [vb] ], true
        | _ -> sstr, false
      in
      let (str, sg, names, newenv) = Typemod.type_toplevel_phrase oldenv sstr in
      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
      let sg' = Typemod.Signature_names.simplify newenv names sg in
      (* Why is this done? *)
      ignore (Includemod.signatures oldenv sg sg');
      Typecore.force_delayed_checks ();
      let module_ident, res, required_globals, size =
        if Config.flambda then
          let { Lambda.module_ident; main_module_block_size = size;
                required_globals; code = res } =
            Translmod.transl_implementation_flambda !phrase_name
              (str, Tcoerce_none)
          in
          remember module_ident 0 sg';
          module_ident, close_phrase res, required_globals, size
        else
          let size, res = Translmod.transl_store_phrases !phrase_name str in
          Ident.create_persistent !phrase_name, res, Ident.Set.empty, size
      in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        let res = load_lambda ppf ~required_globals ~module_ident res size in
        let out_phr =
          match res with
          | Result _ ->
              if Config.flambda then
                (* CR-someday trefis: *)
                ()
              else
                Compilenv.record_global_approx_toplevel ();
              if print_outcome then
                Printtyp.wrap_printing_env ~error:false oldenv (fun () ->
                match str.str_items with
                | [] -> Ophr_signature []
                | _ ->
                    if rewritten then
                      match sg' with
                      | [ Sig_value (id, vd, _) ] ->
                          let outv =
                            outval_of_value newenv (toplevel_value id)
                              vd.val_type
                          in
                          let ty = Printtyp.tree_of_type_scheme vd.val_type in
                          Ophr_eval (outv, ty)
                      | _ -> assert false
                    else
                      Ophr_signature (pr_item oldenv sg'))
              else Ophr_signature []
          | Exception exn ->
              toplevel_env := oldenv;
              if exn = Out_of_memory then Gc.full_major();
              let outv =
                outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        !print_out_phrase ppf out_phr;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; raise x
      end
  | Ptop_dir {pdir_name = {Location.txt = dir_name}; pdir_arg } ->
      let d =
        try Some (Hashtbl.find directive_table dir_name)
        with Not_found -> None
      in
      begin match d with
      | None ->
          fprintf ppf "Unknown directive `%s'.@." dir_name;
          false
      | Some d ->
          match d, pdir_arg with
          | Directive_none f, None -> f (); true
          | Directive_string f, Some {pdira_desc = Pdir_string s} -> f s; true
          | Directive_int f, Some {pdira_desc = Pdir_int (n,None)} ->
             begin match Int_literal_converter.int n with
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
              fprintf ppf "Wrong type of argument for directive `%s'.@."
                dir_name;
              false
      end

(* Read and execute commands from a file, or from stdin if [name] is "". *)

let use_print_results = ref true

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

let use_file ppf wrap_mod name =
  try
    let (filename, ic, must_close) =
      if name = "" then
        ("(stdin)", stdin, false)
      else begin
        let filename = Load_path.find name in
        let ic = open_in_bin filename in
        (filename, ic, true)
      end
    in
    let lb = Lexing.from_channel ic in
    Location.init lb filename;
    (* Skip initial #! line if any *)
    Lexer.skip_hash_bang lb;
    let success =
      protect_refs [ R (Location.input_name, filename) ] (fun () ->
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
        | Sys.Break -> fprintf ppf "Interrupted.@."; false
        | x -> Location.report_exception ppf x; false) in
    if must_close then close_in ic;
    success
  with Not_found -> fprintf ppf "Cannot find file %s.@." name; false

let mod_use_file ppf name = use_file ppf true name
let use_file ppf name = use_file ppf false name

let use_silently ppf name =
  protect_refs [ R (use_print_results, false) ] (fun () -> use_file ppf name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let read_input_default prompt buffer len =
  output_string stdout prompt; flush stdout;
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = input_char stdin in
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

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let _ =
  Sys.interactive := true;
  Compmisc.init_path ();
  Clflags.dlcode := true;
  ()

let load_ocamlinit ppf =
  if !Clflags.noinit then ()
  else match !Clflags.init_file with
  | Some f -> if Sys.file_exists f then ignore (use_silently ppf f)
              else fprintf ppf "Init file not found: \"%s\".@." f
  | None ->
     if Sys.file_exists ".ocamlinit" then ignore (use_silently ppf ".ocamlinit")
     else try
       let home_init = Filename.concat (Sys.getenv "HOME") ".ocamlinit" in
       if Sys.file_exists home_init then ignore (use_silently ppf home_init)
     with Not_found -> ()
;;

let set_paths () =
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
  Load_path.init load_path

let initialize_toplevel_env () =
  toplevel_env := Compmisc.initial_env()

(* The interactive loop *)

exception PPerror

let loop ppf =
  Location.formatter_for_warnings := ppf;
  if not !Clflags.noversion then
    fprintf ppf "        OCaml version %s - native toplevel@.@." Config.version;
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Location.init lb "//toplevel//";
  Location.input_name := "//toplevel//";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  run_hooks After_setup;
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
    | Sys.Break -> fprintf ppf "Interrupted.@."; Btype.backtrack snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack snap
  done

external caml_sys_modify_argv : string array -> unit =
  "caml_sys_modify_argv"

let override_sys_argv new_argv =
  caml_sys_modify_argv new_argv;
  Arg.current := 0

(* Execute a script.  If [name] is "", read the script from stdin. *)

let run_script ppf name args =
  override_sys_argv args;
  Compmisc.init_path ~dir:(Filename.dirname name) ();
                   (* Note: would use [Filename.abspath] here, if we had it. *)
  toplevel_env := Compmisc.initial_env();
  Sys.interactive := false;
  run_hooks After_setup;
  let explicit_name =
    (* Prevent use_silently from searching in the path. *)
    if Filename.is_implicit name
    then Filename.concat Filename.current_dir_name name
    else name
  in
  use_silently ppf explicit_name
