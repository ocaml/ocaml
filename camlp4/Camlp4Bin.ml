(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
(* camlp4r *)
(* $Id$ *)

open Camlp4;
open PreCast.Syntax;
open PreCast;
open Format;

value print_warning = eprintf "%a:\n%s@." Loc.print;

value rec parse_file dyn_loader name pa getdir =
  let directive_handler = Some (fun ast ->
    match getdir ast with
    [ Some x ->
        match x with
        [ (_, "load", s) -> do { DynLoader.load dyn_loader s; None }
        | (_, "directory", s) -> do { DynLoader.include_dir dyn_loader s; None }
        | (_, "use", s) -> Some (parse_file dyn_loader s pa getdir)
        | (loc, _, _) -> Loc.raise loc (Stream.Error "bad directive") ]
    | None -> None ]) in
  let loc = Loc.mk name
  in do {
    Warning.current.val := print_warning;
    let ic = if name = "-" then stdin else open_in_bin name in
    let cs = Stream.of_channel ic in
    let clear () = if name = "-" then () else close_in ic in
    let phr =
      try pa ?directive_handler loc cs
      with x -> do { clear (); raise x }
    in
    clear ();
    phr
  };

value output_file = ref None;

value process dyn_loader name pa pr fold_filters getdir =
  let ast = parse_file dyn_loader name pa getdir in
  let ast = fold_filters (fun t filter -> filter t) ast in
  pr ?input_file:(Some name) ?output_file:output_file.val ast;

value gind =
  fun
  [ <:sig_item@loc< # $n$ $str:s$ >> -> Some (loc, n, s)
  | _ -> None ];

value gimd =
  fun
  [ <:str_item@loc< # $n$ $str:s$ >> -> Some (loc, n, s)
  | _ -> None ];

open Register;

value process_intf dyn_loader name =
  process dyn_loader name CurrentParser.parse_interf CurrentPrinter.print_interf
          AstFilters.fold_interf_filters gind;
value process_impl dyn_loader name =
  process dyn_loader name CurrentParser.parse_implem CurrentPrinter.print_implem
          AstFilters.fold_implem_filters gimd;

value just_print_the_version () =
  do { printf "%s@." Config.version; exit 0 };

value print_version () =
  do { eprintf "Camlp4 version %s@." Config.version; exit 0 };

value print_stdlib () =
  do { printf "%s@." Config.camlp4_standard_library; exit 0 };

value usage ini_sl ext_sl =
  do {
    eprintf "\
Usage: camlp4 [load-options] [--] [other-options]
Options:
<file>.ml        Parse this implementation file
<file>.mli       Parse this interface file
<file>.(cmo|cma) Load this module inside the Camlp4 core@.";
    Options.print_usage_list ini_sl;
    (* loop (ini_sl @ ext_sl) where rec loop =
      fun
      [ [(y, _, _) :: _] when y = "-help" -> ()
      | [_ :: sl] -> loop sl
      | [] -> eprintf "  -help         Display this list of options.@." ];    *)
    if ext_sl <> [] then do {
      eprintf "Options added by loaded object files:@.";
      Options.print_usage_list ext_sl;
    }
    else ();
  };

value warn_noassert () =
  do {
    eprintf "\
camlp4 warning: option -noassert is obsolete
You should give the -noassert option to the ocaml compiler instead.@.";
  };

type file_kind =
  [ Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string ];

value file_name_filter =
  let map = [
    ("pa_r.cmo", "Camlp4Parsers/OCamlr.cmo");
    ("pa_o.cmo", "Camlp4Parsers/OCaml.cmo");
    (* ("q_MLast.cmo", "Camlp4Parsers/OCamlRevisedQuotation.cmo"); *)
    ("pa_rp.cmo", "Camlp4Parsers/OCamlRevisedParser.cmo");
    ("pa_op.cmo", "Camlp4Parsers/OCamlParser.cmo");
    ("pa_extend.cmo", "Camlp4Parsers/Grammar.cmo");
    ("pa_extend_m.cmo", "Camlp4Parsers/Grammar.cmo");
    ("pa_macro.cmo", "Camlp4Parsers/Macro.cmo");
    ("pa_extfun.cmo", "Camlp4Parsers/Extfun.cmo");
    ("pr_dump.cmo", "Camlp4Printers/Dump.cmo");
    ("pr_o.cmo", "Camlp4Printers/OCaml.cmo");
    ("pr_r.cmo", "Camlp4Printers/OCamlr.cmo")
  ] in
  fun file_name ->
    try List.assoc file_name map with [ Not_found -> file_name ];


value search_stdlib = ref True;
value print_loaded_modules = ref False;
value rcall_callback = ref (fun () -> ());
value dyn_loader = ref None;
value (task, do_task) =
  let t = ref None
  in (fun f x ->
         t.val := Some (if t.val = None then (fun _ -> f x)
                        else (fun usage -> usage ())),
      fun usage -> match t.val with [ Some f -> f usage | None -> () ]);
value input_file x =
  let dyn_loader = match dyn_loader.val with [ None -> assert False | Some d -> d ] in (* FIXME *)
  do {
    rcall_callback.val (); 
    match x with
    [ Intf file_name -> task (process_intf dyn_loader) (file_name_filter file_name)
    | Impl file_name -> task (process_impl dyn_loader) (file_name_filter file_name)
    | Str s -> do {
        let (f, o) = Filename.open_temp_file "from_string" ".ml";
        output_string o s;
        close_out o;
        task (process_impl dyn_loader) f;
      }
    | ModuleImpl file_name -> DynLoader.load dyn_loader (file_name_filter file_name)
    | IncludeDir dir -> DynLoader.include_dir dyn_loader dir ];
    rcall_callback.val ();
  };

value initial_spec_list =
  [("-I", Arg.String (fun x -> input_file (IncludeDir x)),
    "<directory>  Add directory in search patch for object files.");
  ("-where", Arg.Unit print_stdlib,
    "Print camlp4 library directory and exit.");
  ("-nolib", Arg.Clear search_stdlib,
    "No automatic search for object files in library directory.");
  ("-intf", Arg.String (fun x -> input_file (Intf x)),
    "<file>  Parse <file> as an interface, whatever its extension.");
  ("-impl", Arg.String (fun x -> input_file (Impl x)),
    "<file>  Parse <file> as an implementation, whatever its extension.");
  ("-str", Arg.String (fun x -> input_file (Str x)),
    "<string>  Parse <string> as an implementation.");
  ("-unsafe", Arg.Set Config.unsafe,
    "Generate unsafe accesses to array and strings.");
  ("-noassert", Arg.Unit warn_noassert,
    "Obsolete, do not use this option.");
  ("-verbose", Arg.Set Config.verbose,
    "More verbose in parsing errors.");
  ("-loc", Arg.Set_string Loc.name,
    "<name>   Name of the location variable (default: " ^ Loc.name.val ^ ").");
  ("-QD", Arg.String (fun x -> Quotation.dump_file.val := Some x),
    "<file> Dump quotation expander result in case of syntax error.");
  ("-o", Arg.String (fun x -> output_file.val := Some x),
    "<file> Output on <file> instead of standard output.");
  ("-v", Arg.Unit print_version,
    "Print Camlp4 version and exit.");
  ("-version", Arg.Unit just_print_the_version,
    "Print Camlp4 version number and exit.");
  ("-no_quot", Arg.Clear Config.quotations,
    "Don't parse quotations, allowing to use, e.g. \"<:>\" as token.");
  ("-loaded-modules", Arg.Set print_loaded_modules, "Print the list of loaded modules.");
  ("-parser", Arg.String (fun x -> input_file (ModuleImpl ("Camlp4Parsers/"^x^".cmo"))),
    "<name>  Load the parser Camlp4Parsers/<name>.cmo");
  ("-printer", Arg.String (fun x -> input_file (ModuleImpl ("Camlp4Printers/"^x^".cmo"))),
    "<name>  Load the printer Camlp4Printers/<name>.cmo");
  ("-filter", Arg.String (fun x -> input_file (ModuleImpl ("Camlp4Filters/"^x^".cmo"))),
    "<name>  Load the filter Camlp4Filters/<name>.cmo")
];

Options.init initial_spec_list;

value anon_fun name =
  input_file
  (if Filename.check_suffix name ".mli" then Intf name
    else if Filename.check_suffix name ".ml" then Impl name
    else if Filename.check_suffix name ".cmo" then ModuleImpl name
    else if Filename.check_suffix name ".cma" then ModuleImpl name
    else raise (Arg.Bad ("don't know what to do with " ^ name)));

value main ?(callback = fun _ -> ()) argv =
  let usage () = do { usage initial_spec_list (Options.ext_spec_list ()); exit 0 } in
  let loaded_modules = Queue.create () in
  try do {
    dyn_loader.val := Some (DynLoader.mk ~ocaml_stdlib:search_stdlib.val
                                         ~camlp4_stdlib:search_stdlib.val ());
    let call_callback () =
      callback (fun (name, module_callback) ->
                  let () = Queue.push name loaded_modules in
                  module_callback ());
    call_callback ();
    rcall_callback.val := call_callback;
    match Options.parse anon_fun argv with
    [ [] -> ()
    | ["-help" :: _] -> usage ()
    | [s :: _] ->
        do { eprintf "%s: unknown or misused option\n" s;
            eprintf "Use option -help for usage@.";
            exit 2 } ];
    do_task usage;
    if print_loaded_modules.val then do {
      Queue.iter (eprintf "%s@.") loaded_modules;
    } else ()
  }
  with
  [ Arg.Bad s -> do { eprintf "Error: %s\n" s;
                      eprintf "Use option -help for usage@.";
                      exit 2 }
  | Arg.Help _ -> usage ()
  | exc -> do { eprintf "@[<v0>%a@]@." ErrorHandler.print exc; exit 2 } ];

main ~callback:Register.iter_and_take_callbacks Sys.argv;
