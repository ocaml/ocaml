(* camlp4r *)
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



open Camlp4;
open PreCast.Syntax;
open PreCast;
open Format;
module CleanAst = Camlp4.Struct.CleanAst.Make Ast;
module SSet = Set.Make String;

value pa_r  = "Camlp4OCamlRevisedParser";
value pa_rr = "Camlp4OCamlReloadedParser";
value pa_o  = "Camlp4OCamlParser";
value pa_rp = "Camlp4OCamlRevisedParserParser";
value pa_op = "Camlp4OCamlParserParser";
value pa_g  = "Camlp4GrammarParser";
value pa_m  = "Camlp4MacroParser";
value pa_qb = "Camlp4QuotationCommon";
value pa_q  = "Camlp4QuotationExpander";
value pa_rq = "Camlp4OCamlRevisedQuotationExpander";
value pa_oq = "Camlp4OCamlOriginalQuotationExpander";
value pa_l  = "Camlp4ListComprehension";

open Register;

value dyn_loader = ref (fun []);
value rcall_callback = ref (fun () -> ());
value loaded_modules = ref SSet.empty;
value add_to_loaded_modules name =
  loaded_modules.val := SSet.add name loaded_modules.val;

value (objext,libext) =
  if DynLoader.is_native then (".cmxs",".cmxs")
  else (".cmo",".cma");

value rewrite_and_load n x =
  let dyn_loader = dyn_loader.val () in
  let find_in_path = DynLoader.find_in_path dyn_loader in
  let real_load name = do {
    add_to_loaded_modules name;
    DynLoader.load dyn_loader name
  } in
  let load = List.iter begin fun n ->
    if SSet.mem n loaded_modules.val || List.mem n Register.loaded_modules.val then ()
    else begin
      add_to_loaded_modules n;
      DynLoader.load dyn_loader (n ^ objext);
    end
  end in
  do {
    match (n, String.lowercase x) with
    [ ("Parsers"|"", "pa_r.cmo"      | "r"  | "ocamlr" | "ocamlrevised" | "camlp4ocamlrevisedparser.cmo") -> load [pa_r]
    | ("Parsers"|"", "rr" | "reloaded" | "ocamlreloaded" | "camlp4ocamlreloadedparser.cmo") -> load [pa_rr]
    | ("Parsers"|"", "pa_o.cmo"      | "o"  | "ocaml" | "camlp4ocamlparser.cmo") -> load [pa_r; pa_o]
    | ("Parsers"|"", "pa_rp.cmo"     | "rp" | "rparser" | "camlp4ocamlrevisedparserparser.cmo") -> load [pa_r; pa_o; pa_rp]
    | ("Parsers"|"", "pa_op.cmo"     | "op" | "parser" | "camlp4ocamlparserparser.cmo") -> load [pa_r; pa_o; pa_rp; pa_op]
    | ("Parsers"|"", "pa_extend.cmo" | "pa_extend_m.cmo" | "g" | "grammar" | "camlp4grammarparser.cmo") -> load [pa_g]
    | ("Parsers"|"", "pa_macro.cmo"  | "m"  | "macro" | "camlp4macroparser.cmo") -> load [pa_m]
    | ("Parsers"|"", "q" | "camlp4quotationexpander.cmo") -> load [pa_qb; pa_q]
    | ("Parsers"|"", "q_mlast.cmo" | "rq" | "camlp4ocamlrevisedquotationexpander.cmo") -> load [pa_qb; pa_rq]
    | ("Parsers"|"", "oq" | "camlp4ocamloriginalquotationexpander.cmo") -> load [pa_r; pa_o; pa_qb; pa_oq]
    | ("Parsers"|"", "rf") -> load [pa_r; pa_rp; pa_qb; pa_q; pa_g; pa_l; pa_m]
    | ("Parsers"|"", "of") -> load [pa_r; pa_o; pa_rp; pa_op; pa_qb; pa_rq; pa_g; pa_l; pa_m]
    | ("Parsers"|"", "comp" | "camlp4listcomprehension.cmo") -> load [pa_l]
    | ("Filters"|"", "lift" | "camlp4astlifter.cmo") -> load ["Camlp4AstLifter"]
    | ("Filters"|"", "exn" | "camlp4exceptiontracer.cmo") -> load ["Camlp4ExceptionTracer"]
    | ("Filters"|"", "prof" | "camlp4profiler.cmo") -> load ["Camlp4Profiler"]
    (* map is now an alias of fold since fold handles map too *)
    | ("Filters"|"", "map" | "camlp4mapgenerator.cmo") -> load ["Camlp4FoldGenerator"]
    | ("Filters"|"", "fold" | "camlp4foldgenerator.cmo") -> load ["Camlp4FoldGenerator"]
    | ("Filters"|"", "meta" | "camlp4metagenerator.cmo") -> load ["Camlp4MetaGenerator"]
    | ("Filters"|"", "trash" | "camlp4trashremover.cmo") -> load ["Camlp4TrashRemover"]
    | ("Filters"|"", "striploc" | "camlp4locationstripper.cmo") -> load ["Camlp4LocationStripper"]
    | ("Printers"|"", "pr_r.cmo" | "r" | "ocamlr" | "camlp4ocamlrevisedprinter.cmo") ->
        Register.enable_ocamlr_printer ()
    | ("Printers"|"", "pr_o.cmo" | "o" | "ocaml" | "camlp4ocamlprinter.cmo") ->
        Register.enable_ocaml_printer ()
    | ("Printers"|"", "pr_dump.cmo" | "p" | "dumpocaml" | "camlp4ocamlastdumper.cmo") ->
        Register.enable_dump_ocaml_ast_printer ()
    | ("Printers"|"", "d" | "dumpcamlp4" | "camlp4astdumper.cmo") ->
        Register.enable_dump_camlp4_ast_printer ()
    | ("Printers"|"", "a" | "auto" | "camlp4autoprinter.cmo") ->
        load ["Camlp4AutoPrinter"]
    | _ ->
      let y = "Camlp4"^n^"/"^x^objext in
      real_load (try find_in_path y with [ Not_found -> x ]) ];
    rcall_callback.val ();
  };

value print_warning = eprintf "%a:\n%s@." Loc.print;

value rec parse_file dyn_loader name pa getdir =
  let directive_handler = Some (fun ast ->
    match getdir ast with
    [ Some x ->
        match x with
        [ (_, "load", s) -> do { rewrite_and_load "" s; None }
        | (_, "directory", s) -> do { DynLoader.include_dir dyn_loader s; None }
        | (_, "use", s) -> Some (parse_file dyn_loader s pa getdir)
        | (_, "default_quotation", s) -> do { Quotation.default.val := s; None }
        | (loc, _, _) -> Loc.raise loc (Stream.Error "bad directive") ]
    | None -> None ]) in
  let loc = Loc.mk name
  in do {
    current_warning.val := print_warning;
    let ic = if name = "-" then stdin else open_in_bin name;
    let cs = Stream.of_channel ic;
    let clear () = if name = "-" then () else close_in ic;
    let phr =
      try pa ?directive_handler loc cs
      with x -> do { clear (); raise x };
    clear ();
    phr
  };

value output_file = ref None;

value process dyn_loader name pa pr clean fold_filters getdir =
  let ast = parse_file dyn_loader name pa getdir in
  let ast = fold_filters (fun t filter -> filter t) ast in
  let ast = clean ast in
  pr ?input_file:(Some name) ?output_file:output_file.val ast;

value gind =
  fun
  [ <:sig_item@loc< # $n$ $str:s$ >> -> Some (loc, n, s)
  | _ -> None ];

value gimd =
  fun
  [ <:str_item@loc< # $n$ $str:s$ >> -> Some (loc, n, s)
  | _ -> None ];

value process_intf dyn_loader name =
  process dyn_loader name CurrentParser.parse_interf CurrentPrinter.print_interf
          (new CleanAst.clean_ast)#sig_item
          AstFilters.fold_interf_filters gind;
value process_impl dyn_loader name =
  process dyn_loader name CurrentParser.parse_implem CurrentPrinter.print_implem
          (new CleanAst.clean_ast)#str_item
          AstFilters.fold_implem_filters gimd;

value just_print_the_version () =
  do { printf "%s@." Camlp4_config.version; exit 0 };

value print_version () =
  do { eprintf "Camlp4 version %s@." Camlp4_config.version; exit 0 };

value print_stdlib () =
  do { printf "%s@." Camlp4_config.camlp4_standard_library; exit 0 };

value usage ini_sl ext_sl =
  do {
    eprintf "\
Usage: camlp4 [load-options] [--] [other-options]
Options:
<file>.ml        Parse this implementation file
<file>.mli       Parse this interface file
<file>.%s Load this module inside the Camlp4 core@."
(if DynLoader.is_native then "cmxs     " else "(cmo|cma)")
;
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

value search_stdlib = ref True;
value print_loaded_modules = ref False;
value (task, do_task) =
  let t = ref None in
  let task f x =
    let () = Camlp4_config.current_input_file.val := x in
    t.val := Some (if t.val = None then (fun _ -> f x)
                   else (fun usage -> usage ())) in
  let do_task usage = match t.val with [ Some f -> f usage | None -> () ] in
  (task, do_task);
value input_file x =
  let dyn_loader = dyn_loader.val () in
  do {
    rcall_callback.val ();
    match x with
    [ Intf file_name -> task (process_intf dyn_loader) file_name
    | Impl file_name -> task (process_impl dyn_loader) file_name
    | Str s ->
        begin
          let (f, o) = Filename.open_temp_file "from_string" ".ml";
          output_string o s;
          close_out o;
          task (process_impl dyn_loader) f;
          at_exit (fun () -> Sys.remove f);
        end
    | ModuleImpl file_name -> rewrite_and_load "" file_name
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
  ("-unsafe", Arg.Set Camlp4_config.unsafe,
    "Generate unsafe accesses to array and strings.");
  ("-noassert", Arg.Unit warn_noassert,
    "Obsolete, do not use this option.");
  ("-verbose", Arg.Set Camlp4_config.verbose,
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
  ("-no_quot", Arg.Clear Camlp4_config.quotations,
    "Don't parse quotations, allowing to use, e.g. \"<:>\" as token.");
  ("-loaded-modules", Arg.Set print_loaded_modules, "Print the list of loaded modules.");
  ("-parser", Arg.String (rewrite_and_load "Parsers"),
    "<name>  Load the parser Camlp4Parsers/<name>.cm(o|a|xs)");
  ("-printer", Arg.String (rewrite_and_load "Printers"),
    "<name>  Load the printer Camlp4Printers/<name>.cm(o|a|xs)");
  ("-filter", Arg.String (rewrite_and_load "Filters"),
    "<name>  Load the filter Camlp4Filters/<name>.cm(o|a|xs)");
  ("-ignore", Arg.String ignore, "ignore the next argument");
  ("--", Arg.Unit ignore, "Deprecated, does nothing")
];

Options.init initial_spec_list;

value anon_fun name =
  input_file
  (if Filename.check_suffix name ".mli" then Intf name
    else if Filename.check_suffix name ".ml" then Impl name
    else if Filename.check_suffix name objext then ModuleImpl name
    else if Filename.check_suffix name libext then ModuleImpl name
    else raise (Arg.Bad ("don't know what to do with " ^ name)));

value main argv =
  let usage () = do { usage initial_spec_list (Options.ext_spec_list ()); exit 0 } in
  try do {
    let dynloader = DynLoader.mk ~ocaml_stdlib:search_stdlib.val
                                 ~camlp4_stdlib:search_stdlib.val ();
    dyn_loader.val := fun () -> dynloader;
    let call_callback () =
      Register.iter_and_take_callbacks
        (fun (name, module_callback) ->
           let () = add_to_loaded_modules name in
           module_callback ());
    call_callback ();
    rcall_callback.val := call_callback;
    match Options.parse anon_fun argv with
    [ [] -> ()
    | ["-help"|"--help"|"-h"|"-?" :: _] -> usage ()
    | [s :: _] ->
        do { eprintf "%s: unknown or misused option\n" s;
            eprintf "Use option -help for usage@.";
            exit 2 } ];
    do_task usage;
    call_callback ();
    if print_loaded_modules.val then do {
      SSet.iter (eprintf "%s@.") loaded_modules.val;
    } else ()
  }
  with
  [ Arg.Bad s -> do { eprintf "Error: %s\n" s;
                      eprintf "Use option -help for usage@.";
                      exit 2 }
  | Arg.Help _ -> usage ()
  | exc -> do { eprintf "@[<v0>%a@]@." ErrorHandler.print exc; exit 2 } ];

main Sys.argv;
