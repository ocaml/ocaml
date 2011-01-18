(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* cvsid $Id$ *)

(** Command-line arguments. *)

open Clflags

module M = Odoc_messages

type source_file =
    Impl_file of string
  | Intf_file of string
  | Text_file of string

let include_dirs = Clflags.include_dirs

class type doc_generator =
    object
      method generate : Odoc_module.t_module list -> unit
    end

let doc_generator = ref (None : doc_generator option)

let merge_options = ref ([] : Odoc_types.merge_option list)

let out_file = ref M.default_out_file

let dot_include_all = ref false

let dot_types = ref false

let dot_reduce = ref false

let dot_colors  = ref (List.flatten M.default_dot_colors)

let man_suffix = ref M.default_man_suffix
let man_section = ref M.default_man_section

let man_mini = ref false

(** Analysis of a string defining options. Return the list of
   options according to the list giving associations between
   [(character, _)] and a list of options. *)
let analyse_option_string l s =
  List.fold_left
    (fun acc -> fun ((c,_), v) ->
      if String.contains s c then
        acc @ v
      else
        acc)
    []
    l

(** Analysis of a string defining the merge options to be used.
   Returns the list of options specified.*)
let analyse_merge_options s =
  let l = [
    (M.merge_description, [Odoc_types.Merge_description]) ;
    (M.merge_author, [Odoc_types.Merge_author]) ;
    (M.merge_version, [Odoc_types.Merge_version]) ;
    (M.merge_see, [Odoc_types.Merge_see]) ;
    (M.merge_since, [Odoc_types.Merge_since]) ;
    (M.merge_before, [Odoc_types.Merge_before]) ;
    (M.merge_deprecated, [Odoc_types.Merge_deprecated]) ;
    (M.merge_param, [Odoc_types.Merge_param]) ;
    (M.merge_raised_exception, [Odoc_types.Merge_raised_exception]) ;
    (M.merge_return_value, [Odoc_types.Merge_return_value]) ;
    (M.merge_custom, [Odoc_types.Merge_custom]) ;
    (M.merge_all, Odoc_types.all_merge_options)
  ]
  in
  analyse_option_string l s

let classic = Clflags.classic

let dump = ref (None : string option)

let load = ref ([] : string list)

(** Allow arbitrary recursive types. *)
let recursive_types = Clflags.recursive_types

let verbose = ref false

(** Optional preprocessor command. *)
let preprocessor = Clflags.preprocessor

let sort_modules = ref false

let no_custom_tags = ref false

let no_stop = ref false

let remove_stars = ref false

let keep_code = ref false

let inverse_merge_ml_mli = ref false

let filter_with_module_constraints = ref true

let title = ref (None : string option)

let intro_file = ref (None : string option)

let with_parameter_list = ref false

let hidden_modules = ref ([] : string list)

let target_dir = ref Filename.current_dir_name

let css_style = ref None

let index_only = ref false

let colorize_code = ref false

let html_short_functors = ref false

let with_header = ref true

let with_trailer = ref true

let separate_files = ref false

let latex_titles = ref [
  1, "section" ;
  2, "subsection" ;
  3, "subsubsection" ;
  4, "paragraph" ;
  5, "subparagraph" ;
]

let with_toc = ref true

let with_index = ref true

let esc_8bits = ref false

let info_section = ref "Objective Caml"

let info_entry = ref []

let files = ref []

let f_latex_title s =
  try
    let pos = String.index s ',' in
    let n = int_of_string (String.sub s 0 pos) in
    let len = String.length s in
    let command = String.sub s (pos + 1) (len - pos - 1) in
    latex_titles := List.remove_assoc n !latex_titles ;
    latex_titles := (n, command) :: !latex_titles
  with
    Not_found
  | Invalid_argument _ ->
      incr Odoc_global.errors ;
      prerr_endline (M.wrong_format s)

let add_hidden_modules s =
  let l = Str.split (Str.regexp ",") s in
  List.iter
    (fun n ->
      let name = Str.global_replace (Str.regexp "[ \n\r\t]+") "" n in
      match name with
        "" -> ()
      | _ ->
          match name.[0] with
            'A'..'Z' -> hidden_modules := name :: !hidden_modules
          | _ ->
              incr Odoc_global.errors;
              prerr_endline (M.not_a_module_name name)
    )
    l

let latex_value_prefix = ref M.default_latex_value_prefix
let latex_type_prefix = ref M.default_latex_type_prefix
let latex_exception_prefix = ref M.default_latex_exception_prefix
let latex_module_prefix = ref M.default_latex_module_prefix
let latex_module_type_prefix = ref M.default_latex_module_type_prefix
let latex_class_prefix = ref M.default_latex_class_prefix
let latex_class_type_prefix = ref M.default_latex_class_type_prefix
let latex_attribute_prefix = ref M.default_latex_attribute_prefix
let latex_method_prefix = ref M.default_latex_method_prefix

let set_doc_generator (dg_opt : doc_generator option) = doc_generator := dg_opt

(** The default html generator. Initialized in the parse function, to be used during the command line analysis.*)
let default_html_generator = ref (None : doc_generator option)

(** The default latex generator. Initialized in the parse function, to be used during the command line analysis.*)
let default_latex_generator = ref (None : doc_generator option)

(** The default texinfo generator. Initialized in the parse function, to be used during the command line analysis.*)
let default_texi_generator = ref (None : doc_generator option)

(** The default man pages generator. Initialized in the parse function, to be used during the command line analysis.*)
let default_man_generator = ref (None : doc_generator option)

(** The default dot generator. Initialized in the parse function, to be used during  the command line analysis.*)
let default_dot_generator = ref (None : doc_generator option)

(** The default option list *)
let default_options = [
  "-version", Arg.Unit (fun () -> print_string M.message_version ; print_newline () ; exit 0) , M.option_version ;
  "-vnum", Arg.Unit (fun () -> print_string M.config_version ;
                               print_newline () ; exit 0) , M.option_version ;
  "-v", Arg.Unit (fun () -> verbose := true), M.verbose_mode ;
  "-I", Arg.String (fun s -> include_dirs := (Misc.expand_directory Config.standard_library s) :: !include_dirs), M.include_dirs ;
  "-pp", Arg.String (fun s -> preprocessor := Some s), M.preprocess ;
  "-impl", Arg.String (fun s -> files := !files @ [Impl_file s]), M.option_impl ;
  "-intf", Arg.String (fun s -> files := !files @ [Intf_file s]), M.option_intf ;
  "-text", Arg.String (fun s -> files := !files @ [Text_file s]), M.option_text ;
  "-rectypes", Arg.Set recursive_types, M.rectypes ;
  "-nolabels", Arg.Unit (fun () -> classic := true), M.nolabels ;
  "-warn-error", Arg.Set Odoc_global.warn_error, M.werr ;
  "-hide-warnings", Arg.Clear Odoc_config.print_warnings, M.hide_warnings ;
  "-o", Arg.String (fun s -> out_file := s), M.out_file ;
  "-d", Arg.String (fun s -> target_dir := s), M.target_dir ;
  "-sort", Arg.Unit (fun () -> sort_modules := true), M.sort_modules ;
  "-no-stop", Arg.Set no_stop, M.no_stop ;
  "-no-custom-tags", Arg.Set no_custom_tags, M.no_custom_tags ;
  "-stars", Arg.Set remove_stars, M.remove_stars ;
  "-inv-merge-ml-mli", Arg.Set inverse_merge_ml_mli, M.inverse_merge_ml_mli ;
  "-no-module-constraint-filter", Arg.Clear filter_with_module_constraints,
  M.no_filter_with_module_constraints ;

  "-keep-code", Arg.Set keep_code, M.keep_code^"\n" ;

  "-dump", Arg.String (fun s -> dump := Some s), M.dump ;
  "-load", Arg.String (fun s -> load := !load @ [s]), M.load^"\n" ;

  "-t", Arg.String (fun s -> title := Some s), M.option_title ;
  "-intro", Arg.String (fun s -> intro_file := Some s), M.option_intro ;
  "-hide", Arg.String add_hidden_modules, M.hide_modules ;
  "-m", Arg.String (fun s -> merge_options := !merge_options @ (analyse_merge_options s)),
  M.merge_options ^
  "\n\n *** choosing a generator ***\n";

(* generators *)
  "-html", Arg.Unit (fun () -> set_doc_generator !default_html_generator), M.generate_html ;
  "-latex", Arg.Unit (fun () -> set_doc_generator !default_latex_generator), M.generate_latex ;
  "-texi", Arg.Unit (fun () -> set_doc_generator !default_texi_generator), M.generate_texinfo ;
  "-man", Arg.Unit (fun () -> set_doc_generator !default_man_generator), M.generate_man ;
  "-dot", Arg.Unit (fun () -> set_doc_generator !default_dot_generator), M.generate_dot ;
  "-customdir", Arg.Unit (fun () -> Printf.printf "%s\n" Odoc_config.custom_generators_path; exit 0),
  M.display_custom_generators_dir ;
  "-i", Arg.String (fun s -> ()), M.add_load_dir ;
  "-g", Arg.String (fun s -> ()), M.load_file ^
  "\n\n *** HTML options ***\n";

(* html only options *)
  "-all-params", Arg.Set with_parameter_list, M.with_parameter_list ;
  "-css-style", Arg.String (fun s -> css_style := Some s), M.css_style ;
  "-index-only", Arg.Set index_only, M.index_only ;
  "-colorize-code", Arg.Set colorize_code, M.colorize_code ;
  "-short-functors", Arg.Set html_short_functors, M.html_short_functors ^
  "\n\n *** LaTeX options ***\n";

(* latex only options *)
  "-noheader", Arg.Unit (fun () -> with_header := false), M.no_header ;
  "-notrailer", Arg.Unit (fun () -> with_trailer := false), M.no_trailer ;
  "-sepfiles", Arg.Set separate_files, M.separate_files ;
  "-latextitle", Arg.String f_latex_title, M.latex_title latex_titles ;
  "-latex-value-prefix", Arg.String (fun s -> latex_value_prefix := s), M.latex_value_prefix ;
  "-latex-type-prefix", Arg.String (fun s -> latex_type_prefix := s), M.latex_type_prefix ;
  "-latex-exception-prefix", Arg.String (fun s -> latex_exception_prefix := s), M.latex_exception_prefix ;
  "-latex-attribute-prefix", Arg.String (fun s -> latex_attribute_prefix := s), M.latex_attribute_prefix ;
  "-latex-method-prefix", Arg.String (fun s -> latex_method_prefix := s), M.latex_method_prefix ;
  "-latex-module-prefix", Arg.String (fun s -> latex_module_prefix := s), M.latex_module_prefix ;
  "-latex-module-type-prefix", Arg.String (fun s -> latex_module_type_prefix := s), M.latex_module_type_prefix ;
  "-latex-class-prefix", Arg.String (fun s -> latex_class_prefix := s), M.latex_class_prefix ;
  "-latex-class-type-prefix", Arg.String (fun s -> latex_class_type_prefix := s), M.latex_class_type_prefix ;
  "-notoc", Arg.Unit (fun () -> with_toc := false),
  M.no_toc ^
  "\n\n *** texinfo options ***\n";

(* tex only options *)
  "-noindex", Arg.Clear with_index, M.no_index ;
  "-esc8", Arg.Set esc_8bits, M.esc_8bits ;
  "-info-section", Arg.String ((:=) info_section), M.info_section ;
  "-info-entry", Arg.String (fun s -> info_entry := !info_entry @ [ s ]),
  M.info_entry ^
  "\n\n *** dot options ***\n";

(* dot only options *)
  "-dot-colors", Arg.String (fun s -> dot_colors := Str.split (Str.regexp_string ",") s), M.dot_colors ;
  "-dot-include-all", Arg.Set dot_include_all, M.dot_include_all ;
  "-dot-types", Arg.Set dot_types, M.dot_types ;
  "-dot-reduce", Arg.Set dot_reduce, M.dot_reduce^
  "\n\n *** man pages options ***\n";

(* man only options *)
  "-man-mini", Arg.Set man_mini, M.man_mini ;
  "-man-suffix", Arg.String (fun s -> man_suffix := s), M.man_suffix ;
  "-man-section", Arg.String (fun s -> man_section := s), M.man_section ;

]

let options = ref default_options

let modified_options () =
  !options != default_options

let append_last_doc suffix =
  match List.rev !options with
  | (key, spec, doc) :: tl ->
      options := List.rev ((key, spec, doc ^ suffix) :: tl)
  | [] -> ()

(** The help option list, overriding the default ones from the Arg module *)
let help_options = ref []
let help_action () =
  Arg.usage (!options @ !help_options) (M.usage^M.options_are)
let () =
  help_options := [
    "-help", Arg.Unit help_action, M.help ;
    "--help", Arg.Unit help_action, M.help
]

let add_option o =
  if not (modified_options ()) then
    append_last_doc "\n *** custom generator options ***\n";
  let (s,_,_) = o in
  let rec iter = function
      [] -> [o]
    | (s2,f,m) :: q ->
        if s = s2 then
          o :: q
        else
          (s2,f,m) :: (iter q)
  in
  options := iter !options

let parse ~html_generator ~latex_generator ~texi_generator ~man_generator ~dot_generator =
  let anonymous f =
    let sf =
      if Filename.check_suffix f "ml" then
        Impl_file f
      else
        if Filename.check_suffix f "mli" then
          Intf_file f
        else
          if Filename.check_suffix f "txt" then
            Text_file f
          else
            failwith (Odoc_messages.unknown_extension f)
    in
    files := !files @ [sf]
  in
  default_html_generator := Some html_generator ;
  default_latex_generator := Some latex_generator ;
  default_texi_generator := Some texi_generator ;
  default_man_generator := Some man_generator ;
  default_dot_generator := Some dot_generator ;
  if modified_options () then append_last_doc "\n";
  let options = !options @ !help_options in
  let _ = Arg.parse options
      anonymous
      (M.usage^M.options_are)
  in
  (* we sort the hidden modules by name, to be sure that for example,
     A.B is before A, so we will match against A.B before A in
     Odoc_name.hide_modules.*)
  hidden_modules := List.sort (fun a -> fun b -> - (compare a b)) !hidden_modules
