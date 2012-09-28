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

(* $Id: odoc_args.mli 12959 2012-09-27 13:12:51Z maranget $ *)

(** Analysis of the command line arguments. *)

(** The current module defining the generator to use. *)
val current_generator : Odoc_gen.generator option ref

(** The include_dirs in the OCaml compiler. *)
val include_dirs : string list ref

(** The class type of documentation generators. *)
class type doc_generator =
  object method generate : Odoc_module.t_module list -> unit end

(** The function to be used to create a generator. *)
val doc_generator : doc_generator option ref

(** The merge options to be used. *)
val merge_options : Odoc_types.merge_option list ref

(** Classic mode or not. *)
val classic : bool ref

(** The file used by the generators outputting only one file. *)
val out_file : string ref

(** The optional file name to dump the collected information into.*)
val dump : string option ref

(** The list of information files to load. *)
val load : string list ref

(** Verbose mode or not. *)
val verbose : bool ref

(** We must sort the list of top modules or not.*)
val sort_modules : bool ref

(** We must not stop at the stop special comments. Default is false (we stop).*)
val no_stop : bool ref

(** We must raise an exception when we find an unknown @-tag. *)
val no_custom_tags : bool ref

(** We must remove the the first characters of each comment line, until the first asterisk '*'. *)
val remove_stars : bool ref

(** To keep the code while merging, when we have both .ml and .mli files for a module. *)
val keep_code : bool ref

(** To inverse implementation and interface files when merging. *)
val inverse_merge_ml_mli : bool ref

(** To filter module elements according to module type constraints. *)
val filter_with_module_constraints : bool ref

(** The optional title to use in the generated documentation. *)
val title : string option ref

(** The optional file whose content can be used as intro text. *)
val intro_file : string option ref

(** Flag to indicate whether we must display the complete list of parameters
   for functions and methods. *)
val with_parameter_list : bool ref

(** The list of module names to hide. *)
val hidden_modules : string list ref

(** The directory where files have to be generated. *)
val target_dir : string ref

(** An optional file to use where a CSS style is defined (for HTML). *)
val css_style : string option ref

(** Generate only index files. (for HTML). *)
val index_only : bool ref

(** To colorize code in HTML generated documentation pages, not code pages. *)
val colorize_code : bool ref

(** Encoding used in HTML pages header. *)
val charset : string ref

(** The flag which indicates if we must generate a header (for LaTeX). *)
val with_header : bool ref

(** The flag which indicates if we must generate a trailer (for LaTeX). *)
val with_trailer : bool ref

(** The flag to indicate if we must generate one file per module (for LaTeX). *)
val separate_files : bool ref

(** The list of pairs (title level, sectionning style). *)
val latex_titles : (int * string) list ref

(** The prefix to use for value labels in LaTeX. *)
val latex_value_prefix : string ref

(** The prefix to use for type labels in LaTeX. *)
val latex_type_prefix : string ref

(** The prefix to use for exception labels in LaTeX. *)
val latex_exception_prefix : string ref

(** The prefix to use for module labels in LaTeX. *)
val latex_module_prefix : string ref

(** The prefix to use for module type labels in LaTeX. *)
val latex_module_type_prefix : string ref

(** The prefix to use for class labels in LaTeX. *)
val latex_class_prefix : string ref

(** The prefix to use for class type labels in LaTeX. *)
val latex_class_type_prefix : string ref

(** The prefix to use for attribute labels in LaTeX. *)
val latex_attribute_prefix : string ref

(** The prefix to use for method labels in LaTeX. *)
val latex_method_prefix : string ref

(** The flag which indicates if we must generate a table of contents (for LaTeX). *)
val with_toc : bool ref

(** The flag which indicates if we must generate an index (for TeXinfo). *)
val with_index : bool ref

(** The flag which indicates if we must escape accentuated characters (for TeXinfo).*)
val esc_8bits : bool ref

(** The Info directory section *)
val info_section : string ref

(** The Info directory entries to insert *)
val info_entry : string list ref

(** Include all modules or only the ones on the command line, for the dot output. *)
val dot_include_all : bool ref

(** Generate dependency graph for types. *)
val dot_types : bool ref

(** Perform transitive reduction before dot output. *)
val dot_reduce : bool ref

(** The colors used in the dot output. *)
val dot_colors : string list ref

(** The suffix for man pages. *)
val man_suffix : string ref

(** The section for man pages. *)
val man_section : string ref

(** The flag to generate all man pages or only for modules and classes.*)
val man_mini : bool ref

(** The files to be analysed. *)
val files : source_file list ref

(** To set the documentation generator. *)
val set_generator : Odoc_gen.generator -> unit

(** Extend current HTML generator.
  @raise Failure if another kind of generator is already set.*)
val extend_html_generator : (module Odoc_gen.Html_functor) -> unit

(** Extend current LaTeX generator.
  @raise Failure if another kind of generator is already set.*)
val extend_latex_generator : (module Odoc_gen.Latex_functor) -> unit

(** Extend current Texi generator.
  @raise Failure if another kind of generator is already set.*)
val extend_texi_generator : (module Odoc_gen.Texi_functor) -> unit

(** Extend current man generator.
  @raise Failure if another kind of generator is already set.*)
val extend_man_generator : (module Odoc_gen.Man_functor) -> unit

(** Extend current dot generator.
  @raise Failure if another kind of generator is already set.*)
val extend_dot_generator : (module Odoc_gen.Dot_functor) -> unit

(** Extend current base generator.
  @raise Failure if another kind of generator is already set.*)
val extend_base_generator : (module Odoc_gen.Base_functor) -> unit

(** Add an option specification. *)
val add_option : string * Arg.spec * string -> unit

(** Parse the args.
   [byte] indicate if we are in bytecode mode (default is [true]).*)
val parse : unit -> unit
