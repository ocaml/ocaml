(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Language grammar, entries and printers.

   Hold variables to be set by language syntax extensions. Some of them
   are provided for quotations management. *)

(** {6 Parsers} *)

value parse_interf :
  ref (Stream.t char -> (list (MLast.sig_item * MLast.loc) * bool));
value parse_implem :
  ref (Stream.t char -> (list (MLast.str_item * MLast.loc) * bool));

value gram : Grammar.g;
value interf : Grammar.Entry.e (list (MLast.sig_item * MLast.loc) * bool);
value implem : Grammar.Entry.e (list (MLast.str_item * MLast.loc) * bool);
value top_phrase : Grammar.Entry.e (option MLast.str_item);
value use_file : Grammar.Entry.e (list MLast.str_item * bool);
value module_type : Grammar.Entry.e MLast.module_type;
value module_expr : Grammar.Entry.e MLast.module_expr;
value sig_item : Grammar.Entry.e MLast.sig_item;
value str_item : Grammar.Entry.e MLast.str_item;
value expr : Grammar.Entry.e MLast.expr;
value patt : Grammar.Entry.e MLast.patt;
value ctyp : Grammar.Entry.e MLast.ctyp;
value let_binding : Grammar.Entry.e (MLast.patt * MLast.expr);
value class_sig_item : Grammar.Entry.e MLast.class_sig_item;
value class_str_item : Grammar.Entry.e MLast.class_str_item;
value class_expr : Grammar.Entry.e MLast.class_expr;
value class_type : Grammar.Entry.e MLast.class_type;
   (** Some grammar and entries of the language, set by [pa_o.cmo] and
       [pa_r.cmo]. *)

value input_file : ref string;
   (** The file currently being parsed. *)
value output_file : ref (option string);
   (** The output file, stdout if None (default) *)
value report_error : exn -> unit;
   (** Prints an error message, using the module [Format]. *)
value quotation_dump_file : ref (option string);
   (** [quotation_dump_file] optionally tells the compiler to dump the
       result of an expander if this result is syntactically incorrect.
       If [None] (default), this result is not dumped. If [Some fname], the
       result is dumped in the file [fname]. *)
value version : string;
   (** The current version of Camlp4. *)
value add_option : string -> Arg.spec -> string -> unit;
   (** Add an option to the command line options. *)
value no_constructors_arity : ref bool;
   (** [True]: dont generate constructor arity. *)
(*value no_assert : ref bool;
   (** [True]: dont generate assertion checks. *)
*)

value sync : ref (Stream.t char -> unit);

value handle_expr_quotation : MLast.loc -> (string * string) -> MLast.expr;
value handle_expr_locate : MLast.loc -> (int * string) -> MLast.expr;

value handle_patt_quotation : MLast.loc -> (string * string) -> MLast.patt;
value handle_patt_locate : MLast.loc -> (int * string) -> MLast.patt;

value expr_reloc :
  (MLast.loc -> MLast.loc) -> int -> MLast.expr -> MLast.expr;
value patt_reloc :
  (MLast.loc -> MLast.loc) -> int -> MLast.patt -> MLast.patt;

(** Allow user to catch exceptions in quotations *)
type err_ctx =
  [ Finding | Expanding | ParsingResult of (int * int) and string | Locating ]
;
exception Qerror of string and err_ctx and exn;

(** {6 Printers} *)

open Spretty;

value print_interf : ref (list (MLast.sig_item * MLast.loc) -> unit);
value print_implem : ref (list (MLast.str_item * MLast.loc) -> unit);
   (** Some printers, set by [pr_dump.cmo], [pr_o.cmo] and [pr_r.cmo]. *)

type printer_t 'a =
  { pr_fun : mutable string -> 'a -> string -> kont -> pretty;
    pr_levels : mutable list (pr_level 'a) }
and pr_level 'a =
  { pr_label : string;
    pr_box : 'a -> Stream.t pretty -> pretty;
    pr_rules : mutable pr_rule 'a }
and pr_rule 'a =
  Extfun.t 'a (curr 'a -> next 'a -> string -> kont -> Stream.t pretty)
and curr 'a = 'a -> string -> kont -> Stream.t pretty
and next 'a = 'a -> string -> kont -> pretty
and kont = Stream.t pretty
;

value pr_str_item : printer_t MLast.str_item;
value pr_sig_item : printer_t MLast.sig_item;
value pr_expr : printer_t MLast.expr;
value pr_patt : printer_t MLast.patt;
value pr_ctyp : printer_t MLast.ctyp;
value pr_class_str_item : printer_t MLast.class_str_item;
value pr_expr_fun_args :
  ref (Extfun.t MLast.expr (list MLast.patt * MLast.expr));

value find_pr_level : string -> list (pr_level 'a) -> pr_level 'a;

value top_printer : printer_t 'a -> 'a -> unit;

value inter_phrases : ref (option string);

(**/**)

(* for system use *)

value warning : ref ((int * int) -> string -> unit);
value expr_eoi : Grammar.Entry.e MLast.expr;
value patt_eoi : Grammar.Entry.e MLast.patt;
value arg_spec_list : unit -> list (string * Arg.spec * string);
