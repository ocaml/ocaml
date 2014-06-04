(* No comments, OCaml stdlib internal use only. *)

open CamlinternalFormatBasics

val is_in_char_set : char_set -> char -> bool
val rev_char_set : char_set -> char_set

type mutable_char_set = bytes
val create_char_set : unit -> mutable_char_set
val add_in_char_set : mutable_char_set -> char -> unit
val freeze_char_set : mutable_char_set -> char_set

val reader_nb_unifier_of_fmtty :
   ('a, 'b, 'c, 'd, 'e, 'f) fmtty -> ('d, 'e, 'd, 'e) reader_nb_unifier

type ('a, 'b, 'c, 'd, 'e, 'f) param_format_ebb = Param_format_EBB :
     ('x -> 'a, 'b, 'c, 'd, 'e, 'f) fmt ->
     ('a, 'b, 'c, 'd, 'e, 'f) param_format_ebb

val param_format_of_ignored_format :
  ('a, 'b, 'c, 'd, 'y, 'x) ignored -> ('x, 'b, 'c, 'y, 'e, 'f) fmt ->
  ('a, 'b, 'c, 'd, 'e, 'f) param_format_ebb

type ('b, 'c) acc =
  | Acc_formatting  of ('b, 'c) acc * formatting
  | Acc_string      of ('b, 'c) acc * string
  | Acc_char        of ('b, 'c) acc * char
  | Acc_delay       of ('b, 'c) acc * ('b -> 'c)
  | Acc_flush       of ('b, 'c) acc
  | Acc_invalid_arg of ('b, 'c) acc * string
  | End_of_acc

type ('a, 'b) heter_list =
  | Cons : 'c * ('a, 'b) heter_list -> ('c -> 'a, 'b) heter_list
  | Nil : ('b, 'b) heter_list

type ('b, 'c, 'e, 'f) fmt_ebb = Fmt_EBB :
     ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt ->
     ('b, 'c, 'e, 'f) fmt_ebb

val make_printf :
  ('b -> ('b, 'c) acc -> 'd) -> 'b -> ('b, 'c) acc ->
  ('a, 'b, 'c, 'c, 'c, 'd) CamlinternalFormatBasics.fmt -> 'a

val output_acc : out_channel -> (out_channel, unit) acc -> unit
val bufput_acc : Buffer.t -> (Buffer.t, unit) acc -> unit
val strput_acc : Buffer.t -> (unit, string) acc -> unit

val type_format :
  ('x, 'b, 'c, 't, 'u, 'v) CamlinternalFormatBasics.fmt ->
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmtty ->
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt

val fmt_ebb_of_string : string -> ('b, 'c, 'e, 'f) fmt_ebb

val format_of_string_fmtty :
  string ->
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmtty ->
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

val format_of_string_format :
  string ->
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

val char_of_iconv : CamlinternalFormatBasics.int_conv -> char
val string_of_formatting : CamlinternalFormatBasics.formatting -> string

val string_of_fmtty :
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmtty -> string
val string_of_fmt :
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt -> string
