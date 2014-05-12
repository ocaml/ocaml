open CamlinternalFormatBasics

let legacy_behavior =
  try Sys.getenv "OCAML_LEGACY_FORMAT" <> "" with _ -> false

(******************************************************************************)
                                 (* Types *)

(* Reversed list of printing atoms. *)
(* Used to accumulate printf arguments. *)
type ('b, 'c) acc =
  | Acc_formatting  of ('b, 'c) acc * formatting(* Special formatting (box)   *)
  | Acc_string      of ('b, 'c) acc * string    (* Literal or generated string*)
  | Acc_char        of ('b, 'c) acc * char      (* Literal or generated char  *)
  | Acc_delay       of ('b, 'c) acc * ('b -> 'c)(* Delayed printing (%a, %t)  *)
  | Acc_flush       of ('b, 'c) acc             (* Flush                      *)
  | Acc_invalid_arg of ('b, 'c) acc * string    (* Raise Invalid_argument msg *)
  | End_of_acc

(* List of heterogeneous values. *)
(* Used to accumulate scanf callback arguments. *)
type ('a, 'b) heter_list =
  | Cons : 'c * ('a, 'b) heter_list -> ('c -> 'a, 'b) heter_list
  | Nil : ('b, 'b) heter_list

(* Existential Black Boxes. *)
(* Used to abstract some existential type parameters. *)

(* GADT type associating a padding and an fmtty. *)
(* See the type_padding function. *)
type ('a, 'b, 'c, 'd, 'e, 'f) padding_fmtty_ebb = Padding_fmtty_EBB :
     ('x, 'y) padding * ('y, 'b, 'c, 'd, 'e, 'f) fmtty ->
     ('x, 'b, 'c, 'd, 'e, 'f) padding_fmtty_ebb

(* GADT type associating a padding, a precision and an fmtty. *)
(* See the type_padprec function. *)
type ('a, 'b, 'c, 'd, 'e, 'f) padprec_fmtty_ebb = Padprec_fmtty_EBB :
     ('x, 'y) padding * ('y, 'z) precision * ('z, 'b, 'c, 'd, 'e, 'f) fmtty ->
     ('x, 'b, 'c, 'd, 'e, 'f) padprec_fmtty_ebb

(* GADT type associating a padding and an fmt. *)
(* See make_padding_fmt_ebb and parse_format functions. *)
type ('a, 'b, 'c, 'e, 'f) padding_fmt_ebb = Padding_fmt_EBB :
     (_, 'x -> 'a) padding *
     ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt ->
     ('x, 'b, 'c, 'e, 'f) padding_fmt_ebb

(* GADT type associating a precision and an fmt. *)
(* See make_precision_fmt_ebb and parse_format functions. *)
type ('a, 'b, 'c, 'e, 'f) precision_fmt_ebb = Precision_fmt_EBB :
     (_, 'x -> 'a) precision *
     ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt ->
     ('x, 'b, 'c, 'e, 'f) precision_fmt_ebb

(* GADT type associating a padding, a precision and an fmt. *)
(* See make_padprec_fmt_ebb and parse_format functions. *)
type ('p, 'b, 'c, 'e, 'f) padprec_fmt_ebb = Padprec_fmt_EBB :
     ('x, 'y) padding * ('y, 'p -> 'a) precision *
     ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt ->
     ('p, 'b, 'c, 'e, 'f) padprec_fmt_ebb

(* Abstract the 'a and 'd parameters of an fmt. *)
(* Output type of the format parsing function. *)
type ('b, 'c, 'e, 'f) fmt_ebb = Fmt_EBB :
     ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt ->
     ('b, 'c, 'e, 'f) fmt_ebb

(* GADT type associating an fmtty and an fmt. *)
(* See the type_ignored_format_substitution function. *)
type ('a, 'b, 'c, 'd, 'e, 'f) fmtty_fmt_ebb = Fmtty_fmt_EBB :
     ('a, 'b, 'c, 'd, 'y, 'x) fmtty *
     ('x, 'b, 'c, 'y, 'e, 'f) CamlinternalFormatBasics.fmt ->
     ('a, 'b, 'c, 'd, 'e, 'f) fmtty_fmt_ebb

(* Abstract all fmtty type parameters. *)
(* Used to compare format types. *)
type fmtty_ebb = Fmtty_EBB : ('a, 'b, 'c, 'd, 'e, 'f) fmtty -> fmtty_ebb

(* Abstract all padding type parameters. *)
(* Used to compare paddings. *)
type padding_ebb = Padding_EBB : ('a, 'b) padding -> padding_ebb

(* Abstract all precision type parameters. *)
(* Used to compare precisions. *)
type precision_ebb = Precision_EBB : ('a, 'b) precision -> precision_ebb

(******************************************************************************)
                               (* Constants *)

(* Default precision for float printing. *)
let default_float_precision = 6

(******************************************************************************)
                               (* Externals *)

external format_float: string -> float -> string
  = "caml_format_float"
external format_int: string -> int -> string
  = "caml_format_int"
external format_int32: string -> int32 -> string
  = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
  = "caml_nativeint_format"
external format_int64: string -> int64 -> string
  = "caml_int64_format"

(******************************************************************************)
                     (* Tools to pretty-print formats *)

(* Type of extensible character buffers. *)
type buffer = {
  mutable ind : int;
  mutable bytes : bytes;
}

(* Create a fresh buffer. *)
let buffer_create init_size = { ind = 0; bytes = Bytes.create init_size }

(* Check size of the buffer and grow it if needed. *)
let buffer_check_size buf overhead =
  let len = Bytes.length buf.bytes in
  let min_len = buf.ind + overhead in
  if min_len > len then (
    let new_len = max (len * 2) min_len in
    let new_str = Bytes.create new_len in
    Bytes.blit buf.bytes 0 new_str 0 len;
    buf.bytes <- new_str;
  )

(* Add the character `c' to the buffer `buf'. *)
let buffer_add_char buf c =
  buffer_check_size buf 1;
  Bytes.set buf.bytes buf.ind c;
  buf.ind <- buf.ind + 1

(* Add the string `s' to the buffer `buf'. *)
let buffer_add_string buf s =
  let str_len = String.length s in
  buffer_check_size buf str_len;
  String.blit s 0 buf.bytes buf.ind str_len;
  buf.ind <- buf.ind + str_len

(* Get the content of the buffer. *)
let buffer_contents buf =
  Bytes.sub_string buf.bytes 0 buf.ind

(***)

(* Convert an integer conversion to char. *)
let char_of_iconv iconv = match iconv with
  | Int_d | Int_pd | Int_sd -> 'd' | Int_i | Int_pi | Int_si -> 'i'
  | Int_x | Int_Cx -> 'x' | Int_X | Int_CX -> 'X' | Int_o | Int_Co -> 'o'
  | Int_u -> 'u'

(* Convert a float conversion to char. *)
let char_of_fconv fconv = match fconv with
  | Float_f | Float_pf | Float_sf -> 'f' | Float_e | Float_pe | Float_se -> 'e'
  | Float_E | Float_pE | Float_sE -> 'E' | Float_g | Float_pg | Float_sg -> 'g'
  | Float_G | Float_pG | Float_sG -> 'G' | Float_F -> 'F'

(* Convert a scanning counter to char. *)
let char_of_counter counter = match counter with
  | Line_counter  -> 'l'
  | Char_counter  -> 'n'
  | Token_counter -> 'N'

(***)

(* Print a char_set in a buffer with the OCaml format lexical convention. *)
let bprint_char_set buf char_set =
  let rec print_start set =
    if is_in_char_set set ']' &&
      (not (is_in_char_set set '\\') || not (is_in_char_set set '^'))
    then buffer_add_char buf ']';
    print_out set 1;
    if is_in_char_set set '-' &&
      (not (is_in_char_set set ',') || not (is_in_char_set set '.'))
    then buffer_add_char buf '-';
  and print_out set i =
    if i < 256 then
      if is_in_char_set set (char_of_int i) then print_first set i
      else print_out set (i + 1)
  and print_first set i =
    match char_of_int i with
    | '\255' -> print_char buf 255;
    | ']' | '-' -> print_out set (i + 1);
    | _ -> print_second set (i + 1);
  and print_second set i =
    if is_in_char_set set (char_of_int i) then
      match char_of_int i with
      | '\255' ->
        print_char buf 254;
        print_char buf 255;
      | ']' | '-' when not (is_in_char_set set (char_of_int (i + 1))) ->
        print_char buf (i - 1);
        print_out set (i + 1);
      | _ when not (is_in_char_set set (char_of_int (i + 1))) ->
        print_char buf (i - 1);
        print_char buf i;
        print_out set (i + 2);
      | _ ->
        print_in set (i - 1) (i + 2);
    else (
      print_char buf (i - 1);
      print_out set (i + 1);
    )
  and print_in set i j =
    if j = 256 || not (is_in_char_set set (char_of_int j)) then (
      print_char buf i;
      print_char buf (int_of_char '-');
      print_char buf (j - 1);
      if j < 256 then print_out set (j + 1);
    ) else
      print_in set i (j + 1);
  and print_char buf i = match char_of_int i with
    | '%' -> buffer_add_char buf '%'; buffer_add_char buf '%';
    | '@' -> buffer_add_char buf '%'; buffer_add_char buf '@';
    | c   -> buffer_add_char buf c;
  in
  buffer_add_char buf '[';
  print_start (
    if is_in_char_set char_set '\000'
    then ( buffer_add_char buf '^'; rev_char_set char_set )
    else char_set
  );
  buffer_add_char buf ']'

(***)

(* Print a padty in a buffer with the format-like syntax. *)
let bprint_padty buf padty = match padty with
  | Left  -> buffer_add_char buf '-'
  | Right -> ()
  | Zeros -> buffer_add_char buf '0'

(* Print the '_' of an ignored flag if needed. *)
let bprint_ignored_flag buf ign_flag =
  if ign_flag then buffer_add_char buf '_'

(***)

let bprint_pad_opt buf pad_opt = match pad_opt with
  | None -> ()
  | Some width -> buffer_add_string buf (string_of_int width)

(***)

(* Print padding in a buffer with the format-like syntax. *)
let bprint_padding : type a b . buffer -> (a, b) padding -> unit =
fun buf pad -> match pad with
  | No_padding -> ()
  | Lit_padding (padty, n) ->
    bprint_padty buf padty;
    buffer_add_string buf (string_of_int n);
  | Arg_padding padty ->
    bprint_padty buf padty;
    buffer_add_char buf '*'

(* Print precision in a buffer with the format-like syntax. *)
let bprint_precision : type a b . buffer -> (a, b) precision -> unit =
  fun buf prec -> match prec with
  | No_precision -> ()
  | Lit_precision n ->
    buffer_add_char buf '.';
    buffer_add_string buf (string_of_int n);
  | Arg_precision ->
    buffer_add_string buf ".*"

(***)

(* Print the optionnal '+', ' ' or '#' associated to an int conversion. *)
let bprint_iconv_flag buf iconv = match iconv with
  | Int_pd | Int_pi -> buffer_add_char buf '+'
  | Int_sd | Int_si -> buffer_add_char buf ' '
  | Int_Cx | Int_CX | Int_Co -> buffer_add_char buf '#'
  | Int_d | Int_i | Int_x | Int_X | Int_o | Int_u -> ()

(* Print an complete int format in a buffer (ex: "%3.*d"). *)
let bprint_int_fmt buf ign_flag iconv pad prec =
  buffer_add_char buf '%';
  bprint_ignored_flag buf ign_flag;
  bprint_iconv_flag buf iconv;
  bprint_padding buf pad;
  bprint_precision buf prec;
  buffer_add_char buf (char_of_iconv iconv)

(* Print a complete int32, nativeint or int64 format in a buffer. *)
let bprint_altint_fmt buf ign_flag iconv pad prec c =
  buffer_add_char buf '%';
  bprint_ignored_flag buf ign_flag;
  bprint_iconv_flag buf iconv;
  bprint_padding buf pad;
  bprint_precision buf prec;
  buffer_add_char buf c;
  buffer_add_char buf (char_of_iconv iconv)

(***)

(* Print the optionnal '+' associated to a float conversion. *)
let bprint_fconv_flag buf fconv = match fconv with
  | Float_pf | Float_pe | Float_pE | Float_pg | Float_pG ->
    buffer_add_char buf '+'
  | Float_sf | Float_se | Float_sE | Float_sg | Float_sG ->
    buffer_add_char buf ' '
  | Float_f | Float_e | Float_E | Float_g | Float_G | Float_F ->
    ()

(* Print a complete float format in a buffer (ex: "%+*.3f"). *)
let bprint_float_fmt buf ign_flag fconv pad prec =
  buffer_add_char buf '%';
  bprint_ignored_flag buf ign_flag;
  bprint_fconv_flag buf fconv;
  bprint_padding buf pad;
  bprint_precision buf prec;
  buffer_add_char buf (char_of_fconv fconv)

(* Compute the literal string representation of a formatting. *)
(* Also used by Printf and Scanf where formatting is not interpreted. *)
let string_of_formatting formatting = match formatting with
  | Open_box (str, _, _) -> str
  | Close_box            -> "@]"
  | Open_tag (str, _)    -> str
  | Close_tag            -> "@}"
  | Break (str, _, _)    -> str
  | FFlush               -> "@?"
  | Force_newline        -> "@\n"
  | Flush_newline        -> "@."
  | Magic_size (str, _)  -> str
  | Escaped_at           -> "@@"
  | Escaped_percent      -> "@%"
  | Scan_indic c -> "@" ^ (String.make 1 c)

(***)

(* Print a literal char in a buffer, escape '%' by "%%". *)
let bprint_char_literal buf chr = match chr with
  | '%' -> buffer_add_string buf "%%"
  | _ -> buffer_add_char buf chr

(* Print a literal string in a buffer, escape all '%' by "%%". *)
let bprint_string_literal buf str =
  for i = 0 to String.length str - 1 do
    bprint_char_literal buf str.[i]
  done

(******************************************************************************)
                          (* Format pretty-printing *)

(* Print a complete format type (an fmtty) in a buffer. *)
let rec bprint_fmtty : type a b c d e f .
    buffer -> (a, b, c, d, e, f) fmtty -> unit =
fun buf fmtty -> match fmtty with
  | Char_ty rest      -> buffer_add_string buf "%c";  bprint_fmtty buf rest;
  | String_ty rest    -> buffer_add_string buf "%s";  bprint_fmtty buf rest;
  | Int_ty rest       -> buffer_add_string buf "%i";  bprint_fmtty buf rest;
  | Int32_ty rest     -> buffer_add_string buf "%li"; bprint_fmtty buf rest;
  | Nativeint_ty rest -> buffer_add_string buf "%ni"; bprint_fmtty buf rest;
  | Int64_ty rest     -> buffer_add_string buf "%Li"; bprint_fmtty buf rest;
  | Float_ty rest     -> buffer_add_string buf "%f";  bprint_fmtty buf rest;
  | Bool_ty rest      -> buffer_add_string buf "%B";  bprint_fmtty buf rest;
  | Alpha_ty rest     -> buffer_add_string buf "%a";  bprint_fmtty buf rest;
  | Theta_ty rest     -> buffer_add_string buf "%t";  bprint_fmtty buf rest;
  | Reader_ty rest    -> buffer_add_string buf "%r";  bprint_fmtty buf rest;

  | Ignored_reader_ty rest ->
    buffer_add_string buf "%_r";
    bprint_fmtty buf rest;

  | Format_arg_ty (sub_fmtty, rest) ->
    buffer_add_string buf "%{"; bprint_fmtty buf sub_fmtty;
    buffer_add_string buf "%}"; bprint_fmtty buf rest;
  | Format_subst_ty (_, sub_fmtty, rest) ->
    buffer_add_string buf "%("; bprint_fmtty buf sub_fmtty;
    buffer_add_string buf "%)"; bprint_fmtty buf rest;

  | End_of_fmtty -> ()

(***)

(* Print a complete format in a buffer. *)
let bprint_fmt buf fmt =
  let rec fmtiter : type a b c d e f .
      (a, b, c, d, e, f) fmt -> bool -> unit =
  fun fmt ign_flag -> match fmt with
    | String (pad, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_padding buf pad; buffer_add_char buf 's';
      fmtiter rest false;
    | Caml_string (pad, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_padding buf pad; buffer_add_char buf 'S';
      fmtiter rest false;

    | Int (iconv, pad, prec, rest) ->
      bprint_int_fmt buf ign_flag iconv pad prec;
      fmtiter rest false;
    | Int32 (iconv, pad, prec, rest) ->
      bprint_altint_fmt buf ign_flag iconv pad prec 'l';
      fmtiter rest false;
    | Nativeint (iconv, pad, prec, rest) ->
      bprint_altint_fmt buf ign_flag iconv pad prec 'n';
      fmtiter rest false;
    | Int64 (iconv, pad, prec, rest) ->
      bprint_altint_fmt buf ign_flag iconv pad prec 'L';
      fmtiter rest false;
    | Float (fconv, pad, prec, rest) ->
      bprint_float_fmt buf ign_flag fconv pad prec;
      fmtiter rest false;

    | Char rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'c'; fmtiter rest false;
    | Caml_char rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'C'; fmtiter rest false;
    | Bool rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'B'; fmtiter rest false;
    | Alpha rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'a'; fmtiter rest false;
    | Theta rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 't'; fmtiter rest false;
    | Reader rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'r'; fmtiter rest false;
    | Flush rest ->
      buffer_add_string buf "%!";
      fmtiter rest ign_flag;

    | String_literal (str, rest) ->
      bprint_string_literal buf str;
      fmtiter rest ign_flag;
    | Char_literal (chr, rest) ->
      bprint_char_literal buf chr;
      fmtiter rest ign_flag;

    | Format_arg (pad_opt, fmtty, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_pad_opt buf pad_opt; buffer_add_char buf '{';
      bprint_fmtty buf fmtty; buffer_add_char buf '%'; buffer_add_char buf '}';
      fmtiter rest false;
    | Format_subst (pad_opt, _, fmtty, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_pad_opt buf pad_opt; buffer_add_char buf '(';
      bprint_fmtty buf fmtty; buffer_add_char buf '%'; buffer_add_char buf ')';
      fmtiter rest false;

    | Scan_char_set (width_opt, char_set, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_pad_opt buf width_opt; bprint_char_set buf char_set;
      fmtiter rest false;
    | Scan_get_counter (counter, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf (char_of_counter counter);
      fmtiter rest false;
    | Ignored_param (ign, rest) ->
      let Param_format_EBB fmt' = param_format_of_ignored_format ign rest in
      fmtiter fmt' true;

    | Formatting (fmting, rest) ->
      bprint_string_literal buf (string_of_formatting fmting);
      fmtiter rest ign_flag;

    | End_of_format -> ()

  in fmtiter fmt false

(***)

(* Convert a format to string. *)
let string_of_fmt fmt =
  let buf = buffer_create 16 in
  bprint_fmt buf fmt;
  buffer_contents buf

(******************************************************************************)
                          (* Type extraction *)

(* Extract the type representation (an fmtty) of a format. *)
let rec fmtty_of_fmt : type a b c d e f .
  (a, b, c, d, e, f) CamlinternalFormatBasics.fmt -> (a, b, c, d, e, f) fmtty =
fun fmtty -> match fmtty with
  | String (pad, rest) ->
    fmtty_of_padding_fmtty pad (String_ty (fmtty_of_fmt rest))
  | Caml_string (pad, rest) ->
    fmtty_of_padding_fmtty pad (String_ty (fmtty_of_fmt rest))

  | Int (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Int_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty
  | Int32 (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Int32_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty
  | Nativeint (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Nativeint_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty
  | Int64 (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Int64_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty
  | Float (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Float_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty

  | Char rest                  -> Char_ty (fmtty_of_fmt rest)
  | Caml_char rest             -> Char_ty (fmtty_of_fmt rest)
  | Bool rest                  -> Bool_ty (fmtty_of_fmt rest)
  | Alpha rest                 -> Alpha_ty (fmtty_of_fmt rest)
  | Theta rest                 -> Theta_ty (fmtty_of_fmt rest)
  | Reader rest                -> Reader_ty (fmtty_of_fmt rest)

  | Format_arg (_, ty, rest) ->
    Format_arg_ty (ty, fmtty_of_fmt rest)
  | Format_subst (_, rnu, ty, rest) ->
    Format_subst_ty (rnu, ty, fmtty_of_fmt rest)

  | Flush rest                 -> fmtty_of_fmt rest
  | String_literal (_, rest)   -> fmtty_of_fmt rest
  | Char_literal (_, rest)     -> fmtty_of_fmt rest

  | Scan_char_set (_, _, rest) -> String_ty (fmtty_of_fmt rest)
  | Scan_get_counter (_, rest) -> Int_ty (fmtty_of_fmt rest)
  | Ignored_param (ign, rest)  -> fmtty_of_ignored_format ign rest
  | Formatting (_, rest)       -> fmtty_of_fmt rest

  | End_of_format              -> End_of_fmtty

(* Extract the fmtty of an ignored parameter followed by the rest of
   the format. *)
and fmtty_of_ignored_format : type x y a b c d e f .
    (a, b, c, d, y, x) ignored ->
    (x, b, c, y, e, f) CamlinternalFormatBasics.fmt ->
    (a, b, c, d, e, f) fmtty =
fun ign fmt -> match ign with
  | Ignored_char                    -> fmtty_of_fmt fmt
  | Ignored_caml_char               -> fmtty_of_fmt fmt
  | Ignored_string _                -> fmtty_of_fmt fmt
  | Ignored_caml_string _           -> fmtty_of_fmt fmt
  | Ignored_int (_, _)              -> fmtty_of_fmt fmt
  | Ignored_int32 (_, _)            -> fmtty_of_fmt fmt
  | Ignored_nativeint (_, _)        -> fmtty_of_fmt fmt
  | Ignored_int64 (_, _)            -> fmtty_of_fmt fmt
  | Ignored_float (_, _)            -> fmtty_of_fmt fmt
  | Ignored_bool                    -> fmtty_of_fmt fmt
  | Ignored_format_arg _            -> fmtty_of_fmt fmt
  | Ignored_format_subst (_, fmtty) -> concat_fmtty fmtty (fmtty_of_fmt fmt)
  | Ignored_reader                  -> Ignored_reader_ty (fmtty_of_fmt fmt)
  | Ignored_scan_char_set _         -> fmtty_of_fmt fmt
  | Ignored_scan_get_counter _      -> fmtty_of_fmt fmt

(* Add an Int_ty node if padding is taken as an extra argument (ex: "%*s"). *)
and fmtty_of_padding_fmtty : type x a b c d e f .
    (x, a) padding -> (a, b, c, d, e, f) fmtty -> (x, b, c, d, e, f) fmtty =
  fun pad fmtty -> match pad with
    | No_padding    -> fmtty
    | Lit_padding _ -> fmtty
    | Arg_padding _ -> Int_ty fmtty

(* Add an Int_ty node if precision is taken as an extra argument (ex: "%.*f").*)
and fmtty_of_precision_fmtty : type x a b c d e f .
    (x, a) precision -> (a, b, c, d, e, f) fmtty -> (x, b, c, d, e, f) fmtty =
  fun prec fmtty -> match prec with
    | No_precision    -> fmtty
    | Lit_precision _ -> fmtty
    | Arg_precision   -> Int_ty fmtty

(******************************************************************************)
                            (* Format typing *)

(* Exception raised by type_XXX when a typing error occurs. *)
exception Type_mismatch

(* Type a padding. *)
(* Take an Int_ty from the fmtty if the integer should be kept as argument. *)
(* Raise Type_mismatch in case of type mismatch. *)
let type_padding : type a b c d e f x y .
    (x, y) padding -> (a, b, c, d, e, f) fmtty ->
      (a, b, c, d, e, f) padding_fmtty_ebb =
fun pad fmtty -> match pad, fmtty with
  | No_padding, _ -> Padding_fmtty_EBB (No_padding, fmtty)
  | Lit_padding (padty, w), _ -> Padding_fmtty_EBB (Lit_padding (padty,w),fmtty)
  | Arg_padding padty, Int_ty rest -> Padding_fmtty_EBB (Arg_padding padty,rest)
  | _ -> raise Type_mismatch

(* Convert a (upadding, uprecision) to a (padding, precision). *)
(* Take one or two Int_ty from the fmtty if needed. *)
(* Raise Type_mismatch in case of type mismatch. *)
let type_padprec : type a b c d e f x y z .
  (x, y) padding -> (y, z) precision -> (a, b, c, d, e, f) fmtty ->
    (a, b, c, d, e, f) padprec_fmtty_ebb =
fun pad prec fmtty -> match prec, type_padding pad fmtty with
  | No_precision, Padding_fmtty_EBB (pad, rest) ->
    Padprec_fmtty_EBB (pad, No_precision, rest)
  | Lit_precision p, Padding_fmtty_EBB (pad, rest) ->
    Padprec_fmtty_EBB (pad, Lit_precision p, rest)
  | Arg_precision, Padding_fmtty_EBB (pad, Int_ty rest) ->
    Padprec_fmtty_EBB (pad, Arg_precision, rest)
  | _, Padding_fmtty_EBB (_, _) -> raise Type_mismatch

(* Type a format according to an fmtty. *)
(* If typing succeed, generate a copy of the format with the same
    type parameters as the fmtty. *)
(* Raise a Failure with an error message in case of type mismatch. *)
let rec type_format : type x t u v a b c d e f .
    (x, b, c, t, u, v) CamlinternalFormatBasics.fmt ->
    (a, b, c, d, e, f) fmtty ->
    (a, b, c, d, e, f) CamlinternalFormatBasics.fmt =
fun fmt fmtty -> match fmt, fmtty with
  | Char fmt_rest, Char_ty fmtty_rest ->
    Char (type_format fmt_rest fmtty_rest)
  | Caml_char fmt_rest, Char_ty fmtty_rest ->
    Caml_char (type_format fmt_rest fmtty_rest)
  | String (pad, fmt_rest), _ -> (
    match type_padding pad fmtty with
    | Padding_fmtty_EBB (pad, String_ty fmtty_rest) ->
      String (pad, type_format fmt_rest fmtty_rest)
    | Padding_fmtty_EBB (_, _) -> raise Type_mismatch
  )
  | Caml_string (pad, fmt_rest), _ -> (
    match type_padding pad fmtty with
    | Padding_fmtty_EBB (pad, String_ty fmtty_rest) ->
      Caml_string (pad, type_format fmt_rest fmtty_rest)
    | Padding_fmtty_EBB (_, _) -> raise Type_mismatch
  )
  | Int (iconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Int_ty fmtty_rest) ->
      Int (iconv, pad, prec, type_format fmt_rest fmtty_rest)
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Int32 (iconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Int32_ty fmtty_rest) ->
      Int32 (iconv, pad, prec, type_format fmt_rest fmtty_rest)
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Nativeint (iconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Nativeint_ty fmtty_rest) ->
      Nativeint (iconv, pad, prec, type_format fmt_rest fmtty_rest)
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Int64 (iconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Int64_ty fmtty_rest) ->
      Int64 (iconv, pad, prec, type_format fmt_rest fmtty_rest)
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Float (fconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Float_ty fmtty_rest) ->
      Float (fconv, pad, prec, type_format fmt_rest fmtty_rest)
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Bool fmt_rest, Bool_ty fmtty_rest ->
    Bool (type_format fmt_rest fmtty_rest)
  | Flush fmt_rest, _ ->
    Flush (type_format fmt_rest fmtty)

  | String_literal (str, fmt_rest), _ ->
    String_literal (str, type_format fmt_rest fmtty)
  | Char_literal (chr, fmt_rest), _ ->
    Char_literal (chr, type_format fmt_rest fmtty)

  | Format_arg (pad_opt, sub_fmtty, fmt_rest),
    Format_arg_ty (sub_fmtty', fmtty_rest) ->
    if Fmtty_EBB sub_fmtty <> Fmtty_EBB sub_fmtty' then raise Type_mismatch;
    Format_arg (pad_opt, sub_fmtty', type_format fmt_rest fmtty_rest)
  | Format_subst (pad_opt, _, sub_fmtty, fmt_rest),
    Format_subst_ty (rnu', sub_fmtty', fmtty_rest) ->
    if Fmtty_EBB sub_fmtty <> Fmtty_EBB sub_fmtty' then raise Type_mismatch;
    Format_subst (pad_opt, rnu', sub_fmtty', type_format fmt_rest fmtty_rest)

  (* Printf and Format specific constructors: *)
  | Alpha fmt_rest, Alpha_ty fmtty_rest ->
    Alpha (type_format fmt_rest fmtty_rest)
  | Theta fmt_rest, Theta_ty fmtty_rest ->
    Theta (type_format fmt_rest fmtty_rest)

  (* Format specific constructors: *)
  | Formatting (formatting, fmt_rest), _ ->
    Formatting (formatting, type_format fmt_rest fmtty)

  (* Scanf specific constructors: *)
  | Reader fmt_rest, Reader_ty fmtty_rest ->
    Reader (type_format fmt_rest fmtty_rest)
  | Scan_char_set (width_opt, char_set, fmt_rest), String_ty fmtty_rest ->
    Scan_char_set
      (width_opt, char_set, type_format fmt_rest fmtty_rest)
  | Scan_get_counter (counter, fmt_rest), Int_ty fmtty_rest ->
    Scan_get_counter (counter, type_format fmt_rest fmtty_rest)
  | Ignored_param (ign, rest), _ ->
    type_ignored_param ign rest fmtty

  | End_of_format, End_of_fmtty -> End_of_format

  | _ -> raise Type_mismatch

(* Type and Ignored_param node according to an fmtty. *)
and type_ignored_param : type p q x t u v a b c d e f .
    (x, b, c, t, q, p) ignored ->
    (p, b, c, q, u, v) CamlinternalFormatBasics.fmt ->
    (a, b, c, d, e, f) fmtty ->
    (a, b, c, d, e, f) CamlinternalFormatBasics.fmt =
fun ign fmt fmtty -> match ign with
  | Ignored_char            as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_caml_char       as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_string _        as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_caml_string _   as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_int _           as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_int32 _         as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_nativeint _     as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_int64 _         as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_float _         as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_bool            as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_scan_char_set _ as ign'-> Ignored_param (ign',type_format fmt fmtty)
  | Ignored_format_arg (pad_opt, sub_fmtty) ->
    let ignored = Ignored_format_arg (pad_opt, sub_fmtty) in
    Ignored_param (ignored, type_format fmt fmtty)
  | Ignored_format_subst (pad_opt, sub_fmtty) ->
    let Fmtty_fmt_EBB (sub_fmtty', fmt') =
      type_ignored_format_substitution sub_fmtty fmt fmtty in
    Ignored_param (Ignored_format_subst (pad_opt, sub_fmtty'), fmt')
  | Ignored_reader ->
    begin match fmtty with
    | Ignored_reader_ty fmtty_rest ->
      Ignored_param (Ignored_reader, type_format fmt fmtty_rest)
    | _ -> raise Type_mismatch
    end
  | Ignored_scan_get_counter _ as ign' ->
    Ignored_param (ign', type_format fmt fmtty)

(* Typing of the complex case: "%_(...%)". *)
and type_ignored_format_substitution : type w z p s t u a b c d e f .
    (w, b, c, z, s, p) fmtty ->
    (p, b, c, s, t, u) CamlinternalFormatBasics.fmt ->
    (a, b, c, d, e, f) fmtty -> (a, b, c, d, e, f) fmtty_fmt_ebb =
fun sub_fmtty fmt fmtty -> match sub_fmtty, fmtty with
  | Char_ty sub_fmtty_rest, Char_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Char_ty sub_fmtty_rest', fmt')
  | String_ty sub_fmtty_rest, String_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (String_ty sub_fmtty_rest', fmt')
  | Int_ty sub_fmtty_rest, Int_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Int_ty sub_fmtty_rest', fmt')
  | Int32_ty sub_fmtty_rest, Int32_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Int32_ty sub_fmtty_rest', fmt')
  | Nativeint_ty sub_fmtty_rest, Nativeint_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Nativeint_ty sub_fmtty_rest', fmt')
  | Int64_ty sub_fmtty_rest, Int64_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Int64_ty sub_fmtty_rest', fmt')
  | Float_ty sub_fmtty_rest, Float_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Float_ty sub_fmtty_rest', fmt')
  | Bool_ty sub_fmtty_rest, Bool_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Bool_ty sub_fmtty_rest', fmt')
  | Alpha_ty sub_fmtty_rest, Alpha_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Alpha_ty sub_fmtty_rest', fmt')
  | Theta_ty sub_fmtty_rest, Theta_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Theta_ty sub_fmtty_rest', fmt')
  | Reader_ty sub_fmtty_rest, Reader_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Reader_ty sub_fmtty_rest', fmt')
  | Ignored_reader_ty sub_fmtty_rest, Ignored_reader_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Ignored_reader_ty sub_fmtty_rest', fmt')

  | Format_arg_ty (sub2_fmtty, sub_fmtty_rest),
    Format_arg_ty (sub2_fmtty', fmtty_rest) ->
    if Fmtty_EBB sub2_fmtty <> Fmtty_EBB sub2_fmtty' then raise Type_mismatch;
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Format_arg_ty (sub2_fmtty', sub_fmtty_rest'), fmt')
  | Format_subst_ty (_, sub2_fmtty, sub_fmtty_rest),
    Format_subst_ty (rnu', sub2_fmtty', fmtty_rest) ->
    if Fmtty_EBB sub2_fmtty <> Fmtty_EBB sub2_fmtty' then raise Type_mismatch;
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Format_subst_ty (rnu', sub2_fmtty', sub_fmtty_rest'),fmt')

  | End_of_fmtty, fmtty ->
    Fmtty_fmt_EBB (End_of_fmtty, type_format fmt fmtty)

  | _ -> raise Type_mismatch

(******************************************************************************)
                             (* Printing tools *)

(* Add padding spaces arround a string. *)
let fix_padding padty width str =
  let len = String.length str in
  if width <= len then str else
    let res = Bytes.make width (if padty = Zeros then '0' else ' ') in
    begin match padty with
    | Left  -> String.blit str 0 res 0 len
    | Right -> String.blit str 0 res (width - len) len
    | Zeros when len > 0 && (str.[0] = '+' || str.[0] = '-' || str.[0] = ' ') ->
      Bytes.set res 0 str.[0];
      String.blit str 1 res (width - len + 1) (len - 1)
    | Zeros when len > 1 && str.[0] = '0' && (str.[1] = 'x' || str.[1] = 'X') ->
      Bytes.set res 1 str.[1];
      String.blit str 2 res (width - len + 2) (len - 2)
    | Zeros ->
      String.blit str 0 res (width - len) len
    end;
    Bytes.unsafe_to_string res

(* Add '0' padding to int, int32, nativeint or int64 string representation. *)
let fix_int_precision prec str =
  let len = String.length str in
  if prec <= len then str else
    let res = Bytes.make prec '0' in
    begin match str.[0] with
    | ('+' | '-' | ' ') as c ->
      Bytes.set res 0 c;
      String.blit str 1 res (prec - len + 1) (len - 1);
    | '0' when len > 1 && (str.[1] = 'x' || str.[1] = 'X') ->
      Bytes.set res 1 str.[1];
      String.blit str 2 res (prec - len + 2) (len - 2);
    | '0' .. '9' ->
      String.blit str 0 res (prec - len) len;
    | _ ->
      assert false
    end;
    Bytes.unsafe_to_string res

(* Escape a string according to the OCaml lexing convention. *)
let string_to_caml_string str =
  String.concat (String.escaped str) ["\""; "\""]

(* Generate the format_int first argument from an int_conv. *)
let format_of_iconv iconv = match iconv with
  | Int_d -> "%d" | Int_pd -> "%+d" | Int_sd -> "% d"
  | Int_i -> "%i" | Int_pi -> "%+i" | Int_si -> "% i"
  | Int_x -> "%x" | Int_Cx -> "%#x"
  | Int_X -> "%X" | Int_CX -> "%#X"
  | Int_o -> "%o" | Int_Co -> "%#o"
  | Int_u -> "%u"

(* Generate the format_int32, format_nativeint and format_int64 first
   argument from an int_conv. *)
let format_of_aconv iconv c =
  let seps = match iconv with
  | Int_d -> ["%";"d"] | Int_pd -> ["%+";"d"] | Int_sd -> ["% ";"d"]
  | Int_i -> ["%";"i"] | Int_pi -> ["%+";"i"] | Int_si -> ["% ";"i"]
  | Int_x -> ["%";"x"] | Int_Cx -> ["%#";"x"]
  | Int_X -> ["%";"X"] | Int_CX -> ["%#";"X"]
  | Int_o -> ["%";"o"] | Int_Co -> ["%#";"o"]
  | Int_u -> ["%";"u"]
  in String.concat (String.make 1 c) seps

(* Generate the format_float first argument form a float_conv. *)
let format_of_fconv fconv prec =
  let symb = if fconv = Float_F then 'g' else char_of_fconv fconv in
  let buf = buffer_create 16 in
  buffer_add_char buf '%';
  bprint_fconv_flag buf fconv;
  buffer_add_char buf '.';
  buffer_add_string buf (string_of_int prec);
  buffer_add_char buf symb;
  buffer_contents buf

(* Convert an integer to a string according to a conversion. *)
let convert_int iconv n = format_int (format_of_iconv iconv) n
let convert_int32 iconv n = format_int32 (format_of_aconv iconv 'l') n
let convert_nativeint iconv n = format_nativeint (format_of_aconv iconv 'n') n
let convert_int64 iconv n = format_int64 (format_of_aconv iconv 'L') n

(* Convert a float to string. *)
(* Fix special case of "OCaml float format". *)
let convert_float fconv prec x =
  let str = format_float (format_of_fconv fconv prec) x in
  if fconv <> Float_F then str else
    let len = String.length str in
    let rec is_valid i =
      if i = len then false else
        match str.[i] with
        | '.' | 'e' | 'E' -> true
        | _ -> is_valid (i + 1)
    in
    match classify_float x with
    | FP_normal | FP_subnormal | FP_zero when not (is_valid 0) -> str ^ "."
    | FP_infinite | FP_nan | FP_normal | FP_subnormal | FP_zero -> str

(* Convert a char to a string according to the OCaml lexical convention. *)
let format_caml_char c =
  String.concat (Char.escaped c) ["'"; "'"]

(* Convert a format type to string *)
let string_of_fmtty fmtty =
  let buf = buffer_create 16 in
  bprint_fmtty buf fmtty;
  buffer_contents buf

(******************************************************************************)
                        (* Generic printing function *)

(* Make a generic printing function. *)
(* Used to generate Printf and Format printing functions. *)
(* Parameters:
     k: a continuation finally applied to the output stream and the accumulator.
     o: the output stream (see k, %a and %t).
     acc: rev list of printing entities (string, char, flush, formatting, ...).
     fmt: the format. *)
let rec make_printf : type a b c d .
    (b -> (b, c) acc -> d) -> b -> (b, c) acc ->
    (a, b, c, c, c, d) CamlinternalFormatBasics.fmt -> a =
fun k o acc fmt -> match fmt with
  | Char rest ->
    fun c ->
      let new_acc = Acc_char (acc, c) in
      make_printf k o new_acc rest
  | Caml_char rest ->
    fun c ->
      let new_acc = Acc_string (acc, format_caml_char c) in
      make_printf k o new_acc rest
  | String (pad, rest) ->
    make_string_padding k o acc rest pad (fun str -> str)
  | Caml_string (pad, rest) ->
    make_string_padding k o acc rest pad string_to_caml_string
  | Int (iconv, pad, prec, rest) ->
    make_int_padding_precision k o acc rest pad prec convert_int iconv
  | Int32 (iconv, pad, prec, rest) ->
    make_int_padding_precision k o acc rest pad prec convert_int32 iconv
  | Nativeint (iconv, pad, prec, rest) ->
    make_int_padding_precision k o acc rest pad prec convert_nativeint iconv
  | Int64 (iconv, pad, prec, rest) ->
    make_int_padding_precision k o acc rest pad prec convert_int64 iconv
  | Float (fconv, pad, prec, rest) ->
    make_float_padding_precision k o acc rest pad prec fconv
  | Bool rest ->
    fun b -> make_printf k o (Acc_string (acc, string_of_bool b)) rest
  | Alpha rest ->
    fun f x -> make_printf k o (Acc_delay (acc, fun o -> f o x)) rest
  | Theta rest ->
    fun f -> make_printf k o (Acc_delay (acc, f)) rest
  | Reader _ ->
    (* This case is impossible, by typing of formats. *)
    (* Indeed, since printf and co. take a format4 as argument, the 'd and 'e
       type parameters of fmt are obviously equals. The Reader is the
       only constructor which touch 'd and 'e type parameters of the format
       type, it adds an (->) to the 'd parameters. Consequently, a format4
       cannot contain a Reader node, except in the sub-format associated to
       an %{...%}. It's not a problem because make_printf do not call
       itself recursively on the sub-format associated to %{...%}. *)
    assert false
  | Flush rest ->
    make_printf k o (Acc_flush acc) rest

  | String_literal (str, rest) ->
    make_printf k o (Acc_string (acc, str)) rest
  | Char_literal (chr, rest) ->
    make_printf k o (Acc_char (acc, chr)) rest

  | Format_arg (_, sub_fmtty, rest) ->
    if legacy_behavior then
      let ty = string_of_fmtty sub_fmtty in
      (fun _str ->
        make_printf k o (Acc_string (acc, ty)) rest)
    else (fun (_, str) -> make_printf k o (Acc_string (acc, str)) rest)
  | Format_subst (_, _, fmtty, rest) ->
    (* Call to type_format can't fail (raise Type_mismatch). *)
    fun (fmt, _) -> make_printf k o acc
      (concat_fmt (type_format fmt fmtty) rest)

  | Scan_char_set (_, _, rest) ->
    let new_acc = Acc_invalid_arg (acc, "Printf: bad conversion %[") in
    fun _ -> make_printf k o new_acc rest
  | Scan_get_counter (_, rest) ->
    (* This case should be refused for Printf. *)
    (* Accepted for backward compatibility. *)
    (* Interpret %l, %n and %L as %u. *)
    fun n ->
      let new_acc = Acc_string (acc, format_int "%u" n) in
      make_printf k o new_acc rest
  | Ignored_param (ign, rest) ->
    make_ignored_param k o acc ign rest

  | Formatting (fmting, rest) ->
    make_printf k o (Acc_formatting (acc, fmting)) rest

  | End_of_format ->
    k o acc

(* Delay the error (Invalid_argument "Printf: bad conversion %_"). *)
(* Generate functions to take remaining arguments (after the "%_"). *)
and make_ignored_param : type x y a b c f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, c, y, x) CamlinternalFormatBasics.ignored ->
    (x, b, c, y, c, f) CamlinternalFormatBasics.fmt -> a =
fun k o acc ign fmt -> match ign with
  | Ignored_char                    -> make_invalid_arg k o acc fmt
  | Ignored_caml_char               -> make_invalid_arg k o acc fmt
  | Ignored_string _                -> make_invalid_arg k o acc fmt
  | Ignored_caml_string _           -> make_invalid_arg k o acc fmt
  | Ignored_int (_, _)              -> make_invalid_arg k o acc fmt
  | Ignored_int32 (_, _)            -> make_invalid_arg k o acc fmt
  | Ignored_nativeint (_, _)        -> make_invalid_arg k o acc fmt
  | Ignored_int64 (_, _)            -> make_invalid_arg k o acc fmt
  | Ignored_float (_, _)            -> make_invalid_arg k o acc fmt
  | Ignored_bool                    -> make_invalid_arg k o acc fmt
  | Ignored_format_arg _            -> make_invalid_arg k o acc fmt
  | Ignored_format_subst (_, fmtty) -> make_from_fmtty k o acc fmtty fmt
  | Ignored_reader                  -> assert false
  | Ignored_scan_char_set _         -> make_invalid_arg k o acc fmt
  | Ignored_scan_get_counter _      -> make_invalid_arg k o acc fmt


(* Special case of printf "%_(". *)
and make_from_fmtty : type x y a b c f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, c, y, x) CamlinternalFormatBasics.fmtty ->
    (x, b, c, y, c, f) CamlinternalFormatBasics.fmt -> a =
fun k o acc fmtty fmt -> match fmtty with
  | Char_ty rest            -> fun _ -> make_from_fmtty k o acc rest fmt
  | String_ty rest          -> fun _ -> make_from_fmtty k o acc rest fmt
  | Int_ty rest             -> fun _ -> make_from_fmtty k o acc rest fmt
  | Int32_ty rest           -> fun _ -> make_from_fmtty k o acc rest fmt
  | Nativeint_ty rest       -> fun _ -> make_from_fmtty k o acc rest fmt
  | Int64_ty rest           -> fun _ -> make_from_fmtty k o acc rest fmt
  | Float_ty rest           -> fun _ -> make_from_fmtty k o acc rest fmt
  | Bool_ty rest            -> fun _ -> make_from_fmtty k o acc rest fmt
  | Alpha_ty rest           -> fun _ _ -> make_from_fmtty k o acc rest fmt
  | Theta_ty rest           -> fun _ -> make_from_fmtty k o acc rest fmt
  | Reader_ty _             -> assert false
  | Ignored_reader_ty _     -> assert false
  | Format_arg_ty (_, rest) -> fun _ -> make_from_fmtty k o acc rest fmt
  | End_of_fmtty            -> make_invalid_arg k o acc fmt
  | Format_subst_ty (_, ty, rest) ->
    fun _ -> make_from_fmtty k o acc (concat_fmtty ty rest) fmt

(* Insert an Acc_invalid_arg in the accumulator and continue to generate
   closures to get the remaining arguments. *)
and make_invalid_arg : type a b c f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, c, c, f) CamlinternalFormatBasics.fmt -> a =
fun k o acc fmt ->
  make_printf k o (Acc_invalid_arg (acc, "Printf: bad conversion %_")) fmt

(* Fix padding, take it as an extra integer argument if needed. *)
and make_string_padding : type x z a b c d .
    (b -> (b, c) acc -> d) -> b -> (b, c) acc ->
    (a, b, c, c, c, d) CamlinternalFormatBasics.fmt ->
    (x, z -> a) padding -> (z -> string) -> x =
  fun k o acc fmt pad trans -> match pad with
  | No_padding ->
    fun x ->
      let new_acc = Acc_string (acc, trans x) in
      make_printf k o new_acc fmt
  | Lit_padding (padty, width) ->
    fun x ->
      let new_acc = Acc_string (acc, fix_padding padty width (trans x)) in
      make_printf k o new_acc fmt
  | Arg_padding padty ->
    fun w x ->
      let new_acc = Acc_string (acc, fix_padding padty w (trans x)) in
      make_printf k o new_acc fmt

(* Fix padding and precision for int, int32, nativeint or int64. *)
(* Take one or two extra integer arguments if needed. *)
and make_int_padding_precision : type x y z a b c d .
    (b -> (b, c) acc -> d) -> b -> (b, c) acc ->
    (a, b, c, c, c, d) CamlinternalFormatBasics.fmt ->
    (x, y) padding -> (y, z -> a) precision -> (int_conv -> z -> string) ->
    int_conv -> x =
  fun k o acc fmt pad prec trans iconv -> match pad, prec with
  | No_padding, No_precision ->
    fun x ->
      let str = trans iconv x in
      make_printf k o (Acc_string (acc, str)) fmt
  | No_padding, Lit_precision p ->
    fun x ->
      let str = fix_int_precision p (trans iconv x) in
      make_printf k o (Acc_string (acc, str)) fmt
  | No_padding, Arg_precision ->
    fun p x ->
      let str = fix_int_precision p (trans iconv x) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Lit_padding (padty, w), No_precision ->
    fun x ->
      let str = fix_padding padty w (trans iconv x) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Lit_padding (padty, w), Lit_precision p ->
    fun x ->
      let str = fix_padding padty w (fix_int_precision p (trans iconv x)) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Lit_padding (padty, w), Arg_precision ->
    fun p x ->
      let str = fix_padding padty w (fix_int_precision p (trans iconv x)) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Arg_padding padty, No_precision ->
    fun w x ->
      let str = fix_padding padty w (trans iconv x) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Arg_padding padty, Lit_precision p ->
    fun w x ->
      let str = fix_padding padty w (fix_int_precision p (trans iconv x)) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Arg_padding padty, Arg_precision ->
    fun w p x ->
      let str = fix_padding padty w (fix_int_precision p (trans iconv x)) in
      make_printf k o (Acc_string (acc, str)) fmt

(* Convert a float, fix padding and precision if needed. *)
(* Take the float argument and one or two extra integer arguments if needed. *)
and make_float_padding_precision : type x y a b c d .
    (b -> (b, c) acc -> d) -> b -> (b, c) acc ->
    (a, b, c, c, c, d) CamlinternalFormatBasics.fmt ->
    (x, y) padding -> (y, float -> a) precision -> float_conv -> x =
  fun k o acc fmt pad prec fconv -> match pad, prec with
  | No_padding, No_precision ->
    fun x ->
      let str = convert_float fconv default_float_precision x in
      make_printf k o (Acc_string (acc, str)) fmt
  | No_padding, Lit_precision p ->
    fun x ->
      let str = convert_float fconv p x in
      make_printf k o (Acc_string (acc, str)) fmt
  | No_padding, Arg_precision ->
    fun p x ->
      let str = convert_float fconv p x in
      make_printf k o (Acc_string (acc, str)) fmt
  | Lit_padding (padty, w), No_precision ->
    fun x ->
      let str = convert_float fconv default_float_precision x in
      let str' = fix_padding padty w str in
      make_printf k o (Acc_string (acc, str')) fmt
  | Lit_padding (padty, w), Lit_precision p ->
    fun x ->
      let str = fix_padding padty w (convert_float fconv p x) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Lit_padding (padty, w), Arg_precision ->
    fun p x ->
      let str = fix_padding padty w (convert_float fconv p x) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Arg_padding padty, No_precision ->
    fun w x ->
      let str = convert_float fconv default_float_precision x in
      let str' = fix_padding padty w str in
      make_printf k o (Acc_string (acc, str')) fmt
  | Arg_padding padty, Lit_precision p ->
    fun w x ->
      let str = fix_padding padty w (convert_float fconv p x) in
      make_printf k o (Acc_string (acc, str)) fmt
  | Arg_padding padty, Arg_precision ->
    fun w p x ->
      let str = fix_padding padty w (convert_float fconv p x) in
      make_printf k o (Acc_string (acc, str)) fmt

(******************************************************************************)
                          (* Continuations for make_printf *)

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in an output_stream. *)
(* Used as a continuation of make_printf. *)
let rec output_acc o acc = match acc with
  | Acc_formatting (p, fmting) ->
    let s = string_of_formatting fmting in
    output_acc o p;
    output_string o s;
  | Acc_string (p, s)        -> output_acc o p; output_string o s
  | Acc_char (p, c)          -> output_acc o p; output_char o c
  | Acc_delay (p, f)         -> output_acc o p; f o
  | Acc_flush p              -> output_acc o p; flush o
  | Acc_invalid_arg (p, msg) -> output_acc o p; invalid_arg msg;
  | End_of_acc               -> ()

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in a buffer. *)
(* Used as a continuation of make_printf. *)
let rec bufput_acc b acc = match acc with
  | Acc_formatting (p, fmting) ->
    let s = string_of_formatting fmting in
    bufput_acc b p;
    Buffer.add_string b s;
  | Acc_string (p, s)        -> bufput_acc b p; Buffer.add_string b s
  | Acc_char (p, c)          -> bufput_acc b p; Buffer.add_char b c
  | Acc_delay (p, f)         -> bufput_acc b p; f b
  | Acc_flush p              -> bufput_acc b p;
  | Acc_invalid_arg (p, msg) -> bufput_acc b p; invalid_arg msg;
  | End_of_acc               -> ()

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in a buffer. *)
(* Differ from bufput_acc by the interpretation of %a and %t. *)
(* Used as a continuation of make_printf. *)
let rec strput_acc b acc = match acc with
  | Acc_formatting (p, fmting) ->
    let s = string_of_formatting fmting in
    strput_acc b p;
    Buffer.add_string b s;
  | Acc_string (p, s)        -> strput_acc b p; Buffer.add_string b s
  | Acc_char (p, c)          -> strput_acc b p; Buffer.add_char b c
  | Acc_delay (p, f)         -> strput_acc b p; Buffer.add_string b (f ())
  | Acc_flush p              -> strput_acc b p;
  | Acc_invalid_arg (p, msg) -> strput_acc b p; invalid_arg msg;
  | End_of_acc               -> ()

(******************************************************************************)
                          (* Error managment *)

(* Raise a Failure with a pretty-printed error message. *)
let failwith_message
    ((fmt, _) : ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6) =
  let buf = Buffer.create 256 in
  let k () acc = strput_acc buf acc; failwith (Buffer.contents buf) in
  make_printf k () End_of_acc fmt

(******************************************************************************)
                            (* Parsing tools *)

(* Create a padding_fmt_ebb from a padding and a format. *)
(* Copy the padding to disjoin the type parameters of argument and result. *)
let make_padding_fmt_ebb : type x y .
    (x, y) padding -> (_, _, _, _, _, _) CamlinternalFormatBasics.fmt ->
      (_, _, _, _, _) padding_fmt_ebb =
fun pad fmt -> match pad with
  | No_padding         -> Padding_fmt_EBB (No_padding, fmt)
  | Lit_padding (s, w) -> Padding_fmt_EBB (Lit_padding (s, w), fmt)
  | Arg_padding s      -> Padding_fmt_EBB (Arg_padding s, fmt)

(* Create a precision_fmt_ebb from a precision and a format. *)
(* Copy the precision to disjoin the type parameters of argument and result. *)
let make_precision_fmt_ebb : type x y .
    (x, y) precision -> (_, _, _, _, _, _) CamlinternalFormatBasics.fmt ->
      (_, _, _, _, _) precision_fmt_ebb =
fun prec fmt -> match prec with
  | No_precision    -> Precision_fmt_EBB (No_precision, fmt)
  | Lit_precision p -> Precision_fmt_EBB (Lit_precision p, fmt)
  | Arg_precision   -> Precision_fmt_EBB (Arg_precision, fmt)

(* Create a padprec_fmt_ebb forma a padding, a precision and a format. *)
(* Copy the padding and the precision to disjoin type parameters of arguments
   and result. *)
let make_padprec_fmt_ebb : type x y z t .
    (x, y) padding -> (z, t) precision ->
    (_, _, _, _, _, _) CamlinternalFormatBasics.fmt ->
    (_, _, _, _, _) padprec_fmt_ebb =
fun pad prec fmt ->
  let Precision_fmt_EBB (prec, fmt') = make_precision_fmt_ebb prec fmt in
  match pad with
  | No_padding         -> Padprec_fmt_EBB (No_padding, prec, fmt')
  | Lit_padding (s, w) -> Padprec_fmt_EBB (Lit_padding (s, w), prec, fmt')
  | Arg_padding s      -> Padprec_fmt_EBB (Arg_padding s, prec, fmt')

(******************************************************************************)
                             (* Format parsing *)

(* Parse a string representing a format and create a fmt_ebb. *)
(* Raise an Failure exception in case of invalid format. *)
let fmt_ebb_of_string str =
  (* Parameters naming convention:                                    *)
  (*   - lit_start: start of the literal sequence.                    *)
  (*   - str_ind: current index in the string.                        *)
  (*   - end_ind: end of the current (sub-)format.                    *)
  (*   - pct_ind: index of the '%' in the current micro-format.       *)
  (*   - zero:  is the '0' flag defined in the current micro-format.  *)
  (*   - minus: is the '-' flag defined in the current micro-format.  *)
  (*   - plus:  is the '+' flag defined in the current micro-format.  *)
  (*   - sharp: is the '#' flag defined in the current micro-format.  *)
  (*   - space: is the ' ' flag defined in the current micro-format.  *)
  (*   - ign:   is the '_' flag defined in the current micro-format.  *)
  (*   - pad: padding of the current micro-format.                    *)
  (*   - prec: precision of the current micro-format.                 *)
  (*   - symb: char representing the conversion ('c', 's', 'd', ...). *)
  (*   - char_set: set of characters as bitmap (see scanf %[...]).    *)

  (* Raise a Failure with a friendly error message. *)
  (* Used when the end of the format (or the current sub-format) was encoutered
      unexpectedly. *)
  let unexpected_end_of_format end_ind =
    failwith_message
      "invalid format %S: at character number %d, unexpected end of format"
      str end_ind;

  (* Raise Failure with a friendly error message about an option dependencie
     problem. *)
  and invalid_format_without str_ind c s =
    failwith_message
      "invalid format %S: at character number %d, '%c' without %s"
      str str_ind c s

  (* Raise Failure with a friendly error message about an unexpected
     character. *)
  and expected_character str_ind expected read =
    failwith_message
     "invalid format %S: at character number %d, %s expected, read %C"
      str str_ind expected read in

  (* Parse the string from beg_ind (included) to end_ind (excluded). *)
  let rec parse : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun beg_ind end_ind -> parse_literal beg_ind beg_ind end_ind

  (* Read literal characters up to '%' or '@' special characters. *)
  and parse_literal : type e f . int -> int -> int -> (_, _, e, f) fmt_ebb =
  fun lit_start str_ind end_ind ->
    if str_ind = end_ind then add_literal lit_start str_ind End_of_format else
      match str.[str_ind] with
      | '%' ->
        let Fmt_EBB fmt_rest = parse_format str_ind end_ind in
        add_literal lit_start str_ind fmt_rest
      | '@' ->
        let Fmt_EBB fmt_rest = parse_after_at (str_ind + 1) end_ind in
        add_literal lit_start str_ind fmt_rest
      | _ ->
        parse_literal lit_start (str_ind + 1) end_ind

  (* Parse a format after '%' *)
  and parse_format : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun pct_ind end_ind -> parse_ign pct_ind (pct_ind + 1) end_ind

  and parse_ign : type e f . int -> int -> int -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind ->
    match str.[str_ind] with
      | '_' -> parse_flags pct_ind (str_ind+1) end_ind true
      | _ -> parse_flags pct_ind str_ind end_ind false

  and parse_flags : type e f . int -> int -> int -> bool -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind ign ->
    let zero = ref false and minus = ref false
    and plus = ref false and space = ref false
    and sharp = ref false in
    let set_flag str_ind flag =
      (* in legacy mode, duplicate flags are accepted *)
      if !flag && not legacy_behavior then
        failwith_message
          "invalid format %S: at character number %d, duplicate flag %C"
          str str_ind str.[str_ind];
      flag := true;
    in
    let rec read_flags str_ind =
      if str_ind = end_ind then unexpected_end_of_format end_ind;
      begin match str.[str_ind] with
      | '0' -> set_flag str_ind zero;  read_flags (str_ind + 1)
      | '-' -> set_flag str_ind minus; read_flags (str_ind + 1)
      | '+' -> set_flag str_ind plus;  read_flags (str_ind + 1)
      | '#' -> set_flag str_ind sharp; read_flags (str_ind + 1)
      | ' ' -> set_flag str_ind space; read_flags (str_ind + 1)
      | _ ->
        parse_padding pct_ind str_ind end_ind
          !zero !minus !plus !sharp !space ign
      end
    in
    read_flags str_ind

  (* Try to read a digital or a '*' padding. *)
  and parse_padding : type e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> bool -> bool ->
        (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind zero minus plus sharp space ign ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    let padty = match zero, minus with
      | false, false -> Right
      | false, true  -> Left
      |  true, false -> Zeros
      |  true, true  ->
        if legacy_behavior then Left
        else incompatible_flag pct_ind str_ind '-' "0" in
    match str.[str_ind] with
    | '0' .. '9' ->
      let new_ind, width = parse_positive str_ind end_ind 0 in
      parse_after_padding pct_ind new_ind end_ind plus sharp space ign
        (Lit_padding (padty, width))
    | '*' ->
      parse_after_padding pct_ind (str_ind + 1) end_ind plus sharp space ign
        (Arg_padding padty)
    | _ ->
      if legacy_behavior then
        parse_after_padding pct_ind str_ind end_ind plus sharp space ign
          No_padding
      else begin match padty with
      | Left  ->
        invalid_format_without (str_ind - 1) '-' "padding"
      | Zeros ->
        invalid_format_without (str_ind - 1) '0' "padding"
      | Right ->
        parse_after_padding pct_ind str_ind end_ind plus sharp space ign
          No_padding
      end

  (* Is precision defined? *)
  and parse_after_padding : type x e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> (x, _) padding ->
        (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind plus sharp space ign pad ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | '.' ->
      parse_precision pct_ind (str_ind + 1) end_ind plus sharp space ign pad
    | symb ->
      parse_conversion pct_ind (str_ind + 1) end_ind plus sharp space ign pad
        No_precision symb

  (* Read the digital or '*' precision. *)
  and parse_precision : type x e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> (x, _) padding ->
        (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind plus sharp space ign pad ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    let parse_literal str_ind =
      let new_ind, prec = parse_positive str_ind end_ind 0 in
      parse_conversion pct_ind (new_ind + 1) end_ind plus sharp space ign pad
        (Lit_precision prec) str.[new_ind] in
    match str.[str_ind] with
    | '0' .. '9' -> parse_literal str_ind
    | ('+' | '-') when legacy_behavior ->
      (* Legacy mode would accept and ignore '+' or '-' before the
         integer describing the desired precision; not that this
         cannot happen for padding width, as '+' and '-' already have
         a semantics there.

         That said, the idea (supported by this tweak) that width and
         precision literals are "integer literals" in the OCaml sense is
         still blatantly wrong, as 123_456 or 0xFF are rejected. *)
      parse_literal (str_ind + 1)
    | '*' ->
      parse_after_precision pct_ind (str_ind + 1) end_ind plus sharp space ign
        pad Arg_precision
    | _ ->
      if legacy_behavior then
        (* note that legacy implementation did not ignore '.' without
           a number (as it does for padding indications), but
           interprets it as '.0' *)
        parse_after_precision pct_ind str_ind end_ind plus sharp space ign pad (Lit_precision 0) else
      invalid_format_without (str_ind - 1) '.' "precision"

  (* Try to read the conversion. *)
  and parse_after_precision : type x z e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> (x, _) padding ->
        (z, _) precision -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind plus sharp space ign pad prec ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    parse_conversion pct_ind (str_ind + 1) end_ind plus sharp space ign pad prec
      str.[str_ind]

  (* Case analysis on conversion. *)
  and parse_conversion : type x y z t e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> (x, y) padding ->
        (z, t) precision -> char -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind plus sharp space ign pad prec symb ->
    (* Flags used to check option usages/compatibilities. *)
    let plus_used  = ref false and sharp_used = ref false
    and space_used = ref false and ign_used   = ref false
    and pad_used   = ref false and prec_used  = ref false in

    (* Access to options, update flags. *)
    let get_plus  () = plus_used  := true; plus
    and get_sharp () = sharp_used := true; sharp
    and get_space () = space_used := true; space
    and get_ign   () = ign_used   := true; ign
    and get_pad   () = pad_used   := true; pad
    and get_prec  () = prec_used  := true; prec in

    (* Check that padty <> Zeros. *)
    let check_no_0 symb (type a) (type b) (pad : (a,b) padding) =
      match pad with
      | No_padding -> pad
      | Lit_padding ((Left | Right), _) -> pad
      | Arg_padding (Left | Right) -> pad
      | Lit_padding (Zeros, width) ->
        if legacy_behavior then Lit_padding (Right, width)
        else incompatible_flag pct_ind str_ind symb "0"
      | Arg_padding Zeros ->
        if legacy_behavior then Arg_padding Right
        else incompatible_flag pct_ind str_ind symb "0"
    in

    (* Get padding as a pad_option (see "%_", "%{", "%(" and "%[").
       (no need for legacy mode tweaking, those were rejected by the
       legacy parser as well) *)
    let get_pad_opt c = match get_pad () with
      | No_padding -> None
      | Lit_padding (Right, width) -> Some width
      | Lit_padding (Zeros, width) ->
        if legacy_behavior then Some width
        else incompatible_flag pct_ind str_ind c "'0'"
      | Lit_padding (Left, width) ->
        if legacy_behavior then Some width
        else incompatible_flag pct_ind str_ind c "'-'"
      | Arg_padding _          -> incompatible_flag pct_ind str_ind c "'*'"
    in

    (* Get precision as a prec_option (see "%_f").
       (no need for legacy mode tweaking, those were rejected by the
       legacy parser as well) *)
    let get_prec_opt () = match get_prec () with
      | No_precision       -> None
      | Lit_precision ndec -> Some ndec
      | Arg_precision      -> incompatible_flag pct_ind str_ind '_' "'*'"
    in

    let fmt_result = match symb with
    | ',' ->
      parse str_ind end_ind
    | 'c' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then Fmt_EBB (Ignored_param (Ignored_char, fmt_rest))
      else Fmt_EBB (Char fmt_rest)
    | 'C' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then Fmt_EBB (Ignored_param (Ignored_caml_char,fmt_rest))
      else Fmt_EBB (Caml_char fmt_rest)
    | 's' ->
      let pad = check_no_0 symb (get_pad ()) in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_string (get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padding_fmt_EBB (pad', fmt_rest') =
          make_padding_fmt_ebb pad fmt_rest in
        Fmt_EBB (String (pad', fmt_rest'))
    | 'S' ->
      let pad = check_no_0 symb (get_pad ()) in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_caml_string (get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padding_fmt_EBB (pad', fmt_rest') =
          make_padding_fmt_ebb pad fmt_rest in
        Fmt_EBB (Caml_string (pad', fmt_rest'))
    | 'd' | 'i' | 'x' | 'X' | 'o' | 'u' ->
      let iconv = compute_int_conv pct_ind str_ind (get_plus ()) (get_sharp ())
        (get_space ()) symb in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_int (iconv, get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Int (iconv, pad', prec', fmt_rest'))
    | 'N' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      let counter = Token_counter in
      if get_ign () then
        let ignored = Ignored_scan_get_counter counter in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
      Fmt_EBB (Scan_get_counter (counter, fmt_rest))
    | 'l' | 'n' | 'L' when str_ind=end_ind || not (is_int_base str.[str_ind]) ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      let counter = counter_of_char symb in
      if get_ign () then
        let ignored = Ignored_scan_get_counter counter in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Scan_get_counter (counter, fmt_rest))
    | 'l' ->
      let iconv =
        compute_int_conv pct_ind (str_ind + 1) (get_plus ()) (get_sharp ())
          (get_space ()) str.[str_ind] in
      let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
      if get_ign () then
        let ignored = Ignored_int32 (iconv, get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Int32 (iconv, pad', prec', fmt_rest'))
    | 'n' ->
      let iconv =
        compute_int_conv pct_ind (str_ind + 1) (get_plus ())
          (get_sharp ()) (get_space ()) str.[str_ind] in
      let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
      if get_ign () then
        let ignored = Ignored_nativeint (iconv, get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Nativeint (iconv, pad', prec', fmt_rest'))
    | 'L' ->
      let iconv =
        compute_int_conv pct_ind (str_ind + 1) (get_plus ()) (get_sharp ())
          (get_space ()) str.[str_ind] in
      let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
      if get_ign () then
        let ignored = Ignored_int64 (iconv, get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Int64 (iconv, pad', prec', fmt_rest'))
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' ->
      let fconv = compute_float_conv pct_ind str_ind (get_plus ())
        (get_space ()) symb in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_float (get_pad_opt '_', get_prec_opt ()) in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Float (fconv, pad', prec', fmt_rest'))
    | 'b' | 'B' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then Fmt_EBB (Ignored_param (Ignored_bool, fmt_rest))
      else Fmt_EBB (Bool fmt_rest)
    | 'a' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Alpha fmt_rest)
    | 't' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Theta fmt_rest)
    | 'r' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then Fmt_EBB (Ignored_param (Ignored_reader, fmt_rest))
      else Fmt_EBB (Reader fmt_rest)
    | '!' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Flush fmt_rest)
    | ('%' | '@') as c ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Char_literal (c, fmt_rest))
    | '{' ->
      let sub_end = search_subformat_end str_ind end_ind '}' in
      let Fmt_EBB sub_fmt = parse str_ind sub_end in
      let Fmt_EBB fmt_rest = parse (sub_end + 2) end_ind in
      let sub_fmtty = fmtty_of_fmt sub_fmt in
      if get_ign () then
        let ignored = Ignored_format_arg (get_pad_opt '_', sub_fmtty) in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Format_arg (get_pad_opt '{', sub_fmtty, fmt_rest))
    | '(' ->
      let sub_end = search_subformat_end str_ind end_ind ')' in
      let Fmt_EBB fmt_rest = parse (sub_end + 2) end_ind in
      let Fmt_EBB sub_fmt = parse str_ind sub_end in
      let sub_fmtty = fmtty_of_fmt sub_fmt in
      if get_ign () then
        let ignored = Ignored_format_subst (get_pad_opt '_', sub_fmtty) in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Format_subst (get_pad_opt '(',
                                  reader_nb_unifier_of_fmtty sub_fmtty,
                                  sub_fmtty, fmt_rest))
    | '[' ->
      let next_ind, char_set = parse_char_set str_ind end_ind in
      let Fmt_EBB fmt_rest = parse next_ind end_ind in
      if get_ign () then
        let ignored = Ignored_scan_char_set (get_pad_opt '_', char_set) in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Scan_char_set (get_pad_opt '[', char_set, fmt_rest))
    | '-' | '+' | '#' | ' ' | '_' ->
      failwith_message
        "invalid format %S: at character number %d, \
         flag %C is only allowed after the '%%', before padding and precision"
        str pct_ind symb
    | _ ->
      failwith_message
        "invalid format %S: at character number %d, \
         invalid conversion \"%%%c\"" str (str_ind - 1) symb
    in
    (* Check for unused options, and reject them as incompatible.

       Such checks need to be disabled in legacy mode, as the legacy
       parser silently ignored incompatible flags. *)
    if not legacy_behavior then begin
    if not !plus_used && plus then
      incompatible_flag pct_ind str_ind symb "'+'";
    if not !sharp_used && sharp then
      incompatible_flag pct_ind str_ind symb "'#'";
    if not !space_used && space then
      incompatible_flag pct_ind str_ind symb "' '";
    if not !pad_used  && Padding_EBB pad <> Padding_EBB No_padding then
      incompatible_flag pct_ind str_ind symb "`padding'";
    if not !prec_used && Precision_EBB prec <> Precision_EBB No_precision then
      incompatible_flag pct_ind str_ind (if ign then '_' else symb)
        "`precision'";
    if ign && plus then incompatible_flag pct_ind str_ind '_' "'+'";
    end;
    (* this last test must not be disabled in legacy mode,
       as ignoring it would typically result in a different typing
       than what the legacy parser used *)
    if not !ign_used && ign then
      begin match symb with
        (* argument-less formats can safely be ignored in legacy mode *)
        | ('@' | '%' | '!' | ',') when legacy_behavior -> ()
        | _ ->
          incompatible_flag pct_ind str_ind symb "'_'"
      end;
    fmt_result

  (* Parse formatting informations (after '@'). *)
  and parse_after_at : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun str_ind end_ind ->
    if str_ind = end_ind then Fmt_EBB (Char_literal ('@', End_of_format))
    else
      match str.[str_ind] with
      | '[' ->
        parse_open_box (str_ind + 1) end_ind
      | ']' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (Close_box, fmt_rest))
      | '{' ->
        parse_open_tag (str_ind + 1) end_ind
      | '}' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (Close_tag, fmt_rest))
      | ',' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (Break ("@,", 0, 0), fmt_rest))
      | ' ' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (Break ("@ ", 1, 0), fmt_rest))
      | ';' ->
        parse_good_break (str_ind + 1) end_ind
      | '?' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (FFlush, fmt_rest))
      | '\n' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (Force_newline, fmt_rest))
      | '.' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (Flush_newline, fmt_rest))
      | '<' ->
        parse_magic_size (str_ind + 1) end_ind
      | '@' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (Escaped_at, fmt_rest))
      | '%' when str_ind + 1 < end_ind && str.[str_ind + 1] = '%' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 2) end_ind in
        Fmt_EBB (Formatting (Escaped_percent, fmt_rest))
      | '%' ->
        let Fmt_EBB fmt_rest = parse str_ind end_ind in
        Fmt_EBB (Char_literal ('@', fmt_rest))
      | c ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting (Scan_indic c, fmt_rest))

  (* Try to read the optionnal <...> after "@[". *)
  and parse_open_box : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun str_ind end_ind ->
    let next_ind, box_ty, indent =
      try
        if str_ind = end_ind then raise Not_found;
        match str.[str_ind] with
        | '<' -> (
          let str_ind_1 = parse_spaces (str_ind + 1) end_ind in
          let i = ref str_ind_1 in
          while !i < end_ind && str.[!i] >= 'a' && str.[!i] <= 'z' do
            incr i;
          done;
          let box_ty = match String.sub str str_ind_1 (!i - str_ind_1) with
            | ""    -> Pp_box
            | "h"   -> Pp_hbox
            | "v"   -> Pp_vbox
            | "hv"  -> Pp_hvbox
            | "hov" -> Pp_hovbox
            | _     -> raise Not_found
          in
          let str_ind_3 = parse_spaces !i end_ind in
          match str.[str_ind_3] with
          | '0' .. '9' | '-' ->
            let str_ind_4, indent = parse_integer str_ind_3 end_ind in
            let str_ind_5 = parse_spaces str_ind_4 end_ind in
            if str.[str_ind_5] <> '>' then raise Not_found;
            str_ind_5 + 1, box_ty, indent
          | '>' ->
            str_ind_3 + 1, box_ty, 0
          | _ ->
            raise Not_found
        )
        | _ -> raise Not_found
      with Not_found | Failure _ ->
        str_ind, Pp_box, 0
    in
    let s = String.sub str (str_ind - 2) (next_ind - str_ind + 2) in
    let Fmt_EBB fmt_rest = parse next_ind end_ind in
    Fmt_EBB (Formatting (Open_box (s, box_ty, indent), fmt_rest))

  (* Try to read the optionnal <name> after "@{". *)
  and parse_open_tag : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun str_ind end_ind ->
    let next_ind, lit, name =
      try
        if str_ind = end_ind then raise Not_found;
        match str.[str_ind] with
        | '<' ->
          let ind = String.index_from str (str_ind + 1) '>' in
          if ind >= end_ind then raise Not_found;
          let lit = String.sub str (str_ind - 1) (ind - str_ind + 2) in
          let name = String.sub str (str_ind + 1) (ind - str_ind - 1) in
          ind + 1, lit, name
        | _ -> raise Not_found
      with Not_found -> str_ind, "@{", ""
    in
    let Fmt_EBB fmt_rest = parse next_ind end_ind in
    Fmt_EBB (Formatting (Open_tag (lit, name), fmt_rest))

  (* Try to read the optionnal <width offset> after "@;". *)
  and parse_good_break : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun str_ind end_ind ->
    let next_ind, formatting =
      try
        if str_ind = end_ind || str.[str_ind] <> '<' then raise Not_found; 
        let str_ind_1 = parse_spaces (str_ind + 1) end_ind in
        match str.[str_ind_1] with
        | '0' .. '9' | '-' -> (
          let str_ind_2, width = parse_integer str_ind_1 end_ind in
            let str_ind_3 = parse_spaces str_ind_2 end_ind in
            match str.[str_ind_3] with
              | '>' ->
                let s = String.sub str (str_ind-2) (str_ind_3-str_ind+3) in
                str_ind_3 + 1, Break (s, width, 0)
              | '0' .. '9' | '-' ->
                let str_ind_4, offset = parse_integer str_ind_3 end_ind in
                let str_ind_5 = parse_spaces str_ind_4 end_ind in
                if str.[str_ind_5] <> '>' then raise Not_found;
                let s = String.sub str (str_ind-2) (str_ind_5-str_ind+3) in
                str_ind_5 + 1, Break (s, width, offset)
              | _ -> raise Not_found
        )
        | _ -> raise Not_found
      with Not_found | Failure _ ->
        str_ind, Break ("@;", 1, 0)
    in
    let Fmt_EBB fmt_rest = parse next_ind end_ind in
    Fmt_EBB (Formatting (formatting, fmt_rest))

  (* Parse the size in a <n>. *)
  and parse_magic_size : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun str_ind end_ind ->
    match
      try
        let str_ind_1 = parse_spaces str_ind end_ind in
        match str.[str_ind_1] with
        | '0' .. '9' | '-' ->
          let str_ind_2, size = parse_integer str_ind_1 end_ind in
          let str_ind_3 = parse_spaces str_ind_2 end_ind in
          if str.[str_ind_3] <> '>' then raise Not_found;
          let s = String.sub str (str_ind - 2) (str_ind_3 - str_ind + 3) in
          Some (str_ind_3 + 1, Magic_size (s, size))
        | _ -> None
      with Not_found | Failure _ ->
        None
    with
    | Some (next_ind, formatting) ->
      let Fmt_EBB fmt_rest = parse next_ind end_ind in
      Fmt_EBB (Formatting (formatting, fmt_rest))
    | None ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Formatting (Scan_indic '<', fmt_rest))

  (* Parse and construct a char set. *)
  and parse_char_set str_ind end_ind =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    let mut_char_set = create_char_set () in
    let str_ind, reverse =
      match str.[str_ind] with
        | '^' -> str_ind + 1, true
        | _ -> str_ind, false in
    let next_ind = parse_char_set_start str_ind end_ind mut_char_set in
    let char_set = freeze_char_set mut_char_set in
    next_ind, (if reverse then rev_char_set char_set else char_set)

  and check_char_set_char str c =
    if c = '%' && legacy_behavior then
      failwith_message
        "non-backward-compatible format %S:\
        \ character %c was not supported as part of a charset"
        str c

  (* Parse the first character of a char set. *)
  and parse_char_set_start str_ind end_ind char_set =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    let c = str.[str_ind] in
    check_char_set_char str c;
    parse_char_set_after_char (str_ind + 1) end_ind char_set c;

  (* Parse the content of a char set until the first ']'. *)
  and parse_char_set_content str_ind end_ind char_set =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | ']' ->
      str_ind + 1
    | '-' ->
      add_in_char_set char_set '-';
      parse_char_set_content (str_ind + 1) end_ind char_set;
    | c ->
      check_char_set_char str c;
      parse_char_set_after_char (str_ind + 1) end_ind char_set c;

  (* Test for range in char set. *)
  and parse_char_set_after_char str_ind end_ind char_set c =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | ']' ->
      add_in_char_set char_set c;
      str_ind + 1
    | '-' ->
      parse_char_set_after_minus (str_ind + 1) end_ind char_set c
    | c' ->
      check_char_set_char str c';
      add_in_char_set char_set c;
      parse_char_set_after_char (str_ind + 1) end_ind char_set c'

  (* Manage range in char set (except if the '-' the last char before ']') *)
  and parse_char_set_after_minus str_ind end_ind char_set c =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | ']' ->
      add_in_char_set char_set c;
      add_in_char_set char_set '-';
      str_ind + 1
    | c' ->
      check_char_set_char str c';
      for i = int_of_char c to int_of_char c' do
        add_in_char_set char_set (char_of_int i);
      done;
      parse_char_set_content (str_ind + 1) end_ind char_set

  (* Consume all next spaces, raise an Failure if end_ind is reached. *)
  and parse_spaces str_ind end_ind =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    if str.[str_ind] = ' ' then parse_spaces (str_ind + 1) end_ind else str_ind

  (* Read a positive integer from the string, raise a Failure if end_ind is
     reached. *)
  and parse_positive str_ind end_ind acc =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | '0' .. '9' as c ->
      let new_acc = acc * 10 + (int_of_char c - int_of_char '0') in
      if new_acc > Sys.max_string_length then
        failwith_message
          "invalid format %S: integer %d is greater than the limit %d"
          str new_acc Sys.max_string_length
      else
        parse_positive (str_ind + 1) end_ind new_acc
    | _ -> str_ind, acc

  (* Read a positive or negative integer from the string, raise a Failure
     if end_ind is reached. *)
  and parse_integer str_ind end_ind =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | '0' .. '9' -> parse_positive str_ind end_ind 0
    | '-' -> (
      if str_ind + 1 = end_ind then unexpected_end_of_format end_ind;
      match str.[str_ind + 1] with
      | '0' .. '9' ->
        let next_ind, n = parse_positive (str_ind + 1) end_ind 0 in
        next_ind, -n
      | c ->
        expected_character (str_ind + 1) "digit" c
    )
    | _ -> assert false

  (* Add a literal to a format from a literal character sub-sequence. *)
  and add_literal : type a d e f .
      int -> int -> (a, _, _, d, e, f) CamlinternalFormatBasics.fmt ->
      (_, _, e, f) fmt_ebb =
  fun lit_start str_ind fmt -> match str_ind - lit_start with
    | 0    -> Fmt_EBB fmt
    | 1    -> Fmt_EBB (Char_literal (str.[lit_start], fmt))
    | size -> Fmt_EBB (String_literal (String.sub str lit_start size, fmt))

  (* Search the end of the current sub-format
     (i.e. the corresponding "%}" or "%)") *)
  and search_subformat_end str_ind end_ind c =
    if str_ind = end_ind then
      failwith_message
        "invalid format %S: unclosed sub-format, \
         expected \"%%%c\" at character number %d" str c end_ind;
    match str.[str_ind] with
    | '%' ->
      if str_ind + 1 = end_ind then unexpected_end_of_format end_ind;
      if str.[str_ind + 1] = c then (* End of format found *) str_ind else
        begin match str.[str_ind + 1] with
        | '_' ->
          (* Search for "%_(" or "%_{". *)
          if str_ind + 2 = end_ind then unexpected_end_of_format end_ind;
          begin match str.[str_ind + 2] with
          | '{' ->
            let sub_end = search_subformat_end (str_ind + 3) end_ind '}' in
            search_subformat_end (sub_end + 2) end_ind c
          | '(' ->
            let sub_end = search_subformat_end (str_ind + 3) end_ind ')' in
            search_subformat_end (sub_end + 2) end_ind c
          | _ -> search_subformat_end (str_ind + 3) end_ind c
          end
        | '{' ->
          (* %{...%} sub-format found. *)
          let sub_end = search_subformat_end (str_ind + 2) end_ind '}' in
          search_subformat_end (sub_end + 2) end_ind c
        | '(' ->
          (* %(...%) sub-format found. *)
          let sub_end = search_subformat_end (str_ind + 2) end_ind ')' in
          search_subformat_end (sub_end + 2) end_ind c
        | '}' ->
          (* Error: %(...%}. *)
          expected_character (str_ind + 1) "character ')'" '}';
        | ')' ->
          (* Error: %{...%). *)
          expected_character (str_ind + 1) "character '}'" ')';
        | _ ->
          search_subformat_end (str_ind + 2) end_ind c
        end
    | _ -> search_subformat_end (str_ind + 1) end_ind c

  (* Check if symb is a valid int conversion after "%l", "%n" or "%L" *)
  and is_int_base symb = match symb with
    | 'd' | 'i' | 'x' | 'X' | 'o' | 'u' -> true
    | _ -> false

  (* Convert a char (l, n or L) to its associated counter. *)
  and counter_of_char symb = match symb with
    | 'l' -> Line_counter  | 'n' -> Char_counter
    | 'L' -> Token_counter | _ -> assert false

  (* Convert (plus, symb) to its associated int_conv. *)
  and compute_int_conv pct_ind str_ind plus sharp space symb =
    match plus, sharp, space, symb with
    | false, false, false, 'd' -> Int_d  | false, false, false, 'i' -> Int_i
    | false, false,  true, 'd' -> Int_sd | false, false,  true, 'i' -> Int_si
    |  true, false, false, 'd' -> Int_pd |  true, false, false, 'i' -> Int_pi
    | false, false, false, 'x' -> Int_x  | false, false, false, 'X' -> Int_X
    | false,  true, false, 'x' -> Int_Cx | false,  true, false, 'X' -> Int_CX
    | false, false, false, 'o' -> Int_o
    | false,  true, false, 'o' -> Int_Co
    | false, false, false, 'u' -> Int_u
    | _, true, _, 'x' when legacy_behavior -> Int_Cx
    | _, true, _, 'X' when legacy_behavior -> Int_CX
    | _, true, _, 'o' when legacy_behavior -> Int_Co
    | _, true, _, _ ->
      if legacy_behavior then (* ignore *)
        compute_int_conv pct_ind str_ind plus false space symb
      else incompatible_flag pct_ind str_ind symb "'#'"
    | true, false, true, _ ->
      if legacy_behavior then
        (* plus and space: legacy implementation prefers plus *)
        compute_int_conv pct_ind str_ind plus sharp false symb
      else incompatible_flag pct_ind str_ind ' ' "'+'"
    | false, false, true, _    ->
      if legacy_behavior then (* ignore *)
        compute_int_conv pct_ind str_ind plus sharp false symb
      else incompatible_flag pct_ind str_ind symb "' '"
    | true, false, false, _    ->
      if legacy_behavior then (* ignore *)
        compute_int_conv pct_ind str_ind false sharp space symb
      else incompatible_flag pct_ind str_ind symb "'+'"
    | false, false, false, _ -> assert false

  (* Convert (plus, symb) to its associated float_conv. *)
  and compute_float_conv pct_ind str_ind plus space symb =
  match plus, space, symb with
    | false, false, 'f' -> Float_f  | false, false, 'e' -> Float_e
    | false,  true, 'f' -> Float_sf | false,  true, 'e' -> Float_se
    |  true, false, 'f' -> Float_pf |  true, false, 'e' -> Float_pe
    | false, false, 'E' -> Float_E  | false, false, 'g' -> Float_g
    | false,  true, 'E' -> Float_sE | false,  true, 'g' -> Float_sg
    |  true, false, 'E' -> Float_pE |  true, false, 'g' -> Float_pg
    | false, false, 'G' -> Float_G
    | false,  true, 'G' -> Float_sG
    |  true, false, 'G' -> Float_pG
    | false, false, 'F' -> Float_F
    |  true,  true, _ ->
      if legacy_behavior then
        (* plus and space: legacy implementation prefers plus *)
        compute_float_conv pct_ind str_ind plus false symb
      else incompatible_flag pct_ind str_ind ' ' "'+'"
    | false,  true, _ ->
      if legacy_behavior then (* ignore *)
        compute_float_conv pct_ind str_ind plus false symb
      else incompatible_flag pct_ind str_ind symb "' '"
    |  true, false, _ ->
      if legacy_behavior then (* ignore *)
        compute_float_conv pct_ind str_ind false space symb
      else incompatible_flag pct_ind str_ind symb "'+'"
    | false, false, _ -> assert false

  (* Raise a Failure with a friendly error message about incompatible options.*)
  and incompatible_flag : type a . int -> int -> char -> string -> a =
    fun pct_ind str_ind symb option ->
      let subfmt = String.sub str pct_ind (str_ind - pct_ind) in
      failwith_message
        "invalid format %S: at character number %d, \
         %s is incompatible with '%c' in sub-format %S"
        str pct_ind option symb subfmt;

  in parse 0 (String.length str)

(******************************************************************************)
                  (* Guarded string to format conversions *)

(* Convert a string to a format according to an fmtty. *)
(* Raise a Failure with an error message in case of type mismatch. *)
let format_of_string_fmtty str fmtty =
  let Fmt_EBB fmt = fmt_ebb_of_string str in
  try (type_format fmt fmtty, str) with Type_mismatch ->
    failwith_message
      "bad input: format type mismatch between %S and %S"
      str (string_of_fmtty fmtty)

(* Convert a string to a format compatible with an other format. *)
(* Raise a Failure with an error message in case of type mismatch. *)
let format_of_string_format str (fmt', str') =
  let Fmt_EBB fmt = fmt_ebb_of_string str in
  try (type_format fmt (fmtty_of_fmt fmt'), str) with Type_mismatch ->
    failwith_message
      "bad input: format type mismatch between %S and %S" str str'
