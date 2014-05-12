(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* Same as ../../stdlib/pervasives.ml, except that I/O functions have
   been redefined to not block the whole process, but only the calling
   thread. *)

(* type 'a option = None | Some of 'a *)

(* Exceptions *)

external register_named_value : string -> 'a -> unit
                              = "caml_register_named_value"

let () =
  (* for asmrun/fail.c *)
  register_named_value "Pervasives.array_bound_error"
    (Invalid_argument "index out of bounds")


external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit

(* Composition operators *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

(* Debugging *)

external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"

external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

(* Comparisons *)

external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external ( & ) : bool -> bool -> bool = "%sequand"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( or ) : bool -> bool -> bool = "%sequor"
external ( || ) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external ( ~- ) : int -> int = "%negint"
external ( ~+ ) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( mod ) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"

let max_int = (-1) lsr 1
let min_int = max_int + 1

(* Floating-point operations *)

external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow" "float"
external exp : float -> float = "caml_exp_float" "exp" "float"
external expm1 : float -> float = "caml_expm1_float" "caml_expm1" "float"
external acos : float -> float = "caml_acos_float" "acos" "float"
external asin : float -> float = "caml_asin_float" "asin" "float"
external atan : float -> float = "caml_atan_float" "atan" "float"
external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
external hypot : float -> float -> float
               = "caml_hypot_float" "caml_hypot" "float"
external cos : float -> float = "caml_cos_float" "cos" "float"
external cosh : float -> float = "caml_cosh_float" "cosh" "float"
external log : float -> float = "caml_log_float" "log" "float"
external log10 : float -> float = "caml_log10_float" "log10" "float"
external log1p : float -> float = "caml_log1p_float" "caml_log1p" "float"
external sin : float -> float = "caml_sin_float" "sin" "float"
external sinh : float -> float = "caml_sinh_float" "sinh" "float"
external sqrt : float -> float = "caml_sqrt_float" "sqrt" "float"
external tan : float -> float = "caml_tan_float" "tan" "float"
external tanh : float -> float = "caml_tanh_float" "tanh" "float"
external ceil : float -> float = "caml_ceil_float" "ceil" "float"
external floor : float -> float = "caml_floor_float" "floor" "float"
external abs_float : float -> float = "%absfloat"
external copysign : float -> float -> float
                  = "caml_copysign_float" "caml_copysign" "float"
external mod_float : float -> float -> float = "caml_fmod_float" "fmod" "float"
external frexp : float -> float * int = "caml_frexp_float"
external ldexp : float -> int -> float = "caml_ldexp_float"
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
let nan =
  float_of_bits 0x7F_F0_00_00_00_00_00_01L
let max_float =
  float_of_bits 0x7F_EF_FF_FF_FF_FF_FF_FFL
let min_float =
  float_of_bits 0x00_10_00_00_00_00_00_00L
let epsilon_float =
  float_of_bits 0x3C_B0_00_00_00_00_00_00L

type fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float : float -> fpclass = "caml_classify_float"

(* String and byte sequence operations -- more in modules String and Bytes *)

external string_length : string -> int = "%string_length"
external bytes_length : bytes -> int = "%string_length"
external bytes_create : int -> bytes = "caml_create_string"
external string_blit : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" "noalloc"
external bytes_blit : bytes -> int -> bytes -> int -> int -> unit
                        = "caml_blit_string" "noalloc"
external bytes_unsafe_to_string : bytes -> string = "%identity"
external bytes_unsafe_of_string : string -> bytes = "%identity"

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = bytes_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  bytes_unsafe_to_string s

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* References *)

type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"

(* String conversion functions *)

external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

let string_of_int n =
  format_int "%d" n

external int_of_string : string -> int = "caml_int_of_string"
external string_get : string -> int -> char = "%string_safe_get"

let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match string_get s i with
    | '0' .. '9' | '-' -> loop (i + 1)
    | _ -> s
  in
  loop 0
;;

let string_of_float f = valid_float_lexem (format_float "%.12g" f);;

external float_of_string : string -> float = "caml_float_of_string"

(* List operations -- more in module List *)

let rec ( @ ) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* I/O operations *)

type in_channel
type out_channel

external open_descriptor_out : int -> out_channel
                             = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"

let stdin = open_descriptor_in 0
let stdout = open_descriptor_out 1
let stderr = open_descriptor_out 2

(* Non-blocking stuff *)

external thread_wait_read_prim : Unix.file_descr -> unit = "thread_wait_read"
external thread_wait_write_prim : Unix.file_descr -> unit = "thread_wait_write"

let thread_wait_read fd = thread_wait_read_prim fd
let thread_wait_write fd = thread_wait_write_prim fd

external descr_inchan : in_channel -> Unix.file_descr
                      = "caml_channel_descriptor"
external descr_outchan : out_channel -> Unix.file_descr
                       = "caml_channel_descriptor"

let wait_inchan ic = thread_wait_read (descr_inchan ic)

let wait_outchan oc len = thread_wait_write (descr_outchan oc)

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"

let open_out_gen mode perm name =
  open_descriptor_out(open_desc name mode perm)

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

external flush_partial : out_channel -> bool = "caml_ml_flush_partial"

let rec flush oc =
  let success =
    try
      flush_partial oc
    with Sys_blocked_io ->
      wait_outchan oc (-1); false in
  if success then () else flush oc

external out_channels_list : unit -> out_channel list
                           = "caml_ml_out_channels_list"

let flush_all () =
  let rec iter = function
      [] -> ()
    | a::l ->
        begin try
            flush a
        with Sys_error _ ->
          () (* ignore channels closed during a preceding flush. *)
        end;
        iter l
  in iter (out_channels_list ())

external unsafe_output_partial : out_channel -> bytes -> int -> int -> int
                        = "caml_ml_output_partial"

let rec unsafe_output oc buf pos len =
  if len > 0 then begin
    let written =
      try
        unsafe_output_partial oc buf pos len
      with Sys_blocked_io ->
        wait_outchan oc len; 0 in
    unsafe_output oc buf (pos + written) (len - written)
  end

external output_char_blocking : out_channel -> char -> unit
                              = "caml_ml_output_char"
external output_byte_blocking : out_channel -> int -> unit
                              = "caml_ml_output_char"

let rec output_char oc c =
  try
    output_char_blocking oc c
  with Sys_blocked_io ->
    wait_outchan oc 1; output_char oc c

let output_bytes oc s =
  unsafe_output oc s 0 (bytes_length s)

let output_string oc s =
  unsafe_output oc (bytes_unsafe_of_string s) 0 (string_length s)

let output oc s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "output"
  else unsafe_output oc s ofs len

let output_substring oc s ofs len =
  output oc (bytes_unsafe_of_string s) ofs len

let rec output_byte oc b =
  try
    output_byte_blocking oc b
  with Sys_blocked_io ->
    wait_outchan oc 1; output_byte oc b

let output_binary_int oc n =
  output_byte oc (n asr 24);
  output_byte oc (n asr 16);
  output_byte oc (n asr 8);
  output_byte oc n

external marshal_to_string : 'a -> unit list -> string
                           = "caml_output_value_to_string"

let output_value oc v = output_string oc (marshal_to_string v [])

external seek_out_blocking : out_channel -> int -> unit = "caml_ml_seek_out"

let seek_out oc pos = flush oc; seek_out_blocking oc pos

external pos_out : out_channel -> int = "caml_ml_pos_out"
external out_channel_length : out_channel -> int = "caml_ml_channel_size"
external close_out_channel : out_channel -> unit = "caml_ml_close_channel"

let close_out oc = (try flush oc with _ -> ()); close_out_channel oc
let close_out_noerr oc =
  (try flush oc with _ -> ());
  (try close_out_channel oc with _ -> ())
external set_binary_mode_out : out_channel -> bool -> unit
                             = "caml_ml_set_binary_mode"

(* General input functions *)

let open_in_gen mode perm name =
  open_descriptor_in(open_desc name mode perm)

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

external input_char_blocking : in_channel -> char = "caml_ml_input_char"
external input_byte_blocking : in_channel -> int = "caml_ml_input_char"

let rec input_char ic =
  try
    input_char_blocking ic
  with Sys_blocked_io ->
    wait_inchan ic; input_char ic

external unsafe_input_blocking : in_channel -> bytes -> int -> int -> int
                               = "caml_ml_input"

let rec unsafe_input ic s ofs len =
  try
    unsafe_input_blocking ic s ofs len
  with Sys_blocked_io ->
    wait_inchan ic; unsafe_input ic s ofs len

let input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "input"
  else unsafe_input ic s ofs len

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then () else begin
    let r = unsafe_input ic s ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic s (ofs + r) (len - r)
  end

let really_input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "really_input"
  else unsafe_really_input ic s ofs len

let really_input_string ic len =
  let s = bytes_create len in
  really_input ic s 0 len;
  bytes_unsafe_to_string s

external bytes_set : bytes -> int -> char -> unit = "%string_safe_set"

let input_line ic =
  let buf = ref (bytes_create 128) in
  let pos = ref 0 in
  begin try
    while true do
      if !pos = bytes_length !buf then begin
        let newbuf = bytes_create (2 * !pos) in
        bytes_blit !buf 0 newbuf 0 !pos;
        buf := newbuf
      end;
      let c = input_char ic in
      if c = '\n' then raise Exit;
      bytes_set !buf !pos c;
      incr pos
    done
  with Exit -> ()
     | End_of_file -> if !pos = 0 then raise End_of_file
  end;
  let res = bytes_create !pos in
  bytes_blit !buf 0 res 0 !pos;
  bytes_unsafe_to_string res

let rec input_byte ic =
  try
    input_byte_blocking ic
  with Sys_blocked_io ->
    wait_inchan ic; input_byte ic

let input_binary_int ic =
  let b1 = input_byte ic in
  let n1 = if b1 >= 128 then b1 - 256 else b1 in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (n1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

external unmarshal : bytes -> int -> 'a = "caml_input_value_from_string"
external marshal_data_size : bytes -> int -> int = "caml_marshal_data_size"

let input_value ic =
  let header = bytes_create 20 in
  really_input ic header 0 20;
  let bsize = marshal_data_size header 0 in
  let buffer = bytes_create (20 + bsize) in
  bytes_blit header 0 buffer 0 20;
  really_input ic buffer 20 bsize;
  unmarshal buffer 0

external seek_in : in_channel -> int -> unit = "caml_ml_seek_in"
external pos_in : in_channel -> int = "caml_ml_pos_in"
external in_channel_length : in_channel -> int = "caml_ml_channel_size"
external close_in : in_channel -> unit = "caml_ml_close_channel"
let close_in_noerr ic = (try close_in ic with _ -> ());;
external set_binary_mode_in : in_channel -> bool -> unit
                            = "caml_ml_set_binary_mode"

(* Output functions on standard output *)

let print_char c = output_char stdout c
let print_string s = output_string stdout s
let print_bytes s = output_bytes stdout s
let print_int i = output_string stdout (string_of_int i)
let print_float f = output_string stdout (string_of_float f)
let print_endline s =
  output_string stdout s; output_char stdout '\n'; flush stdout
let print_newline () = output_char stdout '\n'; flush stdout

(* Output functions on standard error *)

let prerr_char c = output_char stderr c
let prerr_string s = output_string stderr s
let prerr_bytes s = output_bytes stderr s
let prerr_int i = output_string stderr (string_of_int i)
let prerr_float f = output_string stderr (string_of_float f)
let prerr_endline s =
  output_string stderr s; output_char stderr '\n'; flush stderr
let prerr_newline () = output_char stderr '\n'; flush stderr

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_float () = float_of_string(read_line())

(* Operations on large files *)

module LargeFile =
  struct
    external seek_out : out_channel -> int64 -> unit = "caml_ml_seek_out_64"
    external pos_out : out_channel -> int64 = "caml_ml_pos_out_64"
    external out_channel_length : out_channel -> int64
                                = "caml_ml_channel_size_64"
    external seek_in : in_channel -> int64 -> unit = "caml_ml_seek_in_64"
    external pos_in : in_channel -> int64 = "caml_ml_pos_in_64"
    external in_channel_length : in_channel -> int64 = "caml_ml_channel_size_64"
  end

(* Formats *)

module CamlinternalFormatBasics = struct
(* Type of a block used by the Format pretty-printer. *)
type block_type =
  | Pp_hbox   (* Horizontal block no line breaking *)
  | Pp_vbox   (* Vertical block each break leads to a new line *)
  | Pp_hvbox  (* Horizontal-vertical block: same as vbox, except if this block
                 is small enough to fit on a single line *)
  | Pp_hovbox (* Horizontal or Vertical block: breaks lead to new line
                 only when necessary to print the content of the block *)
  | Pp_box    (* Horizontal or Indent block: breaks lead to new line
                 only when necessary to print the content of the block, or
                 when it leads to a new indentation of the current line *)
  | Pp_fits   (* Internal usage: when a block fits on a single line *)

(* Formatting element used by the Format pretty-printter. *)
type formatting =
  | Open_box of string * block_type * int   (* @[   *)
  | Close_box                               (* @]   *)
  | Open_tag of string * string             (* @{   *)
  | Close_tag                               (* @}   *)
  | Break of string * int * int             (* @, | @  | @; | @;<> *)
  | FFlush                                  (* @?   *)
  | Force_newline                           (* @\n  *)
  | Flush_newline                           (* @.   *)
  | Magic_size of string * int              (* @<n> *)
  | Escaped_at                              (* @@   *)
  | Escaped_percent                         (* @%%  *)
  | Scan_indic of char                      (* @X   *)

(***)

(* Padding position. *)
type padty =
  | Left   (* Text is left justified ('-' option).               *)
  | Right  (* Text is right justified (no '-' option).           *)
  | Zeros  (* Text is right justified by zeros (see '0' option). *)

(***)

(* Integer conversion. *)
type int_conv =
  | Int_d | Int_pd | Int_sd        (*  %d | %+d | % d  *)
  | Int_i | Int_pi | Int_si        (*  %i | %+i | % i  *)
  | Int_x | Int_Cx                 (*  %x | %#x        *)
  | Int_X | Int_CX                 (*  %X | %#X        *)
  | Int_o | Int_Co                 (*  %o | %#o        *)
  | Int_u                          (*  %u              *)

(* Float conversion. *)
type float_conv =
  | Float_f | Float_pf | Float_sf  (*  %f | %+f | % f  *)
  | Float_e | Float_pe | Float_se  (*  %e | %+e | % e  *)
  | Float_E | Float_pE | Float_sE  (*  %E | %+E | % E  *)
  | Float_g | Float_pg | Float_sg  (*  %g | %+g | % g  *)
  | Float_G | Float_pG | Float_sG  (*  %G | %+G | % G  *)
  | Float_F                        (*  %F              *)

(***)

(* Char sets (see %[...]) are bitmaps implemented as 32-char strings. *)
type char_set = string

(***)

(* Counter used in Scanf. *)
type counter =
  | Line_counter     (*  %l      *)
  | Char_counter     (*  %n      *)
  | Token_counter    (*  %N, %L  *)

(***)

(* Padding of strings and numbers. *)
type ('a, 'b) padding =
  (* No padding (ex: "%d") *)
  | No_padding  : ('a, 'a) padding
  (* Literal padding (ex: "%8d") *)
  | Lit_padding : padty * int -> ('a, 'a) padding
  (* Padding as extra argument (ex: "%*d") *)
  | Arg_padding : padty -> (int -> 'a, 'a) padding

(* Some formats, such as %_d,
   only accept an optional number as padding option (no extra argument) *)
type pad_option = int option

(* Precision of floats and '0'-padding of integers. *)
type ('a, 'b) precision =
  (* No precision (ex: "%f") *)
  | No_precision : ('a, 'a) precision
  (* Literal precision (ex: "%.3f") *)
  | Lit_precision : int -> ('a, 'a) precision
  (* Precision as extra argument (ex: "%.*f") *)
  | Arg_precision : (int -> 'a, 'a) precision

(* Some formats, such as %_f,
   only accept an optional number as precision option (no extra argument) *)
type prec_option = int option

(***)

(* Type used in Format_subst_ty and Format_subst constructors as "a proof"
   of '->' number equality between two ('d, 'e) relations. *)
(* See the scanf implementation of "%(...%)". *)
(* Not meaningful for Printf and Format since "%r" is Scanf specific. *)
type ('d1, 'e1, 'd2, 'e2) reader_nb_unifier =
  | Zero_reader :
      ('d1, 'd1, 'd2, 'd2) reader_nb_unifier
  | Succ_reader :
      ('d1, 'e1, 'd2, 'e2) reader_nb_unifier ->
        ('x -> 'd1, 'e1, 'x -> 'd2, 'e2) reader_nb_unifier

(***)

(* List of format type elements. *)
(* In particular used to represent %(...%) and %{...%} contents. *)
type ('a, 'b, 'c, 'd, 'e, 'f) fmtty =
  | Char_ty :                                                 (* %c  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (char -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | String_ty :                                               (* %s  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (string -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | Int_ty :                                                  (* %d  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (int -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | Int32_ty :                                                (* %ld *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (int32 -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | Nativeint_ty :                                            (* %nd *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (nativeint -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | Int64_ty :                                                (* %Ld *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (int64 -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | Float_ty :                                                (* %f  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (float -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | Bool_ty :                                                 (* %B  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (bool -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty

  | Format_arg_ty :                                           (* %{...%} *)
      ('g, 'h, 'i, 'j, 'k, 'l) fmtty *
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | Format_subst_ty :                                         (* %(...%) *)
      ('d1, 'q1, 'd2, 'q2) reader_nb_unifier *
      ('x, 'b, 'c, 'd1, 'q1, 'u) fmtty *
      ('u, 'b, 'c, 'q1, 'e1, 'f) fmtty ->
        (('x, 'b, 'c, 'd2, 'q2, 'u) format6 -> 'x, 'b, 'c, 'd1, 'e1, 'f) fmtty

  (* Printf and Format specific constructors. *)
  | Alpha_ty :                                                (* %a  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (('b -> 'x -> 'c) -> 'x -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty
  | Theta_ty :                                                (* %t  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        (('b -> 'c) -> 'a, 'b, 'c, 'd, 'e, 'f) fmtty

  (* Scanf specific constructor. *)
  | Reader_ty :                                               (* %r  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        ('x -> 'a, 'b, 'c, ('b -> 'x) -> 'd, 'e, 'f) fmtty
  | Ignored_reader_ty :                                       (* %_r  *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        ('a, 'b, 'c, ('b -> 'x) -> 'd, 'e, 'f) fmtty

  | End_of_fmtty :
        ('f, 'b, 'c, 'd, 'd, 'f) fmtty

(***)

(* List of format elements. *)
and ('a, 'b, 'c, 'd, 'e, 'f) fmt =
  | Char :                                                   (* %c *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (char -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Caml_char :                                              (* %C *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (char -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | String :                                                 (* %s *)
      ('x, string -> 'a) padding * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Caml_string :                                            (* %S *)
      ('x, string -> 'a) padding * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Int :                                                    (* %[dixXuo] *)
      int_conv * ('x, 'y) padding * ('y, int -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Int32 :                                                  (* %l[dixXuo] *)
      int_conv * ('x, 'y) padding * ('y, int32 -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Nativeint :                                              (* %n[dixXuo] *)
      int_conv * ('x, 'y) padding * ('y, nativeint -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Int64 :                                                  (* %L[dixXuo] *)
      int_conv * ('x, 'y) padding * ('y, int64 -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Float :                                                  (* %[feEgGF] *)
      float_conv * ('x, 'y) padding * ('y, float -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Bool :                                                   (* %[bB] *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (bool -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Flush :                                                  (* %! *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt

  | String_literal :                                         (* abc *)
      string * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt
  | Char_literal :                                           (* x *)
      char * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt

  | Format_arg :                                             (* %{...%} *)
      pad_option * ('g, 'h, 'i, 'j, 'k, 'l) fmtty *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Format_subst :                                           (* %(...%) *)
      pad_option * ('d1, 'q1, 'd2, 'q2) reader_nb_unifier *
      ('x, 'b, 'c, 'd1, 'q1, 'u) fmtty *
      ('u, 'b, 'c, 'q1, 'e1, 'f) fmt ->
        (('x, 'b, 'c, 'd2, 'q2, 'u) format6 -> 'x, 'b, 'c, 'd1, 'e1, 'f) fmt

  (* Printf and Format specific constructor. *)
  | Alpha :                                                  (* %a *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (('b -> 'x -> 'c) -> 'x -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Theta :                                                  (* %t *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (('b -> 'c) -> 'a, 'b, 'c, 'd, 'e, 'f) fmt

  (* Format specific constructor: *)
  | Formatting :                                             (* @_ *)
      formatting * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt

  (* Scanf specific constructors: *)
  | Reader :                                                 (* %r *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x -> 'a, 'b, 'c, ('b -> 'x) -> 'd, 'e, 'f) fmt
  | Scan_char_set :                                          (* %[...] *)
      pad_option * char_set * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (string -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Scan_get_counter :                                       (* %[nlNL] *)
      counter * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (int -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Ignored_param :                                          (* %_ *)
      ('a, 'b, 'c, 'd, 'y, 'x) ignored * ('x, 'b, 'c, 'y, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt

  | End_of_format :
        ('f, 'b, 'c, 'e, 'e, 'f) fmt

(***)

(* Type for ignored parameters (see "%_"). *)
and ('a, 'b, 'c, 'd, 'e, 'f) ignored =
  | Ignored_char :                                           (* %_c *)
      ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_caml_char :                                      (* %_C *)
      ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_string :                                         (* %_s *)
      pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_caml_string :                                    (* %_S *)
      pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int :                                            (* %_d *)
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int32 :                                          (* %_ld *)
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_nativeint :                                      (* %_nd *)
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int64 :                                          (* %_Ld *)
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_float :                                          (* %_f *)
      pad_option * prec_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_bool :                                           (* %_B *)
      ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_format_arg :                                     (* %_{...%} *)
      pad_option * ('x, 'b, 'c, 'y, 'z, 't) fmtty ->
        ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_format_subst :                                   (* %_(...%) *)
      pad_option * ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        ('a, 'b, 'c, 'd, 'e, 'f) ignored
  | Ignored_reader :                                         (* %_r *)
      ('a, 'b, 'c, ('b -> 'x) -> 'd, 'd, 'a) ignored
  | Ignored_scan_char_set :                                  (* %_[...] *)
      pad_option * char_set -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_scan_get_counter :                               (* %_[nlNL] *)
      counter -> ('a, 'b, 'c, 'd, 'd, 'a) ignored

and ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) fmt * string

(******************************************************************************)
                         (* Format type concatenation *)

(* Concatenate two format types. *)
(* Used by:
   * reader_nb_unifier_of_fmtty to count readers in an fmtty,
   * Scanf.take_fmtty_format_readers to extract readers inside %(...%),
   * CamlinternalFormat.fmtty_of_ignored_format to extract format type. *)
let rec concat_fmtty : type a b c d e f g h .
    (a, b, c, d, e, f) fmtty ->
    (f, b, c, e, g, h) fmtty ->
    (a, b, c, d, g, h) fmtty =
fun fmtty1 fmtty2 -> match fmtty1 with
  | Char_ty rest ->
    Char_ty (concat_fmtty rest fmtty2)
  | String_ty rest ->
    String_ty (concat_fmtty rest fmtty2)
  | Int_ty rest ->
    Int_ty (concat_fmtty rest fmtty2)
  | Int32_ty rest ->
    Int32_ty (concat_fmtty rest fmtty2)
  | Nativeint_ty rest ->
    Nativeint_ty (concat_fmtty rest fmtty2)
  | Int64_ty rest ->
    Int64_ty (concat_fmtty rest fmtty2)
  | Float_ty rest ->
    Float_ty (concat_fmtty rest fmtty2)
  | Bool_ty rest ->
    Bool_ty (concat_fmtty rest fmtty2)
  | Alpha_ty rest ->
    Alpha_ty (concat_fmtty rest fmtty2)
  | Theta_ty rest ->
    Theta_ty (concat_fmtty rest fmtty2)
  | Reader_ty rest ->
    Reader_ty (concat_fmtty rest fmtty2)
  | Ignored_reader_ty rest ->
    Ignored_reader_ty (concat_fmtty rest fmtty2)
  | Format_arg_ty (ty, rest) ->
    Format_arg_ty (ty, concat_fmtty rest fmtty2)
  | Format_subst_ty (rnu, ty, rest) ->
    Format_subst_ty (rnu, ty, concat_fmtty rest fmtty2)
  | End_of_fmtty -> fmtty2

(******************************************************************************)
                           (* Format concatenation *)

(* Concatenate two formats. *)
let rec concat_fmt : type a b c d e f g h .
    (a, b, c, d, e, f) fmt ->
    (f, b, c, e, g, h) fmt ->
    (a, b, c, d, g, h) fmt =
fun fmt1 fmt2 -> match fmt1 with
  | String (pad, rest) ->
    String (pad, concat_fmt rest fmt2)
  | Caml_string (pad, rest) ->
    Caml_string (pad, concat_fmt rest fmt2)

  | Int (iconv, pad, prec, rest) ->
    Int (iconv, pad, prec, concat_fmt rest fmt2)
  | Int32 (iconv, pad, prec, rest) ->
    Int32 (iconv, pad, prec, concat_fmt rest fmt2)
  | Nativeint (iconv, pad, prec, rest) ->
    Nativeint (iconv, pad, prec, concat_fmt rest fmt2)
  | Int64 (iconv, pad, prec, rest) ->
    Int64 (iconv, pad, prec, concat_fmt rest fmt2)
  | Float (fconv, pad, prec, rest) ->
    Float (fconv, pad, prec, concat_fmt rest fmt2)

  | Char (rest) ->
    Char (concat_fmt rest fmt2)
  | Caml_char rest ->
    Caml_char (concat_fmt rest fmt2)
  | Bool rest ->
    Bool (concat_fmt rest fmt2)
  | Alpha rest ->
    Alpha (concat_fmt rest fmt2)
  | Theta rest ->
    Theta (concat_fmt rest fmt2)
  | Reader rest ->
    Reader (concat_fmt rest fmt2)
  | Flush rest ->
    Flush (concat_fmt rest fmt2)

  | String_literal (str, rest) ->
    String_literal (str, concat_fmt rest fmt2)
  | Char_literal (chr, rest) ->
    Char_literal   (chr, concat_fmt rest fmt2)

  | Format_arg (pad, fmtty, rest) ->
    Format_arg   (pad, fmtty, concat_fmt rest fmt2)
  | Format_subst (pad, rnu, fmtty, rest) ->
    Format_subst (pad, rnu, fmtty, concat_fmt rest fmt2)

  | Scan_char_set (width_opt, char_set, rest) ->
    Scan_char_set (width_opt, char_set, concat_fmt rest fmt2)
  | Scan_get_counter (counter, rest) ->
    Scan_get_counter (counter, concat_fmt rest fmt2)
  | Ignored_param (ign, rest) ->
    Ignored_param (ign, concat_fmt rest fmt2)

  | Formatting (fmting, rest) ->
    Formatting (fmting, concat_fmt rest fmt2)

  | End_of_format ->
    fmt2
end

type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

let string_of_format (fmt, str) = str

external format_of_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
 ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

let (^^) (fmt1, str1) (fmt2, str2) =
  (CamlinternalFormatBasics.concat_fmt fmt1 fmt2, str1 ^ "%," ^ str2)

(* Miscellaneous *)

external sys_exit : int -> 'a = "caml_sys_exit"

let exit_function = ref flush_all

let at_exit f =
  let g = !exit_function in
  exit_function := (fun () -> f(); g())

let do_at_exit () = (!exit_function) ()

let exit retcode =
  do_at_exit ();
  sys_exit retcode

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit
