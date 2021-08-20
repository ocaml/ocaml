(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** @deprecated Use {!Stdlib} *)

external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"
let invalid_arg = invalid_arg
let failwith = failwith
exception Exit
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"
let min = min
let max = max
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"
external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( & ) : bool -> bool -> bool = "%sequand"
  [@@ocaml.deprecated "Use (&&) instead."]
external ( || ) : bool -> bool -> bool = "%sequor"
external ( or ) : bool -> bool -> bool = "%sequor"
  [@@ocaml.deprecated "Use (||) instead."]
external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"
external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
external ( ~- ) : int -> int = "%negint"
external ( ~+ ) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( mod ) : int -> int -> int = "%modint"
let abs = abs
let max_int = max_int
let min_int = min_int
external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"
let lnot = lnot
external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"
external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc]
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc]
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external acos : float -> float = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc]
external asin : float -> float = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc]
external atan : float -> float = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc]
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]
external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
  [@@unboxed] [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc]
external ceil : float -> float = "caml_ceil_float" "ceil"
  [@@unboxed] [@@noalloc]
external floor : float -> float = "caml_floor_float" "floor"
  [@@unboxed] [@@noalloc]
external abs_float : float -> float = "%absfloat"
external copysign : float -> float -> float
                  = "caml_copysign_float" "caml_copysign"
                  [@@unboxed] [@@noalloc]
external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]
external frexp : float -> float * int = "caml_frexp_float"
external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) =
  "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
let infinity = infinity
let neg_infinity = neg_infinity
let nan = nan
let max_float = max_float
let min_float = min_float
let epsilon_float = epsilon_float
type nonrec fpclass = fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float : (float [@unboxed]) -> fpclass =
  "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]
let ( ^ ) = ( ^ )
external int_of_char : char -> int = "%identity"
let char_of_int = char_of_int
external ignore : 'a -> unit = "%ignore"
let string_of_bool = string_of_bool
let bool_of_string = bool_of_string
let bool_of_string_opt = bool_of_string_opt
let string_of_int = string_of_int
external int_of_string : string -> int = "caml_int_of_string"
let int_of_string_opt = int_of_string_opt
let string_of_float = string_of_float
external float_of_string : string -> float = "caml_float_of_string"
let float_of_string_opt = float_of_string_opt
external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"
let ( @ )  = ( @ )
type nonrec in_channel = in_channel
type nonrec out_channel = out_channel
let stdin = stdin
let stdout = stdout
let stderr = stderr
let print_char = print_char
let print_string = print_string
let print_bytes = print_bytes
let print_int = print_int
let print_float = print_float
let print_endline = print_endline
let print_newline = print_newline
let prerr_char = prerr_char
let prerr_string = prerr_string
let prerr_bytes = prerr_bytes
let prerr_int = prerr_int
let prerr_float = prerr_float
let prerr_endline = prerr_endline
let prerr_newline = prerr_newline
let read_line = read_line
let read_int = read_int
let read_int_opt = read_int_opt
let read_float = read_float
let read_float_opt = read_float_opt
type nonrec open_flag = open_flag =
    Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock
let open_out = open_out
let open_out_bin = open_out_bin
let open_out_gen = open_out_gen
let flush = flush
let flush_all = flush_all
let output_char = output_char
let output_string = output_string
let output_bytes = output_bytes
let output = output
let output_substring = output_substring
let output_byte = output_byte
let output_binary_int = output_binary_int
let output_value = output_value
let seek_out = seek_out
let pos_out = pos_out
let out_channel_length = out_channel_length
let close_out = close_out
let close_out_noerr = close_out_noerr
let set_binary_mode_out = set_binary_mode_out
let open_in = open_in
let open_in_bin = open_in_bin
let open_in_gen = open_in_gen
let input_char = input_char
let input_line = input_line
let input = input
let really_input = really_input
let really_input_string = really_input_string
let input_byte = input_byte
let input_binary_int = input_binary_int
let input_value = input_value
let seek_in = seek_in
let pos_in = pos_in
let in_channel_length = in_channel_length
let close_in = close_in
let close_in_noerr = close_in_noerr
let set_binary_mode_in = set_binary_mode_in
module LargeFile = LargeFile
type nonrec 'a ref = 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"
type nonrec ('a,'b) result = ('a,'b) result = Ok of 'a | Error of 'b
type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
let string_of_format = string_of_format
external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"
let ( ^^ ) = ( ^^ )
let exit = exit
let at_exit = at_exit
let valid_float_lexem = valid_float_lexem
let do_at_exit = do_at_exit
