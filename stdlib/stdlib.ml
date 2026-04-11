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

(* Exceptions *)

external register_named_value : string -> 'a -> unit
                              = "caml_register_named_value"

let () =
  (* for runtime/fail_nat.c *)
  register_named_value "Pervasives.array_bound_error"
    (Invalid_argument "index out of bounds")

external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit
exception Match_failure = Match_failure
exception Assert_failure = Assert_failure
exception Invalid_argument = Invalid_argument
exception Failure = Failure
exception Not_found = Not_found
exception Out_of_memory = Out_of_memory
exception Stack_overflow = Stack_overflow
exception Sys_error = Sys_error
exception End_of_file = End_of_file
exception Division_by_zero = Division_by_zero
exception Sys_blocked_io = Sys_blocked_io
exception Undefined_recursive_module = Undefined_recursive_module

(* Composition operators *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

(* Debugging *)

external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"
external __FUNCTION__ : string = "%loc_FUNCTION"

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
external ( && ) : bool -> bool -> bool = "%sequand"
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
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc]
external acos : float -> float = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc]
external asin : float -> float = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc]
external atan : float -> float = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc]
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]
external hypot : float -> float -> float
               = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc]
external acosh : float -> float = "caml_acosh_float" "caml_acosh"
  [@@unboxed] [@@noalloc]
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc]
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc]
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc]
external asinh : float -> float = "caml_asinh_float" "caml_asinh"
  [@@unboxed] [@@noalloc]
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc]
external atanh : float -> float = "caml_atanh_float" "caml_atanh"
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
external float_of_bits : int64 -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]
let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
let nan =
  float_of_bits 0x7F_F8_00_00_00_00_00_01L
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
external classify_float : (float [@unboxed]) -> fpclass =
  "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]

(* String and byte sequence operations -- more in modules String and Bytes *)

external string_length : string -> int = "%string_length"
external bytes_length : bytes -> int = "%bytes_length"
external bytes_create : int -> bytes = "caml_create_bytes"
external string_blit : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]
external bytes_blit : bytes -> int -> bytes -> int -> int -> unit
                        = "caml_blit_bytes" [@@noalloc]
external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"
external bytes_unsafe_of_string : string -> bytes = "%bytes_to_string"

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

(* Result type *)

type ('a,'b) result = Ok of 'a | Error of 'b

(* String conversion functions *)

external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

let bool_of_string_opt = function
  | "true" -> Some true
  | "false" -> Some false
  | _ -> None

let string_of_int n =
  format_int "%d" n

external int_of_string : string -> int = "caml_int_of_string"

let int_of_string_opt s =
  (* Trashes current backtrace *)
  try Some (int_of_string s)
  with Failure _ -> None

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

let string_of_float f = valid_float_lexem (format_float "%.12g" f)

external float_of_string : string -> float = "caml_float_of_string"

let float_of_string_opt s =
  (* Trashes current backtrace *)
  try Some (float_of_string s)
  with Failure _ -> None

(* List operations -- more in module List *)

let[@tail_mod_cons] rec ( @ ) l1 l2 =
  match l1 with
  | [] -> l2
  | h1 :: [] -> h1 :: l2
  | h1 :: h2 :: [] -> h1 :: h2 :: l2
  | h1 :: h2 :: h3 :: tl -> h1 :: h2 :: h3 :: (tl @ l2)

(* I/O operations *)


type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"


(* native channels. These correspond to the pre-existing
   IO channels implemented in C.
   Now we wrap them in a sum type to allow for user-defined channels. *)

type native_in_channel
type native_out_channel

(* definitions and primitives used to define IO channels *)
open struct

  external int64_add : int64 -> int64 -> int64 = "%int64_add"
  external int64_sub : int64 -> int64 -> int64 = "%int64_sub"
  external int64_of_int : int -> int64 = "%int64_of_int"
  external int64_to_int : int64 -> int = "%int64_to_int"

  external bytes_unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
  external bytes_unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"

  (* Marshal module is not available yet; we use the C primitives directly. *)

  external marshal_to_bytes : 'a -> unit list -> bytes
    = "caml_output_value_to_bytes"
  external marshal_from_bytes_unsafe : bytes -> int -> 'a
    = "caml_input_value_from_bytes"
  external marshal_data_size_unsafe : bytes -> int -> int
    = "caml_marshal_data_size"
  let marshal_header_size = 16

  external native_open_descriptor_in  : int -> native_in_channel
    = "caml_ml_open_descriptor_in"
  external native_open_descriptor_out : int -> native_out_channel
    = "caml_ml_open_descriptor_out"
  external native_flush               : native_out_channel -> unit
    = "caml_ml_flush"
  external native_unsafe_output       : native_out_channel -> bytes -> int -> int -> unit
    = "caml_ml_output_bytes"
  external native_unsafe_output_bigarray : native_out_channel -> 'a -> int -> int -> unit
    = "caml_ml_output_bigarray"
  external native_output_char         : native_out_channel -> char -> unit
    = "caml_ml_output_char"
  external native_marshal_to_channel  : native_out_channel -> 'a -> unit list -> unit
    = "caml_output_value"
  external native_seek_out            : native_out_channel -> int -> unit
    = "caml_ml_seek_out"
  external native_pos_out             : native_out_channel -> int
    = "caml_ml_pos_out"
  external native_out_channel_length  : native_out_channel -> int
    = "caml_ml_channel_size"
  external native_close_channel       : native_out_channel -> unit
    = "caml_ml_close_channel"
  external native_set_binary_mode_out : native_out_channel -> bool -> unit
    = "caml_ml_set_binary_mode"
  external native_is_binary_mode_out  : native_out_channel -> bool
    = "caml_ml_is_binary_mode"
  external native_set_buffered_out    : native_out_channel -> bool -> unit
    = "caml_ml_set_buffered"
  external native_is_buffered_out     : native_out_channel -> bool
    = "caml_ml_is_buffered"
  external native_set_out_name        : native_out_channel -> string -> unit
    = "caml_ml_set_channel_name"
  external native_input_char          : native_in_channel -> char
    = "caml_ml_input_char"
  external native_unsafe_input        : native_in_channel -> bytes -> int -> int -> int
    = "caml_ml_input"
  external native_unsafe_input_bigarray : native_in_channel -> 'a -> int -> int -> int
    = "caml_ml_input_bigarray"
  external native_input_scan_line     : native_in_channel -> int
    = "caml_ml_input_scan_line"
  external native_input_value         : native_in_channel -> 'a
    = "caml_input_value"
  external native_seek_in             : native_in_channel -> int -> unit
    = "caml_ml_seek_in"
  external native_pos_in              : native_in_channel -> int
    = "caml_ml_pos_in"
  external native_in_channel_length   : native_in_channel -> int
    = "caml_ml_channel_size"
  external native_close_in            : native_in_channel -> unit
    = "caml_ml_close_channel"
  external native_set_binary_mode_in  : native_in_channel -> bool -> unit
    = "caml_ml_set_binary_mode"
  external native_is_binary_mode_in   : native_in_channel -> bool
    = "caml_ml_is_binary_mode"
  external native_set_in_name         : native_in_channel -> string -> unit
    = "caml_ml_set_channel_name"
  external native_isatty_out          : native_out_channel -> bool
    = "caml_sys_isatty"
  external native_isatty_in           : native_in_channel -> bool
    = "caml_sys_isatty"
  external native_terminfo_rows       : native_out_channel -> int
    = "caml_terminfo_rows"
  external native_channel_descriptor  : 'a -> int
    = "caml_channel_descriptor"
  external native_seek_out_64         : native_out_channel -> int64 -> unit
    = "caml_ml_seek_out_64"
  external native_pos_out_64          : native_out_channel -> int64
    = "caml_ml_pos_out_64"
  external native_out_channel_length_64 : native_out_channel -> int64
    = "caml_ml_channel_size_64"
  external native_seek_in_64          : native_in_channel -> int64 -> unit
    = "caml_ml_seek_in_64"
  external native_pos_in_64           : native_in_channel -> int64
    = "caml_ml_pos_in_64"
  external native_in_channel_length_64 : native_in_channel -> int64
    = "caml_ml_channel_size_64"
end

let io_buffer_size = 65536

type chan_buffer = {
  buf: bytes;
  mutable off: int;
  mutable len: int;
}

let make_chan_buffer () = { buf = bytes_create io_buffer_size; off = 0; len = 0 }

type 'st out_ops = {
  out_write: 'st -> bytes -> int -> int -> int;
  out_flush: 'st -> unit;
  out_close: 'st -> unit;
  out_seek: ('st -> int64 -> unit) option;
  out_pos: ('st -> int64) option;
  out_length: ('st -> int64) option;
  out_set_binary: ('st -> bool -> unit) option;
  out_isatty: ('st -> bool) option;
  out_is_binary: ('st -> bool) option;
  out_get_fd: ('st -> int) option;
}

type 'st in_ops = {
  in_read: 'st -> chan_buffer -> unit;
  in_close: 'st -> unit;
  in_seek: ('st -> int64 -> unit) option;
  in_pos: ('st -> int64) option;
  in_length: ('st -> int64) option;
  in_set_binary: ('st -> bool -> unit) option;
  in_isatty: ('st -> bool) option;
  in_is_binary: ('st -> bool) option;
  in_get_fd: ('st -> int) option;
}

type out_channel =
  | OC_native of native_out_channel
  | OC_user_defined : {
      st: 'st;
      ops: 'st out_ops;
      buf: chan_buffer;
      mutable closed: bool;
    } -> out_channel

type in_channel =
  | IC_native of native_in_channel
  | IC_user_defined : {
      st: 'st;
      ops: 'st in_ops;
      buf: chan_buffer;
      mutable closed: bool;
    } -> in_channel

let stdin  = IC_native (native_open_descriptor_in  0)
let stdout = OC_native (native_open_descriptor_out 1)
let stderr = OC_native (native_open_descriptor_out 2)

module CamlinternalChannel = struct
  let open_descriptor_in  fd = IC_native (native_open_descriptor_in  fd)
  let open_descriptor_out fd = OC_native (native_open_descriptor_out fd)

  let in_channel_fd ic =
    match ic with
    | IC_native nc -> native_channel_descriptor nc
    | IC_user_defined r ->
      match r.ops.in_get_fd with
      | Some f -> f r.st
      | None -> invalid_arg "in_channel_fd: not a file-descriptor channel"

  let out_channel_fd oc =
    match oc with
    | OC_native nc -> native_channel_descriptor nc
    | OC_user_defined r ->
      match r.ops.out_get_fd with
      | Some f -> f r.st
      | None -> invalid_arg "out_channel_fd: not a file-descriptor channel"

  let out_channel_terminfo_rows (oc : out_channel) =
    match oc with
    | OC_native nc -> native_terminfo_rows nc
    | OC_user_defined _ -> -1

  type nonrec native_in_channel = native_in_channel
  type nonrec native_out_channel = native_out_channel

  let unsafe_output_bigarray_native nc buf ofs len =
    native_unsafe_output_bigarray nc buf ofs len

  let unsafe_input_bigarray_native nc buf ofs len =
    native_unsafe_input_bigarray nc buf ofs len

  let native_in_channel_of (ic : in_channel) =
    match ic with
    | IC_native nc -> Some nc
    | IC_user_defined _ -> None

  let native_out_channel_of (oc : out_channel) =
    match oc with
    | OC_native nc -> Some nc
    | OC_user_defined _ -> None

  let wrap_native_in_channel nc = IC_native nc
  let wrap_native_out_channel nc = OC_native nc
end

let make_in_channel st ops =
  IC_user_defined { st; ops; buf = make_chan_buffer (); closed = false }

let make_out_channel st ops =
  OC_user_defined { st; ops; buf = make_chan_buffer (); closed = false }

let flush_buf_ud st ops (buf : chan_buffer) =
  while buf.len > 0 do
    let n = ops.out_write st buf.buf buf.off buf.len in
    buf.off <- buf.off + n;
    buf.len <- buf.len - n
  done;
  buf.off <- 0

let flush (oc : out_channel) =
  match oc with
  | OC_native nc -> native_flush nc
  | OC_user_defined r ->
    if not r.closed then begin
      flush_buf_ud r.st r.ops r.buf;
      r.ops.out_flush r.st
    end

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

let output_char (oc : out_channel) (c : char) =
  match oc with
  | OC_native nc -> native_output_char nc c
  | OC_user_defined r ->
    if r.closed then raise (Sys_error "output_char: channel is closed");
    let cap = bytes_length r.buf.buf in
    if r.buf.off + r.buf.len >= cap then flush_buf_ud r.st r.ops r.buf;
    bytes_unsafe_set r.buf.buf (r.buf.off + r.buf.len) c;
    r.buf.len <- r.buf.len + 1;
    if r.buf.off + r.buf.len >= cap
    then flush_buf_ud r.st r.ops r.buf

let output_byte oc n = output_char oc (unsafe_char_of_int (n land 0xFF))

let output (oc : out_channel) (s : bytes) (ofs : int) (len : int) =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "output";
  match oc with
  | OC_native nc -> native_unsafe_output nc s ofs len
  | OC_user_defined r ->
    if r.closed then raise (Sys_error "output: channel is closed");
    let cap = bytes_length r.buf.buf in
    let i = ref ofs in
    let remaining = ref len in
    while !remaining > 0 do
      if r.buf.off + r.buf.len >= cap then flush_buf_ud r.st r.ops r.buf;
      let n = min !remaining (cap - r.buf.off - r.buf.len) in
      bytes_blit s !i r.buf.buf (r.buf.off + r.buf.len) n;
      r.buf.len <- r.buf.len + n;
      i := !i + n;
      remaining := !remaining - n
    done;
    if r.buf.off + r.buf.len >= cap
    then flush_buf_ud r.st r.ops r.buf

let output_substring (oc : out_channel) (s : string) (ofs : int) (len : int) =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "output_substring"
  else output oc (bytes_unsafe_of_string s) ofs len

let output_bytes oc s = output oc s 0 (bytes_length s)
let output_string oc s = output_substring oc s 0 (string_length s)

let output_binary_int oc (n : int) =
  output_byte oc (n asr 24);
  output_byte oc (n asr 16);
  output_byte oc (n asr 8);
  output_byte oc n

let output_value oc v =
  match oc with
  | OC_native nc -> native_marshal_to_channel nc v []
  | OC_user_defined _ ->
    let s = marshal_to_bytes v [] in
    output_bytes oc s

let seek_out (oc : out_channel) (pos : int) =
  match oc with
  | OC_native nc -> native_seek_out nc pos
  | OC_user_defined r ->
    if r.closed then raise (Sys_error "seek_out: channel is closed");
    flush_buf_ud r.st r.ops r.buf;
    match r.ops.out_seek with
    | None -> invalid_arg "seek_out: channel does not support seeking"
    | Some f -> f r.st (int64_of_int pos)

let pos_out (oc : out_channel) =
  match oc with
  | OC_native nc -> native_pos_out nc
  | OC_user_defined r ->
    if r.closed then raise (Sys_error "pos_out: channel is closed");
    match r.ops.out_pos with
    | None -> invalid_arg "pos_out: channel does not support position"
    | Some f -> int64_to_int (int64_add (f r.st) (int64_of_int r.buf.len))

let out_channel_length (oc : out_channel) =
  match oc with
  | OC_native nc -> native_out_channel_length nc
  | OC_user_defined r ->
    if r.closed then raise (Sys_error "out_channel_length: channel is closed");
    match r.ops.out_length with
    | None -> invalid_arg "out_channel_length: channel does not support length"
    | Some f -> int64_to_int (f r.st)

let close_out_channel (oc : out_channel) =
  match oc with
  | OC_native nc -> native_close_channel nc
  | OC_user_defined r ->
    if not r.closed then begin
      r.closed <- true;
      (try flush_buf_ud r.st r.ops r.buf with _ -> ());
      r.ops.out_close r.st
    end

let close_out oc = flush oc; close_out_channel oc

let close_out_noerr oc =
  (try flush oc with _ -> ());
  (try close_out_channel oc with _ -> ())

let set_binary_mode_out (oc : out_channel) (bin : bool) =
  match oc with
  | OC_native nc -> native_set_binary_mode_out nc bin
  | OC_user_defined r ->
    if r.closed then raise (Sys_error "set_binary_mode_out: channel is closed");
    flush_buf_ud r.st r.ops r.buf;
    (match r.ops.out_set_binary with None -> () | Some f -> f r.st bin)

let out_channel_isatty (oc : out_channel) =
  match oc with
  | OC_native nc -> native_isatty_out nc
  | OC_user_defined r ->
    (match r.ops.out_isatty with None -> false | Some f -> f r.st)

let out_channel_is_binary_mode (oc : out_channel) =
  match oc with
  | OC_native nc -> native_is_binary_mode_out nc
  | OC_user_defined r ->
    (match r.ops.out_is_binary with None -> false | Some f -> f r.st)

let set_buffered_out (oc : out_channel) (b : bool) =
  match oc with
  | OC_native nc -> native_set_buffered_out nc b
  | OC_user_defined r ->
    if r.closed then raise (Sys_error "set_buffered: channel is closed");
    if not b then
      invalid_arg "set_buffered: user-defined channels are always buffered"

let is_buffered_out (oc : out_channel) : bool =
  match oc with
  | OC_native nc -> native_is_buffered_out nc
  | OC_user_defined _ -> true

let open_out_gen mode perm name =
  let nc = native_open_descriptor_out (open_desc name mode perm) in
  native_set_out_name nc name;
  OC_native nc

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

(* Input functions *)

let input_char (ic : in_channel) =
  match ic with
  | IC_native nc -> native_input_char nc
  | IC_user_defined r ->
    if r.closed then raise (Sys_error "input_char: channel is closed");
    if r.buf.len = 0 then begin
      r.ops.in_read r.st r.buf;
      if r.buf.len = 0 then raise End_of_file
    end;
    let c = bytes_unsafe_get r.buf.buf r.buf.off in
    r.buf.off <- r.buf.off + 1;
    r.buf.len <- r.buf.len - 1;
    c

let input_byte ic = int_of_char (input_char ic)

(* Internal: read into user's bytes buffer without bounds checking *)
let unsafe_input (ic : in_channel) (s : bytes) (ofs : int) (len : int) =
  match ic with
  | IC_native nc -> native_unsafe_input nc s ofs len
  | IC_user_defined r ->
    if r.closed then raise (Sys_error "input: channel is closed");
    if r.buf.len = 0 then begin
      r.ops.in_read r.st r.buf;
      if r.buf.len = 0 then 0
      else begin
        let n = min r.buf.len len in
        bytes_blit r.buf.buf r.buf.off s ofs n;
        r.buf.off <- r.buf.off + n;
        r.buf.len <- r.buf.len - n;
        n
      end
    end else begin
      let n = min r.buf.len len in
      bytes_blit r.buf.buf r.buf.off s ofs n;
      r.buf.off <- r.buf.off + n;
      r.buf.len <- r.buf.len - n;
      n
    end

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

(* Scan [buf] for '\n'. Returns index relative to [buf.off], or -1 if not found. *)
let scan_newline (buf : chan_buffer) : int =
  let b = buf.buf in
  let off = buf.off in
  let limit = off + buf.len in
  let rec loop i =
    if i >= limit then -1
    else if bytes_unsafe_get b i = '\n' then i - off
    else loop (i + 1)
  in
  loop off

(* Consume [n] bytes from buffer, return them as a fresh bytes *)
let consume_buf (buf : chan_buffer) (n : int) : bytes =
  let b = bytes_create n in
  bytes_blit buf.buf buf.off b 0 n;
  buf.off <- buf.off + n;
  buf.len <- buf.len - n;
  b

(* Concatenate a reversed list of chunks into a single string *)
let concat_chunks_rev (chunks : bytes list) (total : int) : string =
  let result = bytes_create total in
  let pos = ref total in
  let rec copy = function
    | [] -> ()
    | c :: rest ->
      let n = bytes_length c in
      pos := !pos - n;
      bytes_blit c 0 result !pos n;
      copy rest
  in
  copy chunks;
  bytes_unsafe_to_string result

(* input_line for native channels: use C's efficient input_scan_line *)
let native_input_line nc =
  let rec build_result buf pos = function
    | [] -> buf
    | hd :: tl ->
      let len = bytes_length hd in
      bytes_blit hd 0 buf (pos - len) len;
      build_result buf (pos - len) tl
  in
  let rec scan accu len =
    let n = native_input_scan_line nc in
    if n = 0 then begin
      match accu with
      | [] -> raise End_of_file
      | _  -> build_result (bytes_create len) len accu
    end else if n > 0 then begin
      let res = bytes_create (n - 1) in
      ignore (native_unsafe_input nc res 0 (n - 1));
      ignore (native_input_char nc);
      match accu with
      | [] -> res
      | _  -> let len = len + n - 1 in
              build_result (bytes_create len) len (res :: accu)
    end else begin
      let beg = bytes_create (-n) in
      ignore (native_unsafe_input nc beg 0 (-n));
      scan (beg :: accu) (len - n)
    end
  in
  bytes_unsafe_to_string (scan [] 0)

let user_input_line st ops buf =
  let rec collect chunks total_len =
    if buf.len = 0 then begin
      ops.in_read st buf;
      if buf.len = 0 then begin
        if total_len = 0 then raise End_of_file
        else concat_chunks_rev chunks total_len
      end else
        collect chunks total_len
    end else
      let nl = scan_newline buf in
      if nl >= 0 then begin
        let chunk = consume_buf buf nl in
        buf.off <- buf.off + 1;
        buf.len <- buf.len - 1;
        concat_chunks_rev (chunk :: chunks) (total_len + nl)
      end else begin
        let chunk = consume_buf buf buf.len in
        ops.in_read st buf;
        collect (chunk :: chunks) (total_len + bytes_length chunk)
      end
  in

  let nl = scan_newline buf in
  if nl >= 0 then begin
    let line = consume_buf buf nl in
    buf.off <- buf.off + 1; (* skip '\n' *)
    buf.len <- buf.len - 1;
    bytes_unsafe_to_string line
  end else begin
    collect [] 0
  end

let input_line (ic : in_channel) =
  match ic with
  | IC_native nc -> native_input_line nc
  | IC_user_defined r ->
    if r.closed then raise (Sys_error "input_line: channel is closed");
    user_input_line r.st r.ops r.buf

let input_binary_int ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  (* Big-endian 4-byte signed integer.
     We compute this portably (bytecode compat-32) by treating b0 as a
     signed byte: on 64-bit platforms this naturally sign-extends the
     result, matching the old C implementation. All constants are small. *)
  let s0 = if b0 land 0x80 <> 0 then b0 - 256 else b0 in
  (s0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

let input_value ic =
  match ic with
  | IC_native nc -> native_input_value nc
  | IC_user_defined _ ->
    let header = bytes_create marshal_header_size in
    really_input ic header 0 marshal_header_size;
    let data_size = marshal_data_size_unsafe header 0 in
    let buf = bytes_create (marshal_header_size + data_size) in
    bytes_blit header 0 buf 0 marshal_header_size;
    really_input ic buf marshal_header_size data_size;
    marshal_from_bytes_unsafe buf 0

let seek_in (ic : in_channel) (pos : int) =
  match ic with
  | IC_native nc -> native_seek_in nc pos
  | IC_user_defined r ->
    if r.closed then raise (Sys_error "seek_in: channel is closed");
    r.buf.off <- 0;
    r.buf.len <- 0;
    match r.ops.in_seek with
    | None -> invalid_arg "seek_in: channel does not support seeking"
    | Some f -> f r.st (int64_of_int pos)

let pos_in (ic : in_channel) =
  match ic with
  | IC_native nc -> native_pos_in nc
  | IC_user_defined r ->
    if r.closed then raise (Sys_error "pos_in: channel is closed");
    match r.ops.in_pos with
    | None -> invalid_arg "pos_in: channel does not support position"
    | Some f -> int64_to_int (int64_sub (f r.st) (int64_of_int r.buf.len))

let in_channel_length (ic : in_channel) =
  match ic with
  | IC_native nc -> native_in_channel_length nc
  | IC_user_defined r ->
    if r.closed then raise (Sys_error "in_channel_length: channel is closed");
    match r.ops.in_length with
    | None -> invalid_arg "in_channel_length: channel does not support length"
    | Some f -> int64_to_int (f r.st)

let close_in (ic : in_channel) =
  match ic with
  | IC_native nc -> native_close_in nc
  | IC_user_defined r ->
    if not r.closed then begin
      r.closed <- true;
      r.buf.off <- 0;
      r.buf.len <- 0;
      r.ops.in_close r.st
    end

let close_in_noerr ic = (try close_in ic with _ -> ())

let set_binary_mode_in (ic : in_channel) (bin : bool) =
  match ic with
  | IC_native nc -> native_set_binary_mode_in nc bin
  | IC_user_defined r ->
    if r.closed then raise (Sys_error "set_binary_mode_in: channel is closed");
    (match r.ops.in_set_binary with None -> () | Some f -> f r.st bin)

let in_channel_isatty (ic : in_channel) =
  match ic with
  | IC_native nc -> native_isatty_in nc
  | IC_user_defined r ->
    (match r.ops.in_isatty with None -> false | Some f -> f r.st)

let in_channel_is_binary_mode (ic : in_channel) =
  match ic with
  | IC_native nc -> native_is_binary_mode_in nc
  | IC_user_defined r ->
    (match r.ops.in_is_binary with None -> false | Some f -> f r.st)

let open_in_gen mode perm name =
  let nc = native_open_descriptor_in (open_desc name mode perm) in
  native_set_in_name nc name;
  IC_native nc

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

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
let read_int_opt () = int_of_string_opt(read_line())
let read_float () = float_of_string(read_line())
let read_float_opt () = float_of_string_opt(read_line())

(* Operations on large files *)

module LargeFile = struct
  let seek_out (oc : out_channel) (pos : int64) =
    match oc with
    | OC_native nc -> native_seek_out_64 nc pos
    | OC_user_defined r ->
      if r.closed then raise (Sys_error "seek_out: channel is closed");
      flush_buf_ud r.st r.ops r.buf;
      match r.ops.out_seek with
      | None -> invalid_arg "seek_out: channel does not support seeking"
      | Some f -> f r.st pos

  let pos_out (oc : out_channel) =
    match oc with
    | OC_native nc -> native_pos_out_64 nc
    | OC_user_defined r ->
      if r.closed then raise (Sys_error "pos_out: channel is closed");
      match r.ops.out_pos with
      | None -> invalid_arg "pos_out: channel does not support position"
      | Some f -> int64_add (f r.st) (int64_of_int r.buf.len)

  let out_channel_length (oc : out_channel) =
    match oc with
    | OC_native nc -> native_out_channel_length_64 nc
    | OC_user_defined r ->
      if r.closed then
        raise (Sys_error "out_channel_length: channel is closed");
      match r.ops.out_length with
      | None ->
        invalid_arg "out_channel_length: channel does not support length"
      | Some f -> f r.st

  let seek_in (ic : in_channel) (pos : int64) =
    match ic with
    | IC_native nc -> native_seek_in_64 nc pos
    | IC_user_defined r ->
      if r.closed then raise (Sys_error "seek_in: channel is closed");
      r.buf.off <- 0;
      r.buf.len <- 0;
      match r.ops.in_seek with
      | None -> invalid_arg "seek_in: channel does not support seeking"
      | Some f -> f r.st pos

  let pos_in (ic : in_channel) =
    match ic with
    | IC_native nc -> native_pos_in_64 nc
    | IC_user_defined r ->
      if r.closed then raise (Sys_error "pos_in: channel is closed");
      match r.ops.in_pos with
      | None -> invalid_arg "pos_in: channel does not support position"
      | Some f -> int64_sub (f r.st) (int64_of_int r.buf.len)

  let in_channel_length (ic : in_channel) =
    match ic with
    | IC_native nc -> native_in_channel_length_64 nc
    | IC_user_defined r ->
      if r.closed then
        raise (Sys_error "in_channel_length: channel is closed");
      match r.ops.in_length with
      | None ->
        invalid_arg "in_channel_length: channel does not support length"
      | Some f -> f r.st
end

(* Formats *)

type ('a, 'b, 'c, 'd, 'e, 'f) format6
   = ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6
   = Format of ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt
               * string

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

let string_of_format (Format (_fmt, str)) = str

external format_of_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
 ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

let ( ^^ ) (Format (fmt1, str1)) (Format (fmt2, str2)) =
  Format (CamlinternalFormatBasics.concat_fmt fmt1 fmt2,
          str1 ^ "%," ^ str2)

(* Miscellaneous *)

external sys_exit : int -> 'a = "caml_sys_exit"

(* for at_exit *)
type 'a atomic_t
external atomic_make : 'a -> 'a atomic_t = "%makemutable"
external atomic_get : 'a atomic_t -> 'a = "%atomic_load"
external atomic_compare_and_set : 'a atomic_t -> 'a -> 'a -> bool
  = "%atomic_cas"

let exit_function = atomic_make flush_all

let rec at_exit f =
  (* MPR#7253, MPR#7796: make sure "f" is executed only once *)
  let f_yet_to_run = atomic_make true in
  let old_exit = atomic_get exit_function in
  let new_exit () =
    if atomic_compare_and_set f_yet_to_run true false then f () ;
    old_exit ()
  in
  let success = atomic_compare_and_set exit_function old_exit new_exit in
  if not success then at_exit f

let do_domain_local_at_exit = ref (fun () -> ())

let do_at_exit () =
  (!do_domain_local_at_exit) ();
  (atomic_get exit_function) ()

let exit retcode =
  do_at_exit ();
  sys_exit retcode

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit

(*MODULE_ALIASES*)
module Arg            = Arg
module Array          = Array
module ArrayLabels    = ArrayLabels
module Atomic         = Atomic
module Bigarray       = Bigarray
module Bool           = Bool
module Buffer         = Buffer
module Bytes          = Bytes
module BytesLabels    = BytesLabels
module Callback       = Callback
module Char           = Char
module Complex        = Complex
module Condition      = Condition
module Digest         = Digest
module Domain         = Domain
module Dynarray       = Dynarray
module Pqueue         = Pqueue
module Effect         = Effect
module Either         = Either
module Ephemeron      = Ephemeron
module Filename       = Filename
module Float          = Float
module Format         = Format
module Fun            = Fun
module Gc             = Gc
module Hashtbl        = Hashtbl
module Iarray         = Iarray
module In_channel     = In_channel
module Int            = Int
module Int32          = Int32
module Int64          = Int64
module Lazy           = Lazy
module Lexing         = Lexing
module List           = List
module ListLabels     = ListLabels
module Map            = Map
module Marshal        = Marshal
module MoreLabels     = MoreLabels
module Mutex          = Mutex
module Nativeint      = Nativeint
module Obj            = Obj
module Oo             = Oo
module Option         = Option
module Out_channel    = Out_channel
module Pair           = Pair
module Parsing        = Parsing
module Printexc       = Printexc
module Printf         = Printf
module Queue          = Queue
module Random         = Random
module Result         = Result
module Repr           = Repr
module Scanf          = Scanf
module Semaphore      = Semaphore
module Seq            = Seq
module Set            = Set
module Stack          = Stack
module StdLabels      = StdLabels
module String         = String
module StringLabels   = StringLabels
module Sys            = Sys
module Type           = Type
module Uchar          = Uchar
module Unit           = Unit
module Weak           = Weak
