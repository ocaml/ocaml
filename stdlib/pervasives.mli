(* The initially opened module *)

(* Predefined in the compiler *)

(***
type int
type char
type string
type float
type bool
type unit = ()
type exn
type 'a array
type 'a list = [] | :: of 'a * 'a list
type ('a, 'b, 'c) format
exception Out_of_memory
exception Invalid_argument of string
exception Failure of string
exception Not_found
exception Sys_error of string
exception End_of_file
exception Division_by_zero
***)

(* Exceptions *)

external raise : exn -> 'a = "%raise"
val failwith: string -> 'a
val invalid_arg: string -> 'a

exception Exit

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "compare"
val min: 'a -> 'a -> 'a
val max: 'a -> 'a -> 'a
external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external (~-) : int -> int = "%negint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"
val abs : int -> int
external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"
val lnot: int -> int
external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "power_float"
external exp : float -> float = "exp_float"
external log : float -> float = "log_float"
external sqrt : float -> float = "sqrt_float"
external sin : float -> float = "sin_float"
external cos : float -> float = "cos_float"
external tan : float -> float = "tan_float"
external asin : float -> float = "asin_float"
external acos : float -> float = "acos_float"
external atan : float -> float = "atan_float"
external atan2 : float -> float -> float = "atan2_float"
val abs_float : float -> float
external float : int -> float = "float_of_int"
external truncate : float -> int = "int_of_float"

(* String operations -- more in module String *)

val (^) : string -> string -> string

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

val string_of_bool : bool -> string
val string_of_int : int -> string
external int_of_string : string -> int = "int_of_string"
val string_of_float : float -> string
external float_of_string : string -> float = "float_of_string"

(* List operations -- more in module List *)

val (@) : 'a list -> 'a list -> 'a list

(* I/O operations *)

type in_channel
type out_channel

val stdin : in_channel
val stdout : out_channel
val stderr : out_channel

(* Output functions on standard output *)

val print_char : char -> unit
val print_string : string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_endline : string -> unit
val print_newline : unit -> unit

(* Output functions on standard error *)

val prerr_char : char -> unit
val prerr_string : string -> unit
val prerr_int : int -> unit
val prerr_float : float -> unit
val prerr_endline : string -> unit
val prerr_newline : unit -> unit

(* Input functions on standard input *)

val read_line : unit -> string
val read_int : unit -> int
val read_float : unit -> float

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_rdwr
  | Open_append | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text

val open_out : string -> out_channel
val open_out_bin : string -> out_channel
val open_out_gen : open_flag list -> int -> string -> out_channel
external flush : out_channel -> unit = "flush"
external output_char : out_channel -> char -> unit = "output_char"
val output_string : out_channel -> string -> unit
val output : out_channel -> string -> int -> int -> unit
external output_byte : out_channel -> int -> unit = "output_char"
external output_binary_int : out_channel -> int -> unit = "output_int"
external output_value : out_channel -> 'a -> unit = "output_value"
external output_compact_value : out_channel -> 'a -> unit = "output_value"
external seek_out : out_channel -> int -> unit = "seek_out"
external pos_out : out_channel -> int = "pos_out"
external size_out : out_channel -> int = "channel_size"
external close_out : out_channel -> unit = "close_out"

(* General input functions *)
val open_in : string -> in_channel
val open_in_bin : string -> in_channel
val open_in_gen : open_flag list -> int -> string -> in_channel
external input_char : in_channel -> char = "input_char"
val input_line : in_channel -> string
val input : in_channel -> string -> int -> int -> int
val really_input : in_channel -> string -> int -> int -> unit
external input_byte : in_channel -> int = "input_char"
external input_binary_int : in_channel -> int = "input_int"
external input_value : in_channel -> 'a = "input_value"
external seek_in : in_channel -> int -> unit = "seek_in"
external pos_in : in_channel -> int = "pos_in"
external in_channel_length : in_channel -> int = "channel_size"
external close_in : in_channel -> unit = "close_in"

(* References *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makeblock"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"

(* Miscellaneous *)

val exit : int -> 'a

type 'a option = None | Some of 'a

(**** For system use, not for the casual user ****)

val unsafe_really_input: in_channel -> string -> int -> int -> unit
