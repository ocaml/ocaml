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

val raise : exn -> 'a = "%raise"
val failwith: string -> 'a
val invalid_arg: string -> 'a

exception Exit

(* Comparisons *)

val (=) : 'a -> 'a -> bool = "%equal"
val (<>) : 'a -> 'a -> bool = "%notequal"
val (<) : 'a -> 'a -> bool = "%lessthan"
val (>) : 'a -> 'a -> bool = "%greaterthan"
val (<=) : 'a -> 'a -> bool = "%lessequal"
val (>=) : 'a -> 'a -> bool = "%greaterequal"
val compare: 'a -> 'a -> int = "compare"
val min: 'a -> 'a -> 'a
val max: 'a -> 'a -> 'a
val (==) : 'a -> 'a -> bool = "%eq"
val (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

val not : bool -> bool = "%boolnot"
val (&) : bool -> bool -> bool = "%sequand"
val (or) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

val (~-) : int -> int = "%negint"
val succ : int -> int = "%succint"
val pred : int -> int = "%predint"
val (+) : int -> int -> int = "%addint"
val (-) : int -> int -> int = "%subint"
val ( * ) : int -> int -> int = "%mulint"
val (/) : int -> int -> int = "%divint"
val (mod) : int -> int -> int = "%modint"
val abs : int -> int
val (land) : int -> int -> int = "%andint"
val (lor) : int -> int -> int = "%orint"
val (lxor) : int -> int -> int = "%xorint"
val lnot: int -> int
val (lsl) : int -> int -> int = "%lslint"
val (lsr) : int -> int -> int = "%lsrint"
val (asr) : int -> int -> int = "%asrint"

(* Floating-point operations *)

val (~-.) : float -> float = "neg_float"
val (+.) : float -> float -> float = "add_float"
val (-.) : float -> float -> float = "sub_float"
val ( *. ) : float -> float -> float = "mul_float"
val (/.) : float -> float -> float = "div_float"
val ( ** ) : float -> float -> float = "power_float"
val exp : float -> float = "exp_float"
val log : float -> float = "log_float"
val sqrt : float -> float = "sqrt_float"
val sin : float -> float = "sin_float"
val cos : float -> float = "cos_float"
val tan : float -> float = "tan_float"
val asin : float -> float = "asin_float"
val acos : float -> float = "acos_float"
val atan : float -> float = "atan_float"
val atan2 : float -> float -> float = "atan2_float"
val abs_float : float -> float
val float : int -> float = "float_of_int"
val truncate : float -> int = "int_of_float"

(* String operations -- more in module String *)

val (^) : string -> string -> string

(* Pair operations *)

val fst : 'a * 'b -> 'a = "%field0"
val snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

val string_of_bool : bool -> string
val string_of_int : int -> string
val int_of_string : string -> int = "int_of_string"
val string_of_float : float -> string
val float_of_string : string -> float = "float_of_string"

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
val flush : out_channel -> unit = "flush"
val output_char : out_channel -> char -> unit = "output_char"
val output_string : out_channel -> string -> unit
val output : out_channel -> string -> int -> int -> unit
val output_byte : out_channel -> int -> unit = "output_char"
val output_binary_int : out_channel -> int -> unit = "output_int"
val output_value : out_channel -> 'a -> unit = "output_value"
val output_compact_value : out_channel -> 'a -> unit = "output_value"
val seek_out : out_channel -> int -> unit = "seek_out"
val pos_out : out_channel -> int = "pos_out"
val size_out : out_channel -> int = "channel_size"
val close_out : out_channel -> unit = "close_out"

(* General input functions *)
val open_in : string -> in_channel
val open_in_bin : string -> in_channel
val open_in_gen : open_flag list -> int -> string -> in_channel
val input_char : in_channel -> char = "input_char"
val input_line : in_channel -> string
val input : in_channel -> string -> int -> int -> int
val really_input : in_channel -> string -> int -> int -> unit
val input_byte : in_channel -> int = "input_char"
val input_binary_int : in_channel -> int = "input_int"
val input_value : in_channel -> 'a = "input_value"
val seek_in : in_channel -> int -> unit = "seek_in"
val pos_in : in_channel -> int = "pos_in"
val in_channel_length : in_channel -> int = "channel_size"
val close_in : in_channel -> unit = "close_in"

(* References *)

type 'a ref = { mutable contents: 'a }
val ref: 'a -> 'a ref = "%makeblock"
val (!): 'a ref -> 'a = "%field0"
val (:=): 'a ref -> 'a -> unit = "%setfield0"
val incr: int ref -> unit = "%incr"
val decr: int ref -> unit = "%decr"

(* Miscellaneous *)

val exit : int -> 'a

type 'a option = None | Some of 'a

(**** For system use, not for the casual user ****)

val unsafe_really_input: in_channel -> string -> int -> int -> unit
