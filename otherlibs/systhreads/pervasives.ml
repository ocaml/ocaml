(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Same as ../../stdlib/pervasives.ml, except that I/O functions have
   been redefined to ensure proper locking around I/O operations. *)

type 'a option = None | Some of 'a

(* Exceptions *)

external raise : exn -> 'a = "%raise"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "compare" "noalloc"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external (~-) : int -> int = "%negint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62)
let max_int = min_int - 1

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "power_float" "pow" "float"
external exp : float -> float = "exp_float" "exp" "float"
external log : float -> float = "log_float" "log" "float"
external sqrt : float -> float = "sqrt_float" "sqrt" "float"
external sin : float -> float = "sin_float" "sin" "float"
external cos : float -> float = "cos_float" "cos" "float"
external tan : float -> float = "tan_float" "tan" "float"
external asin : float -> float = "asin_float" "asin" "float"
external acos : float -> float = "acos_float" "acos" "float"
external atan : float -> float = "atan_float" "atan" "float"
external atan2 : float -> float -> float = "atan2_float" "atan2" "float"
external abs_float : float -> float = "%absfloat"
external float : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"

(* String operations -- more in module String *)

external string_length : string -> int = "ml_string_length"
external string_create: int -> string = "create_string"
external string_blit : string -> int -> string -> int -> int -> unit
                     = "blit_string"

let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let string_of_bool b =
  if b then "true" else "false"

let string_of_int n =
  format_int "%d" n

external int_of_string : string -> int = "int_of_string"

let string_of_float f =
  format_float "%.12g" f

external float_of_string : string -> float = "float_of_string"

(* List operations -- more in module List *)

let rec (@) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* I/O operations *)

type in_channel
type out_channel

external open_descriptor_out: int -> out_channel = "open_descriptor"
external open_descriptor_in: int -> in_channel = "open_descriptor"

let stdin = Iolock.add(open_descriptor_in 0)
let stdout = Iolock.add(open_descriptor_out 1)
let stderr = Iolock.add(open_descriptor_out 2)

(* Wrappers for locking *)

let wrap1 fn chan =
  let m = Iolock.find chan in
  Iolock.lock m;
  try
    let res = fn chan in
    Iolock.unlock m;
    res
  with x ->
    Iolock.unlock m; raise x

let wrap2 fn chan arg =
  let m = Iolock.find chan in
  Iolock.lock m;
  try
    let res = fn chan arg in
    Iolock.unlock m;
    res
  with x ->
    Iolock.unlock m; raise x

let wrap4 fn chan arg1 arg2 arg3 =
  let m = Iolock.find chan in
  Iolock.lock m;
  try
    let res = fn chan arg1 arg2 arg3 in
    Iolock.unlock m;
    res
  with x ->
    Iolock.unlock m; raise x

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc: string -> open_flag list -> int -> int = "sys_open"

let open_out_gen mode perm name =
  Iolock.add(open_descriptor_out(open_desc name mode perm))

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

external flush_unlocked : out_channel -> unit = "flush"
let flush = wrap1 flush_unlocked

external unsafe_output_unlocked : out_channel -> string -> int -> int -> unit
                                = "output"
let unsafe_output = wrap4 unsafe_output_unlocked

external output_char_unlocked : out_channel -> char -> unit = "output_char"
let output_char = wrap2 output_char_unlocked

let output_string oc s =
  unsafe_output oc s 0 (string_length s)

let output oc s ofs len =
  if ofs < 0 or ofs + len > string_length s
  then invalid_arg "output"
  else unsafe_output oc s ofs len

external output_byte_unlocked : out_channel -> int -> unit = "output_char"
let output_byte = wrap2 output_byte_unlocked

external output_binary_int_unlocked : out_channel -> int -> unit = "output_int"
let output_binary_int = wrap2 output_binary_int_unlocked

external output_value_unlocked : out_channel -> 'a -> unit = "output_value"
let output_value oc v = wrap2 output_value_unlocked oc v

external seek_out_unlocked : out_channel -> int -> unit = "seek_out"
let seek_out = wrap2 seek_out_unlocked

external pos_out_unlocked : out_channel -> int = "pos_out"
let pos_out = wrap1 pos_out_unlocked

external out_channel_length_unlocked : out_channel -> int = "channel_size"
let out_channel_length = wrap1 out_channel_length_unlocked

external close_out_channel_unlocked : out_channel -> unit = "close_channel"
let close_out oc =
  flush oc; wrap1 close_out_channel_unlocked oc; Iolock.remove oc

(* General input functions *)

let open_in_gen mode perm name =
  Iolock.add(open_descriptor_in(open_desc name mode perm))

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

external input_char_unlocked : in_channel -> char = "input_char"
let input_char = wrap1 input_char_unlocked

external unsafe_input_unlocked : in_channel -> string -> int -> int -> int
                               = "input"
let unsafe_input = wrap4 unsafe_input_unlocked

let input ic s ofs len =
  if ofs < 0 or ofs + len > string_length s
  then invalid_arg "input"
  else unsafe_input ic s ofs len

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then () else begin
    let r = unsafe_input ic s ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic s (ofs+r) (len-r)
  end

let really_input ic s ofs len =
  if ofs < 0 or ofs + len > string_length s
  then invalid_arg "really_input"
  else unsafe_really_input ic s ofs len

external input_scan_line : in_channel -> int = "input_scan_line"

let rec input_line_unlocked chan =
  let n = input_scan_line chan in
  if n = 0 then                         (* n = 0: we are at EOF *)
    raise End_of_file
  else if n > 0 then begin              (* n > 0: newline found in buffer *)
    let res = string_create (n-1) in
    unsafe_input_unlocked chan res 0 (n-1);
    input_char_unlocked chan;                    (* skip the newline *)
    res
  end else begin                        (* n < 0: newline not found *)
    let beg = string_create (-n) in
    unsafe_input_unlocked chan beg 0 (-n);
    try
      beg ^ input_line_unlocked chan
    with End_of_file ->
      beg
  end

let input_line = wrap1 input_line_unlocked

external input_byte_unlocked : in_channel -> int = "input_char"
let input_byte = wrap1 input_byte_unlocked

external input_binary_int_unlocked : in_channel -> int = "input_int"
let input_binary_int = wrap1 input_binary_int_unlocked

external input_value_unlocked : in_channel -> 'a = "input_value"
let input_value ic = wrap1 input_value_unlocked ic

external seek_in_unlocked : in_channel -> int -> unit = "seek_in"
let seek_in = wrap2 seek_in_unlocked

external pos_in_unlocked : in_channel -> int = "pos_in"
let pos_in = wrap1 pos_in_unlocked

external in_channel_length_unlocked : in_channel -> int = "channel_size"
let in_channel_length = wrap1 in_channel_length_unlocked

external close_in_unlocked : in_channel -> unit = "close_channel"
let close_in ic = wrap1 close_in_unlocked ic; Iolock.remove ic

(* Output functions on standard output *)

let print_char c = output_char stdout c
let print_string s = output_string stdout s
let print_int i = output_string stdout (string_of_int i)
let print_float f = output_string stdout (string_of_float f)
let print_endline s = output_string stdout s; output_char stdout '\n'
let print_newline () = output_char stdout '\n'; flush stdout

(* Output functions on standard error *)

let prerr_char c = output_char stderr c
let prerr_string s = output_string stderr s
let prerr_int i = output_string stderr (string_of_int i)
let prerr_float f = output_string stderr (string_of_float f)
let prerr_endline s =
  output_string stderr s; output_char stderr '\n'; flush stderr
let prerr_newline () = output_char stderr '\n'; flush stderr

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_float () = float_of_string(read_line())

(* References *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"

(* Miscellaneous *)

external sys_exit : int -> 'a = "sys_exit"

let exit_function = ref (fun () -> flush stdout; flush stderr)

let exit retcode =
  (!exit_function)();
  sys_exit retcode

let at_exit f =
  let g = !exit_function in
  exit_function := (fun () -> f(); g())
