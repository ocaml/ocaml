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
   been redefined to not block the whole process, but only the calling
   thread. *)

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

let stdin = open_descriptor_in 0
let stdout = open_descriptor_out 1
let stderr = open_descriptor_out 2

(* Non-blocking stuff *)

external thread_wait_read_prim : Unix.file_descr -> unit = "thread_wait_read"
external thread_wait_write_prim : Unix.file_descr -> unit = "thread_wait_write"

let thread_wait_read fd = thread_wait_read_prim fd
let thread_wait_write fd = thread_wait_write_prim fd

external inchan_ready : in_channel -> bool = "thread_inchan_ready"
external outchan_ready : out_channel -> int -> bool = "thread_outchan_ready"
external descr_inchan : in_channel -> Unix.file_descr = "channel_descriptor"
external descr_outchan : out_channel -> Unix.file_descr = "channel_descriptor"

let wait_inchan ic =
  if not (inchan_ready ic) then thread_wait_read (descr_inchan ic)
let wait_outchan oc len =
  if not (outchan_ready oc len) then thread_wait_write (descr_outchan oc)

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc: string -> open_flag list -> int -> int = "sys_open"

let open_out_gen mode perm name =
  open_descriptor_out(open_desc name mode perm)

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

external flush_partial : out_channel -> bool = "flush_partial"

let rec flush oc =
  wait_outchan oc (-1);
  if flush_partial oc then () else flush oc

external unsafe_output_partial : out_channel -> string -> int -> int -> int
                        = "output_partial"

let rec unsafe_output oc buf pos len =
  if len > 0 then begin
    wait_outchan oc len;
    let written = unsafe_output_partial oc buf pos len in
    unsafe_output oc buf (pos + written) (len - written)
  end

external output_char_blocking : out_channel -> char -> unit = "output_char"
external output_byte_blocking : out_channel -> int -> unit = "output_char"

let output_char oc c = wait_outchan oc 1; output_char_blocking oc c

let output_string oc s =
  unsafe_output oc s 0 (string_length s)

let output oc s ofs len =
  if ofs < 0 or ofs + len > string_length s
  then invalid_arg "output"
  else unsafe_output oc s ofs len

let output_byte oc b = wait_outchan oc 1; output_byte_blocking oc b

let output_binary_int oc n =
  output_byte oc (n asr 24);
  output_byte oc (n asr 16);
  output_byte oc (n asr 8);
  output_byte oc n

external marshal : 'a -> string = "output_value_to_string"

let output_value oc v = output_string oc (marshal v)

external seek_out_blocking : out_channel -> int -> unit = "seek_out"

let seek_out oc pos = flush oc; seek_out_blocking oc pos

external pos_out : out_channel -> int = "pos_out"
external out_channel_length : out_channel -> int = "channel_size"
external close_out_channel : out_channel -> unit = "close_channel"

let close_out oc = flush oc; close_out_channel oc

(* General input functions *)

let open_in_gen mode perm name =
  open_descriptor_in(open_desc name mode perm)

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

external input_char_blocking : in_channel -> char = "input_char"
external input_byte_blocking : in_channel -> int = "input_char"

let input_char ic = wait_inchan ic; input_char_blocking ic

external unsafe_input_blocking : in_channel -> string -> int -> int -> int
                               = "input"

let unsafe_input ic s ofs len =
  wait_inchan ic; unsafe_input_blocking ic s ofs len

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

let input_line ic =
  let rec do_input buf pos =
    if pos >= string_length buf then begin
      let newbuf = string_create (2 * string_length buf) in
      string_blit buf 0 newbuf 0 (string_length buf);
      do_input newbuf pos
    end else begin
      let c = input_char ic in
      if c = '\n' then begin
        let res = string_create pos in
        string_blit buf 0 res 0 pos;
        res
      end else begin
        buf.[pos] <- c;
        do_input buf (pos + 1)
      end
    end in
  do_input (string_create 128) 0

let input_byte ic = wait_inchan ic; input_byte_blocking ic

let input_binary_int ic =
  let b1 = input_byte ic in
  let n1 = if b1 >= 128 then b1 - 256 else b1 in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (n1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

external unmarshal : string -> int -> 'a * int = "input_value_from_string"
external char_code: char -> int = "%identity"

let input_value ic =
  let header = string_create 20 in
  really_input ic header 0 20;
  let bsize =
    (char_code header.[4] lsl 24) +
    (char_code header.[5] lsl 16) +
    (char_code header.[6] lsl 8) +
    char_code header.[7] in
  let buffer = string_create (20 + bsize) in
  string_blit header 0 buffer 0 20;
  really_input ic buffer 20 bsize;
  let (res, pos) = unmarshal buffer 0 in
  res

external seek_in : in_channel -> int -> unit = "seek_in"
external pos_in : in_channel -> int = "pos_in"
external in_channel_length : in_channel -> int = "channel_size"
external close_in : in_channel -> unit = "close_channel"

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
