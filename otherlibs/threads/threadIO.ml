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

(* Module [ThreadIO]: thread-compatible input-output operations *)

external inchan_ready : in_channel -> bool = "thread_inchan_ready"
external outchan_ready : out_channel -> int -> bool = "thread_outchan_ready"
external descr_inchan : in_channel -> Unix.file_descr = "channel_descriptor"
external descr_outchan : out_channel -> Unix.file_descr = "channel_descriptor"

let wait_inchan ic =
  if not (inchan_ready ic) then Thread.wait_read (descr_inchan ic)
let wait_outchan oc len =
  if not (outchan_ready oc len) then Thread.wait_write (descr_outchan oc)

(* Output functions *)

external flush_partial : out_channel -> bool = "flush_partial"
external output_partial : out_channel -> string -> int -> int -> int
                        = "output_partial"
let rec flush oc =
  wait_outchan oc (-1);
  if flush_partial oc then () else flush oc

let output_char oc c = wait_outchan oc 1; output_char oc c

let rec output oc buf pos len =
  if len > 0 then begin
    wait_outchan oc len;
    let written = output_partial oc buf pos len in
    output oc buf (pos + written) (len - written)
  end

let output_string oc s = output oc s 0 (String.length s)

let output_byte oc b = wait_outchan oc 1; output_byte oc b

let output_binary_int oc n =
  output_byte oc (n asr 24);
  output_byte oc (n asr 16);
  output_byte oc (n asr 8);
  output_byte oc n

let output_value oc v =
  output_string oc (Obj.marshal(Obj.repr v))

let seek_out oc pos = flush oc; seek_out oc pos

let close_out oc = flush oc; close_out oc

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

(* Input functions *)

let input_char ic = wait_inchan ic; input_char ic

let input_line ic =
  let rec do_input buf pos =
    if pos >= String.length buf then begin
      let newbuf = String.create (2 * String.length buf) in
      String.blit buf 0 newbuf 0 (String.length buf);
      do_input newbuf pos
    end else begin
      let c = input_char ic in
      if c = '\n' then
        String.sub buf 0 pos 
      else begin
        buf.[pos] <- c;
        do_input buf (pos + 1)
      end
    end in
  do_input (String.create 128) 0

let input ic buf ofs len = wait_inchan ic; input ic buf ofs len

let rec really_input ic s ofs len =
  if ofs < 0 or ofs + len > String.length s then invalid_arg "really_input"
  else if len <= 0 then ()
  else begin
    let r = input ic s ofs len in
    if r = 0
    then raise End_of_file
    else really_input ic s (ofs+r) (len-r)
  end

let input_byte ic = wait_inchan ic; input_byte ic

let input_binary_int ic =
  let b1 = input_byte ic in
  let n1 = if b1 >= 128 then b1 - 256 else b1 in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (n1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

let input_value ic =
  let header = String.create 20 in
  really_input ic header 0 20;
  let bsize =
    (Char.code header.[4] lsl 24) +
    (Char.code header.[5] lsl 16) +
    (Char.code header.[6] lsl 8) +
    Char.code header.[7] in
  let buffer = String.create (20 + bsize) in
  String.blit header 0 buffer 0 20;
  really_input ic buffer 20 bsize;
  let (res, pos) = Obj.unmarshal buffer 0 in
  Obj.magic res

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_float () = float_of_string(read_line())

(* Lexing *)

let lexing_from_channel ic =
  Lexing.from_function (fun buf n -> input ic buf 0 n)
