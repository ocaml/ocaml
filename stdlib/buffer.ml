(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* Extensible buffers *)

type t =
 {mutable buffer : string;
  mutable position : int;
  mutable length : int;
  mutable out_chan : out_channel option;
  initial_buffer : string};;

let length b = b.length;;

let position b = b.position;;
let set_position b p =
 if p < 0 || p >= b.length then invalid_arg "set_position" else
 b.position <- p;;

let of_string s =
 {buffer = s; position = 0; length = String.length s; out_chan = None;
  initial_buffer = s};;

let create n = of_string (String.create n);;

let contents b = String.sub b.buffer 0 b.position;;

let clear b = b.position <- 0;;

let to_string b = let s = contents b in clear b; s;;

let reset b = clear b; b.buffer <- b.initial_buffer;;

let flush b =
 match b.out_chan with
 | None -> ()
 | Some oc ->
     Pervasives.output oc b.buffer 0 b.position;
     clear b;;

let connect_out b oc = b.out_chan <- Some oc;;

let disconnect_out b =
 match b.out_chan with
 | None -> ()
 | Some oc ->
    Pervasives.output oc b.buffer 0 b.position;
    reset b;
    b.out_chan <- None;;

let resize b more =
 flush b;
 let len = b.length in
 if b.position + more > len then 
  let new_len = if more < len then len + len else len + len + more in
  let new_buffer = String.create new_len in
  String.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- new_len;;

(* Give_room is slightly different from resize, since it does not
   flush systematically the buffer. *)
let give_room b l =
 if b.position + l > b.length then resize b l;;

let output_char b c =
 let pos = b.position in
 if pos >= b.length then resize b 1;
 b.buffer.[pos] <- c;
 b.position <- pos + 1;;

let output b s offset l =
 let new_position = b.position + l in
 if new_position > b.length then resize b l;
 String.blit s offset b.buffer b.position l;
 b.position <- new_position;;

let output_string b s = output b s 0 (String.length s);;

let output_buffer b bs = output b bs.buffer 0 bs.position;;

let output_buffer_out oc b = Pervasives.output oc b.buffer 0 b.position;;

(* Input in buffers. *)
let really_input ic b len =
 give_room b len;
 Pervasives.really_input ic b.buffer b.position len;
 b.position <- b.position + len;;

let input ic b len =
 give_room b len;
 let n = Pervasives.input ic b.buffer b.position len in
 b.position <- b.position + n;
 n;;

let read_in_channel ic b =
 let len = in_channel_length ic in
 really_input ic b len;;

let read_file b f =
 let ic = open_in f in
 read_in_channel ic b;
 close_in ic;;

let of_file f =
 let ic = open_in f in
 let len = in_channel_length ic in
 let b = create len in
 really_input ic b len;
 close_in ic;
 b;;

let to_file b f =
 let oc = Pervasives.open_out f in
 output_buffer_out oc b;
 Pervasives.close_out oc;;

(* The printf facility for buffers. *)

external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let bprintf b format =
  let format = (Obj.magic format : string) in

  let rec doprn start i =
    if i >= String.length format then begin
      if i > start then output b format start (i-start);
      (Obj.magic ())
    end else
      if format.[i] != '%' then
        doprn start (i+1)
      else begin
        if i > start then output b format start (i-start);
        let j = skip_args (succ i) in
        match format.[j] with
        | '%' ->
            doprn j (succ j)
        | 's' ->
            Obj.magic(dostring i j)
        | 'c' ->
            Obj.magic(fun c ->
                output_char b c;
                doprn (succ j) (succ j))
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(doint i j)
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            Obj.magic(dofloat i j)
        | 'b' ->
            Obj.magic(fun bool ->
                output_string b (string_of_bool bool);
                doprn (succ j) (succ j))
        | 'a' ->
            Obj.magic(fun printer arg ->
              printer b arg;
              doprn (succ j) (succ j))
        | 't' ->
            Obj.magic(fun printer ->
              printer b;
              doprn (succ j) (succ j))
        | c ->
            invalid_arg ("bprintf: unknown format " ^ Char.escaped c)
      end

  and skip_args j =
    match format.[j] with
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
      ' ' | '.' | '-' ->
        skip_args (succ j)
    | c ->
        j
    
  and dostring i j s =
    if j <= i+1 then
      output_string b s
    else begin
      let p =
        try
          int_of_string (String.sub format (i+1) (j-i-1))
        with _ ->
          invalid_arg "bprintf: bad %s format" in
      if p > 0 && String.length s < p then begin
        output_string b (String.make (p - String.length s) ' ')
      end else if p < 0 && String.length s < -p then begin
        output_string b s;
        output_string b (String.make (-p - String.length s) ' ')
      end else
        output_string b s
    end;
    doprn (succ j) (succ j)

  and doint i j n =
    let len = j-i in
    let fmt = String.create (len+2) in
    String.blit format i fmt 0 len;
    fmt.[len] <- 'l';
    fmt.[len + 1] <- format.[j];
    output_string b (format_int fmt n);
    doprn (succ j) (succ j)

  and dofloat i j f =
    output_string b (format_float (String.sub format i (j-i+1)) f);
    doprn (succ j) (succ j)

  in doprn 0 0
;;
