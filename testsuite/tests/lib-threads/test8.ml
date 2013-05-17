(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Event

type 'a buffer_channel = {
  input: 'a channel;
  output: 'a channel;
  thread: Thread.t;
  finished: bool ref;
}

let new_buffer_channel() =
  let ic = new_channel() in
  let oc = new_channel() in
  let finished = ref false in
  let rec buffer_process front rear =
    if !finished then Thread.exit ();
    match (front, rear) with
    | ([], []) -> buffer_process [sync(receive ic)] []
    | (hd::tl, _) ->
        select [
          wrap (receive ic) (fun x -> buffer_process front (x::rear));
          wrap (send oc hd) (fun () -> buffer_process tl rear)
        ]
    | ([], _) -> buffer_process (List.rev rear) [] in
  let t = Thread.create (buffer_process []) [] in
  { input = ic; output = oc; thread = t; finished = finished }

let buffer_send bc data =
  sync(send bc.input data)

let buffer_receive bc =
  receive bc.output

(* Test *)

let box = new_buffer_channel()
let ch = new_channel()

let f () =
  buffer_send box "un";
  buffer_send box "deux";
  sync (send ch 3)

let g () =
  print_int (sync(receive ch)); print_newline();
  print_string (sync(buffer_receive box)); print_newline();
  print_string (sync(buffer_receive box)); print_newline()

let _ =
  let t = Thread.create f () in
  g();
  box.finished := true; buffer_send box "";
  Thread.join box.thread;
  Thread.join t
