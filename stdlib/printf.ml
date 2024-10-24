(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open CamlinternalFormatBasics
open CamlinternalFormat

let kfprintf k o (Format (fmt, _)) =
  make_printf (fun acc -> output_acc o acc; k o) End_of_acc fmt
let kbprintf k b (Format (fmt, _)) =
  make_printf (fun acc -> bufput_acc b acc; k b) End_of_acc fmt
let ikfprintf k oc (Format (fmt, _)) =
  make_iprintf k oc fmt
let ikbprintf = ikfprintf

let fprintf oc fmt = kfprintf ignore oc fmt
let bprintf b fmt = kbprintf ignore b fmt
let ifprintf oc fmt = ikfprintf ignore oc fmt
let ibprintf b fmt = ikbprintf ignore b fmt
let printf fmt = fprintf stdout fmt
let eprintf fmt = fprintf stderr fmt

let ksprintf k (Format (fmt, _)) =
  let k' acc =
    let buf = Buffer.create 64 in
    strput_acc buf acc;
    k (Buffer.contents buf) in
  make_printf k' End_of_acc fmt

let sprintf fmt = ksprintf (fun s -> s) fmt

let kprintf = ksprintf

(*
  Defining heterogeneous list flavours of functions defined above
*)

module Arg_list = Arg_list

let lfprintf oc (Format (fmt, _)) args =
  make_lprintf (output_acc oc) End_of_acc fmt args

let lbprintf b (Format (fmt, _)) args =
  make_lprintf (bufput_acc b) End_of_acc fmt args

let lprintf fmt args =
  lfprintf stdout fmt args

let leprintf fmt args =
  lfprintf stderr fmt args

let lsprintf (Format (fmt, _)) args =
  let k acc = 
    let buf = Buffer.create 64 in
    strput_acc buf acc;
    (Buffer.contents buf)
  in
  make_lprintf k End_of_acc fmt args

