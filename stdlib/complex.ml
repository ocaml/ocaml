(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Complex numbers *)

type t = { re: float; im: float }

let zero = { re = 0.0; im = 0.0 }
let one = { re = 1.0; im = 0.0 }
let i = { re = 0.0; im = 1.0 }

let add x y = { re = x.re +. y.re; im = x.im +. y.im }

let sub x y = { re = x.re -. y.re; im = x.im -. y.im }

let neg x = { re = -. x.re; im = -. x.im }

let conj x = { re = x.re; im = -. x.im }

let mul x y = { re = x.re *. y.re -. x.im *. y.im;
                im = x.re *. y.im +. x.im *. y.re }

let div x y =
  if abs_float y.re >= abs_float y.im then
    let r = y.im /. y.re in
    let d = y.re +. r *. y.im in
    { re = (x.re +. r *. x.im) /. d;
      im = (x.im -. r *. x.re) /. d }
  else
    let r = y.re /. y.im in
    let d = y.im +. r *. y.re in
    { re = (r *. x.re +. x.im) /. d;
      im = (r *. x.im -. x.re) /. d }

let inv x = div one x

let norm2 x = x.re *. x.re +. x.im *. x.im

let norm x =
  (* Watch out for overflow in computing re^2 + im^2 *)
  let r = abs_float x.re and i = abs_float x.im in
  if r = 0.0 then i
  else if i = 0.0 then r
  else if r >= i then
    let q = i /. r in r *. sqrt(1.0 +. q *. q)
  else
    let q = r /. i in i *. sqrt(1.0 +. q *. q)

let arg x = atan2 x.im x.re

let polar n a = { re = cos a *. n; im = sin a *. n }

let sqrt x =
  (* Avoid cancellation in computing norm x + x.re 
     when x.re < 0 and x.im is small *)
  if x.re >= 0.0 then begin
    let r = sqrt(0.5 *. norm x +. 0.5 *. x.re) in
    { re = r; im = if r = 0.0 then 0.0 else 0.5 *. x.im /. r }
  end else begin
    let s = sqrt(0.5 *. norm x -. 0.5 *. x.re) in
    { re = if s = 0.0 then 0.0 else 0.5 *. abs_float x.im /. s;
      im = if x.im >= 0.0 then s else -. s }
  end

let exp x =
  let e = exp x.re in { re = e *. cos x.im; im = e *. sin x.im }

let log x = { re = log (norm x); im = atan2 x.im x.re }

let pow x y = exp (mul y (log x))
