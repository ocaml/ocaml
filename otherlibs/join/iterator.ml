(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Luc Maranget, projet Moscova, INRIA Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

module type S = sig

  type t
  type elt
  type enum


  val start : t -> enum

  val step : enum -> (elt * enum) option
end

module ListMake(E:sig type elt end) = struct
  type t = E.elt list
  type elt = E.elt
  type enum =  E.elt list

  let start xs = xs

  let step = function
    | [] -> None
    | x::xs -> Some (x,xs)
end
