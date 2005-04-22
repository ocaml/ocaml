(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let ( + ) = generic ( + ) | ( +. ) | Int32.add | Int64.add | Nativeint.add
let ( - ) = generic ( - ) | ( -. ) | Int32.sub | Int64.sub | Nativeint.sub
let ( * ) = generic ( * ) | ( *. ) | Int32.mul | Int64.mul | Nativeint.mul
let ( / ) = generic ( / ) | ( /. ) | Int32.div | Int64.div | Nativeint.div  
