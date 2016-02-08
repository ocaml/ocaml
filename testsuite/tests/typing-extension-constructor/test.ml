(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                 Jeremie Dimino, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type t = ..;;
type t += A;;

[%extension_constructor A];;
([%extension_constructor A] : extension_constructor);;

module M = struct
  type extension_constructor = int
end;;

open M;;

([%extension_constructor A] : extension_constructor);;
