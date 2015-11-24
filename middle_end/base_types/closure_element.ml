(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** CR-someday lwhite: closure_element is not really a kind of variable,
    so this statement is a bit misleading. Perhaps both variable and
    closure_element should include from a parent type
    (e.g. unit_element). *)
include Variable

let wrap t = t
let unwrap t = t

let wrap_map t = t
