(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** CR-someday lwhite: "Closure_id" is quite a generic name. I wonder wether something
    like "Closure_label" would better capture that it is the label of a projection. *)

(** An identifier, unique across the whole program (not just one compilation
    unit), that identifies a closure within a particular set of closures
    (viz. [Project_closure]). *)

include module type of Closure_element
