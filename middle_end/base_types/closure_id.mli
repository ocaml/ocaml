(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Library General Public License version 2.1, with the         *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

(** CR-someday lwhite: "Closure_id" is quite a generic name. I wonder wether something
    like "Closure_label" would better capture that it is the label of a projection. *)

(** An identifier, unique across the whole program (not just one compilation
    unit), that identifies a closure within a particular set of closures
    (viz. [Project_closure]). *)

include module type of Closure_element
