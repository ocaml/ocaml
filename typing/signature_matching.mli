(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Malo Monin, projet Cambium, Inria Paris                 *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Suggestion : sig
  type alteration =
    | Missing_item
    | Possible_match of Ident.t Location.loc

  type 'a t = {
    subject : Types.signature_item;
    alteration : 'a;
  }

  type report = {
      alterations: alteration t list;
      incompatibles: Includemod.Error.sigitem_symptom t list
    }
end

val suggest :
  Includemod.Error.signature_symptom -> Suggestion.report
