(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*               Olivier Nicole, projet Cambium, Inria Paris              *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  include CamlinternalTextloc.S

  (** {2:fmt Formatting} *)

  val pp_ocaml : Format.formatter -> t -> unit
  (** [pp_ocaml] formats text locations like the OCaml compiler. *)

  val pp_gnu : Format.formatter -> t -> unit
  (** [pp_gnu] formats text locations according to the
      {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
      convention}. *)
end


(* For now, both representations are the same. *)

module Compact : S with type t = textloc = struct
  include CamlinternalTextloc

  let is_none view = view.first_byte < 0

  let pf = Format.fprintf
  let pp_ocaml ppf l =
    let l = view l in
    match is_none l with
    | true -> pf ppf "File \"%s\"" l.file
    | false ->
        let pp_lines ppf l = match fst l.first_line = fst l.last_line with
        | true -> pf ppf "line %d" (fst l.first_line)
        | false -> pf ppf "lines %d-%d" (fst l.first_line) (fst l.last_line)
        in
        (* "characters" represent positions (insertion points) not columns *)
        let pos_s = l.first_byte - snd l.first_line in
        let pos_e = l.last_byte - snd l.last_line + 1 in
        if pos_s = 0 && pos_e = 0
        then pf ppf "File \"%s\", %a" l.file pp_lines l
        else pf ppf "File \"%s\", %a, characters %d-%d"
            l.file pp_lines l pos_s pos_e

  let pp_gnu _fmt _l = assert false
end

include Compact

let of_compact l = l
let compact l = l
