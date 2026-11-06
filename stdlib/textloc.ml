(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Daniel C. Buenzli                           *)
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

  val pp_ocaml : Format.formatter -> t -> unit
  val pp_gnu : Format.formatter -> t -> unit
end


(* For now, both representations are the same. *)

module Compact : S with type t = textloc = struct
  include CamlinternalTextloc

  let pf = Format.fprintf

  let pp_ocaml ppf l =
    match is_none l with
    | true -> pf ppf "File \"%s\"" (file l)
    | false ->
        let pp_lines ppf l = match line_range l with
        | Single_line line -> pf ppf "line %d" (fst line)
        | Range (first_line, last_line) ->
            pf ppf "lines %d-%d" (fst first_line) (fst last_line)
        in
        (* "characters" represent positions (insertion points) not columns *)
        let pos_s = (first_byte l) - snd (first_line l) in
        let pos_e = (last_byte l) - snd (last_line l) + 1 in
        if pos_s = 0 && pos_e = 0
        then pf ppf "File \"%s\", %a" (file l) pp_lines l
        else pf ppf "File \"%s\", %a, characters %d-%d"
            (file l) pp_lines l pos_s pos_e

  let pp_gnu ppf l =
    match is_none l with
    | true -> pf ppf "%s:" (file l)
    | false ->
        let pp_lines ppf l =
          let col_s = (first_byte l) - snd (first_line l) + 1 in
          let col_e = (last_byte l) - snd (last_line l) + 1 in
          match line_range l with
          | Single_line line ->  pf ppf "%d.%d-%d" (fst line) col_s col_e
          | Range (first_line, last_line) ->
              pf ppf "%d.%d-%d.%d"
                (fst first_line) col_s (fst last_line) col_e
        in
        pf ppf "%s:%a" (file l) pp_lines l
end

include Compact

let of_compact l = l
let compact l = l
