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

(** {1:locations Text locations} *)

(** [Textloc.t] is the type of text locations, given, notably, to implicit
    location arguments. *)

module type S = sig
  include CamlinternalTextloc.S (** @inline *)

  (** {2:fmt Formatting} *)

  val pp_ocaml : Format.formatter -> t -> unit
  (** [pp_ocaml] formats text locations like the OCaml compiler:
      {ul
      {- If the location is [none], it prints [File "-"].}
      {- Otherwise, it prints [File "filename", lines L1-L2, characters C1-C2],
         where L1-L2 are the line numbers of the lines spanned by the location.
         If the location spans a single line, [line L] is printed instead.
         C1-C2 are zero-based byte positions in a line.}
      {- However, if the start position is at the beginning of its line, and
         the end position is on an empty line, or if the file is empty, then no
         character range is printed.}
      } *)

  val pp_gnu : Format.formatter -> t -> unit
  (** [pp_gnu] formats text locations according to the
      {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
      convention}. *)
end

include S (** @inline *)

(** {1:compact Compact representation} *)

module Compact : S with type t = textloc

val of_compact : Compact.t -> t
val compact : t -> Compact.t
