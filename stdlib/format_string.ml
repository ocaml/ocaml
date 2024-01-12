(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b, 'c, 'd, 'e, 'f) t6 =
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6
= Format of ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt
            * string

type ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) t6


type ('a, 'b, 'c, 'd) t4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) t4


type ('a, 'b, 'c) t = ('a, 'b, 'c, 'c) format4
type ('a, 'b, 'c) format = ('a, 'b, 'c) t

let to_string (Format (_fmt, str)) = str

external id :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
 ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

let ( ^^ ) (Format (fmt1, str1)) (Format (fmt2, str2)) =
  Format (CamlinternalFormatBasics.concat_fmt fmt1 fmt2,
          str1 ^ "%," ^ str2)
