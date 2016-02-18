(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                  Jeremie Dimino, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = ..

module M = struct
  type t += A
  type t += B of int
end

type t += C
type t += D of int * string

let () =
  assert (Obj.extension_constructor  M.A
          == [%extension_constructor M.A]);
  assert (Obj.extension_constructor (M.B 42)
          == [%extension_constructor M.B]);
  assert (Obj.extension_constructor  C
          == [%extension_constructor C]);
  assert (Obj.extension_constructor (D (42, ""))
          == [%extension_constructor D])

let () = print_endline "OK"
