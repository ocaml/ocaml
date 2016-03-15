(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                        Alain Frisch, LexiFi                            *)
(*                                                                        *)
(*   Copyright 1995 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


let f (type t) () =
  let exception E of t in
  (fun x -> E x), (function E _ -> print_endline "OK" | _ -> print_endline "KO")

let inj1, proj1 = f ()
let inj2, proj2 = f ()

let () = proj1 (inj1 42)
let () = proj1 (inj2 42)
