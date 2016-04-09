(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Printf

let bug () =
  let mat = [| [|false|] |]
  and test = ref false in
    printf "Value of test at the beginning : %b\n" !test; flush stdout;
    (try let _ = mat.(0).(-1) in
       (test := true;
        printf "Am I going through this block of instructions ?\n";
        flush stdout)
     with Invalid_argument _ -> printf "Value of test now : %b\n" !test
    );
    (try if mat.(0).(-1) then ()
     with Invalid_argument _ -> ()
    )

let () = bug ()
