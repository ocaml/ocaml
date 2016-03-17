(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*              Pierre Weis, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let s = { Float_record.f = Float_record.make 1.0 };;

print_float (Float_record.from s.Float_record.f);;
print_newline ();;


let b = (Float_array.small_float_array [@inlined]) 12
let c = (Float_array.longer_float_array [@inlined]) 34

let print_array a =
  Array.iter (fun f ->
      print_float f;
      print_newline ()) a;
  print_newline ()

let () =
  print_array (fst b);
  print_array (fst c);
