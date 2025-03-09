(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Ocamlcp_common.Make(struct
  let bytecode = false
  module Make_options(Args : Ocamlcp_common.Ocamlcp_args) =
    Main_args.Make_optcomp_options(struct
      include Main_args.Default.Optmain
      include Args
    end)
end)

let () = main ()
