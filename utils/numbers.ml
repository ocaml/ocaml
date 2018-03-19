(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Int_base = Identifiable.Make (struct
  type t = int

  let compare x y = x - y
  let output oc x = Printf.fprintf oc "%i" x
  let hash i = i
  let equal (i : int) j = i = j
  let print = Format.pp_print_int
end)

module Int = struct
  type t = int

  include Int_base

  let rec zero_to_n n =
    if n < 0 then Set.empty else Set.add n (zero_to_n (n-1))
end

module Float = struct
  type t = float

  include Identifiable.Make (struct
    type t = float

    let compare x y = Pervasives.compare x y
    let output oc x = Printf.fprintf oc "%f" x
    let hash f = Hashtbl.hash f
    let equal (i : float) j = i = j
    let print = Format.pp_print_float
  end)
end
