(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2025 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = int

let shift = Sys.int_size - 8
let of_int n = (n lsl shift) asr shift

let zero = 0
let one = 1
let minus_one = -1
let min_int = -128
let max_int = 127

let neg x = of_int (-x)
let add x y = of_int (x + y)
let sub x y = of_int (x - y)
let mul x y = of_int (x * y)
let div = ( / )
let unsigned_div n d =
  let n = if n < 0 then n + 256 else n in
  let d = if d < 0 then d + 256 else d in
  of_int (n / d)
let rem = ( mod )
let unsigned_rem n d =
  let n = if n < 0 then n + 256 else n in
  let d = if d < 0 then d + 256 else d in
  of_int (n mod d)
let succ n = of_int (n + 1)
let pred n = of_int (n - 1)
let abs n = if n >= 0 then n else of_int (-n)

let logand = ( land )
let logor = ( lor )
let logxor = ( lxor )
let lognot = lnot
let shift_left x n = of_int (x lsl n)
let shift_right = ( asr )
let shift_right_logical x n = ((x lsl shift) lsr n) asr shift

external to_int : t -> int = "%identity"
let unsigned_to_int n = Some (if n >= 0 then n else n + 256)

let of_string s =
  match Stdlib.int_of_string_opt s with
  | Some n when n >= min_int && n <= max_int -> n
  | Some n when n > max_int && n <= 255
      && s.[0] = '0' && (s.[1] = 'u' || s.[1] = 'U') -> n - 256
  | _ -> failwith "Int8.of_string"

let of_string_opt s = try Some (of_string s) with Failure _ -> None
let to_string = Stdlib.string_of_int

let compare = Stdlib.compare
let unsigned_compare n m = compare (sub n min_int) (sub m min_int)
let equal = ( = )

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

let seeded_hash = Int.seeded_hash
let hash = Int.hash
