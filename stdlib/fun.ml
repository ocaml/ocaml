(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external id : 'a -> 'a = "%identity"
let const c _ = c
let flip f x y = f y x
let negate p v = not (p v)

let curry2 f x y = f (x, y)
let curry3 f x y z = f (x, y, z)
let curry4 f x y z w = f (x, y, z, w)
let curry5 f x y z w a = f (x, y, z, w, a)
let curry6 f x y z w a b = f (x, y, z, w, a, b)
let curry7 f x y z w a b c = f (x, y, z, w, a, b, c)
let curry8 f x y z w a b c d = f (x, y, z, w, a, b, c, d)
let curry9 f x y z w a b c d e = f (x, y, z, w, a, b, c, d, e)

let uncurry2 f (x, y) = f x y
let uncurry3 f (x, y, z) = f x y z
let uncurry4 f (x, y, z, w) = f x y z w
let uncurry5 f (x, y, z, w, a) = f x y z w a
let uncurry6 f (x, y, z, w, a, b) = f x y z w a b
let uncurry7 f (x, y, z, w, a, b, c) = f x y z w a b c
let uncurry8 f (x, y, z, w, a, b, c, d) = f x y z w a b c d
let uncurry9 f (x, y, z, w, a, b, c, d, e) = f x y z w a b c d e

let curry = curry2
let uncurry = uncurry2

exception Finally_raised of exn

let () = Printexc.register_printer @@ function
| Finally_raised exn -> Some ("Fun.Finally_raised: " ^ Printexc.to_string exn)
| _ -> None

let protect ~(finally : unit -> unit) work =
  let finally_no_exn () =
    try finally () with e ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Finally_raised e) bt
  in
  match work () with
  | result -> finally_no_exn () ; result
  | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      finally_no_exn () ;
      Printexc.raise_with_backtrace work_exn work_bt
