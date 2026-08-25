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

let id x = x
let const c _ = c
let compose f g x = f (g x)
let flip f x y = f y x
let negate p v = not (p v)

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

type todo = todo_info

type implem = string * int
(* Hidden type between {!todo_info}. Changing it requires changing the
   implementation of the [%todo] primitive. *)

let string_of_todo (t : todo) =
  let (file, line) : implem = Obj.magic t in
  Printf.sprintf
    "File %S, line %d"
    (match file with "" -> "-" | _ -> file)
    line

exception Todo = Todo

let () =
  Printexc.register_printer @@ function
    | Todo info ->
        Some (
          Printf.sprintf "Fun.Todo\n%s" (string_of_todo info))
    | _ -> None

external todo : unit -> _ = "%todo"
