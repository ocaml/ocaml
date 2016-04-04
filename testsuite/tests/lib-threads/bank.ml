(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The bank account example, using events and channels *)

open Printf
open Event

type account = int channel * int channel

let account (put_ch, get_ch) =
  let rec acc balance =
    select [
      wrap (send get_ch balance) (fun () -> acc balance);
      wrap (receive put_ch) (fun amount ->
        if balance + amount < 0 then failwith "negative balance";
        acc (balance + amount))
    ]
  in acc 0

let get ((put_ch, get_ch): account) = sync (receive get_ch)
let put ((put_ch, get_ch): account) amount = sync (send put_ch amount)

let _ =
  let a : account = (new_channel(), new_channel()) in
  ignore (Thread.create account a);
  put a 100;
  printf "Current balance: %d\n" (get a);
  for i = 1 to 99 do put a (-2); put a 1 done;
  printf "Final balance: %d\n" (get a)
