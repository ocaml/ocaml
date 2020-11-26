(* TEST

* hassysthreads
include systhreads
** bytecode
** native

*)

(* The bank account example, using events and channels *)

open Printf
open Event

type account = {
  get: int channel;
  put: int channel;
  stop: unit channel
  }

let account a =
  let rec acc balance =
    select [
      wrap (send a.get balance) (fun () -> acc balance);
      wrap (receive a.put) (fun amount ->
        if balance + amount < 0 then failwith "negative balance";
        acc (balance + amount));
      wrap (receive a.stop) (fun _ -> ())
    ]
  in acc 0

let get a = sync (receive a.get)
let put a amount = sync (send a.put amount)
let stop a = sync (send a.stop ())

let _ =
  let a = { get = new_channel(); put = new_channel(); stop = new_channel() } in
  let th = Thread.create account a in
  put a 100;
  printf "Current balance: %d\n" (get a);
  for i = 1 to 99 do put a (-2); put a 1 done;
  printf "Final balance: %d\n" (get a);
  stop a;
  Thread.join th
