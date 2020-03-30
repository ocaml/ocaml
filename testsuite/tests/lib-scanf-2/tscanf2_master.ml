(* TEST

include unix
modules = "tscanf2_io.ml"
files = "tscanf2_slave.ml"
reference = "${test_source_directory}/reference"

(* The bytcode test *)

* setup-ocamlc.byte-build-env

program = "${test_build_directory}/master.byte"

** ocamlc.byte (* Compiles the master *)

*** ocamlc.byte (* Compiles the slave *)

all_modules = "tscanf2_io.cmo tscanf2_slave.ml"

program = "${test_build_directory}/slave.byte"

**** check-ocamlc.byte-output

***** run

program = "${test_build_directory}/master.byte"

arguments = "${test_build_directory}/slave.byte"

****** check-program-output

(* The native test *)

* setup-ocamlopt.byte-build-env

program = "${test_build_directory}/master.opt"

** ocamlopt.byte (* Compiles the master *)

*** ocamlopt.byte (* Compiles the slave *)

all_modules = "tscanf2_io.cmx tscanf2_slave.ml"

program = "${test_build_directory}/slave.opt"

**** check-ocamlopt.byte-output

***** run

program = "${test_build_directory}/master.opt"

arguments = "${test_build_directory}/slave.opt"

****** check-program-output

*)

(* A very simple master:
   - first launch a slave process,
   - then repeat a random number of times:
     + print the string " Ping" on stderr,
     + send it to the slave,
     + and wait for its answer "-pong",
   - finally send the string "stop" to the slave
     and wait for its answer "OK, bye!"
     and die.

   Use the communication module Tscanf2_io.

   Usage: test_master <slave_name> *)

open Tscanf2_io;;

let slave = Sys.argv.(1);;
let ic, oc = Unix.open_process slave;;
let ib = Scanf.Scanning.from_channel ic;;
let ob = Buffer.create 1024;;

let send_string_ping ob = send_string ob oc " Ping";;
let send_string_stop ob = send_string ob oc "stop";;

let interact i =
  Printf.eprintf " Ping"; flush stderr;
  send_string_ping ob;
  let s = receive_string ib in
  if s <> "-pong" then failwith ("Master: unbound string " ^ s)
;;

begin
(*
  Random.self_init ();
  let n = max (Random.int 8) 1 in
*)
  let n = 8 in
  let rec loop i =
    if i > 0 then (interact i; loop (i - 1)) in
  loop n
end
;;

begin
  send_string_stop ob;
  let ack = receive_string ib in
  if ack = "OK, bye!"
  then (print_endline "Test OK."; exit 0)
  else (print_endline "Test Failed!"; exit 2)
end
;;
