(* TEST
  modules = "domain_name_and_assign_thread_names_cstubs.c"
  ocamlrunparam += ",T=1"
* hasunix
include unix
** bytecode
** native
*)

open Domain

external thread_getname : unit -> string = "thread_getname"

let check_get_name () =
  let domain_name = Domain.get_name () in
  let thread_name = thread_getname () in
  if compare domain_name thread_name == 0 then
    (Printf.printf "ok\n")
  else
    (Printf.printf "%s != %s\n" thread_name domain_name)


let spawn_and_print () =
  check_get_name ();
  let d = Domain.spawn check_get_name in
  join d

let set_name_and_spawn () =
  check_get_name ();
  Domain.set_name "foo";
  check_get_name ();
  let d = Domain.spawn check_get_name in
  join d

let set_name_and_double_spawn () =
  Domain.set_name "bar";
  check_get_name ();
  let d = Domain.spawn spawn_and_print in
  join d

let () =
  spawn_and_print ();

  let d = Domain.spawn set_name_and_spawn in
  join d;

  let d = Domain.spawn set_name_and_double_spawn in
  join d
