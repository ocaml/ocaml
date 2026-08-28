(* TEST
*)

[@@@warning "-unerasable-optional-argument"]
let foo ?a =
    print_endline "a parameter";
    fun ~b ->
    print_endline "b parameter";
    fun ~c ->
    print_endline "c parameter"

let f = foo ~a:(print_endline "a argument") ~c:(print_endline "c argument")

let _ = print_endline "f defined"

let _ = f ~b:(print_endline "b argument")

let f ~a ~b ~c ~d ~e ~f = ()
let _ =
  ((f ~e:(Printf.printf "E\n") ~d:(Printf.printf "D\n"))
     ~c:(Printf.printf "C\n") ~b:(Printf.printf "B\n"))
  ~a:(Printf.printf "A\n") ~f:(Printf.printf "F\n")

let () = Printf.printf "function eager\n"

let eager ?(x=()) =
  Printf.printf "x argument\n";
  fun ?(y=()) ~a:() ~b:() -> ()

let _x = (eager ~x:()) ~b:()

let () = Printf.printf "function delay\n"
let delay =
  fun ?(a=()) -> Printf.printf "param a\n";
  fun ~b:() -> Printf.printf "param b\n";
    fun ?(c=()) -> Printf.printf "param c\n";
      fun ?(d=()) -> Printf.printf "param d\n";
       fun () -> Printf.printf "end\n"

let () = Printf.printf "function f\n"
let f = (delay ~b:()) ~d:() ~a:()

let g = Printf.printf "function g\n"; f ()
