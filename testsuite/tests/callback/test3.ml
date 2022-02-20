(* TEST
   include unix
   modules = "test3_.c"
   * libunix
   ** bytecode
   ** native
*)

(* Tests nested calls from C (main C) to OCaml (main OCaml) to C (caml_to_c) to
 * OCaml (c_to_caml) to C (printf functions). A stack overflow and a heap
 * overflow are triggered in c_to_caml. *)

let printf = Printf.printf

let rec mk_list length acc =
  if length < 1 then acc
  else mk_list (length-1) ((length-1)::acc)

let rec sum n = if n = 0 then 0 else n + sum (n-1)

let c_to_caml () =
  printf "[Caml] Enter c_to_caml\n%!";
  (* Heap overflow *)
  let l = mk_list 1000 [] in
  Printf.printf "%d\n" (List.hd l);
  (* Stack overflow *)
  let s = sum 100000 in
  Printf.printf "%s\n"
    (if s = Int64.to_int 5000050000L then "ok" else "error");
  printf "[Caml] Leave c_to_caml\n%!"

let _ = Callback.register "c_to_caml" c_to_caml

external caml_to_c : unit -> unit = "caml_to_c"

let _ =
    printf "[Caml] Call caml_to_c\n%!";
    caml_to_c ();
    printf "[Caml] Return from caml_to_c\n%!"
