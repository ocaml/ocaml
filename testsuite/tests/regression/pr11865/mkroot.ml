(* TEST
   modules = "mkroot_stubs.c";
 *)

type 'a box
external box_make : 'a -> 'a box = "box_make"
external box_deref : 'a box -> 'a = "box_deref"

type chain =
  | Nil
  | Cons of chain box

let rec make_chain = function
  | 0 -> Nil
  | n -> Cons (box_make (make_chain (n-1)))

let rec check_chain len = function
  | Nil -> assert (len = 0)
  | Cons ch -> check_chain (len - 1) (box_deref ch)

let () =
  let len = 1000 in
  for _i = 1 to 5 do
    let ch = make_chain len in
    for _j = 1 to 5 do
      Gc.compact ();
      check_chain len ch
    done
  done;
  print_endline "ok"
