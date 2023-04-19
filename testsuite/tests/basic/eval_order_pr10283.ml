(* TEST *)

(* Slightly modified version of an example from github user @Ngoguey42,
   submitted as issue number 10283. *)

let[@inline never][@local never] g () =
  let[@local always] f a b = Printf.printf "%d %d\n" a b in

  let i = ref 0 in
  f (incr i; !i) (incr i; !i)

let () = g ()
