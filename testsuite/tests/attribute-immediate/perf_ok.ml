(* Verifies ascribing to immediate type in signature works, also tests performance *)

module Foo : sig type t val x : t ref end = struct
  type t = int
  let x = ref 0
end

module Bar : sig type t [@@immediate] val x : t ref end = struct
  type t = int
  let x = ref 0
end

let test f =
  let start = Sys.time() in f ();
  (Sys.time() -. start)

let test_foo () =
  for i = 0 to 100_000_000 do
    Foo.x := !Foo.x
  done

let test_bar () =
  for i = 0 to 100_000_000 do
    Bar.x := !Bar.x
  done

(* Should see substantial speedup! *)
let () = Printf.printf "No @@immediate: %fs\n" (test test_foo)
let () = Printf.printf "With @@immediate: %fs\n" (test test_bar)
