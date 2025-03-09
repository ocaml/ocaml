(* TEST
 native;
*)
type alloc_count = { mutable total: float }
let allocs = Sys.opaque_identity { total = 0. }
let[@inline never] set_allocs () =
  allocs.total <- Gc.minor_words ()

let[@inline never] count txt =
  let now = int_of_float (Gc.minor_words () -. allocs.total) in
  Printf.printf "%20s: %d\n" txt now;
  set_allocs ()

let v = Sys.opaque_identity (ref 0)

let next () =
  let r = !v in incr v; r

let () = set_allocs ()

include struct
  let x = next ()
  let y = next ()
end

let () = count "no signature"

include (struct
  let a = next ()
  let b = next ()
end : sig val a : int val b : int end)

let () = count "trivial coercion"

include (struct
  let c = next ()
  let d = next ()
end : sig val c : int end)

let () = count "prefix coercion"

include (struct
  let c = next ()
  let d = next ()
end : sig val d : int end)

let () = count "reordering coercion"

module Outer = struct
  include (struct
    module Inner = struct
      include (struct
        let e = next ()
      end)
    end
  end)
end

let () =
  (* The above might actually allocate the module blocks Outer and Inner,
     but should not allocate more than 4 words (2 each) *)
  assert (Gc.minor_words () -. allocs.total <= 4.)

let () =
  Printf.printf "%20s: %d%d%d%d%d%d%d\n" "outputs" x y a b c d Outer.Inner.e
