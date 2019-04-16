(* TEST *)

open Set.Make(String)

let e = empty

open struct
  let x = singleton "hidden"
end

let () = iter print_endline (union x (of_list ["a"; "b"]))

let f =
  let open Set.Make(Int32) in
  let e2 = empty in
  let open struct
    let y = 3
  end in
  (e, e2, y)

module type S = sig
  open Set.Make(Bool)

  type nonrec t = t
end

let hd _ = ()

open (List : sig val map : ('a -> 'b) -> 'a list -> 'b list end)

let l =
  hd (map succ [0; 1; 2; 3])

let y = map succ []
