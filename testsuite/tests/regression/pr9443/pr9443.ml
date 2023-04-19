(* TEST *)

(* Test tail call optimisation with an elided mutable cell *)
let rec loop n =
  if n = 0 then () else begin
    let last = ref 0 in
    last := 0;
    loop (n-1)
  end

let () = loop 1_000_000
