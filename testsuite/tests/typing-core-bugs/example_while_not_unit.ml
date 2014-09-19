let x = ref 1
let y = ref 2
let _ =
   while x <> y do  (* intended !x <> ! y *)
      x = y         (* intended x := y *)
   done
