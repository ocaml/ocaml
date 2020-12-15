let _ = List.map (fun x -> x + 1) [2.0; 3.0]    
(* should have been [+.] instead of [+], or
   should have been [2;3] instead of [2.0;3.0] *)