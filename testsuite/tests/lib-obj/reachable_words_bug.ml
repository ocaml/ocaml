(* TEST
*)

let _ =
  (* In 4.13 this causes Obj.reachable_words to segfault
     because of a missing initialization in caml_obj_reachable_words *)
  ignore (Marshal.(to_string 123 [No_sharing]));
  let n = Obj.reachable_words (Obj.repr (Array.init 10 (fun i -> i))) in
  assert (n = 11)
