(* TEST *)

(* [Bytes.extend] must only fail when the result length is out of range,
   not when an intermediate sum overflows. *)

let show name f =
  match f () with
  | b -> Printf.printf "%s: length %d\n" name (Bytes.length b)
  | exception Invalid_argument msg -> Printf.printf "%s: Invalid_argument %S\n" name msg

let () =
  let abc = Bytes.of_string "abc" in
  show "extend abc max_int min_int" (fun () -> Bytes.extend abc max_int min_int);
  show "extend abc min_int max_int" (fun () -> Bytes.extend abc min_int max_int);
  show "extend abc max_int (-max_int)" (fun () -> Bytes.extend abc max_int (-max_int));
  show "extend abc max_int max_int" (fun () -> Bytes.extend abc max_int max_int);
  show "extend abc min_int min_int" (fun () -> Bytes.extend abc min_int min_int);
  show "extend abc max_int 0" (fun () -> Bytes.extend abc max_int 0);
  show "extend abc min_int 0" (fun () -> Bytes.extend abc min_int 0)
