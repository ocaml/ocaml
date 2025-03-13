(* TEST *)

(* In some situations, these code snippets would cause the
   let-rec size computation to raise a fatal error. *)

(* *)
type t =
  | C of t Lazy.t Lazy.t

let rec x =
  let y = (lazy (C x)) in
  lazy y

(* *)
let todo () : float =
  let rec l =
    let x = lazy (Lazy.force (Lazy.force l)) in
    lazy x
  in
  Lazy.force (Lazy.force l)

(* *)
let () =
  begin match Lazy.force (Lazy.force x) with
    | C _ -> ()
  end;
  begin match todo () with
    | f ->
      (* trying to get the actual value of the float will cause a segfault *)
      let f' = f +. 1. in
      print_float f';
      print_newline ()
    | exception CamlinternalLazy.Undefined ->
      (* this is what should happen if `todo` is compiled correctly *)
      ()
  end;
