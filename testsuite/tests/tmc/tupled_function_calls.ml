(* TEST
   * bytecode
   * native
*)

(* this works as expected *)
let[@tail_mod_cons] rec tupled_map (f, li) =
  match li with
  | [] -> []
  | x :: xs -> f x :: tupled_map (f, xs)

(* The recursive call here is not "direct" for the
   Tupled calling convention (which is only used by the native compiler),
   so it will not be eligible for TMC optimization.
   We expect a warning here, when compiling with the native compiler. *)
let[@tail_mod_cons] rec tupled_map_not_direct (f, li) =
  match li with
  | [] -> []
  | x :: xs ->
      let pair = (f, xs) in
      f x :: (tupled_map_not_direct[@tailcall true]) pair
