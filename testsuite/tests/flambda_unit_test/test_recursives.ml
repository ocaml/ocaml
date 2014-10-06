open Symbol
open Abstract_identifiers
open Flambda
open Test_utils

let f = new_var "f"
let g = new_var "g"
let x = new_var "x"
let y = new_var "y"

let func_non_rec_1 =
  let fun_f =
    f, [x],
    fvar x
  in
  fun_decls' [fun_f]

let func_non_rec_2 =
  let fun_f =
    f, [x],
    fvar x
  in
  let fun_g =
    g, [y],
    fapply (fvar f) [fvar y]
  in
  fun_decls' [fun_f; fun_g]

let func_rec_1 =
  let fun_f =
    f, [x],
    fapply (fvar f) [fvar x]
  in
  fun_decls' [fun_f]

let func_rec_2 =
  let fun_f =
    f, [x],
    fapply (fvar f) [fvar x]
  in
  let fun_g =
    g, [y],
    fapply (fvar f) [fvar y]
  in
  fun_decls' [fun_f; fun_g]

let func_rec_3 =
  let fun_f =
    f, [x],
    fapply (fvar g) [fvar x]
  in
  let fun_g =
    g, [y],
    fapply (fvar f) [fvar y]
  in
  fun_decls' [fun_f; fun_g]

let run () =
  let test f l = VarSet.equal (recursive_functions f) (VarSet.of_list l) in
  assert( test func_non_rec_1 [] );
  assert( test func_non_rec_2 [] );
  assert( test func_rec_1 [f] );
  assert( test func_rec_2 [f] );
  assert( test func_rec_3 [f;g] );
  ()
