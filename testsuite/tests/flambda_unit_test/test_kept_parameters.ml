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

let func_rec_1 =
  let fun_f =
    f, [x],
    fapply (fvar f) [fvar x]
  in
  fun_decls' [fun_f]

let func_rec_2 =
  let fun_f =
    f, [x],
    fapply (fvar f) [int 1] (* x not kept *)
  in
  fun_decls' [fun_f]

let func_rec_3 =
  let fun_f =
    f, [x; y],
    fapply (fvar f) [fvar x] (* y not kept: f partially applied *)
  in
  fun_decls' [fun_f]

let func_rec_4 =
  let fun_f =
    f, [x; y],
    fapply (fvar f) [fvar x; fvar y] (* x and y kept *)
  in
  fun_decls' [fun_f]

let func_rec_5 =
  let fun_f =
    f, [x; y],
    fapply (fvar f) [fadd (fvar x) (fvar y); fvar y] (* x not kept *)
  in
  fun_decls' [fun_f]

let func_rec_6 =
  let fun_f =
    f, [x; y],
    fvar f (* f escape *)
  in
  fun_decls' [fun_f]

let run () =
  let test f l =
    VarSet.equal (Flambdaiter.arguments_kept_in_recursion f) (VarSet.of_list l) in
  assert( test func_non_rec_1 [x] );
  assert( test func_rec_1 [x] );
  assert( test func_rec_2 [] );
  assert( test func_rec_3 [x] );
  assert( test func_rec_4 [x;y] );
  assert( test func_rec_5 [y] );
  assert( test func_rec_6 [] );
  ()
