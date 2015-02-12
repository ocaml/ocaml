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

let func_multi_non_rec_1 =
  let fun_f =
    f, [x],
    fvar x
  in
  let fun_g =
    g, [y],
    fvar y
  in
  fun_decls' [fun_f; fun_g]

let func_multi_rec_1 =
  let fun_f =
    f, [x],
    fapply (fvar g) [fvar x] (* x -> y *)
  in
  let fun_g =
    g, [y],
    fapply (fvar f) [fvar y] (* y -> x *)
  in
  fun_decls' [fun_f; fun_g]

let func_multi_rec_2 =
  let fun_f =
    f, [x],
    fapply (fvar g) [fvar x] (* x -> y, other -> z *)
  in
  let fun_g =
    g, [y; z],
    fapply (fvar f) [fvar y] (* y -> x *)
  in
  fun_decls' [fun_f; fun_g]

let func_multi_rec_3 =
  let fun_f =
    f, [x; y],
    fapply (fvar g) [fvar x; fvar y] (* x -> z, y -> w *)
  in
  let fun_g =
    g, [z; w],
    fapply (fvar f) [fvar w; fvar w] (* w -> x, y *)
  in
  fun_decls' [fun_f; fun_g]

let func_multi_rec_4 =
  let fun_f =
    f, [x; y],
    fapply (fvar g) [fvar x; fvar g] (* x -> z, g escape *)
  in
  let fun_g =
    g, [z; w],
    fapply (fvar f) [fvar z; fvar w] (* z -> x, w -> y *)
  in
  fun_decls' [fun_f; fun_g]

let run () =
  let test f l =
    let b = Variable.Set.equal (Flambdaiter.arguments_kept_in_recursion f) (Variable.Set.of_list l) in
    if not b then
      Format.eprintf "kept: %a@." Variable.Set.print (Flambdaiter.arguments_kept_in_recursion f);
    b
  in
  assert( test func_non_rec_1 [x] );
  assert( test func_rec_1 [x] );
  assert( test func_rec_2 [] );
  assert( test func_rec_3 [x] );
  assert( test func_rec_4 [x;y] );
  assert( test func_rec_5 [y] );
  assert( test func_rec_6 [] );
  assert( test func_multi_non_rec_1 [x;y] );
  assert( test func_multi_rec_1 [x;y] );
  assert( test func_multi_rec_2 [x;y] );
  assert( test func_multi_rec_3 [y;w] );
  assert( test func_multi_rec_4 [] );
  ()
