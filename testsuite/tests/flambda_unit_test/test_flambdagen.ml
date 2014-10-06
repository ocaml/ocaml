open Asttypes
open Lambda
open Test_utils

let lx = Ident.create "x"
let fx = new_var "x"

let test_equal l f =
  let _, fl =
    Flambdagen.intro
      ~current_compilation_unit:compilation_unit
      ~current_unit_id:(Ident.create_persistent compilation_unit_id)
      ~symbol_for_global':(fun _ -> assert false) l in
  equal f fl

let l1,t1 =
  Lconst (Const_base (Const_int 1)),
  int 1

let l2,t2 =
  Llet(Strict,lx,l1,Lvar lx),
  flet fx t1 (fvar fx)

let run () =
  assert( test_equal l1 t1 );
  assert( test_equal l2 t2 );
  ()
