(* TEST
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   flags = "-g -dno-unique-ids -dno-locations -dtypedtree";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/test_quantification.reference";
   check-ocamlc.byte-output;
 }
*)

let x = [] in x;;

match [] with x -> x;;

let f x = let y = x,[] in y;;

class c = let x = [] in object method private x = x end;;


(* From CFML (suggested by Arthur) *)

let let_poly_p0 () =
   let x = (None = None) in ()

let let_poly_p1 () =
   let f x = x in
   let _r = f None in
   ()

let let_poly_p2 () =
   let f x = x in
   let _r =
      let _s = f None in ()
      in
   ()

let let_poly_v1 () =
  []

let let_poly_v2 () =
   let y = [] in
   y

let let_poly_k1 () =
  ref []

let let_poly_k2 () =
   let _x = ref [] in
   ()

let let_poly_k3 () =
  let r = ref [] in
  !r

let let_poly_k4 () =
  let f () = ref [] in
  f()

let let_poly_k5 () =
  let g =
     let f () = ref [] in
     f in
  g

let let_poly_k6 () =
  let r =
    let x = ref [] in
    [] in
  r
