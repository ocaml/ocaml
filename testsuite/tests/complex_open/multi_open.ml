let sp = Printf.sprintf
let not s  = sp "Module %s was not opened" s
let right s = sp "Module %s correctly opened" s
let nested name = sp "Nested module %s correctly opened" name
			
let x = not "M"
and y = not "N"
and z = not "L";;

module M = struct
    let x = right "M"
    module N = struct let y= nested "M.N" end
  end;;
	
module N = struct
    let y = right "N"
    module L = struct let z= nested "N.L" end
  end;;
	
module L = struct
    let z = right "L"
    module M = struct let x= nested "L.M" end
  end;;

let pp = Printf.printf
let test global l=
  pp "%s open:\n" global;
  List.iter (pp "\t%s\n") l;;

let () =
  let open M[@local][@attribute] and N and L [@attribute] in
  test "Local" [x;y;z];;

(* Testing for shadowing warning *)
[@@@ocaml.warning "+44" ]

let which_x =
  let x = not "M" in
  pp "%s\n" x;
  let open M and N in (* x is shadowed here *)
  x;;

let which_x =
  let x = not "M" in
  pp "%s\n" x;
  let open! M and N in (* x is shadowed here but the warning is silenced *)
  x;;

open! M[@one_attribute][@another] and N and L[@local] [@@item]
let () =
  test "Global" [x;y;z];;

open! M and N and L
let () =
  test "Nested global" [x;y;z];;


