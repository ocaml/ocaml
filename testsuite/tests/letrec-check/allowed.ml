(* TEST
*)

let rec x = (x; ());;

let rec x = let x = () in x;;

let rec x = [y]
and y = let x = () in x;;

let rec x = [y]
and y = let rec x = () in x;;

let rec x =
  let a = x in
  fun () -> a ()
and y =
  [x];;

let rec x = let module M = struct let f = x end in ();;

module type T = sig val y: int end

let rec x = let module M =
            struct
              module N =
              struct
                let y = x
              end
            end
  in fun () -> ignore (M.N.y ());;

let rec x = "x";;

class c = object end
let rec x = fun () -> new c;;

let rec x = (y, y)
and y = fun () -> ignore x;;

let rec x = Some y
and y = fun () -> ignore x
;;

let rec x = `A y
and y = fun () -> ignore x
;;

let rec x = { contents = y }
and y = fun () -> ignore x;;

let r = ref (fun () -> ())
let rec x = fun () -> r := x;;

let rec x = fun () -> y.contents and y = { contents = 3 };;

let rec x = function
    Some _ -> ignore (y [])
  | None -> ignore (y [])
and y = function
    [] -> ignore (x None)
  | _ :: _ -> ignore (x None)
    ;;

let rec x = lazy (Lazy.force x + Lazy.force x)
  ;;

let rec x = { x with contents = 3 }  [@ocaml.warning "-23"];;

let rec x = let y = (x; ()) in y;;

let rec x = [|y|] and y = 0;;

(* Recursively constructing arrays of known non-float type is permitted *)
let rec deep_cycle : [`Tuple of [`Shared of 'a] array] as 'a
  = `Tuple [| `Shared deep_cycle |];;

(* Constructing float arrays was disallowed altogether at one point
   by an overzealous check.  Constructing float arrays in recursive
   bindings is fine when they don't partake in the recursion. *)
let rec _x = let _ = [| 1.0 |] in 1. in ();;

(* This test is not allowed if 'a' is unboxed, but should be accepted
   as written *)
type a = {a: b}
and b = X of a | Y

let rec a =
  {a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;

(* This test is not allowed if 'c' is unboxed, but should be accepted
   as written *)
type d = D of e
and e = V of d | W;;

let rec d =
  D
    (if Sys.opaque_identity true then
       V d
     else
       W);;

type r = R of r list [@@unboxed];;
let rec a = R [a];;
