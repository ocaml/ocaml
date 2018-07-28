(* TEST
   * expect
*)

let rec x = let y = () in x;;
[%%expect{|
Line 1, characters 12-27:
  let rec x = let y = () in x;;
              ^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = let module M = struct let f = x let g = x () end in fun () -> ();;
[%%expect{|
Line 1, characters 12-76:
  let rec x = let module M = struct let f = x let g = x () end in fun () -> ();;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = let module M = struct let f = x () let g = x end in fun () -> ();;
[%%expect{|
Line 1, characters 12-76:
  let rec x = let module M = struct let f = x () let g = x end in fun () -> ();;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = (let module M = struct let f = y 0 let g = () end in fun () -> ())
    and y = succ;;
[%%expect{|
Line 1, characters 12-78:
  let rec x = (let module M = struct let f = y 0 let g = () end in fun () -> ())
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = let module M = struct module N = struct let y = x end end in M.N.y;;
[%%expect{|
Line 1, characters 12-78:
  let rec x = let module M = struct module N = struct let y = x end end in M.N.y;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = let module M = struct let f = x () and g = x end in fun () -> ();;
[%%expect{|
Line 1, characters 12-76:
  let rec x = let module M = struct let f = x () and g = x end in fun () -> ();;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

class c _ = object end
let rec x = new c x;;
[%%expect{|
class c : 'a -> object  end
Line 2, characters 12-19:
  let rec x = new c x;;
              ^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = ignore x;;
[%%expect{|
Line 1, characters 12-20:
  let rec x = ignore x;;
              ^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = y 0 and y _ = ();;
[%%expect{|
Line 1, characters 12-15:
  let rec x = y 0 and y _ = ();;
              ^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec c = { c with Complex.re = 1.0 };;
[%%expect{|
Line 1, characters 12-39:
  let rec c = { c with Complex.re = 1.0 };;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec b = if b then true else false;;
[%%expect{|
Line 1, characters 12-37:
  let rec b = if b then true else false;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let r = ref ()
let rec x = r := x;;
[%%expect{|
val r : unit ref = {contents = ()}
Line 2, characters 12-18:
  let rec x = r := x;;
              ^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  for i = 0 to 1 do
    let z = y in ignore z
  done
and y = x; ();;
[%%expect{|
Line 2, characters 2-52:
  ..for i = 0 to 1 do
      let z = y in ignore z
    done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  for i = 0 to y do
    ()
  done
and y = 10;;
[%%expect{|
Line 2, characters 2-33:
  ..for i = 0 to y do
      ()
    done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  for i = y to 10 do
    ()
  done
and y = 0;;
[%%expect{|
Line 2, characters 2-34:
  ..for i = y to 10 do
      ()
    done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  while false do
    let y = x in ignore y
  done
and y = x; ();;
[%%expect{|
Line 2, characters 2-49:
  ..while false do
      let y = x in ignore y
    done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  while y do
    ()
  done
and y = false;;
[%%expect{|
Line 2, characters 2-26:
  ..while y do
      ()
    done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  while y do
    let y = x in ignore y
  done
and y = false;;
[%%expect{|
Line 2, characters 2-45:
  ..while y do
      let y = x in ignore y
    done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = y#m and y = object method m = () end;;
[%%expect{|
Line 1, characters 12-15:
  let rec x = y#m and y = object method m = () end;;
              ^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = (object method m _ = () end)#m x;;
[%%expect{|
Line 1, characters 12-44:
  let rec x = (object method m _ = () end)#m x;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = y.contents and y = { contents = 3 };;
[%%expect{|
Line 1, characters 12-22:
  let rec x = y.contents and y = { contents = 3 };;
              ^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = object val mutable v = 0 method m = v <- y end and y = 1;;
[%%expect{|
Line 1, characters 12-58:
  let rec x = object val mutable v = 0 method m = v <- y end and y = 1;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = assert y and y = true;;
[%%expect{|
Line 1, characters 12-20:
  let rec x = assert y and y = true;;
              ^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = object method m = x end;;
[%%expect{|
Line 1, characters 12-35:
  let rec x = object method m = x end;;
              ^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = object method m = ignore x end;;
[%%expect{|
Line 1, characters 12-42:
  let rec x = object method m = ignore x end;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* The builtin Pervasives.ref is currently treated as a constructor.
   Other functions of the same name should not be so treated. *)
let _ =
  let module Pervasives =
  struct
    let ref _ = assert false
  end in
  let rec x = Pervasives.ref y
  and y = fun () -> ignore x
  in (x, y)
;;
[%%expect{|
Line 6, characters 14-30:
    let rec x = Pervasives.ref y
                ^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* An example, from Leo White, of let rec bindings that allocate
   values of unknown size *)
let foo p x =
  let rec f =
    if p then (fun y -> x + g y) else (fun y -> g y)
  and g =
    if not p then (fun y -> x - f y) else (fun y -> f y)
  in
  (f, g)
;;
[%%expect{|
Line 3, characters 4-52:
      if p then (fun y -> x + g y) else (fun y -> g y)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

module type T = sig end
let rec x = (module (val y : T) : T)
and y = let module M = struct let x = x end in (module M : T)
;;
[%%expect{|
module type T = sig  end
Line 2, characters 12-36:
  let rec x = (module (val y : T) : T)
              ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  match let _ = y in raise Not_found with
    _ -> "x"
  | exception Not_found -> "z"
and y = match x with
  z -> ("y", z);;
[%%expect{|
Line 2, characters 2-85:
  ..match let _ = y in raise Not_found with
      _ -> "x"
    | exception Not_found -> "z"
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;
