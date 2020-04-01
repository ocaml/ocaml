(* TEST
   * expect
*)

let rec x = let module M = struct let f = x end in ();;
[%%expect{|
val x : unit = ()
|}];;

let rec x = let module M = struct let f = x let g = x () end in fun () -> ();;
[%%expect{|
Line 1, characters 12-76:
1 | let rec x = let module M = struct let f = x let g = x () end in fun () -> ();;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = let module _ = struct let _ = x () end in fun () -> ();;
[%%expect{|
Line 1, characters 12-66:
1 | let rec x = let module _ = struct let _ = x () end in fun () -> ();;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = let module M = struct let f = x () let g = x end in fun () -> ();;
[%%expect{|
Line 1, characters 12-76:
1 | let rec x = let module M = struct let f = x () let g = x end in fun () -> ();;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = (let module M = struct let f = y 0 let g = () end in fun () -> ())
    and y = succ;;
[%%expect{|
Line 1, characters 12-78:
1 | let rec x = (let module M = struct let f = y 0 let g = () end in fun () -> ())
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  let module M = struct
    module N = struct let y = x end
  end in M.N.y;;
[%%expect{|
Lines 2-4, characters 2-14:
2 | ..let module M = struct
3 |     module N = struct let y = x end
4 |   end in M.N.y..
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

module type T = sig val y: int end

let rec x = let module M =
            struct
              module N =
              struct
                let y = x
              end
            end
  in fun () -> ignore (M.N.y ());;
[%%expect{|
module type T = sig val y : int end
val x : unit -> unit = <fun>
|}];;

let rec x = let module M = struct let f = x () and g = x end in fun () -> ();;
[%%expect{|
Line 1, characters 12-76:
1 | let rec x = let module M = struct let f = x () and g = x end in fun () -> ();;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

module type T = sig end
let rec x = (module (val y : T) : T)
and y = let module M = struct let x = x end in (module M : T)
;;
[%%expect{|
module type T = sig end
Line 2, characters 12-36:
2 | let rec x = (module (val y : T) : T)
                ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* module constraints *)
module type S = sig               val y : float end;;
module type T = sig val x : float val y : float end;;
type t = T : (module S) -> t;;

let rec x = let module M = (val m) in T (module M)
and (m : (module T)) = (module (struct let x = 10.0 and y = 20.0 end) : T);;
[%%expect{|
module type S = sig val y : float end
module type T = sig val x : float val y : float end
type t = T : (module S) -> t
Line 5, characters 12-50:
5 | let rec x = let module M = (val m) in T (module M)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;
