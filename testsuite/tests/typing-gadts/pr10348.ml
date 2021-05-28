(* TEST
   * expect
*)


type (_, _) eq = Refl : ('a, 'a) eq;;

let foo (type a) (x : (a, int) eq) (a : a) p =
  let module M = struct type t = a let x : t = a end in
  if p then a else M.x;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
val foo : ('a, int) eq -> 'a -> bool -> 'a = <fun>
|}]

let foo (type a) (x : (a, int) eq) (a : a) p =
  let module M = struct type t = a let x : t = a end in
  let Refl = x in
  if p then a else M.x;;
[%%expect{|
val foo : ('a, int) eq -> 'a -> bool -> 'a = <fun>
|}]

let foo (type a) (x : (a, int) eq) (a : a) p =
  let module M = struct type 'a t = a let x : 'a t = a end in
  let Refl = x in
  if p then a else M.x;;
[%%expect{|
val foo : ('a, int) eq -> 'a -> bool -> 'a = <fun>
|}]

let foo (type a) (x : (a, int) eq) (a : a) p =
  let module M = struct type 'a t = 'a let x : a t = a end in
  let Refl = x in
  if p then a else M.x;;
[%%expect{|
val foo : ('a, int) eq -> 'a -> bool -> 'a = <fun>
|}]

type ('a,'b) fst = 'a

let foo4 (type a) (x : (a, int) eq) (a : a) p =
  let module M : sig type t = (a,bool) fst val x : t end =
    struct type t = (a,bool) fst let x = a end in
  let Refl = x in
  if p then a else M.x;;
[%%expect{|
type ('a, 'b) fst = 'a
val foo4 : ('a, int) eq -> 'a -> bool -> 'a = <fun>
|}]
