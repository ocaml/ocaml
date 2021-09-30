(* TEST
   * expect
*)

let id x = x

let apply x f = f x

let pair x y = x, y

module Id = struct

  let (let+) = apply

  let (and+) = pair

end;;
[%%expect{|
val id : 'a -> 'a = <fun>
val apply : 'a -> ('a -> 'b) -> 'b = <fun>
val pair : 'a -> 'b -> 'a * 'b = <fun>
module Id :
  sig
    val ( let+ ) : 'a -> ('a -> 'b) -> 'b
    val ( and+ ) : 'a -> 'b -> 'a * 'b
  end
|}];;

let res =
  Id.(
    let+ x = 1
    and+ y = 2
    and+ z = 3 in
    [x; y; z]
  );;
[%%expect{|
val res : int list = [1; 2; 3]
|}];;

let res2 =
  Id.(
    let+ x = 1 in
    x + 2
  );;
[%%expect{|
val res2 : int = 3
|}];;


module List = struct

  let map l f = List.map f l

  let concat_map l f =
    let l = List.map f l in
    List.concat l

  let product xs ys =
    List.fold_right
      (fun x acc -> (List.map (fun y -> (x, y)) ys) @ acc)
      xs []

  let (let+) = map

  let (and+) = product

  let ( let* ) = concat_map

  let ( and* ) = product

end;;
[%%expect{|
module List :
  sig
    val map : 'a list -> ('a -> 'b) -> 'b list
    val concat_map : 'a list -> ('a -> 'b list) -> 'b list
    val product : 'a list -> 'b list -> ('a * 'b) list
    val ( let+ ) : 'a list -> ('a -> 'b) -> 'b list
    val ( and+ ) : 'a list -> 'b list -> ('a * 'b) list
    val ( let* ) : 'a list -> ('a -> 'b list) -> 'b list
    val ( and* ) : 'a list -> 'b list -> ('a * 'b) list
  end
|}];;

let map =
  List.(
    let+ x = [1; 2; 3] in
    x + 1
  );;
[%%expect{|
val map : int list = [2; 3; 4]
|}];;

let map_and =
  List.(
    let+ x = [1; 2; 3]
    and+ y = [7; 8; 9] in
    x + y
  );;
[%%expect{|
val map_and : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let bind =
  List.(
    let* x = [1; 2; 3] in
    let* y = [7; 8; 9] in
    [x + y]
  );;
[%%expect{|
val bind : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let bind_and =
  List.(
    let* x = [1; 2; 3]
    and* y = [7; 8; 9] in
    [x + y]
  );;
[%%expect{|
val bind_and : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let bind_map =
  List.(
    let* x = [1; 2; 3] in
    let+ y = [7; 8; 9] in
    x + y
  );;
[%%expect{|
val bind_map : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

module Let_unbound = struct
end;;
[%%expect{|
module Let_unbound : sig end
|}];;

let let_unbound =
  Let_unbound.(
    let+ x = 1 in
    x + y
  );;
[%%expect{|
Line 3, characters 4-8:
3 |     let+ x = 1 in
        ^^^^
Error: Unbound value let+
|}];;

module And_unbound = struct
  let (let+) = Id.(let+)
end;;
[%%expect{|
module And_unbound : sig val ( let+ ) : 'a -> ('a -> 'b) -> 'b end
|}];;

let and_unbound =
  And_unbound.(
    let+ x = 1
    and+ y = 2 in
    x + y
  );;
[%%expect{|
Line 4, characters 4-8:
4 |     and+ y = 2 in
        ^^^^
Error: Unbound value and+
|}];;

module Ill_typed_1 = struct

  let (let+) = fun x f -> f (not x)

end;;
[%%expect{|
module Ill_typed_1 : sig val ( let+ ) : bool -> (bool -> 'a) -> 'a end
|}];;

let ill_typed_1 =
  Ill_typed_1.(
    let+ x = 1 in
    x + y
  );;
[%%expect{|
Line 3, characters 13-14:
3 |     let+ x = 1 in
                 ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

module Ill_typed_2 = struct

  let (let+) = apply
  let (and+) = fun x y -> x +. y, x -. y

end;;
[%%expect{|
module Ill_typed_2 :
  sig
    val ( let+ ) : 'a -> ('a -> 'b) -> 'b
    val ( and+ ) : float -> float -> float * float
  end
|}];;

let ill_typed_2 =
  Ill_typed_2.(
    let+ x = 1
    and+ y = 2 in
    x + y
  );;
[%%expect{|
Line 3, characters 13-14:
3 |     let+ x = 1
                 ^
Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `1.'?
|}];;

module Ill_typed_3 = struct

  let (let+) = 7

end;;
[%%expect{|
module Ill_typed_3 : sig val ( let+ ) : int end
|}];;

let ill_typed_3 =
  Ill_typed_3.(
    let+ x = 1 in
    x + y
  );;
[%%expect{|
Line 3, characters 4-8:
3 |     let+ x = 1 in
        ^^^^
Error: The operator let+ has type int but it was expected to have type
         'a -> ('b -> 'c) -> 'd
|}];;

module Ill_typed_4 = struct

  let (let+) = apply
  let (and+) = not

end;;
[%%expect{|
module Ill_typed_4 :
  sig val ( let+ ) : 'a -> ('a -> 'b) -> 'b val ( and+ ) : bool -> bool end
|}];;

let ill_typed_4 =
  Ill_typed_4.(
    let+ x = 1
    and+ y = 2 in
    x + y
  );;
[%%expect{|
Line 4, characters 4-8:
4 |     and+ y = 2 in
        ^^^^
Error: The operator and+ has type bool -> bool
       but it was expected to have type bool -> 'a -> 'b
       Type bool is not compatible with type 'a -> 'b
|}];;

module Ill_typed_5 = struct

  let (let+) = (fun x f -> not x)
  let (and+) = pair

end;;
[%%expect{|
module Ill_typed_5 :
  sig
    val ( let+ ) : bool -> 'a -> bool
    val ( and+ ) : 'a -> 'b -> 'a * 'b
  end
|}];;

let ill_typed_5 =
  Ill_typed_5.(
    let+ x = 1
    and+ y = 2
    and+ z = 3 in
    x + y + z
  );;
[%%expect{|
Lines 3-5, characters 9-14:
3 | .........x = 1
4 |     and+ y = 2
5 |     and+ z = 3...
Error: These bindings have type (int * int) * int
       but bindings were expected of type bool
|}];;

module Ill_typed_6 = struct

  let (let+) = apply
  let (and+) = fun x y -> x + 1, y

end;;
[%%expect{|
module Ill_typed_6 :
  sig
    val ( let+ ) : 'a -> ('a -> 'b) -> 'b
    val ( and+ ) : int -> 'a -> int * 'a
  end
|}];;

let ill_typed_6 =
  Ill_typed_6.(
    let+ x = 1
    and+ y = 2
    and+ z = 3 in
    x + y + z
  );;
[%%expect{|
Lines 3-4, characters 9-14:
3 | .........x = 1
4 |     and+ y = 2
Error: These bindings have type int * int but bindings were expected of type
         int
|}];;


module Ill_typed_7 = struct

  let (let+) f x = f (x + 1)
  let (and+) = pair

end;;
[%%expect{|
module Ill_typed_7 :
  sig
    val ( let+ ) : (int -> 'a) -> int -> 'a
    val ( and+ ) : 'a -> 'b -> 'a * 'b
  end
|}];;

let ill_typed_7 =
  Ill_typed_7.(
    let+ x = 1
    and+ y = 2 in
    x + y
  );;
[%%expect{|
Line 3, characters 4-8:
3 |     let+ x = 1
        ^^^^
Error: The operator let+ has type (int -> 'a) -> int -> 'a
       but it was expected to have type (int -> 'a) -> ('b * 'c -> 'd) -> 'e
       Type int is not compatible with type 'b * 'c -> 'd
|}];;

module Indexed_monad = struct

  type opened = private Opened
  type closed = private Closed

  type (_, _, _) t =
    | Return : 'a -> ('s, 's, 'a) t
    | Map : ('s1, 's2, 'a) t * ('a -> 'b) -> ('s1, 's2, 'b) t
    | Both : ('s1, 's2, 'a) t * ('s2, 's3, 'b) t -> ('s1, 's3, 'a * 'b) t
    | Bind : ('s1, 's2, 'a) t * ('a -> ('s2, 's3, 'b) t) -> ('s1, 's3, 'b) t
    | Open : string -> (closed, opened, unit) t
    | Read : (opened, opened, string) t
    | Close : (opened, closed, unit) t

  let return x = Return x
  let map m f = Map(m, f)
  let both m1 m2 = Both(m1, m2)
  let bind m f = Bind(m, f)
  let open_ s = Open s
  let read = Read
  let close = Close

  type 'a state =
    | Opened : in_channel -> opened state
    | Closed : closed state

  let run (type a) (m : (closed, closed, a) t) : a =
    let rec loop : type a s1 s2. s1 state -> (s1, s2, a) t -> s2 state * a =
      fun state m ->
        match m, state with
        | Return x, _ -> state, x
        | Map(m, f), _ ->
            let state2, x = loop state m in
            state2, f x
        | Both(m1, m2), _ ->
            let state2, x = loop state m1 in
            let state3, y = loop state2 m2 in
            state3, (x, y)
        | Bind(m, f), _ ->
            let state2, x = loop state m in
            loop state2 (f x)
        | Open filename, Closed ->
            let ic = open_in filename in
            Opened ic, ()
        | Read, Opened ic ->
            let c = input_line ic in
            state, c
        | Close, Opened ic ->
            close_in ic;
            Closed, ()
    in
    let Closed, result = loop Closed m in
    result

  let ( let+ ) = map
  let ( and+ ) = both
  let ( let* ) = bind
  let ( and* ) = both

end;;
[%%expect {|
module Indexed_monad :
  sig
    type opened = private Opened
    type closed = private Closed
    type (_, _, _) t =
        Return : 'a -> ('s, 's, 'a) t
      | Map : ('s1, 's2, 'a) t * ('a -> 'b) -> ('s1, 's2, 'b) t
      | Both : ('s1, 's2, 'a) t * ('s2, 's3, 'b) t -> ('s1, 's3, 'a * 'b) t
      | Bind : ('s1, 's2, 'a) t *
          ('a -> ('s2, 's3, 'b) t) -> ('s1, 's3, 'b) t
      | Open : string -> (closed, opened, unit) t
      | Read : (opened, opened, string) t
      | Close : (opened, closed, unit) t
    val return : 'a -> ('b, 'b, 'a) t
    val map : ('a, 'b, 'c) t -> ('c -> 'd) -> ('a, 'b, 'd) t
    val both : ('a, 'b, 'c) t -> ('b, 'd, 'e) t -> ('a, 'd, 'c * 'e) t
    val bind : ('a, 'b, 'c) t -> ('c -> ('b, 'd, 'e) t) -> ('a, 'd, 'e) t
    val open_ : string -> (closed, opened, unit) t
    val read : (opened, opened, string) t
    val close : (opened, closed, unit) t
    type 'a state =
        Opened : in_channel -> opened state
      | Closed : closed state
    val run : (closed, closed, 'a) t -> 'a
    val ( let+ ) : ('a, 'b, 'c) t -> ('c -> 'd) -> ('a, 'b, 'd) t
    val ( and+ ) : ('a, 'b, 'c) t -> ('b, 'd, 'e) t -> ('a, 'd, 'c * 'e) t
    val ( let* ) : ('a, 'b, 'c) t -> ('c -> ('b, 'd, 'e) t) -> ('a, 'd, 'e) t
    val ( and* ) : ('a, 'b, 'c) t -> ('b, 'd, 'e) t -> ('a, 'd, 'c * 'e) t
  end
|}];;

let indexed_monad1 =
  Indexed_monad.(
    let+ () = open_ "foo"
    and+ first = read
    and+ second = read
    and+ () = close in
    first ^ second
  );;
[%%expect{|
val indexed_monad1 :
  (Indexed_monad.closed, Indexed_monad.closed, string) Indexed_monad.t =
  Indexed_monad.Map
   (Indexed_monad.Both
     (Indexed_monad.Both
       (Indexed_monad.Both (Indexed_monad.Open "foo", Indexed_monad.Read),
       Indexed_monad.Read),
     Indexed_monad.Close),
   <fun>)
|}];;

let indexed_monad2 =
  Indexed_monad.(
    let* () = open_ "foo" in
    let* first = read in
    let* second = read in
    let* () = close in
      return (first ^ second)
  );;
[%%expect{|
val indexed_monad2 :
  (Indexed_monad.closed, Indexed_monad.closed, string) Indexed_monad.t =
  Indexed_monad.Bind (Indexed_monad.Open "foo", <fun>)
|}];;

let indexed_monad3 =
  Indexed_monad.(
    let+ first = read
    and+ () = open_ "foo"
    and+ second = read
    and+ () = close in
    first ^ second
  );;
[%%expect{|
Line 4, characters 14-25:
4 |     and+ () = open_ "foo"
                  ^^^^^^^^^^^
Error: This expression has type
         (Indexed_monad.closed, Indexed_monad.opened, unit) Indexed_monad.t
       but an expression was expected of type
         (Indexed_monad.opened, 'a, 'b) Indexed_monad.t
       Type Indexed_monad.closed is not compatible with type
         Indexed_monad.opened
|}];;

let indexed_monad4 =
  Indexed_monad.(
    let* () = open_ "foo" in
    let* first = read in
    let* () = close in
    let* second = read in
      return (first ^ second)
  );;
[%%expect{|
Lines 6-7, characters 4-29:
6 | ....let* second = read in
7 |       return (first ^ second)
Error: This expression has type
         (Indexed_monad.opened, Indexed_monad.opened, string) Indexed_monad.t
       but an expression was expected of type
         (Indexed_monad.closed, 'a, 'b) Indexed_monad.t
       Type Indexed_monad.opened is not compatible with type
         Indexed_monad.closed
|}];;

(* Test principality using constructor disambiguation *)

module A = struct
  type t = A
end

module Let_principal = struct
  let ( let+ ) (x : A.t) f = f x
end;;
[%%expect{|
module A : sig type t = A end
module Let_principal : sig val ( let+ ) : A.t -> (A.t -> 'a) -> 'a end
|}];;

let let_principal =
  Let_principal.(
    let+ A = A in
    ()
  );;
[%%expect{|
val let_principal : unit = ()
|}];;


module And_principal = struct
  let ( let+ ) = apply
  let ( and+ ) (x : A.t) y = x, y
end;;
[%%expect{|
module And_principal :
  sig
    val ( let+ ) : 'a -> ('a -> 'b) -> 'b
    val ( and+ ) : A.t -> 'a -> A.t * 'a
  end
|}];;

let and_principal =
  And_principal.(
    let+ _ = A
    and+ () = () in
    ()
  );;
[%%expect{|
val and_principal : unit = ()
|}];;

module Let_not_principal = struct
  let ( let+ ) = apply
end;;
[%%expect{|
module Let_not_principal : sig val ( let+ ) : 'a -> ('a -> 'b) -> 'b end
|}];;

let let_not_principal =
  Let_not_principal.(
    let+ A = A.A in
    ()
  );;
[%%expect{|
val let_not_principal : unit = ()
|}, Principal{|
Line 3, characters 9-10:
3 |     let+ A = A.A in
             ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
val let_not_principal : unit = ()
|}];;

module And_not_principal = struct
  let ( let+ ) = apply
  let ( and+ ) x y = if true then x,y else y,x
end;;
[%%expect{|
module And_not_principal :
  sig
    val ( let+ ) : 'a -> ('a -> 'b) -> 'b
    val ( and+ ) : 'a -> 'a -> 'a * 'a
  end
|}];;

let and_not_principal =
  And_not_principal.(
    fun x y ->
      let+ A.A = x
      and+ A = y in
      ()
  );;
[%%expect{|
val and_not_principal : A.t -> A.t -> unit = <fun>
|}, Principal{|
Line 5, characters 11-12:
5 |       and+ A = y in
               ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
val and_not_principal : A.t -> A.t -> unit = <fun>
|}];;

module Let_not_propagated = struct
  let ( let+ ) = apply
end;;
[%%expect{|
module Let_not_propagated : sig val ( let+ ) : 'a -> ('a -> 'b) -> 'b end
|}];;

let let_not_propagated : A.t =
  Let_not_propagated.(
    let+ x = 3 in
    A
  );;
[%%expect{|
Line 4, characters 4-5:
4 |     A
        ^
Error: Unbound constructor A
|}];;

module Side_effects_ordering = struct
  let r = ref []
  let msg s =
    r := !r @ [s]
  let output () = !r
  let ( let+ ) x f = msg "Let operator"; f x
  let ( and+ ) a b = msg "First and operator"; a, b
  let ( and++ ) a b = msg "Second and operator"; a, b
end;;
[%%expect{|
module Side_effects_ordering :
  sig
    val r : string list ref
    val msg : string -> unit
    val output : unit -> string list
    val ( let+ ) : 'a -> ('a -> 'b) -> 'b
    val ( and+ ) : 'a -> 'b -> 'a * 'b
    val ( and++ ) : 'a -> 'b -> 'a * 'b
  end
|}];;

let side_effects_ordering =
  Side_effects_ordering.(
    let+ () = msg "First argument"
    and+ () = msg "Second argument"
    and++ () = msg "Third argument" in
    output ()
  );;
[%%expect{|
val side_effects_ordering : string list =
  ["First argument"; "Second argument"; "First and operator";
   "Third argument"; "Second and operator"; "Let operator"]
|}];;

module GADT_ordering = struct
  type point = { x : int; y : int }
  type _ is_point =
    | Is_point : point is_point
  let (let+) = apply
  let (and+) = pair
end;;
[%%expect{|
module GADT_ordering :
  sig
    type point = { x : int; y : int; }
    type _ is_point = Is_point : point is_point
    val ( let+ ) : 'a -> ('a -> 'b) -> 'b
    val ( and+ ) : 'a -> 'b -> 'a * 'b
  end
|}];;

let gadt_ordering =
  GADT_ordering.(
    fun (type a) (is_point : a is_point) (a : a) ->
      let+ Is_point : a is_point = is_point
      and+ { x; y } : a = a in
        x + y
  );;
[%%expect{|
val gadt_ordering : 'a GADT_ordering.is_point -> 'a -> int = <fun>
|}];;

(* This example doesn't produce a good error location.  To fix this we need to handle the
   patterns directly rather than elaborating them to tuples. We'd like to do this in
   future but it is quite a bit of work, so for now we leave the location as it is. It
   should only appear in principal mode when using GADTs anyway. *)
let bad_location =
  GADT_ordering.(
    fun (type a) (is_point : a is_point) (a : a) ->
      let+ Is_point = is_point
      and+ { x; y } = a in
        x + y
  );;
[%%expect{|
val bad_location : 'a GADT_ordering.is_point -> 'a -> int = <fun>
|}, Principal{|
Line 4, characters 11-19:
4 |       let+ Is_point = is_point
               ^^^^^^^^
Warning 18 [not-principal]: typing this pattern requires considering GADT_ordering.point and a as equal.
But the knowledge of these types is not principal.
Line 5, characters 11-19:
5 |       and+ { x; y } = a in
               ^^^^^^^^
Error: This pattern matches values of type GADT_ordering.point
       but a pattern was expected which matches values of type
         a = GADT_ordering.point
       This instance of GADT_ordering.point is ambiguous:
       it would escape the scope of its equation
|}];;
