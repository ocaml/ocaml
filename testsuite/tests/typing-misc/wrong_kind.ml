(* TEST
   * expect
*)

module Constr = struct
  type t = A | B | C

  let get _ _ = A

  let put f = ignore (f () : t)
end

module Record = struct
  type t = { a : int; b : int; c : int }

  let get _ _ = { a = 0; b = 0; c = 0 }

  let put f = ignore (f () : t)
end

module Bool = struct
  type t = true | false

  let get _ _ = true

  let put f = ignore (f () : t)
end

module List = struct
  type 'a t = [] | (::) of 'a * 'a t

  let get _ _ = []

  let put f = ignore (f () : int t)
end

module Unit = struct
  [@@@warning "-redefining-unit"]
  type t = ()

  let get _ _ = ()

  let put f = ignore (f (() : unit) : t)
end;;
[%%expect{|
module Constr :
  sig
    type t = A | B | C
    val get : 'a -> 'b -> t
    val put : (unit -> t) -> unit
  end
module Record :
  sig
    type t = { a : int; b : int; c : int; }
    val get : 'a -> 'b -> t
    val put : (unit -> t) -> unit
  end
module Bool :
  sig
    type t = true | false
    val get : 'a -> 'b -> t
    val put : (unit -> t) -> unit
  end
module List :
  sig
    type 'a t = [] | (::) of 'a * 'a t
    val get : 'a -> 'b -> 'c t
    val put : (unit -> int t) -> unit
  end
module Unit :
  sig type t = () val get : 'a -> 'b -> t val put : (unit -> t) -> unit end
|}]

let () =
  match Constr.get () with
  | A | B | C -> ();;
[%%expect{|
Line 3, characters 4-5:
3 |   | A | B | C -> ();;
        ^
Error: This pattern should not be a constructor, the expected type is
       'a -> Constr.t
|}]

let () =
  match Record.get () with
  | { a; _ } -> ();;
[%%expect{|
Line 3, characters 4-12:
3 |   | { a; _ } -> ();;
        ^^^^^^^^
Error: This pattern should not be a record, the expected type is
       'a -> Record.t
|}]

let () =
  match Bool.get () with
  | true -> ();;
[%%expect{|
Line 3, characters 4-8:
3 |   | true -> ();;
        ^^^^
Error: This pattern should not be a boolean literal, the expected type is
       'a -> Bool.t
|}]

let () =
  match Bool.get () with
  | false -> ();;
[%%expect{|
Line 3, characters 4-9:
3 |   | false -> ();;
        ^^^^^
Error: This pattern should not be a boolean literal, the expected type is
       'a -> Bool.t
|}]

let () =
  match List.get () with
  | [] -> ();;
[%%expect{|
Line 3, characters 4-6:
3 |   | [] -> ();;
        ^^
Error: This pattern should not be a list literal, the expected type is
       'a -> 'b List.t
|}]

let () =
  match List.get () with
  | _ :: _ -> ();;
[%%expect{|
Line 3, characters 4-10:
3 |   | _ :: _ -> ();;
        ^^^^^^
Error: This pattern should not be a list literal, the expected type is
       'a -> 'b List.t
|}]

let () =
  match Unit.get () with
  | () -> ();;
[%%expect{|
Line 3, characters 4-6:
3 |   | () -> ();;
        ^^
Error: This pattern should not be a unit literal, the expected type is
       'a -> Unit.t
|}]

let () = Constr.put A;;
[%%expect{|
Line 1, characters 20-21:
1 | let () = Constr.put A;;
                        ^
Error: This expression should not be a constructor, the expected type is
       unit -> Constr.t
|}]

let () = Record.put { a = 0; b = 0; c = 0 };;
[%%expect{|
Line 1, characters 20-43:
1 | let () = Record.put { a = 0; b = 0; c = 0 };;
                        ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a record, the expected type is
       unit -> Record.t
|}]

let () = Bool.put true;;
[%%expect{|
Line 1, characters 18-22:
1 | let () = Bool.put true;;
                      ^^^^
Error: This expression should not be a boolean literal, the expected type is
       unit -> Bool.t
|}]

let () = Bool.put false;;
[%%expect{|
Line 1, characters 18-23:
1 | let () = Bool.put false;;
                      ^^^^^
Error: This expression should not be a boolean literal, the expected type is
       unit -> Bool.t
|}]

let () = List.put [];;
[%%expect{|
Line 1, characters 18-20:
1 | let () = List.put [];;
                      ^^
Error: This expression should not be a list literal, the expected type is
       unit -> int List.t
|}]

let () = List.put (1 :: 2);;
[%%expect{|
Line 1, characters 18-26:
1 | let () = List.put (1 :: 2);;
                      ^^^^^^^^
Error: This expression should not be a list literal, the expected type is
       unit -> int List.t
|}]

let () = Unit.put ();;
[%%expect{|
Line 1, characters 18-20:
1 | let () = Unit.put ();;
                      ^^
Error: This expression should not be a unit literal, the expected type is
       unit -> Unit.t
|}]

let () =
  ignore ((Record.get ()).a);;
[%%expect{|
Line 2, characters 10-25:
2 |   ignore ((Record.get ()).a);;
              ^^^^^^^^^^^^^^^
Error: This expression has type 'a -> Record.t which is not a record type.
|}]

let () =
  (Record.get ()).a <- 5;;
[%%expect{|
Line 2, characters 2-17:
2 |   (Record.get ()).a <- 5;;
      ^^^^^^^^^^^^^^^
Error: This expression has type 'a -> Record.t which is not a record type.
|}]

let () =
  ignore { (Record.get ()) with a = 5 };;
[%%expect{|
Line 2, characters 11-26:
2 |   ignore { (Record.get ()) with a = 5 };;
               ^^^^^^^^^^^^^^^
Error: This expression has type 'a -> Record.t which is not a record type.
|}]

let foo x =
  Record.put { x with a = 5 };;
[%%expect{|
Line 2, characters 13-29:
2 |   Record.put { x with a = 5 };;
                 ^^^^^^^^^^^^^^^^
Error: This expression should not be a record, the expected type is
       unit -> Record.t
|}]
