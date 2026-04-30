(* TEST
 expect;
*)

type _ t = Int : int t;;
[%%expect{|
type _ t = Int : int t
|}]

(** Test unification of kinds *)
let o =
  object (self)
    method private x = 3
    method m : type a. a t -> a = fun Int -> (self#x : int)
  end;;
[%%expect{|
val o : < m : 'a. 'a t -> 'a > = <obj>
|}]

let o' =
  object (self : 's)
    method private x = 3
    method m : type a. a t -> 's -> a = fun Int other -> (other#x : int)
  end;;

let aargh = assert (o'#m Int o' = 3);;
[%%expect{|
Lines 2-5, characters 2-5:
2 | ..object (self : 's)
3 |     method private x = 3
4 |     method m : type a. a t -> 's -> a = fun Int other -> (other#x : int)
5 |   end..
Warning 15 [implicit-public-methods]: the following private methods were made
  public implicitly: "x".

val o' : < m : 'a. 'a t -> 'b -> 'a; x : int > as 'b = <obj>
val aargh : unit = ()
|}]

let o2 =
  object (self : 's)
    method private x = 3
    method m : 's -> int = fun other -> (other#x : int)
  end;;
[%%expect{|
Lines 2-5, characters 2-5:
2 | ..object (self : 's)
3 |     method private x = 3
4 |     method m : 's -> int = fun other -> (other#x : int)
5 |   end..
Warning 15 [implicit-public-methods]: the following private methods were made
  public implicitly: "x".

val o2 : < m : 'a -> int; x : int > as 'a = <obj>
|}]

let o3 =
  object (self : 's)
    method private x = 3
    method m : 's -> int = fun other ->
      let module M = struct let other = other end in (M.other#x : int)
  end;;

let aargh = assert (o3#m o3 = 3);;
[%%expect{|
Lines 2-6, characters 2-5:
2 | ..object (self : 's)
3 |     method private x = 3
4 |     method m : 's -> int = fun other ->
5 |       let module M = struct let other = other end in (M.other#x : int)
6 |   end..
Warning 15 [implicit-public-methods]: the following private methods were made
  public implicitly: "x".

val o3 : < m : 'a -> int; x : int > as 'a = <obj>
val aargh : unit = ()
|}]

module type T = sig type t end
let type_of (type r) (x:r) = (module struct type t = r end: T with type t = r)

[%%expect{|
module type T = sig type t end
val type_of : 'r -> (module T with type t = 'r) = <fun>
|}]

(** Test moregen of kinds *)
let moregen_public_public = object (self)
  method n = 0
  initializer
     let module M = (val type_of self) in
     let module N: sig val s: unit -> M.t end = struct
       let s () : <n : int> = assert false
     end
     in
     ()
end

[%%expect{|
val moregen_public_public : < n : int > = <obj>
|}, Principal{|
Line 4, characters 20-38:
4 |      let module M = (val type_of self) in
                        ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

val moregen_public_public : < n : int > = <obj>
|}]

let moregen_public_private = object (self)
  method private n = 0
  initializer
     let module M = (val type_of self) in
     let module N: sig val s: unit -> M.t end = struct
       let s () : <n : int> = assert false
     end
     in
     ()
end

[%%expect{|
Lines 5-7, characters 48-8:
5 | ................................................struct
6 |        let s () : <n : int> = assert false
7 |      end
Error: Signature mismatch:
       Modules do not match:
         sig val s : unit -> < n : int > end
       is not included in
         sig val s : unit -> M.t end
       Values do not match:
         val s : unit -> < n : int >
       is not included in
         val s : unit -> M.t
       The type "unit -> < n : int >" is not compatible with the type
         "unit -> M.t"
       Type "< n : int >" is not compatible with type "M.t" = "<  >"
       The method "n" is public and was expected to be private
|}, Principal{|
Line 4, characters 20-38:
4 |      let module M = (val type_of self) in
                        ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

Lines 5-7, characters 48-8:
5 | ................................................struct
6 |        let s () : <n : int> = assert false
7 |      end
Error: Signature mismatch:
       Modules do not match:
         sig val s : unit -> < n : int > end
       is not included in
         sig val s : unit -> M.t end
       Values do not match:
         val s : unit -> < n : int >
       is not included in
         val s : unit -> M.t
       The type "unit -> < n : int >" is not compatible with the type
         "unit -> M.t"
       Type "< n : int >" is not compatible with type "M.t" = "<  >"
       The method "n" is public and was expected to be private
|}]

let moregen_private_public = object (self)
  method private n = 0
  initializer
     let module M = (val type_of self) in
     let module N: sig val s: unit -> <n : int> end = struct
       let s () : M.t = assert false
     end
     in
     ()
end

[%%expect{|
Lines 5-7, characters 54-8:
5 | ......................................................struct
6 |        let s () : M.t = assert false
7 |      end
Error: Signature mismatch:
       Modules do not match:
         sig val s : unit -> M.t end
       is not included in
         sig val s : unit -> < n : int > end
       Values do not match:
         val s : unit -> M.t
       is not included in
         val s : unit -> < n : int >
       The type "unit -> M.t" is not compatible with the type
         "unit -> < n : int >"
       Type "M.t" = "<  >" is not compatible with type "< n : int >"
       The method "n" is private and was expected to be public
|}, Principal{|
Line 4, characters 20-38:
4 |      let module M = (val type_of self) in
                        ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

Lines 5-7, characters 54-8:
5 | ......................................................struct
6 |        let s () : M.t = assert false
7 |      end
Error: Signature mismatch:
       Modules do not match:
         sig val s : unit -> M.t end
       is not included in
         sig val s : unit -> < n : int > end
       Values do not match:
         val s : unit -> M.t
       is not included in
         val s : unit -> < n : int >
       The type "unit -> M.t" is not compatible with the type
         "unit -> < n : int >"
       Type "M.t" = "<  >" is not compatible with type "< n : int >"
       The method "n" is private and was expected to be public
|}]

let moregen_private_private = object (self)
  method private n = 0
  initializer
     let module M1 = (val type_of self) in
     let module M2 = (val type_of self) in
     let module N: sig val s: unit -> M1.t end = struct
       let s () : M2.t = assert false
     end
     in
     ()
end

[%%expect{|
val moregen_private_private : <  > = <obj>
|}, Principal{|
Line 4, characters 21-39:
4 |      let module M1 = (val type_of self) in
                         ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

Line 5, characters 21-39:
5 |      let module M2 = (val type_of self) in
                         ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

val moregen_private_private : <  > = <obj>
|}]

(** Test eqtype of kinds *)
let eqtype_public_public = object (self)
  method n = 0
  method k: unit =
     let module M = (val type_of self) in
     let module N: sig type t = M.t end = struct
             type t = <n: int; k:unit >
     end
     in
     ()
end

[%%expect{|
val eqtype_public_public : < k : unit; n : int > = <obj>
|}, Principal{|
Line 4, characters 20-38:
4 |      let module M = (val type_of self) in
                        ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

val eqtype_public_public : < k : unit; n : int > = <obj>
|}]

let eqtype_public_private = object (self)
  method private n = 0
  method k: unit =
     let module M = (val type_of self) in
     let module N: sig type t = M.t end = struct
             type t = <n: int; k:unit >
     end
     in
     ()
end

[%%expect{|
Lines 5-7, characters 42-8:
5 | ..........................................struct
6 |              type t = <n: int; k:unit >
7 |      end
Error: Signature mismatch:
       Modules do not match:
         sig type t = < k : unit; n : int > end
       is not included in
         sig type t = M.t end
       Type declarations do not match:
         type t = < k : unit; n : int >
       is not included in
         type t = M.t
       The type "< k : unit; n : int >" is not equal to the type "M.t"
       The method "n" is public and was expected to be private
|}, Principal{|
Line 4, characters 20-38:
4 |      let module M = (val type_of self) in
                        ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

Lines 5-7, characters 42-8:
5 | ..........................................struct
6 |              type t = <n: int; k:unit >
7 |      end
Error: Signature mismatch:
       Modules do not match:
         sig type t = < k : unit; n : int > end
       is not included in
         sig type t = M.t end
       Type declarations do not match:
         type t = < k : unit; n : int >
       is not included in
         type t = M.t
       The type "< k : unit; n : int >" is not equal to the type "M.t"
       The method "n" is public and was expected to be private
|}]

let eqtype_private_public = object (self)
  method private n = 0
  method k: unit =
     let module M = (val type_of self) in
     let module N: sig type t = <n: int; k:unit > end = struct
             type t = M.t
     end
     in
     ()
end

[%%expect{|
Lines 5-7, characters 56-8:
5 | ........................................................struct
6 |              type t = M.t
7 |      end
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t end
       is not included in
         sig type t = < k : unit; n : int > end
       Type declarations do not match:
         type t = M.t
       is not included in
         type t = < k : unit; n : int >
       The type "M.t" is not equal to the type "< k : unit; n : int >"
       The method "n" is private and was expected to be public
|}, Principal{|
Line 4, characters 20-38:
4 |      let module M = (val type_of self) in
                        ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

Lines 5-7, characters 56-8:
5 | ........................................................struct
6 |              type t = M.t
7 |      end
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t end
       is not included in
         sig type t = < k : unit; n : int > end
       Type declarations do not match:
         type t = M.t
       is not included in
         type t = < k : unit; n : int >
       The type "M.t" is not equal to the type "< k : unit; n : int >"
       The method "n" is private and was expected to be public
|}]

let eqtype_private_private = object (self)
  method private n = 0
  method k: unit =
     let module M1 = (val type_of self) in
     let module M2 = (val type_of self) in
     let module N: sig type t = M2.t end = struct
             type t = M1.t
     end
     in
     ()
end

[%%expect{|
val eqtype_private_private : < k : unit > = <obj>
|}, Principal{|
Line 4, characters 21-39:
4 |      let module M1 = (val type_of self) in
                         ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

Line 5, characters 21-39:
5 |      let module M2 = (val type_of self) in
                         ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

val eqtype_private_private : < k : unit > = <obj>
|}]
