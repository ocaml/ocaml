(* TEST
   flags = "-dlambda -dno-locations -dcanonical-ids";
   expect;
*)

module NoImmSingleTag = struct
  type t = C of string [@unboxed]

  let f = function
    | C s -> String.length s

  let () =
    assert (f (C "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "NoImmSingleTag/388"
  (let
    (f/0 = (function param/0 : int (string.length param/0))
     *match*/0 =
       (if (== (apply f/0 "foo") 3) 0
         (raise (makeblock 0 (global Assert_failure/0!) [0: "" 8 4]))))
    (makeblock 0 f/0)))
module NoImmSingleTag : sig type t = C of string val f : t -> int end
|}]

module NoImmDenseTags = struct
  type t = A of int | B of int * int | C of string

  let f = function
    | A i -> i
    | B (a, b) -> a + b
    | C s -> String.length s

  let () =
    assert (f (A 12) = 12);
    assert (f (B (4, 3)) = 7);
    assert (f (C "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "NoImmDenseTags/402"
  (let
    (f/1 =
       (function param/1 : int
         (switch* param/1
          case tag 0: (field_int 0 param/1)
          case tag 1: (+ (field_int 0 param/1) (field_int 1 param/1))
          case tag 2: (string.length (field_imm 0 param/1))))
     *match*/1 =
       (seq
         (if (== (apply f/1 [0: 12]) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 10 4])))
         (if (== (apply f/1 [1: 4 3]) 7) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))
         (if (== (apply f/1 [2: "foo"]) 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 12 4])))))
    (makeblock 0 f/1)))
module NoImmDenseTags :
  sig type t = A of int | B of int * int | C of string val f : t -> int end
|}]

module NoImmSparseTags = struct
  type t = A of int | B of int * int | C of string [@unboxed]

  let f = function
    | A i -> i
    | B (a, b) -> a + b
    | C s -> String.length s

  let () =
    assert (f (A 12) = 12);
    assert (f (B (4, 3)) = 7);
    assert (f (C "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "NoImmSparseTags/416"
  (let
    (f/2 =
       (function param/2 : int
         (let (tag/0 =a (caml_obj_tag param/2))
           (if (!= tag/0 1)
             (if (>= tag/0 252) (string.length param/2)
               (field_int 0 param/2))
             (+ (field_int 0 param/2) (field_int 1 param/2)))))
     *match*/2 =
       (seq
         (if (== (apply f/2 [0: 12]) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 10 4])))
         (if (== (apply f/2 [1: 4 3]) 7) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))
         (if (== (apply f/2 "foo") 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 12 4])))))
    (makeblock 0 f/2)))
module NoImmSparseTags :
  sig type t = A of int | B of int * int | C of string val f : t -> int end
|}]

module AnyImmSingleTag = struct
  type t = A of int [@unboxed] | B of string [@unboxed]

  let f = function
    | A i -> i
    | B s -> String.length s

  let () =
    assert (f (A 12) = 12);
    assert (f (B "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "AnyImmSingleTag/428"
  (let
    (f/3 =
       (function param/3 : int
         (if (isint param/3) param/3 (string.length param/3)))
     *match*/3 =
       (seq
         (if (== (apply f/3 12) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 9 4])))
         (if (== (apply f/3 "foo") 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 10 4])))))
    (makeblock 0 f/3)))
module AnyImmSingleTag :
  sig type t = A of int | B of string val f : t -> int end
|}]

module AnyImmDenseTags = struct
  type t = A of int [@unboxed] | B of int * int | C of string

  let f = function
    | A i -> i
    | B (a, b) -> a + b
    | C s -> String.length s

  let () =
    assert (f (A 12) = 12);
    assert (f (B (4, 3)) = 7);
    assert (f (C "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "AnyImmDenseTags/442"
  (let
    (f/4 =
       (function param/4 : int
         (if (isint param/4) param/4
           (switch* param/4
            case tag 0: (+ (field_int 0 param/4) (field_int 1 param/4))
            case tag 1: (string.length (field_imm 0 param/4)))))
     *match*/4 =
       (seq
         (if (== (apply f/4 12) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 10 4])))
         (if (== (apply f/4 [0: 4 3]) 7) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))
         (if (== (apply f/4 [1: "foo"]) 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 12 4])))))
    (makeblock 0 f/4)))
module AnyImmDenseTags :
  sig type t = A of int | B of int * int | C of string val f : t -> int end
|}]

module AnyImmSparseTags = struct
  type t = A of int [@unboxed] | B of int * int | C of string [@unboxed]

  let f = function
    | A i -> i
    | B (a, b) -> a + b
    | C s -> String.length s

  let () =
    assert (f (A 12) = 12);
    assert (f (B (4, 3)) = 7);
    assert (f (C "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "AnyImmSparseTags/456"
  (let
    (f/5 =
       (function param/5 : int
         (if (isint param/5) param/5
           (if (>= (caml_obj_tag param/5) 252) (string.length param/5)
             (+ (field_int 0 param/5) (field_int 1 param/5)))))
     *match*/5 =
       (seq
         (if (== (apply f/5 12) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 10 4])))
         (if (== (apply f/5 [0: 4 3]) 7) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))
         (if (== (apply f/5 "foo") 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 12 4])))))
    (makeblock 0 f/5)))
module AnyImmSparseTags :
  sig type t = A of int | B of int * int | C of string val f : t -> int end
|}]

module SingleImmNoTag = struct
  type u = Foo
  type t = A of u [@unboxed]

  let f = function
    | A Foo -> 12

  let () =
    assert (f (A Foo) = 12);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "SingleImmNoTag/467"
  (let
    (f/6 = (function param/6 : int 12)
     *match*/6 =
       (if (== (apply f/6 0) 12) 0
         (raise (makeblock 0 (global Assert_failure/0!) [0: "" 9 4]))))
    (makeblock 0 f/6)))
module SingleImmNoTag : sig type u = Foo type t = A of u val f : t -> int end
|}]

module SingleImmSingleTag = struct
  type u = Foo
  type t = A of u [@unboxed] | B of string

  let f = function
    | A Foo -> 12
    | B s -> String.length s

  let () =
    assert (f (A Foo) = 12);
    assert (f (B "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "SingleImmSingleTag/479"
  (let
    (f/7 =
       (function param/7 : int
         (if param/7 (string.length (field_imm 0 param/7)) 12))
     *match*/7 =
       (seq
         (if (== (apply f/7 0) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 10 4])))
         (if (== (apply f/7 [0: "foo"]) 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))))
    (makeblock 0 f/7)))
module SingleImmSingleTag :
  sig type u = Foo type t = A of u | B of string val f : t -> int end
|}]

module SingleImmDenseTags = struct
  type u = Foo
  type t = A of u [@unboxed] | B of int * int | C of string

  let f = function
    | A Foo -> 12
    | B (a, b) -> a + b
    | C s -> String.length s

  let () =
    assert (f (A Foo) = 12);
    assert (f (B (4, 3)) = 7);
    assert (f (C "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "SingleImmDenseTags/494"
  (let
    (f/8 =
       (function param/8 : int
         (switch* param/8
          case int 0: 12
          case tag 0: (+ (field_int 0 param/8) (field_int 1 param/8))
          case tag 1: (string.length (field_imm 0 param/8))))
     *match*/8 =
       (seq
         (if (== (apply f/8 0) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))
         (if (== (apply f/8 [0: 4 3]) 7) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 12 4])))
         (if (== (apply f/8 [1: "foo"]) 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 13 4])))))
    (makeblock 0 f/8)))
module SingleImmDenseTags :
  sig
    type u = Foo
    type t = A of u | B of int * int | C of string
    val f : t -> int
  end
|}]

module SingleImmSparseTags = struct
  type u = Foo
  type t = A of u [@unboxed] | B of int * int | C of string [@unboxed]

  let f = function
    | A Foo -> 12
    | B (a, b) -> a + b
    | C s -> String.length s

  let () =
    assert (f (A Foo) = 12);
    assert (f (B (4, 3)) = 7);
    assert (f (C "foo") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "SingleImmSparseTags/509"
  (let
    (f/9 =
       (function param/9 : int
         (if (isint_variant param/9) 12
           (if (>= (caml_obj_tag param/9) 252) (string.length param/9)
             (+ (field_int 0 param/9) (field_int 1 param/9)))))
     *match*/9 =
       (seq
         (if (== (apply f/9 0) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))
         (if (== (apply f/9 [0: 4 3]) 7) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 12 4])))
         (if (== (apply f/9 "foo") 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 13 4])))))
    (makeblock 0 f/9)))
module SingleImmSparseTags :
  sig
    type u = Foo
    type t = A of u | B of int * int | C of string
    val f : t -> int
  end
|}]

module DenseImmNoTags = struct
  type u = Foo | Bar
  type t = A of u [@unboxed]

  let f = function
    | A Foo -> 12
    | A Bar -> 24

  let () =
    assert (f (A Foo) = 12);
    assert (f (A Bar) = 24);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "DenseImmNoTags/521"
  (let
    (f/10 = (function param/10 : int (if param/10 24 12))
     *match*/10 =
       (seq
         (if (== (apply f/10 0) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 10 4])))
         (if (== (apply f/10 1) 24) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))))
    (makeblock 0 f/10)))
module DenseImmNoTags :
  sig type u = Foo | Bar type t = A of u val f : t -> int end
|}]

module DenseImmSingleTag = struct
  type t = Foo | Bar | C of string [@unboxed]

  let f = function
    | Foo -> 12
    | Bar -> 24
    | C s -> String.length s

  let () =
    assert (f Foo = 12);
    assert (f Bar = 24);
    assert (f (C "baz") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "DenseImmSingleTag/532"
  (let
    (f/11 =
       (function param/11 : int
         (if (isint_variant param/11) (if param/11 24 12)
           (string.length param/11)))
     *match*/11 =
       (seq
         (if (== (apply f/11 0) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 10 4])))
         (if (== (apply f/11 1) 24) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))
         (if (== (apply f/11 "baz") 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 12 4])))))
    (makeblock 0 f/11)))
module DenseImmSingleTag :
  sig type t = Foo | Bar | C of string val f : t -> int end
|}]

module DenseImmSparseTags = struct
  type t = Foo | Bar | B of int * int | C of string [@unboxed]

  let f = function
    | Foo -> 12
    | Bar -> 24
    | B (a, b) -> a + b
    | C s -> String.length s

  let () =
    assert (f Foo = 12);
    assert (f Bar = 24);
    assert (f (B (4, 3)) = 7);
    assert (f (C "baz") = 3);
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "DenseImmSparseTags/546"
  (let
    (f/12 =
       (function param/12 : int
         (if (isint_variant param/12) (if param/12 24 12)
           (if (>= (caml_obj_tag param/12) 252) (string.length param/12)
             (+ (field_int 0 param/12) (field_int 1 param/12)))))
     *match*/12 =
       (seq
         (if (== (apply f/12 0) 12) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 11 4])))
         (if (== (apply f/12 1) 24) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 12 4])))
         (if (== (apply f/12 [0: 4 3]) 7) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 13 4])))
         (if (== (apply f/12 "baz") 3) 0
           (raise (makeblock 0 (global Assert_failure/0!) [0: "" 14 4])))))
    (makeblock 0 f/12)))
module DenseImmSparseTags :
  sig type t = Foo | Bar | B of int * int | C of string val f : t -> int end
|}]
