(* TEST *)

module NoImmSingleTag = struct
  type t = C of string [@unboxed]

  let f = function
    | C s -> String.length s

  let () =
    assert (f (C "foo") = 3);
end

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

module AnyImmSingleTag = struct
  type t = A of int [@unboxed] | B of string [@unboxed]

  let f = function
    | A i -> i
    | B s -> String.length s

  let () =
    assert (f (A 12) = 12);
    assert (f (B "foo") = 3);
end

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

module SingleImmNoTag = struct
  type u = Foo
  type t = A of u [@unboxed]

  let f = function
    | A Foo -> 12

  let () =
    assert (f (A Foo) = 12);
end

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
