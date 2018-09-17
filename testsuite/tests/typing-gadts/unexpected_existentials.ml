(* TEST
 * expect
*)
(** Test the error message for existential types apparearing
    in unexpected position *)
type any = Any: 'a -> any
[%%expect {|
type any = Any : 'a -> any
|}]

let Any x = Any ()
[%%expect {|
Line 1, characters 4-9:
  let Any x = Any ()
      ^^^^^
Error:
Existential types are not allowed in toplevel bindings,
but this pattern introduces the existential type $Any_'a.
|}]

let () =
  let Any x = Any () and () = () in
  ()
[%%expect {|
Line 2, characters 6-11:
    let Any x = Any () and () = () in
        ^^^^^
Error:
Existential types are not allowed in "let ... and ..." bindings,
but this pattern introduces the existential type $Any_'a.
|}]


let () =
  let rec Any x = Any () in
  ()
[%%expect {|
Line 2, characters 10-15:
    let rec Any x = Any () in
            ^^^^^
Error:
Existential types are not allowed in recursive bindings,
but this pattern introduces the existential type $Any_'a.
|}]


let () =
  let[@attribute] Any x = Any () in
  ()
[%%expect {|
Line 2, characters 18-23:
    let[@attribute] Any x = Any () in
                    ^^^^^
Error:
Existential types are not allowed in presence of attributes,
but this pattern introduces the existential type $Any_'a.
|}]


class c (Any x) = object end
[%%expect {|
Line 1, characters 8-15:
  class c (Any x) = object end
          ^^^^^^^
Error:
Existential types are not allowed in class arguments,
but this pattern introduces the existential type $Any_'a.
|}]

class c = object(Any x)end
[%%expect {|
Line 1, characters 16-23:
  class c = object(Any x)end
                  ^^^^^^^
Error:
Existential types are not allowed in self patterns,
but this pattern introduces the existential type $Any_'a.
|}]

type other = Any: _ -> other
[%%expect {|
type other = Any : 'a -> other
|}]

let Any x = Any ()
[%%expect {|
Line 1, characters 4-9:
  let Any x = Any ()
      ^^^^^
Error:
Existential types are not allowed in toplevel bindings,
but the constructor Any introduces existential types.
|}]


class c = let Any _x = () in object end
[%%expect {|
Line 1, characters 14-20:
  class c = let Any _x = () in object end
                ^^^^^^
Error:
Existential types are not allowed in bindings inside class definition,
but the constructor Any introduces existential types.
|}]

let () =
  let Any x = Any () and () = () in
  ()
[%%expect {|
Line 2, characters 6-11:
    let Any x = Any () and () = () in
        ^^^^^
Error:
Existential types are not allowed in "let ... and ..." bindings,
but the constructor Any introduces existential types.
|}]


let () =
  let rec Any x = Any () in
  ()
[%%expect {|
Line 2, characters 10-15:
    let rec Any x = Any () in
            ^^^^^
Error:
Existential types are not allowed in recursive bindings,
but the constructor Any introduces existential types.
|}]


let () =
  let[@attribute] Any x = Any () in
  ()
[%%expect {|
Line 2, characters 18-23:
    let[@attribute] Any x = Any () in
                    ^^^^^
Error:
Existential types are not allowed in presence of attributes,
but the constructor Any introduces existential types.
|}]

class c (Any x) = object end
[%%expect {|
Line 1, characters 8-15:
  class c (Any x) = object end
          ^^^^^^^
Error:
Existential types are not allowed in class arguments,
but the constructor Any introduces existential types.
|}]

class c = object(Any x) end
[%%expect {|
Line 1, characters 16-23:
  class c = object(Any x) end
                  ^^^^^^^
Error:
Existential types are not allowed in self patterns,
but the constructor Any introduces existential types.
|}]

class c = let Any _x = () in object end
[%%expect {|
Line 1, characters 14-20:
  class c = let Any _x = () in object end
                ^^^^^^
Error:
Existential types are not allowed in bindings inside class definition,
but the constructor Any introduces existential types.
|}]
