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
Line _, characters 4-9:
  let Any x = Any ()
      ^^^^^
Error: Existential types are not allowed in toplevel bindings,
but this pattern introduces the existential type $Any_'a.
|}]

class c (Any x) = object end
[%%expect {|
Line _, characters 8-15:
  class c (Any x) = object end
          ^^^^^^^
Error: Existential types are not allowed in class arguments,
but this pattern introduces the existential type $Any_'a.
|}]

class c = object(Any x)end
[%%expect {|
Line _, characters 16-23:
  class c = object(Any x)end
                  ^^^^^^^
Error: Existential types are not allowed in self patterns,
but this pattern introduces the existential type $Any_'a.
|}]

type other = Any: _ -> other
[%%expect {|
type other = Any : 'a -> other
|}]

let Any x = Any ()
[%%expect {|
Line _, characters 4-9:
  let Any x = Any ()
      ^^^^^
Error: Existential types are not allowed in toplevel bindings,
but the constructor Any introduces unnamed existential types.
|}]

class c (Any x) = object end
[%%expect {|
Line _, characters 8-15:
  class c (Any x) = object end
          ^^^^^^^
Error: Existential types are not allowed in class arguments,
but the constructor Any introduces unnamed existential types.
|}]

class c = object(Any x) end
[%%expect {|
Line _, characters 16-23:
  class c = object(Any x) end
                  ^^^^^^^
Error: Existential types are not allowed in self patterns,
but the constructor Any introduces unnamed existential types.
|}]
