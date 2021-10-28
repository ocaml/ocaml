(* TEST
   ocamlrunparam += ",l=10"
   * bytecode
*)

(* large with respect to the stack-size=10 setting above *)
let large = 1000

let init n f =
  let[@tail_mod_cons] rec init_aux i n f =
    if i = n then []
    else f i :: init_aux (i + 1) n f
  in init_aux 0 n f

module ListMap = struct
  let[@tail_mod_cons] rec map f = function
    | [] -> []
    | x :: xs ->
        (* Note: tail-mod-cons guarantees that 'map f xs' is evaluated last *)
        f x :: map f xs

  let _ =
    init large Fun.id
    |> map succ
end

module TreeMap = struct
  type 'a tree =
    | Leaf of 'a
    | Node of 'a tree * 'a tree

  let[@tail_mod_cons] rec map f = function
    | Leaf v -> Leaf (f v)
    | Node (left, right) ->
        Node (map f left, (map [@tailcall]) f right)

  let _ =
    init large Fun.id
    |> List.fold_left (fun t n -> Node (Leaf n, t)) (Leaf (-1))
       (* large right-leaning tree *)
    |> map succ
end
