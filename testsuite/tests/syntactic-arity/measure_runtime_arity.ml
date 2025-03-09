(* TEST
 flags = "-w +A-70";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* Check that the runtime arity of a function (i.e., its 'fast path' for
   runtime application) matches its syntactic arity (i.e., the number
   of arguments appearing directly following [fun]).
*)

(* This function will need to change if the runtime representation of closures
   changes. Currently, the arity is the first 8 bits of the second field of
   a closure.
*)
let extract_arity_from_closure (closure : Obj.t) : int =
  assert (Obj.closure_tag = Obj.tag closure);
  let clos_info = Obj.raw_field (Obj.repr closure) 1 in
  Nativeint.(to_int (shift_right clos_info (Sys.word_size - 8)))

type (_, _) raw_arity =
  | One : (int -> 'ret, 'ret) raw_arity
  | Succ : ('f, 'ret) raw_arity -> (int -> 'f, 'ret) raw_arity

let rec numeric_arity : type f ret. (f, ret) raw_arity -> int =
  fun arity ->
    match arity with
    | One -> 1
    | Succ arity -> numeric_arity arity + 1

let rec apply : type f ret. (f, ret) raw_arity -> f -> int -> ret =
  fun arity f arg ->
    match arity with
    | One -> f arg
    | Succ arity -> apply arity (f arg) arg

type 'a arity =
  | Tupled
  | Curried : ('a, _) raw_arity -> 'a arity

type packed_raw_arity = Packed_raw_arity : _ raw_arity -> packed_raw_arity
type packed_arity = Packed_arity : _ arity -> packed_arity

let arity_description (type a) (arity : a arity) =
  match arity with
  | Tupled -> "tupled fun"
  | Curried arity -> Printf.sprintf "%d-ary fun" (numeric_arity arity)

(* [runtime_arity] depends on representation details of functions and
   is subject to change.
*)
let runtime_arity (f : 'a -> 'b) : ('a -> 'b) arity =
  let raw_arity = extract_arity_from_closure (Obj.repr f) in
  if raw_arity < 0 then Tupled else
    let rec build_arity n =
      if n = 1 then Packed_raw_arity One
      else
        let Packed_raw_arity pred = build_arity (n-1) in
        Packed_raw_arity (Succ pred)
    in
    let Packed_raw_arity arity = build_arity raw_arity in
    (* Obj.magic is claiming that [f]'s arity matches the arity
       we've constructed here.
    *)
    Curried (Obj.magic arity : ('a -> 'b, _) raw_arity)

let maybe_runtime_arity (type a) (x : a) : a arity option =
  let open struct
    type _ is_function =
      | Not_function : _ is_function
      | Is_function : (_ -> _) is_function

    let is_function (type a) (x : a) =
      if Obj.tag (Obj.repr x) = Obj.closure_tag
      then (Obj.magic Is_function : a is_function)
      else Not_function
  end
  in
  match is_function x with
  | Is_function -> Some (runtime_arity x)
  | Not_function -> None

(* The "nested arity" of a value is either:
     - the empty list, if the value isn't a function
     - x :: xs if the value is a function [f], where [x] is [f]'s arity, and
       [xs] is the nested arity of the result of applying [f] to [x] many
       values.

   "nested arity" isn't well-defined for a function that, say, returns a 2-ary
   function for some inputs and a 3-ary for others. None of the functions in
   this test do that.
*)
let rec nested_arity : type a. a -> packed_arity list =
  fun f ->
    match maybe_runtime_arity f with
    | None -> []
    | Some x ->
        let rest =
          match x with
          | Tupled -> []
          | Curried arity -> nested_arity (apply arity f 1_234)
        in
        Packed_arity x :: rest

let run ~name f =
  Printf.printf "%s: %s\n" name
    (nested_arity f
     |> List.map (fun (Packed_arity arity) -> arity_description arity)
     |> String.concat " returning ")

let () =
  print_endline "Key:";
  print_endline "  <function description>: <function arity>";
  print_newline ();
  run (fun _ _ _ -> ()) ~name:"3 params";
  run (fun _ _ -> fun _ -> ()) ~name:"2 params then 1 param";
  run (fun _ -> fun _ _ -> ()) ~name:"1 param then 2 params";
  run (fun _ -> fun _ -> fun _ -> ())
    ~name:"1 param, then 1 param, then 1 param";
  run (fun _ -> let g _ _ = () in g)
    ~name:"1 param then let-bound 2 params";
  run (fun _ _ -> let g _ = () in g)
    ~name:"2 params then let-bound 1 param";
  run (fun _ -> let g _ = let h _ = () in h in g)
    ~name:"1 param, then let-bound 1 param, then let-bound 1 param";
;;
