(* TEST
   bytecode;
   native;
   {
     introspect;
     reference = "${test_source_directory}/printval.reference";
   }{
     no-introspect;
     reference = "${test_source_directory}/printval.no-introspect.reference";
   }
*)

open Introspect.Print

let () =
  let normalize_output text =
    (* Normalize extension/exception ids which can be unstable.
       Replace foo/999 by foo/ *)
    let len = String.length text in
    let buf = Buffer.create len in
    let i = ref 0 in
    while !i < len do
      let c = text.[!i] in
      Buffer.add_char buf c;
      incr i;
      if c = '/' then
        while !i < len &&
              let c = text.[!i] in
              c >= '0' && c <= '9'
        do
          incr i;
        done
    done;
    Buffer.contents buf
  in
  let test_case name value =
    print_endline ("--- " ^ name ^ " ---");
    let ppf = Format.get_str_formatter () in
    format_any ppf value;
    let text = Format.flush_str_formatter () in
    print_endline (normalize_output text);
    print_newline ()
  in

  (* 1. Primitives (Immediate vs Boxed) *)
  test_case "Int immediate" 42;
  test_case "Int boxed" [42];

  test_case "Float immediate" 3.14;
  test_case "Float boxed" [3.14];

  test_case "Char immediate" 'a';
  test_case "Char boxed" ['a'];

  (* 2. Constants (None, True, False) *)
  test_case "Constant (None) immediate" None;
  test_case "Constant (None) boxed" [None];
  test_case "Constant (True) immediate" true;
  test_case "Constant (True) boxed" [true];

  (* 3. Collections *)
  test_case "List" [1; 2; 3];
  test_case "Array" [|1; 2; 3|];
  test_case "Empty Array" [||];
  test_case "Tuple" (1, "hello", 3.14);
  test_case "Empty Tuple" ();

  (* 4. Records *)
  let type record = {string: string; int: int; bool: bool} in
  test_case "Record" {string = "hello"; int = 42; bool = true};

  (* 5. Variants *)
  test_case "Poly-variant simple" `A;
  test_case "Poly-variant with data" (`B "payload");
  test_case "Nested Poly-variant" (`C (`D 1));

  (* 6. References (The two allocation paths) *)
  (* path A: Built-in ref primitive *)
  let r = ref 10 in
  test_case "Built-in ref" r;

  (* path B: Manual ref allocation *)
  let rl = { contents = 10 } in
  test_case "Record-based ref" rl;

  (* 7. Closures *)
  let x = 100 in
  let closure_simple = (fun y -> x + y) in
  test_case "Closure (capturing x)" closure_simple;

  (* 8. Lazy *)
  let lazy_val = lazy (1 + 1) in
  test_case "Lazy (unevaluated)" lazy_val;
  let lazy _ = lazy_val in
  test_case "Lazy (evaluated)" lazy_val;

  (* 9. Abstract Types *)
  let module Abs : sig
    type t
    val value : t
  end = struct
    type t = T of int
    let value = T 42
  end in
  test_case "Abstract type" Abs.value;

  (* 10. Recursive / Cyclic structures *)
  let rec cyclic_list = 1 :: cyclic_list in
  test_case "Cyclic List" cyclic_list;
  test_case "Prefixed cyclic list" (0 :: cyclic_list);
  let rec long_cycle = 1 :: 2 :: 3 :: long_cycle in
  test_case "Indirect cycle" long_cycle;

  (* 11. Deep Nesting *)
  let deep = [| [ ( {string = "deep"; int = 1; bool = true}, [1; 2] ) ] |] in
  test_case "Deeply nested structure" deep;

  (* 12. Extensions *)
  let exception Foo in
  test_case "Constant exception" Foo;

  let exception Bar of char in
  test_case "Parametrized exception" (Bar 'c');

  let type t = .. in

  let type t += TFoo in
  test_case "Constant extension" TFoo;

  let type t += TBar of char in
  test_case "Parametrized extension" (TBar 'c');

  (* 13. Standard library *)
  let module IntSet = Set.Make(Int) in
  test_case "Empty set" IntSet.empty;
  test_case "Small set" (IntSet.of_list [1; 2; 3]);

  let module IntMap = Map.Make(Int) in
  test_case "Empty map" IntMap.empty;
  test_case "Small map" (IntMap.of_list [1, "one"; 2, "two"; 3, "three"])
;;
