(* TEST
 include testing;
 flags = "-no-strict-formats";
*)

(*

A test file for the heterogeneous list variants of the Printf module.

*)

open Testing;;
open Printf;;

exception Test of string
type tster = { x: string; y: int }

let test_roundtrip fmt of_string s =
  test (sprintf fmt (of_string s) = s)
;;

try
  lprintf "Miscellaneous tests\n%!" [];
  test (lsprintf "%d, %.2f, %s" [42; 3.14159; "ocaml"] = "42, 3.14, ocaml");

  let pp_custom () p = lsprintf "x = %s, y = %d" [p.x; p.y] in
  let p = { x = "ocaml"; y = 42 } in
  test (lsprintf "%a" [pp_custom; p] = "x = ocaml, y = 42");

  test (lsprintf "%s, %d, %.1f, %a" ["world"; 7; 2.718; pp_custom; p] = "world, 7, 2.7, x = ocaml, y = 42");

  let pp_padded_list () l =
    lsprintf "[%s]" [ List.map (fun x -> lsprintf "%04d" [ x ]) l |> String.concat "; " ] in
  test (lsprintf "%a" [pp_padded_list; [1; 23; 456]] = "[0001; 0023; 0456]");

  let nested_print () = lsprintf "%s, %d, %a" [ "final"; 42; pp_custom; p ] in
  test (lsprintf "%s -> %t" [ "outer"; nested_print ] = "outer -> final, 42, x = ocaml, y = 42");

  let delayed_print () = lsprintf "%s, %d, %a" [ "final"; 42; pp_custom; p ] in
  test (lsprintf "%s, %d, %.3f, %a, %t"
    ["last"; 100; 3.141; pp_custom; p; delayed_print]
    = "last, 100, 3.141, x = ocaml, y = 42, final, 42, x = ocaml, y = 42");

  let l : _ Args.t = [ "ocaml"; 42; 3.14; 'c'; pp_custom; p ] in
  test (lsprintf "%s, %d, %.3f, %c, %a" l = "ocaml, 42, 3.140, c, x = ocaml, y = 42");

  (* emulating kfprintf *)
  let test_exn fmt args = raise (Test (lsprintf fmt args)) in
  test (try test_exn "%d %s" [ 42; "ocaml" ] with Test "42 ocaml" -> true | _ -> false);

  printf "\nend of tests\n%!";
with e ->
  printf "unexpected exception: %s\n%!" (Printexc.to_string e);
  test false;
;;
