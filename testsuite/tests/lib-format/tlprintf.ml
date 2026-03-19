(* TEST
 include testing;
 flags = "-no-strict-formats";
*)

(*

A test file for the heterogeneous list variants Format module.

*)

open Testing;;
open Format;;

exception Test of string
type tster = { x: string; y: int }

let say s = Printf.printf s;;

try

  say "Delayed printf\n%!";
  let t1 fmt = lfprintf fmt "%i - %s" [1; "bar"] in
  test (lasprintf "foo %t" [t1] = "foo 1 - bar");
  let t2 fmt = lfprintf fmt "%a@]" [(pp_print_list pp_print_int); [1; 2; 3]] in
  test (lasprintf "foo @[<v>%t@,%s" [t2; "bar"] = "foo 1\n    2\n    3\nbar");
  test (lasprintf "%t @[<h>%t" [t1; t2] = "1 - bar 123");

  say "\nMiscellaneous tests\n%!";
  test (lasprintf "%d, %.2f, %s" [42; 3.14159; "ocaml"] = "42, 3.14, ocaml");

  let pp_custom fmt p = lfprintf fmt "x = %s, y = %d" [p.x; p.y] in
  let p = { x = "ocaml"; y = 42 } in
  test (lasprintf "%a" [pp_custom; p] = "x = ocaml, y = 42");

  test (lasprintf "%s, %d, %.1f, %a" ["world"; 7; 2.718; pp_custom; p] = "world, 7, 2.7, x = ocaml, y = 42");

  let pp_padded_list fmt l =
    lfprintf fmt "[%s]" [ List.map (fun x -> lasprintf "%04d" [ x ]) l |> String.concat "; " ] in
  test (lasprintf "%a" [pp_padded_list; [1; 23; 456]] = "[0001; 0023; 0456]");

  test (lasprintf "%a" [(Format.pp_print_list Format.pp_print_int); [1; 23; 456]] = "123\n456");

  let nested_format ppf = lfprintf ppf "%s, %d, %a" [ "final"; 42; pp_custom; p ] in
  test (lasprintf "%s -> %t" [ "outer"; nested_format ] = "outer -> final, 42, x = ocaml, y = 42");

  let delayed_format fmt = lfprintf fmt "%s, %d, %a" [ "final"; 42; pp_custom; p ] in
  test (lasprintf "%s, %d, %.3f, %a, %t"
    ["last"; 100; 3.141; pp_custom; p; delayed_format]
    = "last, 100, 3.141, x = ocaml, y = 42, final, 42, x = ocaml, y = 42");

  let l : _ Args.t = [ "ocaml"; 42; 3.14; 'c'; pp_custom; p ] in
  test (lasprintf "%s, %d, %.3f, %c, %a" l = "ocaml, 42, 3.140, c, x = ocaml, y = 42");

  (* emulating kfprintf *)
  let test_exn fmt args = raise (Test (lasprintf fmt args)) in
  test (try test_exn "%d %s" [ 42; "ocaml" ] with Test "42 ocaml" -> true | _ -> false);

  say "\nend of tests\n%!";

with e ->
  say "unexpected exception: %s\n%!" (Printexc.to_string e);
  test false;
;;
