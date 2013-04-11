[%%eval.load "unix.cma"]

[%%eval.start both]
type t = A | B of string
[%%eval.stop]

[%%eval.start]
let () = print_endline "Now compiling..."
[%%eval.stop]

let () =
  begin match [%eval B "x"] with
  | A -> print_endline "A"
  | B s -> Printf.printf "B %S\n%!" s
  end;
  Printf.printf "Home dir at compile time = %s\n" [%eval Sys.getenv "HOME"];
  Printf.printf "Word-size = %i\n" [%eval Sys.word_size];
  Array.iter (Printf.printf "%s;") [%eval Sys.readdir "."];
  print_endline "";
  [%eval print_endline "COUCOU"]

let () =
  let tm = [%eval Unix.(localtime (gettimeofday ()))] in
  Printf.printf "This program was compiled in %i\n%!" (1900 + tm.Unix.tm_year)

let () =
  let debug =
    [%eval try Some (Sys.getenv "DEBUG") with Not_found -> None]
  in
  match debug with
  | Some x -> Printf.printf "DEBUG %s\n%!" x
  | None -> Printf.printf "NODEBUG\n%!"


