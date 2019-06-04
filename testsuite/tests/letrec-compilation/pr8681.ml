(* TEST *)
let rec h =
  let rec f n = if n >= 0 then g (n - 1)
  and g n = h n; f n in
  f

let () = Gc.minor ()
let () = ignore (h 10)

let mooo x =
  let rec h =
    ignore (Sys.opaque_identity x);
    let rec g n = h n; f n
    and f n = if n >= 0 then g (n - 1) in
    f
  in
  h

let h = mooo 3
let () = Gc.minor ()
let () = ignore (h 10)


let rec foo =
  let rec f = function
    | 0 -> 100
    | n -> foo (n-1)
  and g = function
    | 0 -> 200
    | n -> f (n-1) in
  g

let () = print_int (foo 2); print_newline ()
let () = print_int (foo 7); print_newline ()


let with_free_vars a b c =
  let rec foo =
    let rec f = function
      | 0 -> 100 + a + b + c
      | n -> foo (n-1)
    and g = function
      | 0 -> 200 + a + b + c
      | n -> f (n-1) in
    g in
  foo

let () = print_int (with_free_vars 1 2 3 2); print_newline ()
let () = print_int (with_free_vars 1 2 3 7); print_newline ()

let bar =
  let rec f = function
    | 0 -> 3
    | n -> g (n - 1)
  and g = function
    | 0 -> 10 + f 10
    | n -> f (n - 1)
  in
  let rec foof = f
  and goof = g
  in (foof, goof)

let () = print_int (snd bar 42); print_newline ()
