(* TEST

* frame_pointers
** native
readonly_files = "fp_backtrace.c"
all_modules = "${readonly_files} exception_handler.ml"

*)

(* https://github.com/ocaml/ocaml/pull/11031 *)
external fp_backtrace : unit -> unit = "fp_backtrace" [@@noalloc]

exception Exn1
exception Exn2

(* We want to be sure to use some stack space so that rbp is shifted,
* preventing inlining seems enough *)
let[@inline never] raiser i =
  match i with
  | 1 -> raise Exn1
  | 2 -> raise Exn2
  | _ -> 42 (* shouldn't happen *)

let[@inline never][@local never] f x = x

(* This give us a chance to overwrite the memory address pointed by rbp if it
* is still within 'raiser' stack frame.
* Technically we don't need to overwrite it but by doing so we avoid some
* infinite loop while walking the stack. *)
let[@inline never] handler () =
  (* Force spilling of x0, x1, x2 *)
  let x0 = Sys.opaque_identity 0x6f56df77 (* 0xdeadbeef *) in
  let x1 = Sys.opaque_identity 0x6f56df77 (* 0xdeadbeef *) in
  let x2 = Sys.opaque_identity 0x6f56df77 (* 0xdeadbeef *) in
  let _ = f x0 in
  let _ = f x1 in
  let _ = f x2 in
  let _ = Sys.opaque_identity x0 in
  let _ = Sys.opaque_identity x1 in
  let _ = Sys.opaque_identity x2 in
  fp_backtrace ()

let[@inline never] nested i =
  begin
    try
      try ignore (raiser i) with Exn1 -> handler ()
    with
    | Exn2 -> handler ()
  end;
  i

(* Check that we haven't broken anything by raising directly from this
* function, it doesn't require rbp to be adjusted *)
let[@inline never] bare i =
  begin
    try
      try (if i == 1 then raise Exn1 else raise Exn2) with
      | Exn1 -> handler ()
    with
    | Exn2 -> handler ()
  end;
  i

let () =
  ignore (bare 1);
  ignore (bare 2);
  ignore (nested 1);
  ignore (nested 2)
