(* TEST

* frame_pointers
** native

readonly_files = "fp_backtrace.c stack_realloc_.c"
all_modules = "${readonly_files} stack_realloc2.ml"

*)

open Effect
open Effect.Deep

type _ t += E : int -> int t

external fp_backtrace : unit -> unit = "fp_backtrace" [@@noalloc]
external c_fun : unit -> int = "c_fun"

let[@inline never][@local never] f x = x

let[@inline never] consume_stack () =
  (* TODO Somehow get a value that would always cause a stack reallocation
   * Currently anything above 32 should cause a stack reallocation since a new
   * fiber stack size is given by caml_fiber_wsz = 2 * Stack_threshold_words
   * and Stack_threshold_words = 32 *)
  (* in words *)
  let size = 128 in
  let allocated = 2 * 2 (* 2 spilled registers *) + 1 (* saved rbp *) in
  let count = size / allocated in
  let[@inline never] rec gobbler i =
    (* Force spilling of x0 and x1 *)
    let x0 = Sys.opaque_identity 42 in
    let x1 = Sys.opaque_identity 42 in
    let _ = f x0 in
    let _ = f x1 in
    let _ = Sys.opaque_identity x0 in
    let _ = Sys.opaque_identity x1 in
    let v = if i = 1 then 42 (* dummy *) else gobbler (i - 1) in
    v - 1 (* dummy *)
  in
  ignore (gobbler count)

let[@inline never] callback () =
  fp_backtrace ();
  0

let _ = Callback.register "callback" callback

let[@inline never] f () =
  let[@inline never] f_comp () =
    let v = perform (E 0) in
    let w = c_fun () in
    v + w + 1
  in
  let f_effc (type a) (eff : a t) : ((a, 'b) continuation -> 'b) option =
    let[@inline never] f_effc_e v k =
      consume_stack ();
      continue k (v + 1)
    in
    match eff with
    | E v -> Some (f_effc_e v)
    | e -> None
  in
  match_with f_comp ()
  { retc = (fun v -> v);
    exnc = (fun e -> raise e);
    effc = f_effc }

let () = assert (f () == 3)
