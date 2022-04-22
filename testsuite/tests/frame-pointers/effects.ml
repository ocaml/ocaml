(* TEST

* frame_pointers
** native
readonly_files = "fp_backtrace.c"
all_modules = "${readonly_files} effects.ml"

*)

open Printf
open Effect
open Effect.Deep

external fp_backtrace : unit -> unit = "fp_backtrace" [@@noalloc]

type _ t += E : int -> int t

let[@inline never] f () =
  printf "# computation f\n%!";
  fp_backtrace ();
  printf "# perform effect (E 0)\n%!";
  let v = perform (E 0) in
  printf "# perform returns %d\n%!" v;
  fp_backtrace ();
  v + 1

let h (type a) (eff : a t) : ((a, 'b) continuation -> 'b) option =
  let[@inline never] h_effect_e v k =
    printf "# caught effect (E %d). continuing...\n%!" v;
    fp_backtrace ();
    let v = continue k (v + 1) in
    printf "# continue returns %d\n%!" v;
    fp_backtrace ();
    v + 1
  in
  match eff with
  | E v -> Some (h_effect_e v)
  | e -> None


let v =
  let[@inline never] v_retc v =
    printf "# done %d\n%!" v;
    fp_backtrace ();
    v + 1
  in
  match_with f ()
  { retc = v_retc;
    exnc = (fun e -> raise e);
    effc = h }

let () = printf "# result=%d\n%!" v
