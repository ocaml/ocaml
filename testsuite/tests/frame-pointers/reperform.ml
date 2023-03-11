(* TEST

* frame_pointers
** native

readonly_files = "fp_backtrace.c"
all_modules = "${readonly_files} reperform.ml"

*)

open Effect
open Effect.Deep

external fp_backtrace : unit -> unit = "fp_backtrace" [@@noalloc]

type _ Effect.t += E : unit t
                 | F : unit t

let rec foo n =
  if n = 10 then 0
  else begin
    if n = 5 then begin
      perform E;
      print_endline "# resumed...";
      fp_backtrace ()
    end;
    foo (n + 1) + n
  end

let rec bar n =
  if n = 10 then 0
  else begin
    if n = 5 then begin
      match_with foo 0
      { retc = ignore;
        exnc = raise;
        effc = fun (type a) (eff : a t) ->
          match eff with
          | F -> Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None }
    end;
    bar (n + 1) + n
  end

let _ =
  try_with bar 0
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | E -> Some (fun (k : (a, _) continuation) -> continue k ())
      | _ -> None }
