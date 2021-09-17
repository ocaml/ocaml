(* TEST
   flags = "-g"
*)

open Obj.Effect_handlers
open Obj.Effect_handlers.Deep

type _ eff += E : unit eff
            | Inc : unit eff

let blorp () =
  perform Inc;
  perform E;
  42

let baz () =
    try_with blorp ()
    { effc = fun (type a) (e : a eff) ->
        match e with
        | Inc -> Some (fun (k : (a, _) continuation) ->
            1 + continue k ())
        | _ -> None }

let f () =
  match_with baz ()
  { retc = (fun x -> Printf.printf "%d\n" x);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a eff) ->
          match e with
          | E -> Some (fun (k : (a, _) continuation) ->
              Printexc.(get_kcallstack_deep k 100 |>
                        raw_backtrace_to_string |>
                        print_string);
              continue k ())
          | _ -> None }

let () = f ()
