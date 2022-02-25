(* TEST
 *)

open Effect
open Effect.Shallow

(*
let handle_state init f x =
  let rec loop state k x =
    continue k x with
    | result -> result, state
    | effect Get, k -> loop state k state
    | effect Set new_state, k -> loop new_state k ()
  in
  loop init (fiber f) x
*)

type _ t += Get : int t
          | Set : int -> unit t

let handle_state init f x =
  let rec loop : type a r. int -> (a, r) continuation -> a -> r * int =
    fun state k x ->
      continue_with k x
      { retc = (fun result -> result, state);
        exnc = (fun e -> raise e);
        effc = (fun (type b) (eff : b t) ->
          match eff with
          | Get -> Some (fun (k : (b,r) continuation) ->
              loop state k state)
          | Set new_state -> Some (fun (k : (b,r) continuation) ->
              loop new_state k ())
          | e -> None) }
  in
  loop init (fiber f) x


let comp () =
  Printf.printf "Initial state: %d\n" (perform Get);
  perform (Set 42);
  Printf.printf "Updated state: %d\n" (perform Get);
  perform (Set 43)

let main () =
  let (), i = handle_state 0 comp () in
  Printf.printf "Final state: %d\n" i

let _ = main ()
