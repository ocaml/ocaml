(* TEST
 ocamlrunparam += ",s=512";
 native;
*)

let count = ref 0

let queue = Queue.create ()

let callback (i:int) (ts: int64) =
  (* add items to queue *)
  incr count;
  Queue.push ( (i, ts, ())) queue

external caml_callback : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = "caml_callback2_exn"

(* main loop *)
let main () =
  for i = 0 to 10000 do
    caml_callback callback (-1) (Int64.of_int i)
  done

type empty = effect |

let empty : empty Effect.t = Effect.create ()

(* a dummy effect handler *)
let () =
  Effect.run_with empty main () {
    result = ignore;
    exn = raise;
    operation = function _ -> .
  }
