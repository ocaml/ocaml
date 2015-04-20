type 'a effect = ..

type ('a, 'b) continuation

external perform : 'a effect -> 'a = "%perform"

type ('b,'c) handler =
   { effect: 'a. 'a effect -> ('a, 'c) continuation -> 'c;
     exn: exn -> 'c;
     return: 'b -> 'c; }

external handle_raw : ('a -> 'b) -> ('b -> 'c) -> (exn -> 'c) ->
                      ('d effect -> ('d, 'c) continuation -> 'c) -> 'c = "%handle"


let handle {effect; exn; return} f x =
  handle_raw (fun () -> f x) return exn effect

external continue: ('a, 'b) continuation -> 'a -> 'b = "%continue"

external discontinue: ('a, 'b) continuation -> exn -> 'b = "%discontinue"

let delegate e k =
  match perform e with
  | v -> continue k v
  | exception e -> discontinue k e
