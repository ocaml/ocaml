external perform : 'a eff -> 'a = "%perform"

type ('b,'c) handler =
   { eff: 'a. 'a eff -> ('a, 'c) continuation -> 'c;
     exn: exn -> 'c;
     return: 'b -> 'c; }

external handle_raw : ('a -> 'b) -> ('b -> 'c) -> (exn -> 'c) ->
                      ('d eff -> ('d, 'c) continuation -> 'c) -> 'c = "%handle"


let handle {eff; exn; return} f x =
  handle_raw (fun () -> f x) return exn eff

external continue: ('a, 'b) continuation -> 'a -> 'b = "%continue"

external discontinue: ('a, 'b) continuation -> exn -> 'b = "%discontinue"

let delegate e k =
  match perform e with
  | v -> continue k v
  | exception e -> discontinue k e
