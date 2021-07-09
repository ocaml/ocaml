(* TEST
   * expect
*)

(* expect-tests currently do not collect I/O,
   so we emulate I/O by collecting output in a "log" *)
let logger () =
  let log = ref [] in
  let show_log v = List.rev !log, v in
  let log v = log := v :: !log in
  log, show_log
[%%expect{|
val logger : unit -> ('a -> unit) * ('b -> 'a list * 'b) = <fun>
|}]

let _ =
  let log, show_log = logger () in
  let x = lazy (log "x"; 41) in
  let y =
    log "map";
    Lazy.map (fun n -> log "y"; n+1) x in
  log "force y";
  show_log (Lazy.force y)
;;
[%%expect{|
- : string list * int = (["map"; "force y"; "x"; "y"], 42)
|}]

let _ =
  let log, show_log = logger () in
  let x = lazy (log "x"; 41) in
  let y =
    log "map_val";
    Lazy.map_val (fun n -> log "y"; n+1) x in
  assert (not (Lazy.is_val y));
  log "force y";
  show_log (Lazy.force y)
;;
[%%expect{|
- : string list * int = (["map_val"; "force y"; "x"; "y"], 42)
|}]

let _ =
  let log, show_log = logger () in
  let x = lazy (log "x"; 41) in
  log "force x";
  let () = ignore (Lazy.force x) in
  let y =
    log "map_val";
    Lazy.map_val (fun n -> log "y"; n+1) x in
  assert (Lazy.is_val y);
  log "y is val";
  show_log (Lazy.force y)
;;
[%%expect{|
- : string list * int = (["force x"; "x"; "map_val"; "y"; "y is val"], 42)
|}]
