(* TEST
 include runtime_events;
*)
open Runtime_events

type state =
  | INIT
  | BEGIN
  | EVACUATING_BEGIN
  | EVACUATING_END
  | FORWARDING_BEGIN
  | FORWARDING_END
  | RELEASING_BEGIN
  | RELEASING_END
  | END

let compact_state = ref INIT

let () =
    start ();
    let cursor = create_cursor None in
    let runtime_begin domain_id ts phase =
      match phase with
      | EV_COMPACT ->
        begin
          match !compact_state with
          | INIT -> compact_state := BEGIN
          | _ -> assert(false)
        end
      | EV_COMPACT_EVACUATE -> begin
          match !compact_state with
          | BEGIN -> compact_state := EVACUATING_BEGIN
          | _ -> assert(false)
        end
      | EV_COMPACT_FORWARD -> begin
          match !compact_state with
          | EVACUATING_END -> compact_state := FORWARDING_BEGIN
          | _ -> assert(false)
        end
      | EV_COMPACT_RELEASE -> begin
          match !compact_state with
          | FORWARDING_END -> compact_state := RELEASING_BEGIN
          | _ -> assert(false)
        end
      | _ -> () in
      let runtime_end domain_id ts phase =
        match phase with
        | EV_COMPACT ->
          begin
            match !compact_state with
            | RELEASING_END -> compact_state := END
            | _ -> assert(false)
          end
        | EV_COMPACT_EVACUATE -> begin
            match !compact_state with
            | EVACUATING_BEGIN -> compact_state := EVACUATING_END
            | _ -> assert(false)
          end
        | EV_COMPACT_FORWARD -> begin
            match !compact_state with
            | FORWARDING_BEGIN -> compact_state := FORWARDING_END
            | _ -> assert(false)
          end
        | EV_COMPACT_RELEASE -> begin
            match !compact_state with
            | RELEASING_BEGIN -> compact_state := RELEASING_END
            | _ -> assert(false)
          end
        | _ -> ()
        in
    let callbacks = Callbacks.create ~runtime_begin ~runtime_end ()
    in
    Gc.compact ();
    ignore(read_poll cursor callbacks (Some 1_000));
    assert(!compact_state = END)
