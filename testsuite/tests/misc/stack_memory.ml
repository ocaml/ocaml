(* TEST
 bytecode;
*)
let memory_stat ()  = Gc.quick_stat ()
let stack_size x = x.Gc.live_stacks_bytes

let start = ref (memory_stat())

let pp_memory ppf {Gc.live_stacks_bytes; _ } =
  let live_stack_bytes = live_stacks_bytes - !start.Gc.live_stacks_bytes in
  if live_stack_bytes mod 8 = 0 then
    Format.fprintf ppf "%d" (live_stack_bytes/8)
  else
    Format.fprintf ppf "%dB" live_stack_bytes
type _ Effect.t += Unit: unit Effect.t
let rec use_stack stack_use d =
  if d = 0 then (Effect.perform Unit; 0.)
  else
    let stat = memory_stat () in
    let new_stack_use = stack_size stat in
    if new_stack_use > stack_use then
      Format.printf "Stack memory increase: %a@." pp_memory stat;
    1. +. use_stack new_stack_use (d-1)

let capture_fiber d =
  let r = ref None in
  let before = memory_stat () in
  begin match use_stack (stack_size before) d  with
  | _ -> ()
  | effect Unit, k ->
      Format.printf "memory added to cache stack: %a -> %a@."
        pp_memory before
        pp_memory (memory_stat ());
      r := Some (k: (unit,unit) continuation)
  end;
  !r

let ephemeral_fiber d =
  let before = memory_stat () in
  begin match use_stack (stack_size before) d with
  | _ -> ()
  | effect Unit, k ->
      let middle = memory_stat () in
      Format.printf "ephemeral use of stack memory: %a -> %a"
        pp_memory before
        pp_memory middle;
      Effect.Deep.continue k ()
  end;
  Format.printf " -> %a@." pp_memory (memory_stat ())

let () = Gc.full_major ()
let () = start := memory_stat ()
let _ = ephemeral_fiber 10_000
let _ = ephemeral_fiber 10_000
let a = Array.init 10 (fun _ -> capture_fiber 0)

let () = Array.iter (function None -> () | Some k ->
    let before = memory_stat () in
    Effect.Deep.continue k ();
    Format.printf "check that cached stacks are never released: %a -> %a@."
      pp_memory before
      pp_memory (memory_stat ())
  ) a
