(* TEST
 include runtime_events;
 include unix;
 readonly_files = "gc_stats_child.ml";
 set OCAML_RUNTIME_EVENTS_PRESERVE = "1";
 set OCAML_RUNTIME_EVENTS_START = "1";
 hasunix;
 not target-windows;
 {
   setup-ocamlc.byte-build-env;
   program = "${test_build_directory}/gc_stats_child.byte";
   all_modules = "gc_stats_child.ml";
   ocamlc.byte;
   program = "${test_build_directory}/test_gc_stats_match.byte";
   all_modules = "test_gc_stats_match.ml";
   ocamlc.byte;
   arguments = "${test_build_directory}/gc_stats_child.byte";
   run;
   check-program-output;
 } {
   setup-ocamlopt.byte-build-env;
   program = "${test_build_directory}/gc_stats_child.exe";
   all_modules = "gc_stats_child.ml";
   ocamlopt.byte;
   program = "${test_build_directory}/test_gc_stats_match.exe";
   all_modules = "test_gc_stats_match.ml";
   ocamlopt.byte;
   arguments = "${test_build_directory}/gc_stats_child.exe";
   run;
   check-program-output;
 }
*)

(* This test verifies that runtime event counters match the GC statistics
   printed by OCAMLRUNPARAM=v=0x400 at program exit.

   It runs a child process that:
   1. Does allocation (triggering minor and major GCs)
   2. Exits - runtime prints v=0x400 stats to stderr

   The parent then:
   1. Parses the v=0x400 stats from stderr
   2. Reads the child's runtime events
   3. Compares them, requiring an exact match.

   The child is run in two configurations: the default one, and with
   cleanup_on_exit (c=1) so that the runtime shuts down gracefully on exit.
   Both configurations must match for the test to pass. *)

type gc_stats = {
  mutable minor_words : int;
  mutable promoted_words : int;
  mutable major_words : int;
  mutable minor_collections : int;
  mutable major_collections : int;
}

type event_stats = {
  mutable ev_minor_words : int;
  mutable ev_promoted_words : int;
  mutable ev_major_words : int;
  mutable ev_minor_collections : int;
  mutable ev_major_collections : int;
}

(* Parse v=0x400 output format: "name: value" *)
let parse_gc_stats lines =
  let stats = {
    minor_words = 0;
    promoted_words = 0;
    major_words = 0;
    minor_collections = 0;
    major_collections = 0;
  } in
  List.iter (fun line ->
    match String.split_on_char ':' line with
    | [name; v] ->
        let name = String.trim name in
        let v = String.trim v in
        (try
          let value = int_of_string v in
          match name with
          | "minor_words" -> stats.minor_words <- value
          | "promoted_words" -> stats.promoted_words <- value
          | "major_words" -> stats.major_words <- value
          | "minor_collections" -> stats.minor_collections <- value
          | "major_collections" -> stats.major_collections <- value
          | _ -> ()
        with Failure _ -> ())
    | _ -> ()
  ) lines;
  stats

let read_all_lines ic =
  let lines = ref [] in
  (try
    while true do
      lines := input_line ic :: !lines
    done
  with End_of_file -> ());
  List.rev !lines

(* Run [child_prog] with the given OCAMLRUNPARAM and check that the v=0x400 GC
   statistics printed on its stderr match the runtime events counters it
   records. [label] identifies the configuration in failure messages. Returns
   [true] on an exact match. *)
let run_and_check child_prog cwd label runparam =
  (* Run the child program and capture its stderr (v=0x400 output) *)
  let (stderr_read, stderr_write) = Unix.pipe () in
  let pid = Unix.fork () in

  if pid = 0 then begin
    (* Child: redirect stderr and exec the child program *)
    Unix.close stderr_read;
    Unix.dup2 stderr_write Unix.stderr;
    Unix.close stderr_write;
    (* Build environment with the requested OCAMLRUNPARAM *)
    let env = Unix.environment () |> Array.to_list in
    let env = List.filter (fun s ->
      not (String.length s >= 14 && String.sub s 0 14 = "OCAMLRUNPARAM=")
    ) env in
    let env = ("OCAMLRUNPARAM=" ^ runparam) :: env in
    Unix.execve child_prog [| child_prog |] (Array.of_list env)
  end;

  (* Parent *)
  Unix.close stderr_write;
  let ic = Unix.in_channel_of_descr stderr_read in
  let lines = read_all_lines ic in
  close_in ic;

  let (_, status) = Unix.waitpid [] pid in
  (match status with
  | Unix.WEXITED 0 -> ()
  | _ -> Printf.eprintf "Child exited with error\n"; exit 1);

  (* Parse the v=0x400 stats from stderr *)
  let gc_stats = parse_gc_stats lines in

  (* Read runtime events from the child *)
  let events_file = Filename.concat cwd (string_of_int pid ^ ".events") in
  if not (Sys.file_exists events_file) then begin
    Printf.eprintf "Events file not found: %s\n" events_file;
    exit 1
  end;

  let cursor = Runtime_events.create_cursor (Some (cwd, pid)) in
  let ev = {
    ev_minor_words = 0;
    ev_promoted_words = 0;
    ev_major_words = 0;
    ev_minor_collections = 0;
    ev_major_collections = 0;
  } in

  let runtime_counter _domain_id _ts counter value =
    let open Runtime_events in
    match counter with
    | EV_C_MINOR_ALLOCATED_WORDS ->
        ev.ev_minor_words <- ev.ev_minor_words + value
    | EV_C_MINOR_PROMOTED_WORDS ->
        ev.ev_promoted_words <- ev.ev_promoted_words + value
    | EV_C_MAJOR_ALLOCATED_WORDS ->
        ev.ev_major_words <- ev.ev_major_words + value
    | _ -> ()
  in

  let runtime_end _domain_id _ts phase =
    let open Runtime_events in
    match phase with
    | EV_MINOR ->
        ev.ev_minor_collections <- ev.ev_minor_collections + 1
    | EV_MAJOR_GC_CYCLE_DOMAINS ->
        ev.ev_major_collections <- ev.ev_major_collections + 1
    | _ -> ()
  in

  let callbacks = Runtime_events.Callbacks.create
    ~runtime_counter
    ~runtime_end
    ()
  in

  (* Read all events *)
  let rec read_all () =
    let count = Runtime_events.read_poll cursor callbacks None in
    if count > 0 then read_all ()
  in
  read_all ();

  (* Tidy up the events file and cursor. *)
  Runtime_events.free_cursor cursor;
  Unix.unlink events_file;

  (* Compare gc stats, we require an exact match for all stats. *)
  let check name gc_val ev_val =
    if gc_val <> ev_val then begin
      Printf.printf "MISMATCH [%s] %s: v=0x400=%d events=%d\n"
        label name gc_val ev_val;
      false
    end else
      true
  in

  let ok =
    check "minor_words" gc_stats.minor_words ev.ev_minor_words
    && check "promoted_words" gc_stats.promoted_words ev.ev_promoted_words
    && check "major_words" gc_stats.major_words ev.ev_major_words
    && check "minor_collections" gc_stats.minor_collections
         ev.ev_minor_collections
    && check "major_collections" gc_stats.major_collections
         ev.ev_major_collections
  in

  if not ok then begin
    Printf.printf "\n[%s] v=0x400 stats:\n" label;
    Printf.printf "  minor_words=%d\n" gc_stats.minor_words;
    Printf.printf "  promoted_words=%d\n" gc_stats.promoted_words;
    Printf.printf "  major_words=%d\n" gc_stats.major_words;
    Printf.printf "  minor_collections=%d\n" gc_stats.minor_collections;
    Printf.printf "  major_collections=%d\n" gc_stats.major_collections;
    Printf.printf "\n[%s] Runtime events:\n" label;
    Printf.printf "  minor_words=%d\n" ev.ev_minor_words;
    Printf.printf "  promoted_words=%d\n" ev.ev_promoted_words;
    Printf.printf "  major_words=%d\n" ev.ev_major_words;
    Printf.printf "  minor_collections=%d\n" ev.ev_minor_collections;
    Printf.printf "  major_collections=%d\n" ev.ev_major_collections
  end;
  ok

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <child_program>\n" Sys.argv.(0);
    exit 2
  end;

  let child_prog = Sys.argv.(1) in
  let cwd = Sys.getcwd () in

  (* Check both the default runtime shutdown and the cleanup_on_exit (c=1)
     shutdown; the counters must match the printed stats in both cases. *)
  let ok_default = run_and_check child_prog cwd "default" "v=0x400" in
  let ok_cleanup =
    run_and_check child_prog cwd "cleanup_on_exit" "v=0x400,c=1" in

  if ok_default && ok_cleanup then
    Printf.printf "OK\n"
  else
    exit 1
