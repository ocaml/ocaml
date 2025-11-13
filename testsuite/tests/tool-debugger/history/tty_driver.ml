(* PTY-based test driver for ocamldebug interactive features *)

open Unix

external openpty : unit -> Unix.file_descr * Unix.file_descr = "caml_openpty"

let write_all fd s =
  let len = String.length s in
  let rec loop offset =
    if offset < len then
      let written = write fd (Bytes.of_string s) offset (len - offset) in
      loop (offset + written)
  in
  loop 0

let read_with_timeout fd timeout =
  let buf = Bytes.create 4096 in
  let output = Buffer.create 1024 in
  let end_time = Unix.gettimeofday () +. timeout in
  let rec drain () =
    let remaining = end_time -. Unix.gettimeofday () in
    if remaining <= 0. then ()
    else
      let ready, _, _ = select [fd] [] [] remaining in
      if ready = [] then ()
      else
        try
          let n = read fd buf 0 (Bytes.length buf) in
          if n > 0 then begin
            Buffer.add_subbytes output buf 0 n;
            drain ()
          end
        with Unix_error _ -> ()
  in
  drain ();
  Buffer.contents output

type action =
  | Write of string
  | Sleep of float
  | Signal of int

let run_session ocamldebug program actions =
  let master_fd, slave_fd = openpty () in

  match fork () with
  | 0 ->
      (* Child process *)
      close master_fd;
      dup2 slave_fd stdin;
      dup2 slave_fd stdout;
      dup2 slave_fd stderr;
      if slave_fd <> stdin && slave_fd <> stdout && slave_fd <> stderr then
        close slave_fd;

      (* ocamldebug may be "runtime/ocamlrun debugger/ocamldebug" *)
      let parts = String.split_on_char ' ' ocamldebug in
      (match parts with
       | [] -> failwith "empty ocamldebug path"
       | [single] -> execv single [| single; program |]
       | runner :: debugger_parts ->
           let debugger = String.concat " " debugger_parts in
           execv runner (Array.of_list (runner :: debugger :: [program])))

  | child_pid ->
      (* Parent process *)
      close slave_fd;

      (* Execute actions *)
      List.iter (fun action ->
        match action with
        | Write s ->
            write_all master_fd s;
            ignore (read_with_timeout master_fd 0.05)
        | Sleep delay ->
            ignore (Unix.select [] [] [] delay)
        | Signal sig_num ->
            kill child_pid sig_num;
            ignore (read_with_timeout master_fd 0.2)
      ) actions;

      (* Collect remaining output *)
      let rec collect_output () =
        match waitpid [WNOHANG] child_pid with
        | 0, _ ->
            ignore (read_with_timeout master_fd 0.05);
            ignore (Unix.select [] [] [] 0.05);
            collect_output ()
        | _, status ->
            let final_output = read_with_timeout master_fd 0.05 in
            close master_fd;
            (status, final_output)
      in
      collect_output ()

let count_occurrences s pattern =
  let rec loop pos count =
    try
      let idx = String.index_from s pos pattern.[0] in
      let len = String.length pattern in
      if idx + len <= String.length s &&
         String.sub s idx len = pattern then
        loop (idx + len) (count + 1)
      else
        loop (idx + 1) count
    with Not_found -> count
  in
  loop 0 0

let exit_code_of_status = function
  | WEXITED n -> n
  | WSIGNALED n -> -n
  | WSTOPPED n -> -n

let test_history_session ocamldebug program =
  let actions = [
    Write "run\n";
    Sleep 0.2;
    Write "\027[A";  (* Up arrow *)
    Sleep 0.2;
    Write "\n";
    Sleep 0.2;
    Write "quit\n";
  ] in
  let status, output = run_session ocamldebug program actions in
  let exit_code = exit_code_of_status status in
  let exits = count_occurrences output "Program exit." in
  let times = count_occurrences output "Time:" in
  (exit_code, exits, times)

let test_sigtstp_session ocamldebug program =
  let actions = [
    Write "\026";  (* Ctrl+Z *)
    Sleep 0.2;
    Signal Sys.sigcont;
    Sleep 0.2;
    Write "\n";
    Sleep 0.2;
    Write "quit\n";
  ] in
  let status, output = run_session ocamldebug program actions in
  let exit_code = exit_code_of_status status in
  let prompt_seen = String.contains output '(' && String.contains output ')' in
  (exit_code, prompt_seen)

let test_completion_session ocamldebug program =
  let actions = [
    Write "info";
    Sleep 0.2;
    Write "\t";
    Sleep 0.2;
    Write "modules\n";
    Sleep 0.2;
    Write "quit\n";
  ] in
  let status, output = run_session ocamldebug program actions in
  let exit_code = exit_code_of_status status in
  let ok = count_occurrences output "Used modules" > 0 in
  (exit_code, ok)

let test_sigquit_session ocamldebug program =
  let actions = [
    Write "\028";  (* Ctrl+\ *)
    Sleep 0.2;
  ] in
  let status, output = run_session ocamldebug program actions in
  let exit_code = exit_code_of_status status in
  (exit_code, output)

let () =
  if Array.length Sys.argv <> 3 then begin
    Printf.eprintf "usage: tty_driver <ocamldebug> <program>\n";
    exit 2
  end;

  let ocamldebug = Sys.argv.(1) in
  let program = Sys.argv.(2) in

  let history_code, exits, times = test_history_session ocamldebug program in
  let sigtstp_code, prompt_seen = test_sigtstp_session ocamldebug program in
  let completion_code, completion_ok = test_completion_session ocamldebug program in
  let sigquit_code, sigquit_output = test_sigquit_session ocamldebug program in

  (* Emit compact, deterministic summary *)
  Printf.printf "history exit_code=%d runs=%d time_lines=%d\n"
    history_code exits times;
  Printf.printf "sigtstp exit_code=%d prompt_seen=%b\n"
    sigtstp_code prompt_seen;
  Printf.printf "completion exit_code=%d ok=%b\n"
    completion_code completion_ok;

  let quit_signal = abs sigquit_code in
  let sigquit_marker = String.contains sigquit_output 'Q' &&
                       String.contains sigquit_output 'u' in
  Printf.printf "sigquit exit_signal=%d quit_message=%b\n"
    quit_signal sigquit_marker;

  (* Validate results *)
  let ok =
    exits = 2 && times >= 2 && history_code = 0 &&
    sigtstp_code = 0 && prompt_seen &&
    completion_code = 0 && completion_ok &&
    quit_signal = Sys.sigquit && sigquit_marker
  in

  exit (if ok then 0 else 1)
