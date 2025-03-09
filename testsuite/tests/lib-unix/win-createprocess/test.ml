(* TEST
 include unix;
 libwin32unix;
 {
   bytecode;
 }{
   native;
 }
*)

(* This test is inspired by [Creating a Child Process with Redirected
   Input and Output][1]. A parent process feeds data to a child
   process stdin, the child copies the data to its stdout, which the
   parent reads back.

   The child's end of the pipes used to share data are created
   inheritable (keep-on-exec) in the parent, as in the example.

   We then test that Unix.create_process doesn't duplicate and leak
   the child's end of the pipe in the child process: if the child
   spawns a sub-child process, the sub-child will inherit the handles
   too. If the sub-child outlives the child by a long delay, the
   parent process will receive the end-of-file signal long after the
   child process exits; when it should receive it as soon as the child
   process closes its standard handles.

   [1]: https://docs.microsoft.com/en-us/windows/win32/procthread/creating-a-child-process-with-redirected-input-and-output
*)

let sub_child_lifetime = 1      (* seconds *)
let child_timeout = 0.5         (* seconds *)

let bufsize = 4096

let parent ~inherit_children_pipes () =
  let name = Filename.(concat (dirname Sys.argv.(0)) "test.ml") in
  let ch = open_in_bin name in
  let len = in_channel_length ch in
  let buf = Bytes.create len in
  really_input ch buf 0 len;
  close_in ch;

  (* Parent's end of pipes are non-inheritable (close-on-exec), child's
     end are inheritable (keep-on-exec). *)

  let child_stdout_in, child_stdout_out = Unix.pipe ~cloexec:true () in
  if inherit_children_pipes then
    Unix.clear_close_on_exec child_stdout_out;

  let child_stdin_in, child_stdin_out = Unix.pipe ~cloexec:true () in
  if inherit_children_pipes then
    Unix.clear_close_on_exec child_stdin_in;

  let child = Unix.create_process Sys.argv.(0) [|Sys.argv.(0); "child"|]
              child_stdin_in
              child_stdout_out
              Unix.stderr in

  Unix.close child_stdout_out;
  Unix.close child_stdin_in;

  assert (Unix.write child_stdin_out buf 0 len = len);
  Unix.close child_stdin_out;

  let t = Unix.gettimeofday () in

  let buf' = Bytes.create len in
  let rec aux i =
    let n = Unix.read child_stdout_in buf' i (len - i) in
    if n = 0 then ()
    else aux (i + n)
  in
  aux 0;

  let t' = Unix.gettimeofday () in

  begin match snd (Unix.waitpid [Unix.WUNTRACED] child) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> failwith (Printf.sprintf "Child exited with status %d" n)
  | Unix.WSIGNALED n -> failwith (Printf.sprintf "Child signalled %d" n)
  | Unix.WSTOPPED n -> failwith (Printf.sprintf "Child stopped %d" n)
  end;

  assert (buf = buf');

  (* Check that the read time was determined solely by the lifetime of
     the child process and _not_ the subchild. *)
  assert (t' -. t <= float_of_int sub_child_lifetime)


let child () =
  (* Make sure standard handles are not inherited. *)
  Unix.set_close_on_exec Unix.stdin;
  Unix.set_close_on_exec Unix.stdout;
  Unix.set_close_on_exec Unix.stderr;

  let null = Unix.openfile Filename.null [O_RDWR; O_CLOEXEC] 0o0 in

  (* The child's "leaked" handles are also inherited by the
     sub-child. *)
  let sub_child = Unix.create_process Sys.argv.(0) [|Sys.argv.(0); "subchild"|]
                    null null Unix.stderr in
  ignore sub_child;

  let buf = Bytes.create bufsize in
  let rec aux () =
    match Unix.select [Unix.stdin] [] [] child_timeout with
    | [fd], [], [] ->
       let n = Unix.read fd buf 0 bufsize in
       if n = 0 then ()
       else aux (assert (Unix.write Unix.stdout buf 0 n = n))
    | [], [], [] -> failwith "Timeout exceeded, stdin was never closed."
    | _ -> assert false
  in
  aux ()


let sub_child () =
  (* Sub-child lifetime outlasts child, but as the file handles leak,
     the parent has to wait for sub-child to exit. *)
  Unix.sleep sub_child_lifetime


let () =
  match Sys.argv with
  | [|_|] -> parent ~inherit_children_pipes:true ()
  | [|_; "child"|] -> child ()
  | [|_; "subchild"|] -> sub_child ()
  | _ -> invalid_arg "Sys.argv"
