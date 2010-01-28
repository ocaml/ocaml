(* Torture test - lots of GC *)

let gc_thread () =
  while true do
(*    print_string "gc"; print_newline(); *)
    Gc.minor();
    Thread.yield()
  done

let stdin_thread () =
  while true do
    print_string "> "; flush stdout;
    let s = read_line() in
    print_string ">>> "; print_string s; print_newline()
  done

let writer_thread (oc, size) =
  while true do
(*    print_string "writer "; print_int size; print_newline(); *)
    let buff = String.make size 'a' in
    Unix.write oc buff 0 size
  done

let reader_thread (ic, size) =
  while true do
(*    print_string "reader "; print_int size; print_newline(); *)
    let buff = String.create size in
    let n = Unix.read ic buff 0 size in
(*    print_string "reader "; print_int n; print_newline(); *)
    for i = 0 to n-1 do
      if buff.[i] <> 'a' then prerr_endline "error in reader_thread"
    done
  done

let main() =
  Thread.create gc_thread ();
  let (out1, in1) = Unix.pipe() in
  Thread.create writer_thread (in1, 4096);
  Thread.create reader_thread (out1, 4096);
  let (out2, in2) = Unix.pipe() in
  Thread.create writer_thread (in2, 16);
  Thread.create reader_thread (out2, 16);
  stdin_thread()

let _ = main()
