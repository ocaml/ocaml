(* TEST
   modules = "stub.c";
   set ASAN_OPTIONS="use_sigaltstack=0,exitcode=126,log_path=asanlog";
   set LSAN_OPTIONS="detect_leaks=0";
   not windows;
   include unix;
   run_can_skip = "true";
   {
     exit_status = "0";
     arguments = "6 7";
     bytecode;
     native;
   }
   {
     exit_status = "126";
     arguments = "7 7";
     bytecode;
     native;
   }
   {
     exit_status = "126";
     arguments = "15 7";
     bytecode;
     native;
   }
 *)

external asan_enabled: unit -> bool = "stub_asan_enabled"

external pagesize: unit -> int = "stub_pagesize"

(** [test_read offset len] creates an anonymous mapping of [len] bytes,
    and reads from [offset].
 *)
external test_read: int -> int -> string = "stub_test_read"

let lookup = function
  | "PAGESIZE" -> pagesize () |> string_of_int
  | u -> invalid_arg u

let arg n =
  let b = Buffer.create 16 in
  Buffer.add_substitute b lookup Sys.argv.(n);
  b |> Buffer.contents |> int_of_string

let () =
  if not @@ asan_enabled () then begin
    print_endline "ASAN not enabled in runtime, skipping";
    exit 125;
  end;
  let (_ : string) = test_read (arg 1) (arg 2) in
  ()
