(* TEST
   flags = "-g -w -5"
   * native
     compare_programs = "false"
   * bytecode
*)

open Gc.Memprof

let alloc_list_literal () =
  ignore (Sys.opaque_identity [Sys.opaque_identity 1])

let alloc_pair () =
  ignore (Sys.opaque_identity (Sys.opaque_identity 1, Sys.opaque_identity 2))

type record = { a : int; b : int }
let alloc_record () =
  ignore (Sys.opaque_identity
            {a = Sys.opaque_identity 1; b = Sys.opaque_identity 2})

let alloc_some () =
  ignore (Sys.opaque_identity (Some (Sys.opaque_identity 2)))

let alloc_array_literal () =
  ignore (Sys.opaque_identity [|Sys.opaque_identity 1|])

let alloc_float_array_literal () =
  ignore (Sys.opaque_identity
            [|Sys.opaque_identity 1.; Sys.opaque_identity 2.|])

let[@inline never] do_alloc_unknown_array_literal x =
  Sys.opaque_identity [|x|]
let alloc_unknown_array_literal () =
  ignore (Sys.opaque_identity (do_alloc_unknown_array_literal 1.))

let alloc_small_array () =
  ignore (Sys.opaque_identity (Array.make 10 (Sys.opaque_identity 1)))

let alloc_large_array () =
  ignore (Sys.opaque_identity (Array.make 100000 (Sys.opaque_identity 1)))

let alloc_closure () =
  let x = Sys.opaque_identity 1 in
  ignore (Sys.opaque_identity (fun () -> x))

let floatarray = [| 1.; 2. |]
let[@inline never] get0 a = a.(0)
let getfloatfield () =
  ignore (Sys.opaque_identity (get0 floatarray))

let marshalled =
  Marshal.to_string [Sys.opaque_identity 1] []
let alloc_unmarshal () =
  ignore (Sys.opaque_identity
            ((Marshal.from_string [@inlined never]) (Sys.opaque_identity marshalled) 0))

let alloc_ref () =
  ignore (Sys.opaque_identity (ref (Sys.opaque_identity 1)))

let fl = 1.
let[@inline never] prod_floats a b = a *. b
let alloc_boxedfloat () =
  ignore (Sys.opaque_identity (prod_floats fl fl))

let allocators =
  [alloc_list_literal; alloc_pair; alloc_record; alloc_some;
   alloc_array_literal; alloc_float_array_literal; alloc_unknown_array_literal;
   alloc_small_array; alloc_large_array; alloc_closure;
   getfloatfield; alloc_unmarshal; alloc_ref; alloc_boxedfloat]

let test alloc =
  Printf.printf "-----------\n%!";
  let callstack = ref None in
  start ~callstack_size:10 ~sampling_rate:1.
    { null_tracker with
      alloc_minor = (fun info ->
         callstack := Some info.callstack;
         None
      );
      alloc_major = (fun info ->
         callstack := Some info.callstack;
         None
      );
    };
  alloc ();
  stop ();
  match !callstack with
  | None -> assert false
  | Some cs -> Printexc.print_raw_backtrace stdout cs

let () =
  List.iter test allocators
