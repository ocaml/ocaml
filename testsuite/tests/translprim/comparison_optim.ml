(* TEST *)

let check_list name list =
  Printf.printf "testing %S...\n" name;
    List.iteri
    (fun i (x, y) ->
       Printf.printf "  #%d: %s\n"
         i
         (if x = y then "OK" else "KO"))
    list;
  Printf.printf "\n%!"

let check_int () = check_list "int" [
  compare (Sys.opaque_identity 1)       2       <= 0, true;
  compare (Sys.opaque_identity 3)       3       <= 0, true;
  compare (Sys.opaque_identity 1)       min_int <= 0, false;
  compare (Sys.opaque_identity 1)       2       <  0, true;
  compare (Sys.opaque_identity 3)       3       <  0, false;
  compare (Sys.opaque_identity 1)       min_int <  0, false;
  compare (Sys.opaque_identity max_int) (-1)    >= 0, true;
  compare (Sys.opaque_identity 3)       3       >= 0, true;
  compare (Sys.opaque_identity 2)       min_int >= 0, true;
  compare (Sys.opaque_identity max_int) (-1)    >  0, true;
  compare (Sys.opaque_identity 3)       3       >  0, false;
  compare (Sys.opaque_identity 2)       min_int >  0, true;
  compare (Sys.opaque_identity min_int) min_int =  0, true;
  compare (Sys.opaque_identity max_int) max_int =  0, true;
  compare (Sys.opaque_identity 1)       2       =  0, false;
  compare (Sys.opaque_identity min_int) min_int <> 0, false;
  compare (Sys.opaque_identity max_int) max_int <> 0, false;
  compare (Sys.opaque_identity 1)       2       <> 0, true;
]

let () = check_int ()

let check_int32 () = check_list "int32" [
  compare (Sys.opaque_identity 1l)            2l            <= 0, true;
  compare (Sys.opaque_identity 3l)            3l            <= 0, true;
  compare (Sys.opaque_identity 1l)            Int32.min_int <= 0, false;
  compare (Sys.opaque_identity 1l)            2l            <  0, true;
  compare (Sys.opaque_identity 3l)            3l            <  0, false;
  compare (Sys.opaque_identity 1l)            Int32.min_int <  0, false;
  compare (Sys.opaque_identity Int32.max_int) (-1l)         >= 0, true;
  compare (Sys.opaque_identity 3l)            3l            >= 0, true;
  compare (Sys.opaque_identity 2l)            Int32.min_int >= 0, true;
  compare (Sys.opaque_identity Int32.max_int) (-1l)         >  0, true;
  compare (Sys.opaque_identity 3l)            3l            >  0, false;
  compare (Sys.opaque_identity 2l)            Int32.min_int >  0, true;
  compare (Sys.opaque_identity Int32.min_int) Int32.min_int =  0, true;
  compare (Sys.opaque_identity Int32.max_int) Int32.max_int =  0, true;
  compare (Sys.opaque_identity 1l)            2l            =  0, false;
  compare (Sys.opaque_identity Int32.min_int) Int32.min_int <> 0, false;
  compare (Sys.opaque_identity Int32.max_int) Int32.max_int <> 0, false;
  compare (Sys.opaque_identity 1l)            2l            <> 0, true;
]

let () = check_int32 ()

let check_int64 () = check_list "int64" [
  compare (Sys.opaque_identity 1L)            2L            <= 0, true;
  compare (Sys.opaque_identity 3L)            3L            <= 0, true;
  compare (Sys.opaque_identity 1L)            Int64.min_int <= 0, false;
  compare (Sys.opaque_identity 1L)            2L            <  0, true;
  compare (Sys.opaque_identity 3L)            3L            <  0, false;
  compare (Sys.opaque_identity 1L)            Int64.min_int <  0, false;
  compare (Sys.opaque_identity Int64.max_int) (-1L)         >= 0, true;
  compare (Sys.opaque_identity 3L)            3L            >= 0, true;
  compare (Sys.opaque_identity 2L)            Int64.min_int >= 0, true;
  compare (Sys.opaque_identity Int64.max_int) (-1L)         >  0, true;
  compare (Sys.opaque_identity 3L)            3L            >  0, false;
  compare (Sys.opaque_identity 2L)            Int64.min_int >  0, true;
  compare (Sys.opaque_identity Int64.min_int) Int64.min_int =  0, true;
  compare (Sys.opaque_identity Int64.max_int) Int64.max_int =  0, true;
  compare (Sys.opaque_identity 1L)            2L            =  0, false;
  compare (Sys.opaque_identity Int64.min_int) Int64.min_int <> 0, false;
  compare (Sys.opaque_identity Int64.max_int) Int64.max_int <> 0, false;
  compare (Sys.opaque_identity 1L)            2L            <> 0, true;
]

let () = check_int64 ()

let check_nativeint () = check_list "nativeint" [
  compare (Sys.opaque_identity 1n)                2n                <= 0, true;
  compare (Sys.opaque_identity 3n)                3n                <= 0, true;
  compare (Sys.opaque_identity 1n)                Nativeint.min_int <= 0, false;
  compare (Sys.opaque_identity 1n)                2n                <  0, true;
  compare (Sys.opaque_identity 3n)                3n                <  0, false;
  compare (Sys.opaque_identity 1n)                Nativeint.min_int <  0, false;
  compare (Sys.opaque_identity Nativeint.max_int) (-1n)             >= 0, true;
  compare (Sys.opaque_identity 3n)                3n                >= 0, true;
  compare (Sys.opaque_identity 2n)                Nativeint.min_int >= 0, true;
  compare (Sys.opaque_identity Nativeint.max_int) (-1n)             >  0, true;
  compare (Sys.opaque_identity 3n)                3n                >  0, false;
  compare (Sys.opaque_identity 2n)                Nativeint.min_int >  0, true;
  compare (Sys.opaque_identity Nativeint.min_int) Nativeint.min_int =  0, true;
  compare (Sys.opaque_identity Nativeint.max_int) Nativeint.max_int =  0, true;
  compare (Sys.opaque_identity 1n)                2n                =  0, false;
  compare (Sys.opaque_identity Nativeint.min_int) Nativeint.min_int <> 0, false;
  compare (Sys.opaque_identity Nativeint.max_int) Nativeint.max_int <> 0, false;
  compare (Sys.opaque_identity 1n)                2n                <> 0, true;
]

let () = check_nativeint ()
