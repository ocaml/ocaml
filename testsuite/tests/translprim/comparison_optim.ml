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

let () = check_list "int" [
  compare 1       2       <= 0, true;
  compare 3       3       <= 0, true;
  compare 1       min_int <= 0, false;
  compare 1       2       <  0, true;
  compare 3       3       <  0, false;
  compare 1       min_int <  0, false;
  compare max_int (-1)    >= 0, true;
  compare 3       3       >= 0, true;
  compare 2       min_int >= 0, true;
  compare max_int (-1)    >  0, true;
  compare 3       3       >  0, false;
  compare 2       min_int >  0, true;
  compare min_int min_int =  0, true;
  compare max_int max_int =  0, true;
  compare 1       2       =  0, false;
  compare min_int min_int <> 0, false;
  compare max_int max_int <> 0, false;
  compare 1       2       <> 0, true;
]

let () = check_list "int32" [
  compare 1l            2l            <= 0, true;
  compare 3l            3l            <= 0, true;
  compare 1l            Int32.min_int <= 0, false;
  compare 1l            2l            <  0, true;
  compare 3l            3l            <  0, false;
  compare 1l            Int32.min_int <  0, false;
  compare Int32.max_int (-1l)         >= 0, true;
  compare 3l            3l            >= 0, true;
  compare 2l            Int32.min_int >= 0, true;
  compare Int32.max_int (-1l)         >  0, true;
  compare 3l            3l            >  0, false;
  compare 2l            Int32.min_int >  0, true;
  compare Int32.min_int Int32.min_int =  0, true;
  compare Int32.max_int Int32.max_int =  0, true;
  compare 1l            2l            =  0, false;
  compare Int32.min_int Int32.min_int <> 0, false;
  compare Int32.max_int Int32.max_int <> 0, false;
  compare 1l            2l            <> 0, true;
]

let () = check_list "int64" [
  compare 1L            2L            <= 0, true;
  compare 3L            3L            <= 0, true;
  compare 1L            Int64.min_int <= 0, false;
  compare 1L            2L            <  0, true;
  compare 3L            3L            <  0, false;
  compare 1L            Int64.min_int <  0, false;
  compare Int64.max_int (-1L)         >= 0, true;
  compare 3L            3L            >= 0, true;
  compare 2L            Int64.min_int >= 0, true;
  compare Int64.max_int (-1L)         >  0, true;
  compare 3L            3L            >  0, false;
  compare 2L            Int64.min_int >  0, true;
  compare Int64.min_int Int64.min_int =  0, true;
  compare Int64.max_int Int64.max_int =  0, true;
  compare 1L            2L            =  0, false;
  compare Int64.min_int Int64.min_int <> 0, false;
  compare Int64.max_int Int64.max_int <> 0, false;
  compare 1L            2L            <> 0, true;
]

let () = check_list "nativeint" [
  compare 1n                2n                <= 0, true;
  compare 3n                3n                <= 0, true;
  compare 1n                Nativeint.min_int <= 0, false;
  compare 1n                2n                <  0, true;
  compare 3n                3n                <  0, false;
  compare 1n                Nativeint.min_int <  0, false;
  compare Nativeint.max_int (-1n)             >= 0, true;
  compare 3n                3n                >= 0, true;
  compare 2n                Nativeint.min_int >= 0, true;
  compare Nativeint.max_int (-1n)             >  0, true;
  compare 3n                3n                >  0, false;
  compare 2n                Nativeint.min_int >  0, true;
  compare Nativeint.min_int Nativeint.min_int =  0, true;
  compare Nativeint.max_int Nativeint.max_int =  0, true;
  compare 1n                2n                =  0, false;
  compare Nativeint.min_int Nativeint.min_int <> 0, false;
  compare Nativeint.max_int Nativeint.max_int <> 0, false;
  compare 1n                2n                <> 0, true;
]
