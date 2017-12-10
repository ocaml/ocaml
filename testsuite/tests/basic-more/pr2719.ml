(* TEST
   include testing
*)

open Printf

let bug () =
  let mat = [| [|false|] |]
  and test = ref false in
    printf "Value of test at the beginning : %B\n" !test; flush stdout;
    (try let _ = mat.(0).(-1) in
       (test := true;
        printf "Am I going through this block of instructions ?\n";
        flush stdout)
     with Invalid_argument _ -> printf "Value of test now : %B\n" !test
    );
    (try if mat.(0).(-1) then ()
     with Invalid_argument _ -> ()
    )

let () = bug ()
