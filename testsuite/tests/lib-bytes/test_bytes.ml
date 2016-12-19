let test_raises_invalid_argument f x =
  ignore
    (Testing.test_raises_exc_p (function Invalid_argument _ -> true | _ -> false)
         f x)

let () =
  let abcde = Bytes.of_string "abcde" in
  let open Bytes in
  begin
    (*
           abcde
    ?????     
    *)
    Testing.test
      (length (extend abcde 7 (-7)) = 5); 

    (*
    abcde
           ?????
    *)
    Testing.test
      (length (extend abcde (-7) 7) = 5);

    (*
      abcde
      abcde
    *)
    Testing.test
      (let r = extend abcde 0 0 in
       length r = 5
       && r.[0] = 'a' && r.[1] = 'b' && r.[2] = 'c' && r.[3] = 'd' && r.[4] = 'e'
       && r != abcde);

    (*
      abcde
    ??abc
    *)
    Testing.test
      (let r = extend abcde 2 (-2) in
       length r = 5 && r.[2] = 'a' && r.[3] = 'b' && r.[4] = 'c');

    (*
      abcde
       bcd
    *)
    Testing.test
      (let r = extend abcde (-1) (-1) in
       length r = 3 && r.[0] = 'b' && r.[1] = 'c' && r.[2] = 'd');

    (*
      abcde
         de??
    *)
    Testing.test
      (let r = extend abcde (-3) 2 in
       length r = 4 && r.[0] = 'd' && r.[1] = 'e');

    (*
      abcde
      abc
    *)
    Testing.test
      (let r = extend abcde 0 (-2) in
       length r = 3 && r.[0] = 'a' && r.[1] = 'b' && r.[2] = 'c');

    (*
      abcde
        cde
    *)
    Testing.test
      (let r = extend abcde (-2) 0 in
       length r = 3 && r.[0] = 'c' && r.[1] = 'd' && r.[2] = 'e');

    (*
      abcde
      abcde??
    *)
    Testing.test
      (let r = extend abcde 0 2 in
       length r = 7
       && r.[0] = 'a' && r.[1] = 'b' && r.[2] = 'c' && r.[3] = 'd' && r.[4] = 'e');

    (*
        abcde
      ??abcde
    *)
    Testing.test
      (let r = extend abcde 2 0 in
       length r = 7
       && r.[2] = 'a' && r.[3] = 'b' && r.[4] = 'c' && r.[5] = 'd' && r.[6] = 'e');

    (*
       abcde
      ?abcde?
    *)
    Testing.test
      (let r = extend abcde 1 1 in
       length r = 7
       && r.[1] = 'a' && r.[2] = 'b' && r.[3] = 'c' && r.[4] = 'd' && r.[5] = 'e');

    (* length + left + right < 0 *)
    test_raises_invalid_argument
      (fun () -> extend abcde (-3) (-3)) ();

    (* length + left > max_int *)
    test_raises_invalid_argument
      (fun () -> extend abcde max_int 0) ();

    (* length + right > max_int *)
    test_raises_invalid_argument
      (fun () -> extend abcde 0 max_int) ();

    (* length + left + right > max_int *)
    test_raises_invalid_argument
      (fun () -> extend abcde max_int max_int) ();
  end
