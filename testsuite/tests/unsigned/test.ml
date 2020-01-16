(* TEST
*)

let () = begin
  (* Uint32 *)
  assert (Uint32.max_int = 4294967295u);
  assert (Uint32.(to_string max_int) = "4294967295");
  assert Uint32.(zero = 0u);
  assert Uint32.(one = 1u);
  assert Uint32.(succ zero = one);
  assert Uint32.(zero = pred one);
  assert Uint32.(succ max_int = zero);
  assert Uint32.(max_int = pred zero);

  assert Uint32.(add 1000u 1000u = 2000u);
  assert Uint32.(sub 2000u 1000u = 1000u);
  assert Uint32.(mul 1000u 1000u = 1000_000u);
  assert Uint32.(div 1000u 10u = 100u);
  begin match Uint32.div 1000u 0u with
  | exception Division_by_zero -> ()
  | _ -> assert false
  end;
  assert Uint32.(rem 21u 20u = 1u);
  begin match Uint32.rem 21u 0u with
  | exception Division_by_zero -> ()
  | _ -> assert false
  end;
  assert (Uint32.[0u; 1000u; 1_000_000u; 1_000_000_000u; max_int]
          =
          List.sort Stdlib.compare
            Uint32.[1000u; 1_000_000_000u; 1_000_000u; max_int; 0u]);

  assert (Uint32.[0u; 1000u; 1_000_000u; 1_000_000_000u; max_int]
          =
          List.sort Uint32.compare
            Uint32.[1000u; 1_000_000_000u; 1_000_000u; max_int; 0u]);
  assert (Uint32.of_string_opt "x" = None);
  assert (Uint32.of_string_opt "12345" = Some 12345u);
  assert (Uint32.of_string "12345" = 12345u);
  begin match Uint32.of_string "?" with
  | exception Failure _ -> ()
  | _ -> assert false
  end;

  assert (Marshal.from_string
            (Marshal.to_string
               Uint32.[0u; 1000u; 1_000_000u; 1_000_000_000u; max_int] [])
            0
          =
          Uint32.[0u; 1000u; 1_000_000u; 1_000_000_000u; max_int]);

  assert (Uint32.shift_left 0b10111u 4 = 0b101110000u);
  assert (Uint32.shift_left 1u 31 = 0b10000000000000000000000000000000u);
  assert (Uint32.shift_right 0b101110000u 4 = 0b10111u);
  assert (Uint32.shift_right 0b10000000000000000000000000000000u 31 = 1u);
  assert Uint32.(logand 0B0_0_1_1u
                        0B1_0_1_0u
                 =      0B0_0_1_0u);
  assert Uint32.(logor  0B0_0_1_1u
                        0B1_0_1_0u
                 =      0B1_0_1_1u);
  assert Uint32.(logxor 0B0_0_1_1u
                        0B1_0_1_0u
                 =      0B1_0_0_1u);
  assert Uint32.(lognot 0B0_0_1_1u
                 =      0B1111111111111111111111111111_1_1_0_0u);

  if Sys.int_size = 63 then begin
    assert (
      List.map Uint32.to_int
        Uint32.[0u; 1000u; 1000_000u; 1000_000_000u; max_int]
      =
      Int32.[0; 1000; 1000_000; 1000_000_000; int_of_string "4294967295"]);

    assert (Uint32.of_int (int_of_string "4294967295") = Uint32.max_int);
    assert (Uint32.of_int (-1) = Uint32.max_int);
    assert (Uint32.of_int (int_of_string "4294967296") = Uint32.zero);

  end
  else if Sys.int_size = 31 then begin
    assert (
      List.map Uint32.to_int
        Uint32.[0u; 1000u; 1000_000u; 1000_000_000u; max_int]
      =
      Int32.[0; 1000; 1000_000; 1000_000_000; -1]);

    assert (Uint32.of_int (-1) = Uint32.max_int);
  end
  else ();

  assert (List.map Uint32.to_int32
            Uint32.[0u; 1000u; 1000_000u; 1000_000_000u; max_int]
          =
          Int32.[0l; 1000l; 1000_000l; 1000_000_000l; -1l]);

  assert (List.map Uint32.of_int32
            Int32.[0l; 1000l; 1000_000l; 1000_000_000l; -1l; max_int]
          =
          Uint32.[0u; 1000u; 1000_000u; 1000_000_000u; max_int; 0x7fffffffu]);

  ListLabels.iter
    Uint32.[0u; 1000u; 1000_000u; 1000_000_000u; max_int]
    ~f:(fun i ->
        let j = Uint32.(add i zero) in
        assert (i = j);
        assert (i != j);
        assert (Hashtbl.hash i = Hashtbl.hash j));

  for i = 0 to 1000 do
    for j = succ i to 1000 do
      assert (Hashtbl.hash (Uint32.of_int i)
           <> Hashtbl.hash (Uint32.of_int j))
    done
  done;

  begin match 0u, Some 1000u, [1000_000u],
              (1000_000_000u, ()), Uint32.max_int with
    0u, Some 1000u, [1000_000u],
    (1000_000_000u, ()), 4294967295u -> ()
  | _ -> assert false
  end;

  let check_equal convert l =
    List.fold_left (fun ok (x, y) -> convert x = y) true l
  in

  assert (check_equal Int64.to_uint32
            [Int64.min_int, 0u;
             -1L,           Uint32.max_int;
             Int64.zero,    0u;
             1000L,         1000u;
             1000_000_000L, 1000_000_000u;
             Int64.max_int, Uint32.max_int]);

  assert (check_equal Int64.of_uint32
            [0u,          0L;
             0x1ffu,      0x1ffL;
             0xffffffffu, 0xffffffffL]);

  assert (check_equal Nativeint.of_uint32
            [0u,             0n;
             0x7fffffffu,    0x7fffffffn;
             Uint32.max_int, 0xffffffffn]);

  assert (check_equal Nativeint.to_uint32
            [0n,          0u;
             0x7fffffffn, 0x7fffffffu;
             -1n,         Uint32.max_int]);

  assert (check_equal Uint32.of_float
            [0.3,   0u;
             1.3,   1u;
             1e9,  1_000_000_000u]);

  assert (check_equal Uint32.to_float
            [0u,       0.0;
             1000u,    1e3;
             0xfffffu, 0xfffff.]);

  (* Uint64 *)

  assert (Uint64.max_int = 18446744073709551615U);
  assert (Uint64.(to_string max_int) = "18446744073709551615");
  assert (Uint64.zero = 0U);
  assert (Uint64.one = 1U);
  assert Uint64.(succ zero = one);
  assert Uint64.(zero = pred one);
  assert Uint64.(succ max_int = zero);
  assert Uint64.(max_int = pred zero);

  assert Uint64.(add 1000U 1000U = 2000U);
  assert Uint64.(sub 2000U 1000U = 1000U);
  assert Uint64.(mul 1000U 1000U = 1000_000U);
  assert Uint64.(mul 1000_000U 1000_000U = 1000_000_000_000U);
  assert Uint64.(div 1000U 10U = 100U);
  begin match Uint64.div 1000U 0U with
  | exception Division_by_zero -> ()
  | _ -> assert false
  end;
  assert Uint64.(rem 21U 20U = 1U);
  begin match Uint64.rem 21U 0U with
  | exception Division_by_zero -> ()
  | _ -> assert false
  end;
  assert (Uint64.[0U; 1000U; 1_000_000U; 1_000_000_000U;
                  1_000_000_000_000U;
                  1_000_000_000_000_000U;
                  1_000_000_000_000_000_000U;
                  max_int]
          =
          List.sort Stdlib.compare
            Uint64.[1000U;
                    1_000_000_000_000U;
                    1_000_000_000_000_000_000U;
                    max_int;
                    1_000_000_000_000_000U;
                    1_000_000_000U; 1_000_000U; 0U]);

  assert (Uint64.[0U; 1000U; 1_000_000U; 1_000_000_000U;
                  1_000_000_000_000U;
                  1_000_000_000_000_000U;
                  1_000_000_000_000_000_000U;
                  max_int]
          =
          List.sort Uint64.compare
            Uint64.[1000U;
                    1_000_000_000_000U;
                    1_000_000_000_000_000_000U;
                    max_int;
                    1_000_000_000_000_000U;
                    1_000_000_000U; 1_000_000U; 0U]);

  assert (Uint64.of_string_opt "x" = None);
  assert (Uint64.of_string_opt "12345" = Some 12345U);
  assert (Uint64.of_string "12345" = 12345U);
  assert (Uint64.of_string "1234567890123456789"
          = 1234567890123456789U);
  begin match Uint64.of_string "?" with
  | exception Failure _ -> ()
  | _ -> assert false
  end;

  assert (Marshal.from_string
            (Marshal.to_string
               Uint64.[0U; 1000U; 1_000_000U; 1_000_000_000U;
                       1_000_000_000_000U;
                       1_000_000_000_000_000U;
                       1_000_000_000_000_000_000U;
                       max_int] [])
            0
          =
          Uint64.[0U; 1000U; 1_000_000U; 1_000_000_000U;
                  1_000_000_000_000U;
                  1_000_000_000_000_000U;
                  1_000_000_000_000_000_000U;
                  max_int]);

  assert (Uint64.shift_left 0b10111U 4 = 0b101110000U);
  assert (Uint64.shift_left 1U 31 = 0b10000000000000000000000000000000U);
  assert (Uint64.shift_right 0b101110000U 4 = 0b10111U);
  assert (Uint64.shift_right 0b10000000000000000000000000000000U 31 = 1U);
  assert Uint64.(logand 0B0_0_1_1U
                        0B1_0_1_0U
                 =      0B0_0_1_0U);
  assert Uint64.(logor  0B0_0_1_1U
                        0B1_0_1_0U
                 =      0B1_0_1_1U);
  assert Uint64.(logxor 0B0_0_1_1U
                        0B1_0_1_0U
                 =      0B1_0_0_1U);
  assert Uint64.(lognot 0B0_0_1_1U
                 =      0B111111111111111111111111111111111111111111111111111111111111_1_1_0_0U);

  assert (
    List.map Uint64.to_int
      Uint64.[0U; 1000U; 1000_000U; 1000_000_000U;
              max_int]
    =
    Int32.[0; 1000; 1000_000; 1000_000_000;
           -1]);


  if Sys.int_size = 63 then
    assert (
      List.map Uint64.to_int
        [1_000_000_000_000U;
         1_000_000_000_000_000U;
         1_000_000_000_000_000_000U]
        = [int_of_string "1_000_000_000_000";
           int_of_string "1_000_000_000_000_000";
           int_of_string "1_000_000_000_000_000_000"])
  ;

  assert (Uint64.of_int (-1) = Uint64.max_int);


  assert (
    List.map Uint64.to_int64
      [0U; 1000U; 1000_000U; 1000_000_000U;
       1_000_000_000_000U;
       1_000_000_000_000_000U;
       1_000_000_000_000_000_000U;
       Uint64.max_int]
    = [0L; 1000L; 1000_000L; 1000_000_000L;
       1_000_000_000_000L;
       1_000_000_000_000_000L;
       1_000_000_000_000_000_000L;
       -1L])
  ;

  assert (Uint64.of_int64 (-1L) = Uint64.max_int);

  assert (
    List.map Uint64.to_uint32
      [0U; 1000U; 1000_000U; 1000_000_000U;
       Uint64.max_int]
    = [0u; 1000u; 1000_000u; 1000_000_000u;
       Uint32.max_int])
  ;

  ListLabels.iter
    Uint64.[0U; 1000U; 1000_000U; 1000_000_000U;
            1_000_000_000_000U;
            1_000_000_000_000_000U;
            1_000_000_000_000_000_000U;
            max_int]
    ~f:(fun i ->
        let j = Uint64.(add i zero) in
        assert (i = j);
        assert (i != j);
        assert (Hashtbl.hash i = Hashtbl.hash j));

  for i = 0 to 1000 do
    for j = succ i to 1000 do
      assert (Hashtbl.hash (Uint64.of_int i)
           <> Hashtbl.hash (Uint64.of_int j))
    done
  done;

  begin match 0U, Some 1000U, [1000_000U],
              (1000_000_000U, ()), Uint64.max_int with
    0U, Some 1000U, [1000_000U],
    (1000_000_000U, ()),
    0xffffffffffffffffU -> ()
  | _ -> assert false
  end;

  assert (check_equal Uint64.to_int32
            [0U,             0l;
             0x7fffffffU,    0x7fffffffl;
             Uint64.max_int, -1l]);

  assert (check_equal Uint64.of_int32
            [0l,          0U;
             0x7fffffffl, 0x7fffffffU;
             -1l,         Uint64.max_int]);

  assert (check_equal Uint64.to_nativeint
            [0U,             0n;
             0x7fffffffU,    0x7fffffffn;
             Uint64.max_int, -1n]);

  assert (check_equal Uint64.of_nativeint
            [0n,          0U;
             0x7fffffffn, 0x7fffffffU;
             -1n,         Uint64.max_int]);

  assert (check_equal Uint64.of_float
            [0.3,            0U;
             1.3,            1U;
             1e10,           10_000_000_000U;
             0xfffffffffff., 0xfffffffffffU]);

  assert (check_equal Uint64.to_float
            [0U,              0.;
             1U,              1.;
             10_000_000_000U, 1e10;
             0xfffffffffffU,  0xfffffffffff.]);
end
