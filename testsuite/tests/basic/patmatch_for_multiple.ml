(* TEST
   flags = "-drawlambda"
   * expect
*)

(* Successful flattening *)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let
  (*match*/92 = 3
   *match*/93 = 2
   *match*/94 = 1
   *match*/95 = *match*/92
   *match*/96 = *match*/93
   *match*/97 = *match*/94)
  (catch
    (catch
      (catch (if (!= *match*/96 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/95 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
- : bool = false
|}];;

(* Failed flattening: we need to allocate the tuple to bind x. *)

match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let
  (*match*/100 = 3
   *match*/101 = 2
   *match*/102 = 1
   *match*/103 = (makeblock 0 *match*/100 *match*/101 *match*/102))
  (catch
    (catch
      (let (*match*/104 =a (field_imm 0 *match*/103))
        (catch
          (let (*match*/105 =a (field_imm 1 *match*/103))
            (if (!= *match*/105 3) (exit 7)
              (let (*match*/106 =a (field_imm 2 *match*/103))
                (exit 5 *match*/103))))
         with (7)
          (if (!= *match*/104 1) (exit 6)
            (let
              (*match*/108 =a (field_imm 2 *match*/103)
               *match*/107 =a (field_imm 1 *match*/103))
              (exit 5 *match*/103)))))
     with (6) 0)
   with (5 x/98) (seq (ignore x/98) 1)))
- : bool = false
|}];;
