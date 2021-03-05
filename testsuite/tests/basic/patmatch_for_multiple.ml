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
  (*match*/90 = 3
   *match*/91 = 2
   *match*/92 = 1
   *match*/93 = *match*/90
   *match*/94 = *match*/91
   *match*/95 = *match*/92)
  (catch
    (catch
      (catch (if (!= *match*/94 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/93 1) (exit 2) (exit 1)))
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
  (*match*/98 = 3
   *match*/99 = 2
   *match*/100 = 1
   *match*/101 = (makeblock 0 *match*/98 *match*/99 *match*/100))
  (catch
    (catch
      (let (*match*/102 =a (field_imm 0 *match*/101))
        (catch
          (let (*match*/103 =a (field_imm 1 *match*/101))
            (if (!= *match*/103 3) (exit 7)
              (let (*match*/104 =a (field_imm 2 *match*/101))
                (exit 5 *match*/101))))
         with (7)
          (if (!= *match*/102 1) (exit 6)
            (let
              (*match*/106 =a (field_imm 2 *match*/101)
               *match*/105 =a (field_imm 1 *match*/101))
              (exit 5 *match*/101)))))
     with (6) 0)
   with (5 x/96) (seq (ignore x/96) 1)))
- : bool = false
|}];;
