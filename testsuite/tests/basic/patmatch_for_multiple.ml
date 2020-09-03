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
  (*match*/89 = 3
   *match*/90 = 2
   *match*/91 = 1
   *match*/92 = *match*/89
   *match*/93 = *match*/90
   *match*/94 = *match*/91)
  (catch
    (catch
      (catch (if (!= *match*/93 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/92 1) (exit 2) (exit 1)))
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
  (*match*/97 = 3
   *match*/98 = 2
   *match*/99 = 1
   *match*/100 = (makeblock 0 *match*/97 *match*/98 *match*/99))
  (catch
    (catch
      (let (*match*/101 =a (field 0 *match*/100))
        (catch
          (let (*match*/102 =a (field 1 *match*/100))
            (if (!= *match*/102 3) (exit 7)
              (let (*match*/103 =a (field 2 *match*/100))
                (exit 5 *match*/100))))
         with (7)
          (if (!= *match*/101 1) (exit 6)
            (let
              (*match*/105 =a (field 2 *match*/100)
               *match*/104 =a (field 1 *match*/100))
              (exit 5 *match*/100)))))
     with (6) 0)
   with (5 x/95) (seq (ignore x/95) 1)))
- : bool = false
|}];;
