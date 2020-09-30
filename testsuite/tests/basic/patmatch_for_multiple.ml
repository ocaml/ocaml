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
  (*match*/88 = 3
   *match*/89 = 2
   *match*/90 = 1
   *match*/91 = *match*/88
   *match*/92 = *match*/89
   *match*/93 = *match*/90)
  (catch
    (catch
      (catch (if (!= *match*/92 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/91 1) (exit 2) (exit 1)))
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
  (*match*/96 = 3
   *match*/97 = 2
   *match*/98 = 1
   *match*/99 = (makeblock 0 *match*/96 *match*/97 *match*/98))
  (catch
    (catch
      (let (*match*/100 =a (field 0 *match*/99))
        (catch
          (let (*match*/101 =a (field 1 *match*/99))
            (if (!= *match*/101 3) (exit 7)
              (let (*match*/102 =a (field 2 *match*/99)) (exit 5 *match*/99))))
         with (7)
          (if (!= *match*/100 1) (exit 6)
            (let
              (*match*/104 =a (field 2 *match*/99)
               *match*/103 =a (field 1 *match*/99))
              (exit 5 *match*/99)))))
     with (6) 0)
   with (5 x/94) (seq (ignore x/94) 1)))
- : bool = false
|}];;
