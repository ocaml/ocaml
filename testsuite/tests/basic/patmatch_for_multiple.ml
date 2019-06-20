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
  (*match*/87 = 3
   *match*/88 = 2
   *match*/89 = 1
   *match*/90 = *match*/87
   *match*/91 = *match*/88
   *match*/92 = *match*/89)
  (catch
    (catch
      (catch (if (!= *match*/91 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/90 1) (exit 2) (exit 1)))
     with (2) 0a)
   with (1) 1a))
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
  (*match*/95 = 3
   *match*/96 = 2
   *match*/97 = 1
   *match*/98 = (makeblock 0 *match*/95 *match*/96 *match*/97))
  (catch
    (catch
      (let (*match*/99 =a (field 0 *match*/98))
        (catch
          (let (*match*/100 =a (field 1 *match*/98))
            (if (!= *match*/100 3) (exit 7)
              (let (*match*/101 =a (field 2 *match*/98)) (exit 5 *match*/98))))
         with (7)
          (if (!= *match*/99 1) (exit 6)
            (let
              (*match*/103 =a (field 2 *match*/98)
               *match*/102 =a (field 1 *match*/98))
              (exit 5 *match*/98)))))
     with (6) 0a)
   with (5 x/93) (seq (ignore x/93) 1a)))
- : bool = false
|}];;

