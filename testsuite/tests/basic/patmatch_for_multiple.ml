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
(let (*match*/92 = 3 *match*/93 = 2 *match*/94 = 1)
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
(let (*match*/97 = 3 *match*/98 = 2 *match*/99 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/98 3) (exit 6)
          (let (x/101 =a (makeblock 0 *match*/97 *match*/98 *match*/99))
            (exit 4 x/101)))
       with (6)
        (if (!= *match*/97 1) (exit 5)
          (let (x/100 =a (makeblock 0 *match*/97 *match*/98 *match*/99))
            (exit 4 x/100))))
     with (5) 0)
   with (4 x/95) (seq (ignore x/95) 1)))
- : bool = false
|}];;
