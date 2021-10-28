(* TEST
   flags = "-dlambda -dno-unique-ids"
   * expect *)

(* Check that the code produced by TMC reads reasonably well. *)
let[@tail_mod_cons] rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs
;;
[%%expect{|
(letrec
  (map
     (function f param tail_mod_cons
       (if param
         (let (block = (makemutable 0 (apply f (field 0 param)) 24029))
           (seq (apply map_dps block 1 f (field 1 param)) block))
         0))
    map_dps
      (function dst offset[int] f param tail_mod_cons
        (if param
          (let
            (block0_arg0 = (apply f (field 0 param))
             block = (makemutable 0 block0_arg0 24029))
            (seq (setfield_ptr(heap-init)_computed dst offset block)
              (apply map_dps block 1 f (field 1 param) tailcall)))
          (setfield_ptr(heap-init)_computed dst offset 0))))
  (apply (field 1 (global Toploop!)) "map" map))
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
|}]

(* check that TMC works for records as well *)
type 'a cell = { hd : 'a; tl : 'a rec_list }
and 'a rec_list = 'a cell option
[%%expect{|
0
type 'a cell = { hd : 'a; tl : 'a rec_list; }
and 'a rec_list = 'a cell option
|}]

let[@tail_mod_cons] rec rec_map f = function
  | None -> None
  | Some {hd; tl} -> Some { hd = f hd; tl = rec_map f tl }
;;
[%%expect{|
(letrec
  (rec_map
     (function f param tail_mod_cons
       (if param
         (let (*match* =a (field 0 param))
           (makeblock 0
             (let (block = (makemutable 0 (apply f (field 0 *match*)) 24029))
               (seq (apply rec_map_dps block 1 f (field 1 *match*)) block))))
         0))
    rec_map_dps
      (function dst offset[int] f param tail_mod_cons
        (if param
          (let
            (*match* =a (field 0 param)
             block1_arg0 = (apply f (field 0 *match*))
             block = (makemutable 0 block1_arg0 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst offset
                (makeblock 0 block))
              (apply rec_map_dps block 1 f (field 1 *match*) tailcall)))
          (setfield_ptr(heap-init)_computed dst offset 0))))
  (apply (field 1 (global Toploop!)) "rec_map" rec_map))
val rec_map : ('a -> 'b) -> 'a rec_list -> 'b rec_list = <fun>
|}]

(* check the case where several constructors are nested;
   we want to avoid creating an intermediate destination
   for each constructor.  *)
let[@tail_mod_cons] rec trip = function
  | [] -> []
  | x :: xs -> (x, 0) :: (x, 1) :: (x, 2) :: trip xs
;;
[%%expect{|
(letrec
  (trip
     (function param tail_mod_cons
       (if param
         (let (x =a (field 0 param))
           (makeblock 0 (makeblock 0 (*,int) x 0)
             (makeblock 0 (makeblock 0 (*,int) x 1)
               (let (block = (makemutable 0 (makeblock 0 (*,int) x 2) 24029))
                 (seq (apply trip_dps block 1 (field 1 param)) block)))))
         0))
    trip_dps
      (function dst offset[int] param tail_mod_cons
        (if param
          (let
            (x =a (field 0 param)
             block0_arg0 = (makeblock 0 (*,int) x 0)
             block1_arg0 = (makeblock 0 (*,int) x 1)
             block2_arg0 = (makeblock 0 (*,int) x 2)
             block = (makemutable 0 block2_arg0 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst offset
                (makeblock 0 block0_arg0 (makeblock 0 block1_arg0 block)))
              (apply trip_dps block 1 (field 1 param) tailcall)))
          (setfield_ptr(heap-init)_computed dst offset 0))))
  (apply (field 1 (global Toploop!)) "trip" trip))
val trip : 'a list -> ('a * int) list = <fun>
|}]

(* check nested-constructors whose arguments
   are effectful: they need to be let-bound appropriately
   (ideally, only in the DPS version) *)
let[@tail_mod_cons] rec effects f = function
  | [] -> []
  | (x, y) :: xs -> f x :: f y :: effects f xs
;;
[%%expect{|
(letrec
  (effects
     (function f param tail_mod_cons
       (if param
         (let (*match* =a (field 0 param))
           (makeblock 0 (apply f (field 0 *match*))
             (let (block = (makemutable 0 (apply f (field 1 *match*)) 24029))
               (seq (apply effects_dps block 1 f (field 1 param)) block))))
         0))
    effects_dps
      (function dst offset[int] f param tail_mod_cons
        (if param
          (let
            (*match* =a (field 0 param)
             block0_arg0 = (apply f (field 0 *match*))
             block1_arg0 = (apply f (field 1 *match*))
             block = (makemutable 0 block1_arg0 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst offset
                (makeblock 0 block0_arg0 block))
              (apply effects_dps block 1 f (field 1 param) tailcall)))
          (setfield_ptr(heap-init)_computed dst offset 0))))
  (apply (field 1 (global Toploop!)) "effects" effects))
val effects : ('a -> 'b) -> ('a * 'a) list -> 'b list = <fun>
|}]

(* Check the case where several constructors
   are nested across a duplicating context: the [f None ::]
   part should not be duplicated in each branch. *)
let[@tail_mod_cons] rec map_stutter f xs =
  f None :: (
    match xs with
    | [] -> []
    | x :: xs -> f (Some x) :: map_stutter f xs
  )
;;
[%%expect{|
(letrec
  (map_stutter
     (function f xs tail_mod_cons
       (makeblock 0 (apply f 0)
         (if xs
           (let
             (block =
                (makemutable 0 (apply f (makeblock 0 (field 0 xs))) 24029))
             (seq (apply map_stutter_dps block 1 f (field 1 xs)) block))
           0)))
    map_stutter_dps
      (function dst offset[int] f xs tail_mod_cons
        (let
          (block0_arg0 = (apply f 0)
           block = (makemutable 0 block0_arg0 24029))
          (seq (setfield_ptr(heap-init)_computed dst offset block)
            (if xs
              (let
                (block0_arg0 = (apply f (makeblock 0 (field 0 xs)))
                 block = (makemutable 0 block0_arg0 24029))
                (seq (setfield_ptr(heap-init)_computed block 1 block)
                  (apply map_stutter_dps block 1 f (field 1 xs) tailcall)))
              (setfield_ptr(heap-init)_computed block 1 0))))))
  (apply (field 1 (global Toploop!)) "map_stutter" map_stutter))
val map_stutter : ('a option -> 'b) -> 'a list -> 'b list = <fun>
|}]

(* Check the case where several constructors
   are nested across a non-duplicating context;
   the [f None :: .] part can be delayed below the let..in,
   buts it expression argument must be let-bound
   before the let..in is evaluated. *)
type 'a stream = { hd : 'a; tl : unit -> 'a stream }
let[@tail_mod_cons] rec smap_stutter f xs n =
  if n = 0 then []
  else f None :: (
    let v = f (Some xs.hd) in
    v :: smap_stutter f (xs.tl ()) (n - 1)
  )
;;
[%%expect{|
0
type 'a stream = { hd : 'a; tl : unit -> 'a stream; }
(letrec
  (smap_stutter
     (function f xs n[int] tail_mod_cons
       (if (== n 0) 0
         (makeblock 0 (apply f 0)
           (let
             (v = (apply f (makeblock 0 (field 0 xs)))
              block = (makemutable 0 v 24029))
             (seq
               (apply smap_stutter_dps block 1 f (apply (field 1 xs) 0)
                 (- n 1))
               block)))))
    smap_stutter_dps
      (function dst offset[int] f xs n[int] tail_mod_cons
        (if (== n 0) (setfield_ptr(heap-init)_computed dst offset 0)
          (let
            (block0_arg0 = (apply f 0)
             v = (apply f (makeblock 0 (field 0 xs)))
             block = (makemutable 0 v 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst offset
                (makeblock 0 block0_arg0 block))
              (apply smap_stutter_dps block 1 f (apply (field 1 xs) 0)
                (- n 1) tailcall))))))
  (apply (field 1 (global Toploop!)) "smap_stutter" smap_stutter))
val smap_stutter : ('a option -> 'b) -> 'a stream -> int -> 'b list = <fun>
|}]
