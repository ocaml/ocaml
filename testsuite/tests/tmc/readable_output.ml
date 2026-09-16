(* TEST
 flags = "-dlambda -dno-locations -dcanonical-ids";
 expect;
*)

(* Check that the code produced by TMC reads reasonably well. *)
let[@tail_mod_cons] rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs
;;
[%%expect{|
(letrec
  (map/0
     (function f/0 param/0 tail_mod_cons
       (if param/0
         (let
           (xs/0 =a (field_imm 1 param/0)
            x/0 =a (field_imm 0 param/0)
            block/0 = (makemutable 0 (apply f/0 x/0) 24029))
           (seq (apply map_dps/0 block/0 1 f/0 xs/0) block/0))
         0))
    map_dps/0
      (function dst/0 offset/0[int] f/1 param/1 tail_mod_cons
        (if param/1
          (let
            (xs/1 =a (field_imm 1 param/1)
             x/1 =a (field_imm 0 param/1)
             block0_arg0/0 = (apply f/1 x/1)
             block/1 = (makemutable 0 block0_arg0/0 24029))
            (seq (setfield_ptr(heap-init)_computed dst/0 offset/0 block/1)
              (apply map_dps/0 block/1 1 f/1 xs/1 tailcall)))
          (setfield_ptr(heap-init)_computed dst/0 offset/0 0))))
  (apply (field_mut 1 (global Toploop!)) "map" map/0))
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
  (rec_map/0
     (function f/2 param/2 tail_mod_cons
       (if param/2
         (let
           (*match*/0 =a (field_imm 0 param/2)
            tl/0 =a (field_imm 1 *match*/0)
            hd/0 =a (field_imm 0 *match*/0))
           (makeblock 0
             (let (block/2 = (makemutable 0 (apply f/2 hd/0) 24029))
               (seq (apply rec_map_dps/0 block/2 1 f/2 tl/0) block/2))))
         0))
    rec_map_dps/0
      (function dst/1 offset/1[int] f/3 param/3 tail_mod_cons
        (if param/3
          (let
            (*match*/1 =a (field_imm 0 param/3)
             tl/1 =a (field_imm 1 *match*/1)
             hd/1 =a (field_imm 0 *match*/1)
             block1_arg0/0 = (apply f/3 hd/1)
             block/3 = (makemutable 0 block1_arg0/0 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst/1 offset/1
                (makeblock 0 block/3))
              (apply rec_map_dps/0 block/3 1 f/3 tl/1 tailcall)))
          (setfield_ptr(heap-init)_computed dst/1 offset/1 0))))
  (apply (field_mut 1 (global Toploop!)) "rec_map" rec_map/0))
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
  (trip/0
     (function param/4 tail_mod_cons
       (if param/4
         (let (xs/2 =a (field_imm 1 param/4) x/2 =a (field_imm 0 param/4))
           (makeblock 0 (makeblock 0 (*,int) x/2 0)
             (makeblock 0 (makeblock 0 (*,int) x/2 1)
               (let
                 (block/4 = (makemutable 0 (makeblock 0 (*,int) x/2 2) 24029))
                 (seq (apply trip_dps/0 block/4 1 xs/2) block/4)))))
         0))
    trip_dps/0
      (function dst/2 offset/2[int] param/5 tail_mod_cons
        (if param/5
          (let
            (xs/3 =a (field_imm 1 param/5)
             x/3 =a (field_imm 0 param/5)
             block0_arg0/1 = (makeblock 0 (*,int) x/3 0)
             block1_arg0/1 = (makeblock 0 (*,int) x/3 1)
             block2_arg0/0 = (makeblock 0 (*,int) x/3 2)
             block/5 = (makemutable 0 block2_arg0/0 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst/2 offset/2
                (makeblock 0 block0_arg0/1
                  (makeblock 0 block1_arg0/1 block/5)))
              (apply trip_dps/0 block/5 1 xs/3 tailcall)))
          (setfield_ptr(heap-init)_computed dst/2 offset/2 0))))
  (apply (field_mut 1 (global Toploop!)) "trip" trip/0))
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
  (effects/0
     (function f/4 param/6 tail_mod_cons
       (if param/6
         (let
           (xs/4 =a (field_imm 1 param/6)
            *match*/2 =a (field_imm 0 param/6)
            y/0 =a (field_imm 1 *match*/2)
            x/4 =a (field_imm 0 *match*/2))
           (makeblock 0 (apply f/4 x/4)
             (let (block/6 = (makemutable 0 (apply f/4 y/0) 24029))
               (seq (apply effects_dps/0 block/6 1 f/4 xs/4) block/6))))
         0))
    effects_dps/0
      (function dst/3 offset/3[int] f/5 param/7 tail_mod_cons
        (if param/7
          (let
            (xs/5 =a (field_imm 1 param/7)
             *match*/3 =a (field_imm 0 param/7)
             y/1 =a (field_imm 1 *match*/3)
             x/5 =a (field_imm 0 *match*/3)
             block0_arg0/2 = (apply f/5 x/5)
             block1_arg0/2 = (apply f/5 y/1)
             block/7 = (makemutable 0 block1_arg0/2 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst/3 offset/3
                (makeblock 0 block0_arg0/2 block/7))
              (apply effects_dps/0 block/7 1 f/5 xs/5 tailcall)))
          (setfield_ptr(heap-init)_computed dst/3 offset/3 0))))
  (apply (field_mut 1 (global Toploop!)) "effects" effects/0))
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
  (map_stutter/0
     (function f/6 xs/6 tail_mod_cons
       (makeblock 0 (apply f/6 0)
         (if xs/6
           (let
             (xs/7 =a (field_imm 1 xs/6)
              x/6 =a (field_imm 0 xs/6)
              block/8 = (makemutable 0 (apply f/6 (makeblock 0 x/6)) 24029))
             (seq (apply map_stutter_dps/0 block/8 1 f/6 xs/7) block/8))
           0)))
    map_stutter_dps/0
      (function dst/4 offset/4[int] f/7 xs/8 tail_mod_cons
        (let
          (block0_arg0/3 = (apply f/7 0)
           block/9 = (makemutable 0 block0_arg0/3 24029))
          (seq (setfield_ptr(heap-init)_computed dst/4 offset/4 block/9)
            (if xs/8
              (let
                (xs/9 =a (field_imm 1 xs/8)
                 x/7 =a (field_imm 0 xs/8)
                 block0_arg0/4 = (apply f/7 (makeblock 0 x/7))
                 block/10 = (makemutable 0 block0_arg0/4 24029))
                (seq (setfield_ptr(heap-init)_computed block/9 1 block/10)
                  (apply map_stutter_dps/0 block/10 1 f/7 xs/9 tailcall)))
              (setfield_ptr(heap-init)_computed block/9 1 0))))))
  (apply (field_mut 1 (global Toploop!)) "map_stutter" map_stutter/0))
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
  (smap_stutter/0
     (function f/8 xs/10 n/0[int] tail_mod_cons
       (if (== n/0 0) 0
         (makeblock 0 (apply f/8 0)
           (let
             (v/0 = (apply f/8 (makeblock 0 (field_imm 0 xs/10)))
              block/11 = (makemutable 0 v/0 24029))
             (seq
               (apply smap_stutter_dps/0 block/11 1 f/8
                 (apply (field_imm 1 xs/10) 0) (- n/0 1))
               block/11)))))
    smap_stutter_dps/0
      (function dst/5 offset/5[int] f/9 xs/11 n/1[int] tail_mod_cons
        (if (== n/1 0) (setfield_ptr(heap-init)_computed dst/5 offset/5 0)
          (let
            (block0_arg0/5 = (apply f/9 0)
             v/1 = (apply f/9 (makeblock 0 (field_imm 0 xs/11)))
             block/12 = (makemutable 0 v/1 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst/5 offset/5
                (makeblock 0 block0_arg0/5 block/12))
              (apply smap_stutter_dps/0 block/12 1 f/9
                (apply (field_imm 1 xs/11) 0) (- n/1 1) tailcall))))))
  (apply (field_mut 1 (global Toploop!)) "smap_stutter" smap_stutter/0))
val smap_stutter : ('a option -> 'b) -> 'a stream -> int -> 'b list = <fun>
|}]
