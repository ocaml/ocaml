(* TEST
   flags = "-dlambda -dno-locations -dno-unique-ids";
   expect;
*)

(* Basic usage: redefine atomics. *)

module Basic = struct
  type 'a atomic = { mutable filler: unit; mutable x : 'a [@atomic] }

  let get (type a) (r : a atomic) : a = r.x

  let set (type a) (r : a atomic) (v : a) : unit = r.x <- v

  let cas (type a) (r : a atomic) oldv newv =
    Atomic.Loc.compare_and_set [%atomic.loc r.x] oldv newv

  let[@inline never] get_loc (type a) (r : a atomic) : a Atomic.Loc.t =
    [%atomic.loc r.x]

  let slow_cas (type a) (r : a atomic) oldv newv =
    Atomic.Loc.compare_and_set (get_loc r) oldv newv
end
[%%expect{|
(apply (field_mut 1 (global Toploop!)) "Basic/336"
  (let
    (get = (function r (atomic_load r 1))
     set = (function r v : int (ignore (caml_atomic_exchange_field r 1 v)))
     cas = (function r oldv newv : int (caml_atomic_cas_field r 1 oldv newv))
     get_loc = (function r never_inline (makeblock 0 (*,int) r 1))
     slow_cas =
       (function r oldv newv : int
         (let (atomic_arg = (apply get_loc r))
           (caml_atomic_cas_field (field_imm 0 atomic_arg)
             (field_int 1 atomic_arg) oldv newv))))
    (makeblock 0 get set cas get_loc slow_cas)))
module Basic :
  sig
    type 'a atomic = { mutable filler : unit; mutable x : 'a [@atomic]; }
    val get : 'a atomic -> 'a
    val set : 'a atomic -> 'a -> unit
    val cas : 'a atomic -> 'a -> 'a -> bool
    val get_loc : 'a atomic -> 'a Atomic.Loc.t
    val slow_cas : 'a atomic -> 'a -> 'a -> bool
  end
|}];;


(* Atomic fields must be mutable. *)
module Error1 = struct
  type t = { x : int [@atomic] }
end
[%%expect{|
Line 2, characters 13-30:
2 |   type t = { x : int [@atomic] }
                 ^^^^^^^^^^^^^^^^^
Error: The label "x" must be mutable to be declared atomic.
|}];;


(* [%atomic.loc _] payload must be a record field access *)
module Error2 = struct
  type t = { mutable x : int [@atomic] }
  let f t = [%atomic.loc t]
end
[%%expect{|
Line 3, characters 12-27:
3 |   let f t = [%atomic.loc t]
                ^^^^^^^^^^^^^^^
Error: Invalid "[%atomic.loc]" payload, a record field access is expected.
|}];;


(* [%atomic.loc _] only works on atomic fields *)
module Error3 = struct
  type t = { x : int }
  let f t = [%atomic.loc t.x]
end
[%%expect{|
Line 3, characters 12-29:
3 |   let f t = [%atomic.loc t.x]
                ^^^^^^^^^^^^^^^^^
Error: The record field "x" is not atomic
|}];;


(* Check module interface checking: it is not allowed to remove or add
   atomic attributes. *)

module Wrong1 = (struct
  type t = { mutable x : int }
end : sig
  (* adding an 'atomic' attribute missing in the implementation: invalid. *)
  type t = { mutable x : int [@atomic] }
end)
[%%expect{|
Lines 1-3, characters 17-3:
1 | .................struct
2 |   type t = { mutable x : int }
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int; } end
       is not included in
         sig type t = { mutable x : int [@atomic]; } end
       Type declarations do not match:
         type t = { mutable x : int; }
       is not included in
         type t = { mutable x : int [@atomic]; }
       Fields do not match:
         "mutable x : int;"
       is not the same as:
         "mutable x : int [@atomic];"
       The second is atomic and the first is not.
|}];;

module Wrong2 = (struct
  type t = { mutable x : int [@atomic] }
end : sig
  (* removing an 'atomic' attribute present in the implementation: invalid. *)
  type t = { mutable x : int }
end)
[%%expect{|
Lines 1-3, characters 17-3:
1 | .................struct
2 |   type t = { mutable x : int [@atomic] }
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int [@atomic]; } end
       is not included in
         sig type t = { mutable x : int; } end
       Type declarations do not match:
         type t = { mutable x : int [@atomic]; }
       is not included in
         type t = { mutable x : int; }
       Fields do not match:
         "mutable x : int [@atomic];"
       is not the same as:
         "mutable x : int;"
       The first is atomic and the second is not.
|}];;

module Ok = (struct
  type t = { mutable x : int [@atomic] }
end : sig
  type t = { mutable x : int [@atomic] }
end)
[%%expect{|
(apply (field_mut 1 (global Toploop!)) "Ok/363" (makeblock 0))
module Ok : sig type t = { mutable x : int [@atomic]; } end
|}];;



(* Inline records are supported, including in extensions. *)

module Inline_record = struct
  type t = A of { mutable x : int [@atomic] }

  let test : t -> int = fun (A r) -> r.x
end
[%%expect{|
(apply (field_mut 1 (global Toploop!)) "Inline_record/371"
  (let (test = (function param : int (atomic_load param 0)))
    (makeblock 0 test)))
module Inline_record :
  sig type t = A of { mutable x : int [@atomic]; } val test : t -> int end
|}];;

module Extension_with_inline_record = struct
  type t = ..
  type t += A of { mutable x : int [@atomic] }

  (* one should see in the -dlambda output below that the field offset is not 0
     as one could expect, but 1, due to an extra argument in extensible variants. *)
  let test : t -> int = function
    | A r -> r.x
    | _ -> 0

  let () = assert (test (A { x = 42 }) = 42)
end
[%%expect{|
(apply (field_mut 1 (global Toploop!)) "Extension_with_inline_record/379"
  (let
    (A =
       (makeblock 248 "Extension_with_inline_record.A" (caml_fresh_oo_id 0))
     test =
       (function param : int
         (if (== (field_imm 0 param) A) (atomic_load param 1) 0))
     *match* =
       (if (== (apply test (makemutable 0 (*,int) A 42)) 42) 0
         (raise (makeblock 0 (global Assert_failure!) [0: "" 11 11]))))
    (makeblock 0 A test)))
module Extension_with_inline_record :
  sig
    type t = ..
    type t += A of { mutable x : int [@atomic]; }
    val test : t -> int
  end
|}];;


(* Marking a field [@atomic] in a float-only record disables the unboxing optimization. *)
module Float_records = struct
  type t = { x : float; mutable y : float [@atomic] }

  (* one should see in the -dlambda output below that this creates a block of tag 0. *)
  let mk_t x y : t = { x; y }
  let get v = v.y
end
[%%expect{|
(apply (field_mut 1 (global Toploop!)) "Float_records/394"
  (let
    (mk_t = (function x[float] y[float] (makemutable 0 (float,float) x y))
     get = (function v : float (atomic_load v 1)))
    (makeblock 0 mk_t get)))
module Float_records :
  sig
    type t = { x : float; mutable y : float [@atomic]; }
    val mk_t : float -> float -> t
    val get : t -> float
  end
|}];;
