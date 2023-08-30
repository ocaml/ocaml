(* TEST
 readonly_files = "contexts_1.ml contexts_2.ml contexts_3.ml";
 flags = "-dsource -dlambda";
 expect;
*)

#use "contexts_1.ml";;
[%%expect {|

#use  "contexts_1.ml";;

type u = {
  a: bool ;
  mutable b: (bool, int) Either.t };;
0
type u = { a : bool; mutable b : (bool, int) Either.t; }

let example_1 () =
  let input = { a = true; b = (Either.Left true) } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = Either.Right _ } -> Result.Error 2
  | { a = _; b = _ } when input.b <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = Either.Left y } -> Result.Ok y;;
(let
  (example_1/309 =
     (function param/333[int]
       (let
         (input/311 = (makemutable 0 (int,*) 1 [0: 1])
          *strict*/334 =o (field_mut 1 input/311))
         (if (field_int 0 input/311)
           (switch* *strict*/334
            case tag 0:
             (if (seq (setfield_ptr 1 input/311 [1: 3]) 0) [1: 3]
               (makeblock 0 (int) (field_imm 0 *strict*/334)))
            case tag 1: [1: 2])
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_1" example_1/309))
val example_1 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_2.ml";;
(* Notice that (field_mut 0 (field_imm 1 input)) occurs twice, it is
   evaluated once in the 'false' branch and once in the 'true'
   branch. On the second case, the compiler assumes that matching on
   (Either.Left y) only is exhaustive -- because the type-checker says
   so -- and does not generate a constructor test and Match_failure
   case, which is unsound as the value can be mutated to be
   Either.Right.
*)
[%%expect {|

#use  "contexts_2.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = {
  a: bool ;
  b: (bool, int) Either.t myref };;
0
type u = { a : bool; b : (bool, int) Either.t myref; }

let example_2 () =
  let input = { a = true; b = { mut = (Either.Left true) } } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = { mut = Either.Right _ } } -> Result.Error 2
  | { a = _; b = _ } when (input.b).mut <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = { mut = Either.Left y } } -> Result.Ok y;;
(let
  (example_2/343 =
     (function param/347[int]
       (let (input/345 = (makeblock 0 (int,*) 1 (makemutable 0 [0: 1])))
         (if (field_int 0 input/345)
           (let (*strict*/351 =o (field_mut 0 (field_imm 1 input/345)))
             (switch* *strict*/351
              case tag 0:
               (if (seq (setfield_ptr 0 (field_imm 1 input/345) [1: 3]) 0)
                 [1: 3]
                 (let (*strict*/354 =o (field_mut 0 (field_imm 1 input/345)))
                   (makeblock 0 (int) (field_imm 0 *strict*/354))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_2" example_2/343))
val example_2 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_3.ml";;
[%%expect {|

#use  "contexts_3.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = (bool * (bool, int) Either.t) myref;;
0
type u = (bool * (bool, int) Either.t) myref

let example_3 () =
  let input = { mut = (true, (Either.Left true)) } in
  match input with
  | { mut = (false, _) } -> Result.Error 1
  | { mut = (_, Either.Right _) } -> Result.Error 2
  | { mut = (_, _) } when input.mut <- (true, (Either.Right 3)); false ->
      Result.Error 3
  | { mut = (true, Either.Left y) } -> Result.Ok y;;
(let
  (example_3/360 =
     (function param/364[int]
       (let (input/362 =mut [0: 1 [0: 1]] *strict*/365 =o *input/362)
         (if (field_imm 0 *strict*/365)
           (switch* (field_imm 1 *strict*/365)
            case tag 0:
             (if (seq (assign input/362 [0: 1 [1: 3]]) 0) [1: 3]
               (makeblock 0 (int) (field_imm 0 (field_imm 1 *strict*/365))))
            case tag 1: [1: 2])
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_3" example_3/360))
val example_3 : unit -> (bool, int) Result.t = <fun>
|}]
