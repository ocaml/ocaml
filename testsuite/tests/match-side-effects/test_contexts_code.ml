(* TEST
 readonly_files = "contexts_1.ml contexts_2.ml contexts_3.ml";
 flags = "-dsource -dlambda";
 expect;
*)

#use "contexts_1.ml";;
(* Notice that (field_mut 1 input) occurs twice, it
   is evaluated once in the 'false' branch and once in the 'true'
   branch. The compiler does not assume that its static knowledge about the
   first read (it cannot be a [Right] as we already matched against it
   and failed) also applies to the second read, and it inserts a Match_failure
   case if [Right] is read again.
*)
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
  (example_1/311 =
     (function param/335[int]
       (let (input/313 = (makemutable 0 (int,*) 1 [0: 1]))
         (if (field_int 0 input/313)
           (let (*match*/338 =o (field_mut 1 input/313))
             (switch* *match*/338
              case tag 0:
               (if (seq (setfield_ptr 1 input/313 [1: 3]) 0) [1: 3]
                 (let (*match*/340 =o (field_mut 1 input/313))
                   (switch* *match*/340
                    case tag 0: (makeblock 0 (int) (field_imm 0 *match*/340))
                    case tag 1:
                     (raise
                       (makeblock 0 (global Match_failure/20!)
                         [0: "contexts_1.ml" 17 2])))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_1" example_1/311))
val example_1 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_2.ml";;
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
  (example_2/347 =
     (function param/351[int]
       (let (input/349 = (makeblock 0 (int,*) 1 (makemutable 0 [0: 1])))
         (if (field_int 0 input/349)
           (let (*match*/355 =o (field_mut 0 (field_imm 1 input/349)))
             (switch* *match*/355
              case tag 0:
               (if (seq (setfield_ptr 0 (field_imm 1 input/349) [1: 3]) 0)
                 [1: 3]
                 (let (*match*/358 =o (field_mut 0 (field_imm 1 input/349)))
                   (switch* *match*/358
                    case tag 0: (makeblock 0 (int) (field_imm 0 *match*/358))
                    case tag 1:
                     (raise
                       (makeblock 0 (global Match_failure/20!)
                         [0: "contexts_2.ml" 11 2])))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_2" example_2/347))
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
  (example_3/364 =
     (function param/368[int]
       (let (input/366 =mut [0: 1 [0: 1]] *match*/369 =o *input/366)
         (if (field_imm 0 *match*/369)
           (switch* (field_imm 1 *match*/369)
            case tag 0:
             (if (seq (assign input/366 [0: 1 [1: 3]]) 0) [1: 3]
               (makeblock 0 (int) (field_imm 0 (field_imm 1 *match*/369))))
            case tag 1: [1: 2])
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_3" example_3/364))
val example_3 : unit -> (bool, int) Result.t = <fun>
|}]
