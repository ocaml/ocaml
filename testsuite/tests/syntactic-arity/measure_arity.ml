(* TEST
flags = "-w -5-8";
expect;
*)

(* A series of tests that observe the syntactic arity of a function,
   by constructing functions whose pattern matching involves side
   effects.

   The tests are the same in each numbered section, so please change
   them everywhere if you change them in one place.
 *)


(* I. Measuring arity with lazy patterns *)

type t = int lazy_t

let measure_arity f =
  let exception Arity of int in
  let bot = lazy (assert false) in
  let ret d = lazy (raise (Arity d)) in
  try
    f (ret 1);
    f (ret 2) bot;
    f (ret 3) bot bot;
    f (ret 4) bot bot bot;
    failwith "arity too large"
  with
  | Arity d -> d
;;

[%%expect{|
type t = int lazy_t
val measure_arity :
  ('a lazy_t -> 'b lazy_t -> 'c lazy_t -> 'd lazy_t -> 'e) -> int = <fun>
|}];;

let unary (lazy _ : t) = assert false;;
let arity = measure_arity unary;;
[%%expect{|
val unary : t -> 'a = <fun>
val arity : int = 1
|}];;

let fun_lambda = fun (lazy _ : t) (lazy _ : t) -> assert false;;
let arity = measure_arity fun_lambda;;
[%%expect{|
val fun_lambda : t -> t -> 'a = <fun>
val arity : int = 2
|}];;

let nested_arity = fun (lazy _ : t) -> fun (lazy _ : t) -> assert false;;
let arity1 = measure_arity nested_arity;;
let arity2 = measure_arity (nested_arity (lazy 0));;
[%%expect{|
val nested_arity : t -> t -> 'a = <fun>
val arity1 : int = 1
val arity2 : int = 1
|}];;

let fun_lambda_with_function_body =
  fun (lazy _ : t) (lazy _ : t) -> function (lazy _ : t) -> assert false;;
let arity = measure_arity fun_lambda_with_function_body;;
[%%expect{|
val fun_lambda_with_function_body : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_single_case (lazy _ : t) (lazy _ : t) = function
  | (lazy _ : t) -> assert false;;
let arity = measure_arity function_body_single_case;;
[%%expect{|
val function_body_single_case : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_constraint (lazy _ : t) (lazy _ : t) : _ = function
  | (lazy _ : t) -> assert false;;
let arity = measure_arity function_body_constraint;;
[%%expect{|
val function_body_constraint : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_multiple_cases (lazy _ : t) (lazy _ : t) = function
  | (lazy 2) -> (fun (lazy _ : t) -> assert false)
  | (lazy _ : t) -> (fun (lazy _ : t) -> assert false);;
let arity = measure_arity function_body_multiple_cases;;
[%%expect{|
val function_body_multiple_cases : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_constraint_multiple_cases (lazy _ : t) (lazy _ : t) : _ =
  function
  | (lazy 2) -> (fun (lazy _ : t) -> assert false)
  | (lazy _ : t) -> (fun (lazy _ : t) -> assert false)
let arity = measure_arity function_body_constraint_multiple_cases
[%%expect{|
val function_body_constraint_multiple_cases : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_coercion (lazy _ : t) (lazy _ : t) :> _ = function
  | (lazy 2) -> (fun (lazy _ : t) -> assert false)
  | (lazy _ : t) -> (fun (lazy _ : t) -> assert false)
let arity = measure_arity function_body_coercion
[%%expect{|
val function_body_coercion : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_coercion_constraint (lazy _ : t) (lazy _ : t) : _ :> _ =
  function
  | (lazy 2) -> (fun (lazy _ : t) -> assert false)
  | (lazy _ : t) -> (fun (lazy _ : t) -> assert false)
let arity = measure_arity function_body_coercion_constraint
[%%expect{|
val function_body_coercion_constraint : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

(* partial application: applying a function with arity 3 yields a
  function with arity 2. *)
let partial_application (lazy _ : t) (lazy _ : t) = function
  | (lazy 2) -> (fun (lazy _ : t) -> assert false)
  | (lazy _ : t) -> (fun (lazy _ : t) -> assert false)
let arity1 = measure_arity partial_application
let arity2 = measure_arity (partial_application (lazy 0))
[%%expect{|
val partial_application : t -> t -> t -> t -> 'a = <fun>
val arity1 : int = 3
val arity2 : int = 2
|}];;

(* newtype doesn't interrupt arity *)
let f1 (type a) (lazy _ : t) (lazy _ : t) = (assert false : a)
let f2 (lazy _ : t) (type a) (lazy _ : t) = (assert false : a)
let f3 (lazy _ : t) (lazy _ : t) (type a) = (assert false : a)
let f4  (type a a) (lazy _ : t) (type a a) (lazy _ : t) (type a a) =
  assert false
let arity1 = measure_arity f1
let arity2 = measure_arity f2
let arity3 = measure_arity f3
let arity4 = measure_arity f4
[%%expect{|
val f1 : t -> t -> 'a = <fun>
val f2 : t -> t -> 'a = <fun>
val f3 : t -> t -> 'a = <fun>
val f4 : t -> t -> 'a = <fun>
val arity1 : int = 2
val arity2 : int = 2
val arity3 : int = 2
val arity4 : int = 2
|}];;

(* II. Measuring arity with non-exhaustive pattern matches  *)

type t = A | B | C

let measure_arity f =
  let exception Arity of int in
  let go arity g =
    try g () with Match_failure _ -> raise (Arity arity)
  in
  try
    go 1 (fun () -> f B);
    go 2 (fun () -> f B B);
    go 3 (fun () -> f B B B);
    go 4 (fun () -> f B B B B);
    failwith "arity too large"
  with
  | Arity arity -> arity
;;

[%%expect{|
type t = A | B | C
val measure_arity : (t -> t -> t -> t -> 'a) -> int = <fun>
|}];;

let unary A = assert false;;
let arity = measure_arity unary;;
[%%expect{|
val unary : t -> 'a = <fun>
val arity : int = 1
|}];;

let fun_lambda = fun A A -> assert false;;
let arity = measure_arity fun_lambda;;
[%%expect{|
val fun_lambda : t -> t -> 'a = <fun>
val arity : int = 2
|}];;

let nested_arity = fun A -> fun A -> assert false;;
let arity1 = measure_arity nested_arity;;
let arity2 = measure_arity (nested_arity A);;
[%%expect{|
val nested_arity : t -> t -> 'a = <fun>
val arity1 : int = 1
val arity2 : int = 1
|}];;

let fun_lambda_with_function_body =
  fun A A -> function A -> assert false;;
let arity = measure_arity fun_lambda_with_function_body;;
[%%expect{|
val fun_lambda_with_function_body : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_single_case A A = function
  | A -> assert false;;
let arity = measure_arity function_body_single_case;;
[%%expect{|
val function_body_single_case : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_constraint A A : _ = function
  | A -> assert false;;
let arity = measure_arity function_body_constraint;;
[%%expect{|
val function_body_constraint : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_multiple_cases A A = function
  | C -> (fun A -> assert false)
  | A -> (fun A -> assert false);;
let arity = measure_arity function_body_multiple_cases;;
[%%expect{|
val function_body_multiple_cases : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_constraint_multiple_cases A A : _ = function
  | C -> (fun A -> assert false)
  | A -> (fun A -> assert false)
let arity = measure_arity function_body_constraint_multiple_cases
[%%expect{|
val function_body_constraint_multiple_cases : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_coercion A A :> _ = function
  | C -> (fun A -> assert false)
  | A -> (fun A -> assert false)
let arity = measure_arity function_body_coercion
[%%expect{|
val function_body_coercion : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_coercion_constraint A A : _ :> _ = function
  | C -> (fun A -> assert false)
  | A -> (fun A -> assert false)
let arity = measure_arity function_body_coercion_constraint
[%%expect{|
val function_body_coercion_constraint : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

(* partial application: applying a function with arity 3 yields a
  function with arity 2. *)
let partial_application A A = function
  | C -> (fun A -> assert false)
  | A -> (fun A -> assert false)
let arity1 = measure_arity partial_application
let arity2 = measure_arity (partial_application A)
[%%expect{|
val partial_application : t -> t -> t -> t -> 'a = <fun>
val arity1 : int = 3
val arity2 : int = 2
|}];;

(* newtype doesn't interrupt arity *)
let f1 (type a) A A = (assert false : a)
let f2 A (type a) A = (assert false : a)
let f3 A A (type a) = (assert false : a)
let f4  (type a a) A (type a a) A (type a a) = assert false
let arity1 = measure_arity f1
let arity2 = measure_arity f2
let arity3 = measure_arity f3
let arity4 = measure_arity f4
[%%expect{|
val f1 : t -> t -> 'a = <fun>
val f2 : t -> t -> 'a = <fun>
val f3 : t -> t -> 'a = <fun>
val f4 : t -> t -> 'a = <fun>
val arity1 : int = 2
val arity2 : int = 2
val arity3 : int = 2
val arity4 : int = 2
|}];;

(* III. Measuring arity with mutable pattern matches  *)

exception Return of int

let return x = raise (Return x)

type t = { mutable x : int }
type nothing = |

let measure_arity f =
  let exception Arity of int in
  (* Measure arity by observing differences between the
     behavior of these applications:

     f  {x=2} {x=2} ... {x=2}
     f !!t     t  !!...  t

     Where t.x is set to 1 between the !!s, and set to 2 outside
     of the !!s.

     In the above example, if [f] is 2-ary, it will read the value of t.x as 1
     for the first two arguments. If it's 3-ary, it will read the value as 2.
  *)
  let go arity ~while_x_is_1 ~while_x_is_2 =
    let t = { x = 2 } in
    let answer1 =
      match f {x=2} {x=2} {x=2} {x=2} {x=2} with
      | exception Return r -> r
      | (_ : nothing) -> .
    in
    t.x <- 1;
    let answer2 =
      match while_x_is_1 f t with
      | exception Return r -> r
      | f' ->
          t.x <- 2;
          match while_x_is_2 f' t with
          | exception Return r -> r
          | (_ : nothing) -> .
    in
    if answer1 <> answer2
    then raise (Arity arity)
  in
  try
    let one   f t = f t in
    let two   f t = f t t in
    let three f t = f t t t in
    let four  f t = f t t t t in
    go 1 ~while_x_is_1:one   ~while_x_is_2:four;
    go 2 ~while_x_is_1:two   ~while_x_is_2:three;
    go 3 ~while_x_is_1:three ~while_x_is_2:two;
    go 4 ~while_x_is_1:four  ~while_x_is_2:one;
    failwith "arity too large"
  with
  | Arity arity -> arity
;;

[%%expect{|
exception Return of int
val return : int -> 'a = <fun>
type t = { mutable x : int; }
type nothing = |
val measure_arity : (t -> t -> t -> t -> t -> nothing) -> int = <fun>
|}];;

let unary {x} = return x;;
let arity = measure_arity unary;;
[%%expect{|
val unary : t -> 'a = <fun>
val arity : int = 1
|}];;

let fun_lambda = fun {x=x1} {x=x2} -> return (x1+x2);;
let arity = measure_arity fun_lambda;;
[%%expect{|
val fun_lambda : t -> t -> 'a = <fun>
val arity : int = 2
|}];;

let nested_arity = fun {x=x1} -> fun {x=x2} -> return (x1+x2);;
let arity1 = measure_arity nested_arity;;
let arity2 = measure_arity (nested_arity {x=7});;
[%%expect{|
val nested_arity : t -> t -> 'a = <fun>
val arity1 : int = 1
val arity2 : int = 1
|}];;

let fun_lambda_with_function_body =
  fun {x=x1} {x=x2} -> function {x=x3} -> return (x1+x2+x3);;
let arity = measure_arity fun_lambda_with_function_body;;
[%%expect{|
val fun_lambda_with_function_body : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_single_case {x=x1} {x=x2} = function
  | {x=x3} -> return (x1+x2+x3);;
let arity = measure_arity function_body_single_case;;
[%%expect{|
val function_body_single_case : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_constraint {x=x1} {x=x2} : _ = function
  | {x=x3} -> return (x1+x2+x3);;
let arity = measure_arity function_body_constraint;;
[%%expect{|
val function_body_constraint : t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_multiple_cases {x=x1} {x=x2} = function
  | {x=3} -> (fun {x=x3} -> return (x1+x2+x3))
  | {x=x3} -> (fun {x=x4} -> return (x1+x2+x3+x4));;
let arity = measure_arity function_body_multiple_cases;;
[%%expect{|
val function_body_multiple_cases : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_constraint_multiple_cases {x=x1} {x=x2} : _ = function
  | {x=3} -> (fun {x=x3} -> return (x1+x2+x3))
  | {x=x3} -> (fun {x=x4} -> return (x1+x2+x3+x4))
let arity = measure_arity function_body_constraint_multiple_cases
[%%expect{|
val function_body_constraint_multiple_cases : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_coercion {x=x1} {x=x2} :> _ = function
  | {x=0} -> (fun {x=x3} -> return (x1+x2+x3))
  | {x=x3} -> (fun {x=x4} -> return (x1+x2+x3+x4))
let arity = measure_arity function_body_coercion
[%%expect{|
val function_body_coercion : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

let function_body_coercion_constraint {x=x1} {x=x2} : _ :> _ = function
  | {x=0} -> (fun {x=x3} -> return (x1+x2+x3))
  | {x=x3} -> (fun {x=x4} -> return (x1+x2+x3+x4))
let arity = measure_arity function_body_coercion_constraint
[%%expect{|
val function_body_coercion_constraint : t -> t -> t -> t -> 'a = <fun>
val arity : int = 3
|}];;

(* partial application: applying a function with arity 3 yields a
  function with arity 2. *)
let partial_application {x=x1} {x=x2} = function
  | {x=0} -> (fun {x=x3} -> return (x1+x2+x3))
  | {x=x3} -> (fun {x=x4} -> return (x1+x2+x3+x4))
let arity1 = measure_arity partial_application
let arity2 = measure_arity (partial_application {x=7})
[%%expect{|
val partial_application : t -> t -> t -> t -> 'a = <fun>
val arity1 : int = 3
val arity2 : int = 2
|}];;

(* newtype doesn't interrupt arity *)
let f1 (type a) {x=x1} {x=x2} = return (x1+x2)
let f2 {x=x1} (type a) {x=x2} = return (x1+x2)
let f3 {x=x1} {x=x2} (type a) = return (x1+x2)
let f4  (type a a) {x=x1} (type a a) {x=x2} (type a a) = return (x1+x2)
let arity1 = measure_arity f1
let arity2 = measure_arity f2
let arity3 = measure_arity f3
let arity4 = measure_arity f4
[%%expect{|
val f1 : t -> t -> 'a = <fun>
val f2 : t -> t -> 'a = <fun>
val f3 : t -> t -> 'a = <fun>
val f4 : t -> t -> 'a = <fun>
val arity1 : int = 2
val arity2 : int = 2
val arity3 : int = 2
val arity4 : int = 2
|}];;
