open Symbol
open Abstract_identifiers
open Flambda
open Test_utils

let check expr =
  Flambdacheck.check
    ~current_compilation_unit:compilation_unit
    expr

let v = new_var "v"
let f = new_var "f"
let g = new_var "g"
let x = new_var "x"
let y = new_var "y"
let z = new_var "z"
let a = new_var "a"
let b = new_var "b"

let impure_expr () = fccall []

let expr1 = flet v (int 1) (fvar v)
let res1 = flet v (int 1) (fvar v)

let expr2 = flet x (int 1) (flet v (int 1) (fvar v))
let res2 = flet v (int 1) (fvar v)

let expr3 = flet x (impure_expr ()) (int 1)
let res3 = flet x (impure_expr ()) (int 1)

let expr4 =
  flet x (impure_expr ())
    (flet y (impure_expr ()) (int 1))
let res4 =
  flet x (impure_expr ())
    (flet y (impure_expr ()) (int 1))

let expr5 =
  flet x
    (flet y (int 1) (fvar y))
    (fvar x)
let res5 =
  flet x
    (flet y (int 1) (fvar y))
    (fvar x)

let expr6 =
  flet x
    (int 1)
    (fif (fbool true)
       (int 1)
       (fvar x))
let res6 =
  fif (fbool true)
    (int 1)
    (flet x (int 1) (fvar x))

let expr7 =
  flet x
    (int 1)
    (fif (fbool true)
       (fvar x)
       (fvar x))
let res7 =
  flet x
    (int 1)
    (fif (fbool true)
       (fvar x)
       (fvar x))

let expr8 =
  flet y
    (int 1)
    (flet x
       (fvar y)
       (fif (fbool true)
          (fvar y)
          (fvar x)))
let res8 =
  flet y
    (int 1)
    (fif (fbool true)
       (fvar y)
       (flet x
          (fvar y)
          (fvar x)))

let expr9 =
  flet y
    (int 1)
    (flet z
       (fvar y)
       (flet x
          (fvar y)
          (fif (fbool true)
             (fvar z)
             (fvar x))))
let res9 =
  flet y
    (int 1)
    (fif (fbool true)
       (flet z
          (fvar y)
          (fvar z))
       (flet x (fvar y)
          (fvar x)))

let expr10 =
  flet x (int 1)
    (fwhile (fbool true)
       (fvar x))
let res10 =
  flet x (int 1)
    (fwhile (fbool true)
       (fvar x))

let expr11 =
    (fwhile (fbool true)
       (flet x (int 1)
          (fvar x)))
let res11 =
  flet x (int 1)
    (fwhile (fbool true)
       (fvar x))

let expr12' =
  (flet y (impure_expr ())
     (flet x (fvar y)
        (fvar x)))
let res12' =
  (flet y (impure_expr ())
     (flet x (fvar y)
        (fvar x)))

let expr12 =
    (fwhile (fbool true)
       (flet y (impure_expr ())
          (flet x (fvar y)
             (fvar x))))
let res12 =
    (fwhile (fbool true)
       (flet y (impure_expr ())
          (flet x (fvar y)
             (fvar x))))

let expr13 =
    (fwhile (fbool true)
       (flet y (impure_expr ())
          (flet x (fvar y)
             (flet z (int 1)
                (fadd (fvar x) (fvar z))))))
let res13 =
  (flet z (int 1)
     (fwhile (fbool true)
        (flet y (impure_expr ())
           (fadd (flet x (fvar y) (fvar x)) (fvar z)))))

let expr14 =
  let inner_while =
    (fwhile (fbool true)
       (flet y (impure_expr ())
          (flet x (fvar y)
             (flet z (fvar b)
                (flet a (int 1)
                   (fadd
                      (fadd (fvar x) (fvar z))
                      (fadd (fvar a) (int 1))))))))
  in
  (fwhile (fbool true)
     (flet b (impure_expr ())
        inner_while))
let res14 =
  let inner_while =
    (fwhile (fbool true)
       (flet y (impure_expr ())
          (fadd
             (fadd (flet x (fvar y) (fvar x)) (fvar z))
             (fadd (fvar a) (int 1)))))
  in
  (flet a (int 1)
     (fwhile (fbool true)
        (flet b (impure_expr ())
           (flet z (fvar b)
              inner_while))))

let expr15 =
    (fwhile (fbool true)
       (flet y (impure_expr ())
          (flet a (int 1)
             (flet x (fvar a)
                (flet z (int 1)
                   (fadd (fvar x) (fvar z)))))))
let res15 =
  (flet z (int 1)
     (flet x (flet a (int 1) (fvar a))
        (fwhile (fbool true)
           (flet y (impure_expr ())
              (fadd (fvar x) (fvar z))))))

let expr16 =
  (ffor x (int 1) (int 3)
     (flet y (fvar x)
        (fvar x)))
let res16 =
  (ffor x (int 1) (int 3)
     (fvar x))

let expr17 =
  (ffor x (int 1) (int 3)
     (flet y (fvar x)
        (fif (fbool true)
           (fvar x)
           (fvar y))))
let res17 =
  (ffor x (int 1) (int 3)
     (fif (fbool true)
        (fvar x)
        (flet y (fvar x) (fvar y))))

let expr18 =
  flet z (int 1)
    (flet y (fvar z)
       (flet x (fvar y)
          (int 2)))
let res18 = int 2

let expr19 = fibonacci
let res19 = fibonacci

let expr20 = flet f fibonacci (int 1)
let res20 = int 1

let expr21 = fclosure [f, [z], expr17] []
let res21 = fclosure [f, [z], res17] []

let expr6 =
  flet x
    (int 1)
    (fif (fbool true)
       (int 1)
       (fvar x))
let res6 =
  fif (fbool true)
    (int 1)
    (flet x (int 1) (fvar x))


let expr22 =
  fclosure [f, [z],
            flet a (fvar z)
              (flet b (fvar y)
                 (fif (fbool true)
                    (fvar a)
                    (fvar b)))
           ] [y, int 2]
let res22 =
  fclosure [f, [z],
            (fif (fbool true)
               (flet a (fvar z) (fvar a))
               (flet b (fvar y) (fvar b)))
           ] [y, int 2]

let expr23 =
  flet y (int 2)
    (fclosure [f, [x], fvar z] [z, fvar y])
let res23 =
  (* expected result if there is no expression_free_variable hack:
     fclosure [f, [x], fvar z] [z, flet y (int 2) (fvar y)] *)
  flet y (int 2)
    (fclosure [f, [x], fvar z] [z, fvar y])

let launch (s,e) =
  let e' = Flambdamovelets.move_lets e in
  Format.printf "%s@ orig:@ %a@.moved:@ %a@."
    s
    Printflambda.flambda e
    Printflambda.flambda e';
  check e;
  check e'

let test (s,e1,e2) =
  Format.printf "run %s@." s;
  let e' = Flambdamovelets.move_lets e1 in
  check e1;
  check e';
  if not (equal e' e2)
  then
    (Format.printf "fail movelet: %s@ orig:@ %a@.moved:@ %a@.expected:@ %a@."
       s
       Printflambda.flambda e1
       Printflambda.flambda e'
       Printflambda.flambda e2;
     failwith (Printf.sprintf "fail movelet: %s" s))


let run () =
  List.iter launch [ ];
  List.iter test
    [ "1", expr1, res1;
      "2", expr2, res2;
      "3", expr3, res3;
      "4", expr4, res4;
      "5", expr5, res5;
      "6", expr6, res6;
      "7", expr7, res7;
      "8", expr8, res8;
      "9", expr9, res9;
      "10", expr10, res10;
      "11", expr11, res11;
      "12'", expr12', res12';
      "12", expr12, res12;
      "13", expr13, res13;
      "14", expr14, res14;
      "15", expr15, res15;
      "16", expr16, res16;
      "17", expr17, res17;
      "18", expr18, res18;
      "19", expr19, res19;
      "20", expr20, res20;
      "21", expr21, res21;
      "22", expr22, res22;
      "23", expr23, res23;
    ];
  Format.printf "movelet passed@."
