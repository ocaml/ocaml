open Test;;
open Nat;;
open Big_int;;
open Int_misc;;

testing_function "compare_big_int";;

test 1
eq_int (compare_big_int zero_big_int zero_big_int, 0);;
test 2
eq_int (compare_big_int zero_big_int (big_int_of_int 1), (-1));;
test 3
eq_int (compare_big_int zero_big_int (big_int_of_int (-1)), 1);;
test 4
eq_int (compare_big_int (big_int_of_int 1) zero_big_int, 1);;
test 5
eq_int (compare_big_int (big_int_of_int (-1)) zero_big_int, (-1));;
test 6
eq_int (compare_big_int (big_int_of_int 1) (big_int_of_int 1), 0);;
test 7
eq_int (compare_big_int (big_int_of_int (-1)) (big_int_of_int (-1)), 0);;
test 8
eq_int (compare_big_int (big_int_of_int 1) (big_int_of_int (-1)), 1);;
test 9
eq_int (compare_big_int (big_int_of_int (-1)) (big_int_of_int 1), (-1));;
test 10
eq_int (compare_big_int (big_int_of_int 1) (big_int_of_int 2), (-1));;
test 11
eq_int (compare_big_int (big_int_of_int 2) (big_int_of_int 1), 1);;
test 12
eq_int (compare_big_int (big_int_of_int (-1)) (big_int_of_int (-2)), 1);;
test 13
eq_int (compare_big_int (big_int_of_int (-2)) (big_int_of_int (-1)), (-1));;


testing_function "pred_big_int";;

test 1
eq_big_int (pred_big_int zero_big_int, big_int_of_int (-1));;
test 2
eq_big_int (pred_big_int unit_big_int, zero_big_int);;
test 3
eq_big_int (pred_big_int (big_int_of_int (-1)), big_int_of_int (-2));;

testing_function "succ_big_int";;

test 1
eq_big_int (succ_big_int zero_big_int, unit_big_int);;
test 2
eq_big_int (succ_big_int unit_big_int, big_int_of_int 2);;
test 3
eq_big_int (succ_big_int (big_int_of_int (-1)), zero_big_int);;

testing_function "add_big_int";;

test 1
eq_big_int (add_big_int zero_big_int zero_big_int, zero_big_int);;
test 2
eq_big_int (add_big_int zero_big_int (big_int_of_int 1), 
            big_int_of_int 1);;
test 3
eq_big_int (add_big_int (big_int_of_int 1) zero_big_int, 
            big_int_of_int 1);;
test 4
eq_big_int (add_big_int zero_big_int (big_int_of_int (-1)), 
            big_int_of_int (-1));;
test 5
eq_big_int (add_big_int (big_int_of_int (-1)) zero_big_int, 
            big_int_of_int (-1));;
test 6
eq_big_int (add_big_int (big_int_of_int 1) (big_int_of_int 1), 
            big_int_of_int 2);;
test 7
eq_big_int (add_big_int (big_int_of_int 1) (big_int_of_int 2), 
            big_int_of_int 3);;
test 8
eq_big_int (add_big_int (big_int_of_int 2) (big_int_of_int 1), 
            big_int_of_int 3);;
test 9
eq_big_int (add_big_int (big_int_of_int (-1)) (big_int_of_int (-1)), 
            big_int_of_int (-2));;
test 10
eq_big_int (add_big_int (big_int_of_int (-1)) (big_int_of_int (-2)), 
            big_int_of_int (-3));;
test 11
eq_big_int (add_big_int (big_int_of_int (-2)) (big_int_of_int (-1)), 
            big_int_of_int (-3));;
test 12
eq_big_int (add_big_int (big_int_of_int 1) (big_int_of_int (-1)), 
            zero_big_int);;
test 13
eq_big_int (add_big_int (big_int_of_int (-1)) (big_int_of_int 1), 
            zero_big_int);;
test 14
eq_big_int (add_big_int (big_int_of_int 1) (big_int_of_int (-2)), 
            big_int_of_int (-1));;
test 15
eq_big_int (add_big_int (big_int_of_int (-2)) (big_int_of_int 1), 
            big_int_of_int (-1));;
test 16
eq_big_int (add_big_int (big_int_of_int (-1)) (big_int_of_int 2), 
            big_int_of_int 1);;
test 17
eq_big_int (add_big_int (big_int_of_int 2) (big_int_of_int (-1)), 
            big_int_of_int 1);;


testing_function "sub_big_int";;

test 1
eq_big_int (sub_big_int zero_big_int zero_big_int, zero_big_int);;
test 2
eq_big_int (sub_big_int zero_big_int (big_int_of_int 1), 
            big_int_of_int (-1));;
test 3
eq_big_int (sub_big_int (big_int_of_int 1) zero_big_int, 
            big_int_of_int 1);;
test 4
eq_big_int (sub_big_int zero_big_int (big_int_of_int (-1)), 
            big_int_of_int 1);;
test 5
eq_big_int (sub_big_int (big_int_of_int (-1)) zero_big_int, 
            big_int_of_int (-1));;
test 6
eq_big_int (sub_big_int (big_int_of_int 1) (big_int_of_int 1), 
            zero_big_int);;
test 7
eq_big_int (sub_big_int (big_int_of_int 1) (big_int_of_int 2), 
            big_int_of_int (-1));;
test 8
eq_big_int (sub_big_int (big_int_of_int 2) (big_int_of_int 1), 
            big_int_of_int 1);;
test 9
eq_big_int (sub_big_int (big_int_of_int (-1)) (big_int_of_int (-1)), 
            zero_big_int);;
test 10
eq_big_int (sub_big_int (big_int_of_int (-1)) (big_int_of_int (-2)), 
            big_int_of_int 1);;
test 11
eq_big_int (sub_big_int (big_int_of_int (-2)) (big_int_of_int (-1)), 
            big_int_of_int (-1));;
test 12
eq_big_int (sub_big_int (big_int_of_int 1) (big_int_of_int (-1)), 
            big_int_of_int 2);;
test 13
eq_big_int (sub_big_int (big_int_of_int (-1)) (big_int_of_int 1), 
            big_int_of_int (-2));;
test 14
eq_big_int (sub_big_int (big_int_of_int 1) (big_int_of_int (-2)), 
            big_int_of_int 3);;
test 15
eq_big_int (sub_big_int (big_int_of_int (-2)) (big_int_of_int 1), 
            big_int_of_int (-3));;
test 16
eq_big_int (sub_big_int (big_int_of_int (-1)) (big_int_of_int 2), 
            big_int_of_int (-3));;
test 17
eq_big_int (sub_big_int (big_int_of_int 2) (big_int_of_int (-1)), 
            big_int_of_int 3);;

testing_function "mult_int_big_int";;

test 1
eq_big_int (mult_int_big_int 0 (big_int_of_int 3), zero_big_int);;
test 2
eq_big_int (mult_int_big_int 1 (big_int_of_int 3), big_int_of_int 3);;
test 3
eq_big_int (mult_int_big_int 1 zero_big_int, zero_big_int);;
test 4
eq_big_int (mult_int_big_int 2 (big_int_of_int 3), big_int_of_int 6);;

testing_function "mult_big_int";;

test 1
eq_big_int (mult_big_int zero_big_int zero_big_int, 
            zero_big_int);;
test 2
eq_big_int (mult_big_int (big_int_of_int 2) (big_int_of_int 3), 
            big_int_of_int 6);;
test 3
eq_big_int (mult_big_int (big_int_of_int 2) (big_int_of_int (-3)), 
            big_int_of_int (-6));;
test 4 
eq_big_int (mult_big_int (big_int_of_string "12724951") 
                         (big_int_of_string "81749606400"), 
            big_int_of_string "1040259735709286400");;
test 5 
eq_big_int (mult_big_int (big_int_of_string "26542080") 
                          (big_int_of_string "81749606400"), 
            big_int_of_string "2169804593037312000");;

testing_function "quomod_big_int";;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 1) (big_int_of_int 1) in
 test 1 eq_big_int (quotient, big_int_of_int 1) &
 test 2 eq_big_int (modulo, zero_big_int);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 1) (big_int_of_int (-1)) in
 test 3 eq_big_int (quotient, big_int_of_int (-1)) &
 test 4 eq_big_int (modulo, zero_big_int);;

let (quotient, modulo) = 
      quomod_big_int (big_int_of_int (-1)) (big_int_of_int 1) in
 test 5 eq_big_int (quotient, big_int_of_int (-1)) & 
 test 6 eq_big_int (modulo, zero_big_int);;

let (quotient, modulo) = 
      quomod_big_int (big_int_of_int 3) (big_int_of_int 2) in
 test 7 eq_big_int (quotient, big_int_of_int 1) & 
 test 8 eq_big_int (modulo, big_int_of_int 1);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 5) (big_int_of_int 3) in
 test 9 eq_big_int (quotient, big_int_of_int 1) &
 test 10 eq_big_int (modulo, big_int_of_int 2);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int (-5)) (big_int_of_int 3) in
 test 11 eq_big_int (quotient, big_int_of_int (-2)) &
 test 12 eq_big_int (modulo, big_int_of_int 1);;

let (quotient, modulo) = 
      quomod_big_int (big_int_of_int 1) (big_int_of_int 2) in
 test 13 eq_big_int (quotient, zero_big_int) & 
 test 14 eq_big_int (modulo, big_int_of_int 1);;

let (quotient, modulo) = 
      quomod_big_int (big_int_of_int (-1)) (big_int_of_int 3) in
 test 15 eq_big_int (quotient, minus_big_int unit_big_int) &
 test 16 eq_big_int (modulo, big_int_of_int 2);;

failwith_test 17
(quomod_big_int (big_int_of_int 1)) zero_big_int
Division_by_zero
;;

testing_function "gcd_big_int";;

test 1
eq_big_int (gcd_big_int zero_big_int zero_big_int, 
            zero_big_int);;
test 2
eq_big_int (gcd_big_int zero_big_int (big_int_of_int 1), 
            big_int_of_int 1);;
test 3
eq_big_int (gcd_big_int (big_int_of_int 1) zero_big_int, 
            big_int_of_int 1);;
test 4
eq_big_int (gcd_big_int (big_int_of_int 1) (big_int_of_int 2), 
            big_int_of_int 1);;
test 5
eq_big_int (gcd_big_int (big_int_of_int 2) (big_int_of_int 1), 
            big_int_of_int 1);;
test 6
eq_big_int (gcd_big_int (big_int_of_int 1) (big_int_of_int 1), 
            big_int_of_int 1);;
test 7
eq_big_int (gcd_big_int (big_int_of_int 9) (big_int_of_int 16), 
            big_int_of_int 1);;
test 8
eq_big_int (gcd_big_int (big_int_of_int 12) (big_int_of_int 16), 
            big_int_of_int 4);;

for i = 9 to 28 do
  let n1 = Random.int 1000000000
  and n2 = Random.int 100000 in
    test i eq
      (int_of_big_int (gcd_big_int (big_int_of_int n1) (big_int_of_int n2)),
       gcd_int n1 n2)
done;;

testing_function "int_of_big_int";;

test 1
eq_int (int_of_big_int (big_int_of_int 1), 1);;
test 2
eq (is_int_big_int (big_int_of_int 1), true);;
test 3
eq (is_int_big_int (succ_big_int (big_int_of_int biggest_int)),false);;

testing_function "sys_string_of_big_int";;

test 1
eq_string (string_of_big_int (big_int_of_int 1), "1");;


testing_function "big_int_of_string";;

test 1
eq_big_int (big_int_of_string "1", big_int_of_int 1);;
test 2
eq_big_int (big_int_of_string "-1", big_int_of_int (-1));;
test 4
eq_big_int (big_int_of_string "0", zero_big_int);;

failwith_test 5 big_int_of_string "sdjdkfighdgf"
  (Failure "invalid digit");;

test 6
eq_big_int (big_int_of_string "123", big_int_of_int 123);;
test 7
eq_big_int (big_int_of_string "3456", big_int_of_int 3456);;

test 9
eq_big_int (big_int_of_string "-3456", big_int_of_int (-3456));;


let implode = List.fold_left (^) "";; (* Au diable l'efficacite *)

let l = List.rev [
"174679877494298468451661416292903906557638850173895426081611831060970135303";
"044177587617233125776581034213405720474892937404345377707655788096850784519";
"539374048533324740018513057210881137248587265169064879918339714405948322501";
"445922724181830422326068913963858377101914542266807281471620827145038901025";
"322784396182858865537924078131032036927586614781817695777639491934361211399";
"888524140253852859555118862284235219972858420374290985423899099648066366558";
"238523612660414395240146528009203942793935957539186742012316630755300111472";
"852707974927265572257203394961525316215198438466177260614187266288417996647";
"132974072337956513457924431633191471716899014677585762010115338540738783163";
"739223806648361958204720897858193606022290696766988489073354139289154127309";
"916985231051926209439373780384293513938376175026016587144157313996556653811";
"793187841050456120649717382553450099049321059330947779485538381272648295449";
"847188233356805715432460040567660999184007627415398722991790542115164516290";
"619821378529926683447345857832940144982437162642295073360087284113248737998";
"046564369129742074737760485635495880623324782103052289938185453627547195245";
"688272436219215066430533447287305048225780425168823659431607654712261368560";
"702129351210471250717394128044019490336608558608922841794819375031757643448";
"32"
] in

let bi1=big_int_of_string (implode (List.rev l)) in

let bi2=big_int_of_string (implode (List.rev ("3" :: List.tl l))) in

test 10
eq_big_int (bi1, (add_big_int (mult_big_int bi2 (big_int_of_string "10")) 
                              (big_int_of_string "2")));;

testing_function "power_base_int";;

test 1
eq_big_int (big_int_of_nat (power_base_int 10 0), unit_big_int)
;;
test 2
eq_big_int (big_int_of_nat (power_base_int 10 8), big_int_of_int 100000000)
;;
test 3
eq_big_int (big_int_of_nat (power_base_int 2 (if sixtyfour then 64 else 32)), 
            big_int_of_nat (let nat = make_nat 2 in
                              set_digit_nat nat 1 1;
                              nat))
;;

testing_function "base_power_big_int";;

test 1
eq_big_int (base_power_big_int 10 0 (big_int_of_int 2), big_int_of_int 2);;
test 2
eq_big_int (base_power_big_int 10 2 (big_int_of_int 2), big_int_of_int 200);;
test 3
eq_big_int (base_power_big_int 10 1 (big_int_of_int 123), big_int_of_int 1230)
;;

