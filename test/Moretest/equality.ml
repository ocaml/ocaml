let test n exp res =
  prerr_string "Test "; prerr_int n;
  if exp = res then prerr_string " passed.\n" else prerr_string " FAILED.\n";
  flush stderr

let x = [1;2;3]

let f x = 1 :: 2 :: 3 :: x

let mklist len =
  let l = ref [] in
  for i = 1 to len do l := i :: !l done;
  !l

type tree = Dummy | Leaf | Node of tree * tree

let rec mktree depth =
  if depth <= 0 then Leaf else Node(mktree(depth - 1), mktree(depth - 1))

type 'a leftlist = Nil | Cons of 'a leftlist * 'a

let mkleftlist len =
  let l = ref Nil in
  for i = 1 to len do l := Cons(!l, i) done;
  !l

let _ =
  test 1 0 (compare 0 0);
  test 2 (-1) (compare 0 1);
  test 3 1 (compare 1 0);
  test 4 0 (compare max_int max_int);
  test 5 (-1) (compare min_int max_int);
  test 6 1 (compare max_int min_int);
  test 7 0 (compare "foo" "foo");
  test 8 (-1) (compare "foo" "zorglub");
  test 9 (-1) (compare "abcdef" "foo");
  test 10 (-1) (compare "abcdefghij" "abcdefghijkl");
  test 11 1 (compare "abcdefghij" "abcdefghi");
  test 12 0 (compare (0,1) (0,1));
  test 13 (-1) (compare (0,1) (0,2));
  test 14 (-1) (compare (0,1) (1,0));
  test 15 1 (compare (0,1) (0,0));
  test 16 1 (compare (1,0) (0,1));
  test 17 0 (compare 0.0 0.0);
  test 18 (-1) (compare 0.0 1.0);
  test 19 (-1) (compare (-1.0) 0.0);
  test 20 0 (compare [| 0.0; 1.0; 2.0 |] [| 0.0; 1.0; 2.0 |]);
  test 21 (-1) (compare [| 0.0; 1.0; 2.0 |] [| 0.0; 1.0; 3.0 |]);
  test 22 1 (compare [| 0.0; 5.0; 2.0 |] [| 0.0; 1.0; 2.0 |]);
  test 23 0 (compare [1;2;3;4] [1;2;3;4]);
  test 24 (-1) (compare [1;2;3;4] [1;2;5;6]);
  test 25 (-1) (compare [1;2;3;4] [1;2;3;4;5]);
  test 26 1 (compare [1;2;3;4] [1;2;3]);
  test 27 1 (compare [1;2;3;4] [1;2;0;4]);
  test 28 0 (compare (mklist 1000) (mklist 1000));
  test 29 0 (compare (mkleftlist 1000) (mkleftlist 1000));
  test 30 0 (compare (mktree 12) (mktree 12));
  test 31 true (x = f []);
  test 32 true (stdout <> stderr);
  test 33 (-1) (compare nan 0.0);
  test 34 (-1) (compare nan neg_infinity);
  test 35 0 (compare nan nan);
  test 36 (-1) (compare (0.0, nan) (0.0, 0.0));
  test 37 (-1) (compare (0.0, nan) (0.0, neg_infinity));
  test 38 0 (compare (nan, 0.0) (nan, 0.0));
  let cmpgen x y = (x=y, x<>y, x<y, x<=y, x>y, x>=y) in
  let cmpfloat (x:float) (y:float) = (x=y, x<>y, x<y, x<=y, x>y, x>=y) in
  test 39 (false,true,false,false,false,false) (cmpgen nan nan);
  test 40 (false,true,false,false,false,false) (cmpgen nan 0.0);
  test 41 (false,true,false,false,false,false) (cmpfloat nan nan);
  test 42 (false,true,false,false,false,false) (cmpfloat nan 0.0)
