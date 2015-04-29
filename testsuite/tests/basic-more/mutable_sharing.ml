
let r1 = ref 0
let r2 = ref r1

let () =
  incr r1;
  assert(! !r2 = 1)

let () =
  for i = 0 to 5 do
    let r = ref 0 in
    incr r;
    assert(!r = 1);
    r2 := r
  done

let () =
  let i = ref 0 in
  while !i < 5  do
    let r = ref 0 in
    incr r;
    assert(!r = 1);
    r2 := r;
    incr i;
  done

let rec rec_test n =
  if n > 0 then begin
    let r = ref 0 in
    incr r;
    assert(!r = 1);
    r2 := r;
    rec_test (n-1)
  end

let f () =
  let r = ref 0 in
  incr r;
  assert(!r = 1);
  r2 := r

let g () =
  for i = 0 to 5 do
    let r = ref 0 in
    incr r;
    assert(!r = 1);
    r2 := r
  done

let h () =
  let i = ref 0 in
  while !i < 5  do
    let r = ref 0 in
    incr r;
    assert(!r = 1);
    r2 := r;
    incr i;
  done


let () =
  rec_test 3; rec_test 3;
  f (); f (); f ();
  g (); g (); g ();
  h (); h (); h ()
