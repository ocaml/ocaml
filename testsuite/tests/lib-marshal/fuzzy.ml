(* TEST
  arguments = "-n 10000";
*)

(* Can also be used with an external fuzzer such as AFL:
      ./fuzzy -o fuzzy.in/data
      afl-fuzz -i fuzzy.in -o fuzzy.out -- ./fuzzy -r
*)

(* Some data to be marshaled *)

type t = A | B of int | C of float | D of string | E of char
       | F of t | G of t * t | H of int * t | I of t * float | J

let longstring =
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

let bigint = Int64.to_int 0x123456789ABCDEF0L

let closures () =
  let t = true and
      f = false in
  let rec odd n =
    if n = 0
    then f
    else even (n-1)
  and even n =
    if n = 0
    then t
    else odd (n-1)
  in (even, odd)

let data () = [|
  Obj.repr 1;
  Obj.repr (-1);
  Obj.repr 20000;
  Obj.repr 0x12345678;
  Obj.repr bigint;
  Obj.repr "foo";
  Obj.repr longstring;
  Obj.repr 3.141592654;
  Obj.repr A;
  Obj.repr (B 1);
  Obj.repr (C 2.718);
  Obj.repr (D "hello");
  Obj.repr (E 'l');
  Obj.repr (F(B 1));
  Obj.repr (G(A, G(B 2, G(C 3.14, G(D "", E 'e')))));
  Obj.repr (H(1, A));
  Obj.repr (I(B 2, 1e-6));
  (let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  Obj.repr z);
  Obj.repr [|1;2;3;4;5;6;7;8|];
  Obj.repr [|3.14; 2.718|];
  Obj.repr (closures());
  Obj.repr 0l;
  Obj.repr 123456l;
  Obj.repr 0L;
  (let i = Int64.of_string "123456789123456" in Obj.repr (i,i));
  Obj.repr (Failure "fail");
  Obj.repr Bigarray.(Array1.init int16_unsigned c_layout 5 (fun x -> 8*x))
|]

(* Generate file with marshaled data *)

let generate filename =
  Out_channel.with_open_bin filename
    (fun oc -> Marshal.(to_channel oc (data()) [Closures]))

(* Try to unmarshal possibly malicious data.  Clean failure is success. *)

let test ic =
  In_channel.set_binary_mode ic true;
  begin try
    ignore (Marshal.from_channel ic)
  with Failure _ | Invalid_argument _ | Out_of_memory -> ()
  end;
  Gc.full_major()

(* Internal fuzzing.  Rather naive. *)

let random_offset b =
  (* Leave the header unchanged *)
  20 + Random.int (Bytes.length b - 20)

let flip_one_byte b =
  let p = random_offset b in
  Bytes.set_uint8 b p (Random.int 0x100)

let flip_one_bit b =
  let p = random_offset b in
  let m = 1 lsl (Random.int 8) in
  Bytes.set_uint8 b p (Bytes.get_uint8 b p lxor m)

let fuzz niter =
  let d = Marshal.(to_string (data()) [Closures]) in
  for i = 1 to niter do
    let b = Bytes.of_string d in
    begin match i land 4 with
    | 0 -> flip_one_byte b
    | 1 -> flip_one_bit b
    | 2 -> flip_one_byte b; flip_one_byte b
    | _ (*3*) -> flip_one_bit b; flip_one_bit b
    end;
    begin try
      ignore (Marshal.from_bytes b 0)
    with Failure _ | Invalid_argument _ | Out_of_memory -> ()
    end;
    Gc.full_major()
  done

let fuzz1 () =
  let d = Marshal.(to_string (data()) [Closures]) in
  let b = Bytes.of_string d in
  for i = 0 to String.length d - 1 do
    for x = 0 to 255 do
      Bytes.set_uint8 b i x;
      begin try
        ignore (Marshal.from_bytes b 0)
      with Failure _ | Invalid_argument _ | Out_of_memory -> ()
      end;
      Gc.full_major()
    done;
    Bytes.set_uint8 b i (Bytes.get_uint8 b i)
  done

let () =
  Arg.parse [
    "-o", Arg.String generate,
      "<file>  Save marshaled data to <file>";
    "-n", Arg.Int fuzz,
      "<num iter>  Perform internal fuzzing test (random)";
    "-x", Arg.Unit fuzz1,
      "<num iter>  Perform internal fuzzing test (exhaustive 1-byte)";
    "-r", Arg.Unit (fun () -> test stdin),
      "  Read marshaled data from standard input"
  ]
  (fun s -> raise (Arg.Bad ("don't know what to do with " ^ s)))
  "Usage: fuzzy [option].\nOptions are:"
