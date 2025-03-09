(* TEST *)

(* Check that [full_int] and [int_in_range] yield the same outputs
 * - for 31-bit input on every tested platform;
 * - for 32-bit input on every tested platform where [Sys.int_size >= 32]. *)

let min_int31 = -0x4000_0000 (* = -2^30 *)
let max_int31 = 0x3FFF_FFFF (* = 2^30-1 *)
let min_int32 = -(1 lsl 31)
    (* = -0x8000_0000 on platforms where [Sys.int_size >= 32] *)
let max_int32 = (1 lsl 31) - 1
    (* =  0x7FFF_FFFF on platforms where [Sys.int_size >= 32] *)

let expected__full_int__int32 =
  List.map Int64.to_int
    [
      451177519L; 819244110L; 1569039028L; 1898166382L; 382924821L;
      369901015L; 352113804L; 405163262L; 1999817968L; 626859825L;
      1606096148L; 768468192L; 2112068721L; 360408220L; 1086772984L;
      155961157L; 407238554L; 422041964L; 315222802L; 2141327316L;
      927599929L; 307430453L; 1936714909L; 1017363199L; 2125175059L;
      1311994963L; 759137410L; 212550723L; 1706544910L; 810893211L;
      2056429253L; 1079086093L; 1713675986L; 753193678L; 276572642L;
      530348252L; 617726581L; 944299189L; 674895562L; 32787006L;
      1830016271L; 1067904883L; 589173623L; 950337835L; 2078987417L;
      1487106135L; 569957530L; 2015304950L; 885035468L; 234722862L;
    ]

let expected__int_in_range__int32 =
  List.map Int64.to_int
    [
      -2147483542L; -2147483052L; -2147483634L; -2147483498L; -2147483224L;
      -2147482666L; -2147483631L; -2147483278L; -2147483612L; -2147482799L;
      -2147483421L; -2147482825L; -2147483593L; -2147483470L; -2147483177L;
      -2147482829L; -2147483463L; -2147482933L; -2147483591L; -2147482786L;
      -2147483446L; -2147483415L; -2147482710L; -2147482883L; -2147483199L;
      -2147482957L; -2147482912L; -2147483195L; -2147483513L; -2147482887L;
      -2147483394L; -2147483434L; -2147483251L; -2147483597L; -2147482693L;
      -2147483453L; -2147482724L; -2147483596L; -2147482822L; -2147482813L;
      -2147483565L; -2147482939L; -2147483428L; -2147483274L; -2147483612L;
      -2147483279L; -2147482698L; -2147483617L; -2147483301L; -2147483082L;
    ]

let expected__int_in_range__full_int32_range =
  List.map Int64.to_int
    [
      316667342L; -295023111L; 1894765368L; 185794834L; -981816301L;
      740994304L; -680489891L; 278403874L; -2032365355L; -71571733L;
      -313777328L; -689756819L; -980386453L; 82099031L; 1573778309L;
      -760895485L; 511322260L; 1825182001L; 954732521L; -1492321820L;
      773383493L; -1681170167L; 2067003710L; -1742733312L; 1195851762L;
      1602173427L; -357434044L; -1334661233L; -128246722L; -2094933952L;
      -431878364L; 1978816493L; 808773565L; -1454547995L; 364583207L;
      2002713488L; -786979985L; 964103855L; -1430475164L; 1303482935L;
      -356009461L; -111417631L; 1106019824L; -1988580373L; 1586895675L;
      -1580402270L; 2061313798L; -1676201176L; 975189254L; -361726938L;
    ]

let _ =

  (* Test outputs for 31-bit input: *)
  if Sys.int_size >= 31 then begin

    (* [full_int], range that fits in 30 bits: *)
    for i = 0 to 49 do
      Printf.printf "%i\n" (Random.full_int max_int31)
    done;
    print_newline ();

    (* [int_in_range], all-negative range whose length fits in 30 bits: *)
    for i = 0 to 49 do
      Printf.printf "%i\n"
        (Random.int_in_range ~min:min_int31 ~max:(min_int31 + 996))
    done;
    print_newline ();

    (* [int_in_range], full 31-bit range: *)
    for i = 0 to 49 do
      Printf.printf "%i\n"
        (Random.int_in_range ~min:min_int31 ~max:max_int31)
    done;
    print_newline ();

  end;

  (* Test outputs for 32-bit input: *)
  if Sys.int_size >= 32 then begin

    (* [full_int], range that fits in 31 bits: *)
    let ok = ref true in
    expected__full_int__int32 |>
    List.iter begin fun expected ->
      ok := !ok && expected =
        (Random.full_int max_int32)
    end ;
    Printf.printf "%b\n" !ok;

    (* [int_in_range], all-negative range whose length fits in 31 bits: *)
    let ok = ref true in
    expected__int_in_range__int32 |>
    List.iter begin fun expected ->
      ok := !ok && expected =
        (Random.int_in_range ~min:min_int32 ~max:(min_int32 + 996))
    end ;
    Printf.printf "%b\n" !ok;

    (* [int_in_range], full 32-bit range: *)
    let ok = ref true in
    expected__int_in_range__full_int32_range |>
    List.iter begin fun expected ->
      ok := !ok && expected =
        (Random.int_in_range ~min:min_int32 ~max:max_int32)
    end ;
    Printf.printf "%b\n" !ok;

  end
  else begin
    Printf.printf "true\ntrue\ntrue\n" ;
  end;

  ()

let _ = exit 0
