(* TEST
expect;
*)

let test n =
  let open Format in
  set_geometry ~max_indent:(n-2) ~margin:n;
  printf (format_text
            "@[<v>@[qualibus @[in@] @[tenebris@] vitae quantisque periclis@] \
             @[degitur hoc aevi quod cumquest! nonne videre@] \
             @[nihil aliud sibinaturam latrare, nisi ut qui@] \
             @[corpore seiunctus dolor absit, mente fruatur@] \
             @[iucundo sensu cura semota metuque?@] %s@]@."
         )
    "De rerum natura, Lucretius"
[%%expect {|
val test : int -> unit = <fun>
|}]


let () = test 20
[%%expect {|
qualibus in
tenebris vitae
quantisque periclis
degitur hoc aevi
quod cumquest!
nonne videre
nihil aliud
sibinaturam
latrare, nisi ut
qui
corpore seiunctus
dolor absit, mente
fruatur
iucundo sensu cura
semota metuque?
De rerum natura, Lucretius
|}]


let () = test 40
[%%expect {|
qualibus in tenebris vitae quantisque
periclis
degitur hoc aevi quod cumquest! nonne
videre
nihil aliud sibinaturam latrare, nisi
ut qui
corpore seiunctus dolor absit, mente
fruatur
iucundo sensu cura semota metuque?
De rerum natura, Lucretius
|}]

let () = test 80
[%%expect {|
qualibus in tenebris vitae quantisque periclis
degitur hoc aevi quod cumquest! nonne videre
nihil aliud sibinaturam latrare, nisi ut qui
corpore seiunctus dolor absit, mente fruatur
iucundo sensu cura semota metuque?
De rerum natura, Lucretius
|}]
