(* TEST
 flags = "-I ${ocamlsrcdir}/toplevel";
 expect;
*)


let setup ~out_width n =
  Format.set_geometry ~max_indent:(n-2) ~margin:n;
  let fns = Format.get_formatter_out_functions () in
  Format.set_formatter_out_functions  { fns with out_width }

let zws = "\u{200B}"

let () = Toploop.max_printer_steps := 1 + 100 * String.length zws

let zero_width_spaces =
  let len = String.length zws in
  String.init (20 * len) (fun i -> zws.[i mod len])

let test ~out_width n s =
  setup ~out_width n;
  Format.printf "@[@<0>%s%a@]@."
    zero_width_spaces
    Format.pp_print_text s
[%%expect {|
val setup : out_width:(string -> pos:int -> len:int -> int) -> int -> unit =
  <fun>
val zws : string = "​"
val zero_width_spaces : string = "​​​​​​​​​​​​​​​​​​​​"
val test :
  out_width:(string -> pos:int -> len:int -> int) -> int -> string -> unit =
  <fun>
|}]

(** Alphabetic scripts *)
let () =
  test ~out_width:Format.utf_8_scalar_width 40
  "Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος οὐλομένην, ἣ μυρί᾿ Ἀχαιοῖς ἄλγε᾿ ἔθηκε, \
   πολλὰς δ᾿ ἰφθίμους ψυχὰς Ἄϊδι προΐαψεν ἡρώων, αὐτοὺς δὲ ἑλώρια τεῦχε κύνεσσιν \
   οἰωνοῖσί τε πᾶσι· Διὸς δ᾿ ἐτελείετο βουλή ἐξ οὗ δὴ τὰ πρῶτα διαστήτην \
   ἐρίσαντε Ἀτρεΐδης τε ἄναξ ἀνδρῶν καὶ δῖος Ἀχιλλεύς."
[%%expect {|
​​​​​​​​​​​​​​​​​​​​Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος
οὐλομένην, ἣ μυρί᾿ Ἀχαιοῖς ἄλγε᾿ ἔθηκε,
πολλὰς δ᾿ ἰφθίμους ψυχὰς Ἄϊδι προΐαψεν
ἡρώων, αὐτοὺς δὲ ἑλώρια τεῦχε κύνεσσιν
οἰωνοῖσί τε πᾶσι· Διὸς δ᾿ ἐτελείετο
βουλή ἐξ οὗ δὴ τὰ πρῶτα διαστήτην
ἐρίσαντε Ἀτρεΐδης τε ἄναξ ἀνδρῶν καὶ
δῖος Ἀχιλλεύς.
|}]

let () =
  setup ~out_width:Format.utf_8_scalar_width 40;
  Format.printf
    "@[Μῆνιν@ ἄειδε@ θεὰ@ Πηληϊάδεω@ Ἀχιλῆος@ οὐλομένην,@ ἣ@ μυρί᾿@ Ἀχαιοῖς@ ἄλγε᾿@ ἔθηκε,@ \
     πολλὰς@ δ᾿@ ἰφθίμους@ ψυχὰς@ Ἄϊδι@ προΐαψεν@ ἡρώων,@ αὐτοὺς@ δὲ@ ἑλώρια@ τεῦχε@ \
     κύνεσσιν@ οἰωνοῖσί@ τε@ πᾶσι·@ Διὸς@ δ᾿@ ἐτελείετο@ βουλή@ ἐξ@ οὗ@ δὴ@ τὰ@ πρῶτα@ \
     διαστήτην@ ἐρίσαντε@ Ἀτρεΐδης@ τε@ ἄναξ@ ἀνδρῶν@ καὶ@ δῖος@ Ἀχιλλεύς.@]@."
[%%expect {|
Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος
οὐλομένην, ἣ μυρί᾿ Ἀχαιοῖς ἄλγε᾿ ἔθηκε,
πολλὰς δ᾿ ἰφθίμους ψυχὰς Ἄϊδι προΐαψεν
ἡρώων, αὐτοὺς δὲ ἑλώρια τεῦχε κύνεσσιν
οἰωνοῖσί τε πᾶσι· Διὸς δ᾿ ἐτελείετο
βουλή ἐξ οὗ δὴ τὰ πρῶτα διαστήτην
ἐρίσαντε Ἀτρεΐδης τε ἄναξ ἀνδρῶν καὶ
δῖος Ἀχιλλεύς.
|}]

let () =
  test ~out_width:Format.ascii_width 40
  "Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος οὐλομένην, ἣ μυρί᾿ Ἀχαιοῖς ἄλγε᾿ ἔθηκε, \
   πολλὰς δ᾿ ἰφθίμους ψυχὰς Ἄϊδι προΐαψεν ἡρώων, αὐτοὺς δὲ ἑλώρια τεῦχε κύνεσσιν \
   οἰωνοῖσί τε πᾶσι· Διὸς δ᾿ ἐτελείετο βουλή ἐξ οὗ δὴ τὰ πρῶτα διαστήτην \
   ἐρίσαντε Ἀτρεΐδης τε ἄναξ ἀνδρῶν καὶ δῖος Ἀχιλλεύς."
[%%expect {|
​​​​​​​​​​​​​​​​​​​​Μῆνιν ἄειδε θεὰ
Πηληϊάδεω Ἀχιλῆος
οὐλομένην, ἣ μυρί᾿
Ἀχαιοῖς ἄλγε᾿
ἔθηκε, πολλὰς δ᾿
ἰφθίμους ψυχὰς
Ἄϊδι προΐαψεν
ἡρώων, αὐτοὺς δὲ
ἑλώρια τεῦχε
κύνεσσιν οἰωνοῖσί
τε πᾶσι· Διὸς δ᾿
ἐτελείετο βουλή ἐξ
οὗ δὴ τὰ πρῶτα
διαστήτην ἐρίσαντε
Ἀτρεΐδης τε ἄναξ
ἀνδρῶν καὶ δῖος
Ἀχιλλεύς.
|}]

(** Vietnamese tends to use many combining diacritics *)
let () =
  test ~out_width:Format.utf_8_scalar_width 40
    "Trăm năm trong cõi người ta, \
     Chữ tài chữ mệnh khéo là ghét nhau. \
     Trải qua một cuộc bể dâu, \
     Những điều trông thấy mà đau đớn lòng."
[%%expect{|
​​​​​​​​​​​​​​​​​​​​Trăm năm trong cõi người ta, Chữ tài
chữ mệnh khéo là ghét nhau. Trải qua
một cuộc bể dâu, Những điều trông thấy
mà đau đớn lòng.
|}]

let () =
  test ~out_width:Format.ascii_width 40
    "Trăm năm trong cõi người ta, \
     Chữ tài chữ mệnh khéo là ghét nhau. \
     Trải qua một cuộc bể dâu, \
     Những điều trông thấy mà đau đớn lòng."
[%%expect{|
​​​​​​​​​​​​​​​​​​​​Trăm năm trong cõi người ta,
Chữ tài chữ mệnh khéo là ghét
nhau. Trải qua một cuộc bể
dâu, Những điều trông thấy mà
đau đớn lòng.
|}]

(** Logographic *)
let () =
  test ~out_width:Format.ascii_width 40
    "めぐり逢ひ \
     見しやそれとも \
     わかぬ間に \
     雲隠れにし"
[%%expect {|
​​​​​​​​​​​​​​​​​​​​めぐり逢ひ 見しやそれとも
わかぬ間に 雲隠れにし
|}]

let () =
  test ~out_width:Format.utf_8_scalar_width 40
    "めぐり逢ひ \
     見しやそれとも \
     わかぬ間に \
     雲隠れにし"
[%%expect {|
​​​​​​​​​​​​​​​​​​​​めぐり逢ひ 見しやそれとも わかぬ間に 雲隠れにし
|}]



(** The number of unicode scalar values is really not a good approximation for
    abugida scripts, but those scripts width tend to be really dependent on
    shaping anyway. *)
let () =
  test  ~out_width:Format.utf_8_scalar_width 40
    "अग्निमीळे पुरोहितं यज्ञस्य देवं रत्वीजम होतारं रत्नधातमम अग्निः \
     पूर्वेभिर्र्षिभिरीड्यो नूतनैरुत स देवानेह वक्षति"
[%%expect{|
​​​​​​​​​​​​​​​​​​​​अग्निमीळे पुरोहितं यज्ञस्य देवं रत्वीजम
होतारं रत्नधातमम अग्निः
पूर्वेभिर्र्षिभिरीड्यो नूतनैरुत स
देवानेह वक्षति
|}]

let () =
  test  ~out_width:Format.ascii_width 40
    "अग्निमीळे पुरोहितं यज्ञस्य देवं रत्वीजम होतारं रत्नधातमम अग्निः \
     पूर्वेभिर्र्षिभिरीड्यो नूतनैरुत स देवानेह वक्षति"
[%%expect{|
​​​​​​​​​​​​​​​​​​​​अग्निमीळे
पुरोहितं
यज्ञस्य देवं
रत्वीजम
होतारं
रत्नधातमम
अग्निः
पूर्वेभिर्र्षिभिरीड्यो
नूतनैरुत स
देवानेह
वक्षति
|}]
