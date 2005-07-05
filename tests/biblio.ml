type biblio  = {{<bibliography>[heading paper*]}}
and heading = {{<heading>[ PCDATA ]}}
and paper   = {{<paper>[author+ title ((conference series?) |(journal volume? number)) publisher? year file abstract?]}}
and author  = {{<author>[ PCDATA ]}}
and title   = {{<title>[ PCDATA ]}}
and conference = {{<conference>[ PCDATA ]}}
and series = {{<series>[ PCDATA ]}}
and journal = {{<journal>[ PCDATA ]}}
and publisher = {{<publisher>[ PCDATA ]}}
and volume  = {{<volume>[ Int ]}}
and number  = {{<number>[ Int ]}}
and year  = {{<year>[ 1970--2010 ]}}
and file    = {{<file>[ PCDATA ]}}
and abstract= {{<abstract> text}}
and text    = {{[ PCDATA ]}}

type html  = {{<html>[head? body]}}
and head   = {{<head>[ <title>[ PCDATA ] ]}}
and body   = {{<body>[mix*]}}
and mix    = {{<h1>[mix*]
           | <a href=[PCDATA]>[mix*]
           | <p>[mix*]
           | <em>[mix*]
           | <ul>[ <li>[mix*] +]
           | Char}}

let rec do_authors (x : {{[author+]}}) : {{[mix*]}} =
{{ match x with
 | [ <author>a ] -> a
 | [ <author>a <author>b ] -> a @ " and, " @ b
 | [ <author>a x::_*] -> a @ ", " @ (do_authors x) }}

let do_paper (p : paper) : {{<li>[mix*]}} =
{{ match p with
  <paper>[ x::_*  <title>t <_>c _* <year>y <file>f _* ] ->
    let authors = do_authors x in
    <li>([ <a href=f>t ] @ authors @ "; in " @ [ <em>c ] @ "." ) }}

let do_biblio (b : biblio) : html =
{{ match b with
   <bibliography>[ <heading>h p::_* ]  ->
     let b = match p with
      | [] -> "Empty bibliography"
      | l -> [ <h1>h <ul>(map l with x -> [ (do_paper x) ] ) ]
      in
      <html>[ <head>[ <title>h ] <body>b ] }}

let bib : biblio = {{
  <bibliography>[
    <heading>"Alain's bibliography"
    <paper>[
      <author>"Alain Frisch"
      <title>"CDuce and OCaml"
      <conference>"ObscureWorkshop"
      <year>[2010]
      <file>"cduceocaml.ps.gz"
      <abstract>"In this work,..."
    ] ]
  }}

let () = 
  Format.fprintf Format.std_formatter "%a@."
    Cduce_types.Value.print 
    (Obj.magic bib)
