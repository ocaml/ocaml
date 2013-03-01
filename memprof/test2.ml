
type toto = I of int | F of float | T of toto * toto

type titi = {
  f : float;
  t : toto;
}

let create () =
 {
        f = 1.1 +. 45.;
        t = T (I (42), F 42.);
      }
