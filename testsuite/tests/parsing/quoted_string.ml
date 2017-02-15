let foo =
  [
    {| a |};
    {t| b |t};
    {x_yz| c |x_yz};
    {_| c |_};
  ]

let bar =
  [
    {| a |} ^ "a";
    {t| b |t} ^ "b";
    {x_yz| c |x_yz} ^ "c";
    {_| d |_} ^ "d";

    "a" ^ {| a |};
    "b" ^ {t| b |t} ^ "b";
    "c" ^ {x_yz| c |x_yz} ^ "c";
    "d" ^ {_| d |_} ^ "d";

    {| a |} ^ {t| b |t} ^ {x_yz| c |x_yz} ^ {_| c |_};
  ]
