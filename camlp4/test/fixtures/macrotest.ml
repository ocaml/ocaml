DEFINE A = 42;
DEFINE B = 51;

IFDEF A THEN
  value a_should_be_present = B + 2;
  print_int (a_should_be_present + 1);
ENDIF;

print_int (a_should_be_present + 2);

IFNDEF C THEN
  print_int (a_should_be_present + 3);
ENDIF;

IFNDEF C THEN
  print_int (a_should_be_present + 4);
ELSE
  print_int (c_should_not_be_present + 1);
ENDIF;

IFDEF C THEN
  print_int (c_should_not_be_present + 2);
ELSIF
  print_int (A * a_should_be_present + 5);
ENDIF;

IFDEF DNE THEN
  print_int (c_should_not_be_present + 2);
ELSIF
  print_int (A * a_should_be_present + 5);
ENDIF;

IFDEF OPT THEN
  print_int (c_should_not_be_present + 2);
ELSIF
  print_int (A * a_should_be_present + 5);
ENDIF;

value e =
  IFDEF DNE THEN
    print_int (c_should_not_be_present + 2)
  ELSE
    print_int (A * a_should_be_present + 5)
  ENDIF;

value f =
  fun _ ->
    IFDEF DNE THEN
      print_int (c_should_not_be_present + 2)
    ELSE
      print_int (A * a_should_be_present + 5)
    ENDIF;

IFDEF A THEN
  DEFINE Z = "ok";
ELSE
  DEFINE Z = "ko";
ENDIF;

Z;

IFDEF DNE THEN
  DEFINE Z = "ko2";
ELSE
  DEFINE Z = "ok2";
ENDIF;

Z;

pouet;
