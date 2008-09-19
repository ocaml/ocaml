DEFINE A;
DEFINE B;

IFDEF A THEN
  value a_should_be_present : int;
ENDIF;

IFNDEF C THEN
  value b_should_be_present : int;
ENDIF;

IFNDEF C THEN
  value c_should_be_present : int;
ELSE
  value a_should_NOT_be_present : int;
END;

IFDEF C THEN
  value b_should_NOT_be_present : int;
ELSE
  value d_should_be_present : int;
  value e_should_be_present : int;
ENDIF;

value f_should_be_present : int;
