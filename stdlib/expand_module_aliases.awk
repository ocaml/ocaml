# This script adds the Stdlib__ prefixes to the module aliases in
# stdlib.ml and stdlib.mli
BEGIN { state=0 }
NR == 1 { printf ("# 1 \"%s\"\n", FILENAME) }
/\(\*MODULE_ALIASES\*\)\r?/ { state=1 }
{ if (state==0)
    print;
  else if (state==1)
    state=2;
  else if ($1 == "module")
    printf ("\n(** @canonical %s *)\nmodule %s = Stdlib__%s%s\n",
            $2, $2, tolower(substr($4,1,1)), substr($4,2));
}
