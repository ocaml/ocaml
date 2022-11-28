/^CAMLprim value/ {
  :loop
  /)/ ! {
    N; b loop
  }

  s/\n  */ /g

  s/^CAMLprim value \([a-z0-9_][a-z0-9_]*\) *(\([^)]*\)).*$/\1(\2)/p;
}
