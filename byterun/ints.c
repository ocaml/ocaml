#include <stdio.h>
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"

value int_of_string(s)          /* ML */
     value s;
{
  long res;
  int sign;
  int base;
  char * p;
  int c, d;

  p = String_val(s);
  if (*p == 0) failwith("int_of_string");
  sign = 1;
  if (*p == '-') {
    sign = -1;
    p++;
  }
  base = 10;
  if (*p == '0') {
    switch (p[1]) {
    case 'x': case 'X':
      base = 16; p += 2; break;
    case 'o': case 'O':
      base = 8; p += 2; break;
    case 'b': case 'B':
      base = 2; p += 2; break;
    }
  }
  res = 0;
  while (1) {
    c = *p;
    if (c >= '0' && c <= '9')
      d = c - '0';
    else if (c >= 'A' && c <= 'F')
      d = c - 'A' + 10;
    else if (c >= 'a' && c <= 'f')
      d = c - 'a' + 10;
    else
      break;
    if (d >= base) break;
    res = base * res + d;
    p++;
  }
  if (*p != 0)
    failwith("int_of_string");
  return Val_long(sign < 0 ? -res : res);
}

value format_int(fmt, arg)      /* ML */
     value fmt, arg;
{
  char format_buffer[32];
  int prec;
  char * p;
  char * dest;
  value res;

  prec = 32;
  for (p = String_val(fmt); *p != 0; p++) {
    if (*p >= '0' && *p <= '9') {
      prec = atoi(p) + 5;
      break;
    }
  }
  if (prec <= sizeof(format_buffer)) {
    dest = format_buffer;
  } else {
    dest = stat_alloc(prec);
  }
  sprintf(dest, String_val(fmt), Long_val(arg));
  res = copy_string(dest);
  if (dest != format_buffer) {
    stat_free(dest);
  }
  return res;
}
