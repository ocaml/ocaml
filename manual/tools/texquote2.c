#include <stdio.h>
#include <ctype.h>

char * transl[256];

#define LINE_LENGTH 1024

char line[LINE_LENGTH];

int isprefix(s, pref)
     char * s;
     char * pref;
{
  while (1) {
    if (*pref == 0) return 1;
    if (*s == 0) return 0;
    if (*s != *pref) return 0;
    s++;
    pref++;
  }
}

int main(argc, argv)
     int argc;
     char * argv [];
{
  unsigned char * p;
  int c;
  int inquote;
  int inverb;
  int inverbatim;
  int incaml;

  for (c = 0; c < 256; c++) transl[c] = NULL;
#ifdef TIE_BLANKS
  transl[' '] = "~";
  transl['\n'] = "~";
#else
  transl[' '] = "\\ ";
  transl['\n'] = "\\ ";
#endif
  transl['{'] = "{\\char123}";
  transl['}'] = "{\\char125}";
  transl['^'] = "{\\char94}";
  transl['_'] = "{\\char95}";
  transl['\\'] = "{\\char92}";
  transl['~'] = "{\\char126}";
  transl['$'] = "\\$";
  transl['&'] = "{\\char38}";
  transl['#'] = "\\#";
  transl['%'] = "\\%";
  inverbatim = 0;
  incaml = 0;
  inquote = 0;

  while(fgets(line, LINE_LENGTH, stdin) != NULL) {
    if (inverbatim) {
      fputs(line, stdout);
      if (isprefix(line, "\\end{verbatim")
          || isprefix(line, "\\end{caml_")) inverbatim = 0;
      continue;
    }
    if (incaml) {
      fputs(line, stdout);
      if (isprefix(line, "\\endcaml")) incaml = 0;
      continue;
    }
    if (isprefix(line, "\\begin{verbatim")
        || isprefix(line, "\\begin{caml_")) {
      fputs(line, stdout);
      inverbatim = 1;
      continue;
    }
    if (isprefix(line, "\\caml")) {
      fputs(line, stdout);
      incaml = 1;
      continue;
    }
    inverb = 0;
    for (p = (unsigned char *) line; *p != 0; p++) {
      c = *p;
      if (inverb) {
        if (c == inverb) inverb = 0;
        putchar(c);
        continue;
      }
      switch(c) {
      case '"':
        if (inquote) {
          fputs("}", stdout);
          inquote = 0;
        } else {
          fputs("{\\machine ", stdout);
          inquote = 1;
        }
        break;
      case '\\':
        if (isprefix(p, "\\verb") && p[5] != 0 && !isalpha(p[5])) {
          inverb = p[5];
          p = p + 5;
          fputs("\\verb", stdout);
          putchar(inverb);
        } else if (inquote) {
          if (p[1] == '"' || p[1] == '\\') {
            c = p[1];
            p++;
          }
          if (transl[c] != NULL)
            fputs(transl[c], stdout);
          else
            putchar(c);
        } else {
          putchar('\\');
        }
        break;
      default:
        if (inquote && transl[c] != NULL)
          fputs(transl[c], stdout);
        else
          putchar(c);
      }
    }
  }
  return 0;
}
