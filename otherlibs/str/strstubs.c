#if !macintosh
#include <sys/types.h>
#else
#include <SizeTDef.h>
#endif
#include <string.h>
#include <regex.h>
#include <mlvalues.h>
#include <alloc.h>
#include <custom.h>
#include <fail.h>
#include <memory.h>

struct regexp_struct {
  struct custom_operations * ops;
  struct re_pattern_buffer re;
};

typedef struct regexp_struct * regexp;

static void free_regexp(value vexpr)
{
  regexp expr = (regexp) Bp_val(vexpr);
  expr->re.translate = NULL;
  re_free(&(expr->re));
}

static struct custom_operations regexp_ops = {
  "_regexp",
  free_regexp,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static regexp alloc_regexp(void)
{
  value res =
    alloc_custom(&regexp_ops, sizeof(struct regexp_struct), 1, 10000);
  return (regexp) res;
}

#define RE_SYNTAX RE_SYNTAX_EMACS

static char * case_fold_table = NULL;

CAMLprim value str_compile_regexp(value src, value fold)
{
  regexp expr;
  char * msg;

  Begin_root(src);
  expr = alloc_regexp();
  End_roots();
  re_syntax_options = RE_SYNTAX;
  if (Bool_val(fold) && case_fold_table == NULL) {
    int i;
    case_fold_table = stat_alloc(256);
    for (i = 0; i <= 255; i++) case_fold_table[i] = i;
    for (i = 'A'; i <= 'Z'; i++) case_fold_table[i] = i + 32;
    for (i = 192; i <= 214; i++) case_fold_table[i] = i + 32;
    for (i = 216; i <= 222; i++) case_fold_table[i] = i + 32;
  }
  expr->re.translate = Bool_val(fold) ? case_fold_table : NULL;
  expr->re.fastmap = stat_alloc(256);
  expr->re.buffer = NULL;
  expr->re.allocated = 0;
  msg = (char *) re_compile_pattern(String_val(src), string_length(src),
                                    &(expr->re));
  if (msg != NULL) failwith(msg);
  re_compile_fastmap(&(expr->re));
  expr->re.regs_allocated = REGS_FIXED;
  return (value) expr;
}

static regoff_t start_regs[10], end_regs[10];

static struct re_registers match_regs = { 10, start_regs, end_regs };

CAMLprim value str_string_match(regexp expr, value text, value pos)
{
  int len = string_length(text);
  int start = Int_val(pos);
  if (start < 0 || start > len)
    invalid_argument("Str.string_match");
  switch (re_match(&(expr->re), String_val(text), len,
                   start, &match_regs)) {
  case -2:
    failwith("Str.string_match");
  case -1:
  case -3:
    return Val_false;
  default:
    return Val_true;
  }
}

CAMLprim value str_string_partial_match(regexp expr, value text, value pos)
{
  int len = string_length(text);
  int start = Int_val(pos);
  if (start < 0 || start > len)
    invalid_argument("Str.string_partial_match");
  switch (re_match(&(expr->re), String_val(text), len,
                   start, &match_regs)) {
  case -2:
    failwith("Str.string_partial_match");
  case -1:
    return Val_false;
  default:
    return Val_true;
  }
}

CAMLprim value str_search_forward(regexp expr, value text, value pos)
{
  int res;
  int len = string_length(text);
  int start = Int_val(pos);
  if (start < 0 || start > len)
    invalid_argument("Str.search_forward");
  res = re_search(&(expr->re), String_val(text), len, start, len-start,
                  &match_regs);
  switch(res) {
  case -2:
    failwith("Str.search_forward");
  case -1:
    raise_not_found();
  default:
    return Val_int(res);
  }
}

CAMLprim value str_search_backward(regexp expr, value text, value pos)
{
  int res;
  int len = string_length(text);
  int start = Int_val(pos);
  if (start < 0 || start > len)
    invalid_argument("Str.search_backward");
  res = re_search(&(expr->re), String_val(text), len, start, -start-1,
                  &match_regs);
  switch(res) {
  case -2:
    failwith("Str.search_backward");
  case -1:
    raise_not_found();
  default:
    return Val_int(res);
  }
}

CAMLprim value str_beginning_group(value ngroup)
{
  return Val_int(start_regs[Int_val(ngroup)]);
}

CAMLprim value str_end_group(value ngroup)
{
  return Val_int(end_regs[Int_val(ngroup)]);
}

CAMLprim value str_replacement_text(value repl, value orig)
{
  value res;
  mlsize_t len, n;
  char * p, * q;
  int c;

  len = 0;
  p = String_val(repl);
  n = string_length(repl);
  while (n > 0) {
    c = *p++; n--;
    if(c != '\\')
      len++;
    else {
      if (n == 0) failwith("Str.replace: illegal backslash sequence");
      c = *p++; n--;
      switch (c) {
      case '\\':
        len++; break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c -= '0';
        len += end_regs[c] - start_regs[c]; break;
      default:
        len += 2; break;
      }
    }
  }
  Begin_roots2(orig,repl);
    res = alloc_string(len);
  End_roots();
  p = String_val(repl);
  q = String_val(res);
  n = string_length(repl);
  while (n > 0) {
    c = *p++; n--;
    if(c != '\\')
      *q++ = c;
    else {
      c = *p++; n--;
      switch (c) {
      case '\\':
        *q++ = '\\'; break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c -= '0';
        len = end_regs[c] - start_regs[c];
        memmove (q, &Byte(orig, start_regs[c]), len);
        q += len;
        break;
      default:
        *q++ = '\\'; *q++ = c; break;
      }
    }
  }
  return res;
}

