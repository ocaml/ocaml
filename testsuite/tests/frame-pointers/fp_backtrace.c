#include <execinfo.h>
#include <regex.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>

#define ARR_SIZE(a)    (sizeof(a) / sizeof(*(a)))

#if defined(__APPLE__)
#define RE_FUNC_NAME "^[[:digit:]]+[[:space:]]+[[:alnum:]_\\.]+[[:space:]]+0x[[:xdigit:]]+[[:space:]]([[:alnum:]_\\$]+).*$"
#else
#define RE_FUNC_NAME  "^.*\\((.+)\\+0x[[:xdigit:]]+\\) \\[0x[[:xdigit:]]+\\]$"
#endif
#define RE_TRIM_FUNC  "(caml.*)_[[:digit:]]+"
#define CAML_ENTRY    "caml_program"

typedef struct frame_info
{
  struct frame_info*  prev;     /* base pointer / frame pointer */
  void*               retaddr;  /* instruction pointer / program counter */
} frame_info;

/*
 * A backtrace symbol looks like this on Linux:
 * ./path/to/binary(camlModule.fn_123+0xAABBCC) [0xAABBCCDDEE]
 *
 * or this on macOS:
 * 0   c_call.opt                          0x000000010e621079 camlC_call.entry + 57
 *
 */
static const char* backtrace_symbol(const struct frame_info* fi)
{
  char** symbols = backtrace_symbols(&fi->retaddr, 1);
  if (!symbols) {
    perror("backtrace_symbols");
    return NULL;
  }

  const char* symbol = strdup(symbols[0]);
  free(symbols);
  return symbol;
}

static regmatch_t func_name_from_symbol(const char* symbol)
{
  regex_t     regex;
  regmatch_t  match[2] = { {-1, -1}, {-1, -1}};
  char        errbuf[128];
  int         err;

  err = regcomp(&regex, RE_FUNC_NAME, REG_EXTENDED);
  if (err) {
    regerror(err, &regex, errbuf, ARR_SIZE(errbuf));
    fprintf(stderr, "regcomp: %s\n", errbuf);
    return match[0];
  }

  err = regexec(&regex, symbol, ARR_SIZE(match), match, 0);
  if (err == REG_NOMATCH)
    return match[0];

  return match[1];
}

static bool is_caml_entry(const char* symbol, const regmatch_t* funcname)
{
  size_t len = funcname->rm_eo - funcname->rm_so;
  return strncmp(symbol + funcname->rm_so, CAML_ENTRY, len) == 0;
}

static regmatch_t trim_func_name(const char* symbol, const regmatch_t* funcname)
{
  regex_t     regex;
  regmatch_t  match[2] = { {-1, -1}, {-1, -1}};
  char        errbuf[128];
  int         err;

  err = regcomp(&regex, RE_TRIM_FUNC, REG_EXTENDED);
  if (err) {
    regerror(err, &regex, errbuf, ARR_SIZE(errbuf));
    fprintf(stderr, "regcomp: %s\n", errbuf);
    return match[0];
  }

  match[0] = *funcname;
  err = regexec(&regex, symbol, ARR_SIZE(match), match, REG_STARTEND);
  if (err == REG_NOMATCH) {
    /* match[0] has already been overwritten to hold the function full name for
       regexec */
    return match[1];
  }

  return match[1];
}

static void print_symbol(const char* symbol, const regmatch_t* match)
{
  regoff_t off = match->rm_so;
  regoff_t len = match->rm_eo - match->rm_so;

  fprintf(stdout, "%.*s\n", (int)len, symbol + off);
  fflush(stdout);
}

void fp_backtrace(value argv0)
{
  const char* execname = String_val(argv0);
  const char* symbol = NULL;

  for (struct frame_info *fi = __builtin_frame_address(0), *next = NULL;
       fi;
       fi = next) {
    next = fi->prev;

    /* Detect the simplest kind of infinite loop */
    if (fi == next) {
      fprintf(stderr, "fp_backtrace: loop detected\n");
      break;
    }

    symbol = backtrace_symbol(fi);
    if (!symbol)
      continue;

    /* Extract the full function name */
    regmatch_t funcname = func_name_from_symbol(symbol);
    if (funcname.rm_so == -1)
      goto skip;

    /* Trim numeric suffix from caml functions */
    regmatch_t functrimmed = trim_func_name(symbol, &funcname);

    /* Use the trimmed caml name if available, otherwise use the full function
       name */
    const regmatch_t* match = (functrimmed.rm_so != -1) ?
      &functrimmed : &funcname;

    print_symbol(symbol, match);

    /* Stop the backtrace at caml_program */
    if (is_caml_entry(symbol, &funcname))
      break;

skip:
    free((void*)symbol);
    symbol = NULL;
  }

  if (symbol)
    free((void*)symbol);
}
