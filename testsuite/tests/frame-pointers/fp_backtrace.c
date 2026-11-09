#include <execinfo.h>
#include <regex.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include "misc_internals.h"

#if defined(__APPLE__)
#define RE_FUNC_NAME "^[[:digit:]]+[[:space:]]+[[:alnum:]_\\.]+[[:space:]]+0x[[:xdigit:]]+[[:space:]]([[:alnum:]_\\$]+).*$"
#elif defined(__FreeBSD__) || defined(__DragonFly__)
#define RE_FUNC_NAME  "^0x[[:xdigit:]]+ <(.+)\\+0x[[:xdigit:]]+>.*$"
#else
#define RE_FUNC_NAME  "^.*\\((.+)\\+0x[[:xdigit:]]+\\) \\[0x[[:xdigit:]]+\\]$"
#endif
#define RE_TRIM_FUNC  "(caml.*)_[[:digit:]]+"
#define CAML_ENTRY    "caml_program"

/*
 * Stack frame layout differs by architecture:
 *
 * x86_64 / ARM64:
 *   offset 0: previous frame pointer
 *   offset 8: return address
 *   The return address for frame fi is at fi->retaddr.
 *
 * Power64 (ELFv2 ABI):
 *   offset 0: back chain (previous SP)
 *   offset 8: CR save area / TOC save area
 *   offset 16: LR save area (return address)
 *   IMPORTANT: On Power, the callee saves LR into the CALLER's frame at
 *   offset 16 before allocating its own frame. So the return address for
 *   frame fi is at fi->prev + 16, not fi + 16.
 */
#if defined(__powerpc64__)
typedef struct frame_info
{
  struct frame_info*  prev;     /* back chain at offset 0 */
} frame_info;

/* On Power, return address is saved by callee into caller's frame at offset 16 */
static inline void* get_retaddr(const struct frame_info* fi) {
  if (!fi->prev || (uintptr_t)fi->prev < 0x1000) return NULL;
  return *((void**)((char*)fi->prev + 16));
}
#else
typedef struct frame_info
{
  struct frame_info*  prev;     /* base pointer / frame pointer */
  void*               retaddr;  /* instruction pointer / program counter */
} frame_info;

static inline void* get_retaddr(const struct frame_info* fi) {
  return fi->retaddr;
}
#endif

/*
 * A backtrace symbol looks like this on Linux:
 * ./path/to/binary(camlModule.fn_123+0xAABBCC) [0xAABBCCDDEE]
 *
 * or this on macOS:
 * 0   c_call.opt                          0x000000010e621079 camlC_call.entry + 57
 *
 * or this on FreeBSD (or DragonFly):
 * 0x22eea7 <camlModule.fn_123+0xb7> at ./path/to/binary
 */
static const char* backtrace_symbol(const struct frame_info* fi)
{
  void* retaddr = get_retaddr(fi);
  if (!retaddr)
    return NULL;

  char** symbols = backtrace_symbols(&retaddr, 1);
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
    regerror(err, &regex, errbuf, countof(errbuf));
    fprintf(stderr, "regcomp: %s\n", errbuf);
    return match[0];
  }

  err = regexec(&regex, symbol, countof(match), match, 0);
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
    regerror(err, &regex, errbuf, countof(errbuf));
    fprintf(stderr, "regcomp: %s\n", errbuf);
    return match[0];
  }

  match[0] = *funcname;
  err = regexec(&regex, symbol, countof(match), match, REG_STARTEND);
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

#if defined(__APPLE__)
  /* Replace $ with . to normalize symbol names across platforms.
     None of the examples require escaping so we can safely
     replace just the character.
  */
  for (regoff_t i = 0; i < len; i++) {
    char c = symbol[off + i];
    fputc(c == '$' ? '.' : c, stdout);
  }
  fputc('\n', stdout);
#else
  fprintf(stdout, "%.*s\n", (int)len, symbol + off);
#endif
  fflush(stdout);
}

void fp_backtrace(CAMLunused value argv0)
{
  const char* symbol = NULL;

  for (struct frame_info *frame = __builtin_frame_address(0), *next = NULL;
       frame;
       frame = next) {
#if defined(__riscv)
    /* On RISC-V, __builtin_frame_address returns s0 = CFA, which points
       past the frame record.  Subtract one record to reach {prev, retaddr}. */
    frame--;
#endif
    next = frame->prev;

    /* Stop if back chain is NULL or points to very low memory (invalid) */
    if (!next || (uintptr_t)next < 0x1000) {
      break;
    }

    /* Detect the simplest kind of infinite loop */
#if defined(__riscv)
    /* On RISC-V, frame is CFA-16 (record) but next is a CFA value,
       so a self-loop means next == frame + 1 (in struct units). */
    if (next == frame + 1) {
#else
    if (frame == next) {
#endif
      fprintf(stderr, "fp_backtrace: loop detected\n");
      break;
    }

    symbol = backtrace_symbol(frame);
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
