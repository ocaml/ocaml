/*
   PCRE-OCAML - Perl Compatibility Regular Expressions for OCaml

   Copyright (C) 1999-2005  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* $Id$ */

#if defined(_WIN32) && defined(_DLL)
#  define PCREextern __declspec(dllexport)
#else
#  define PCREextern
#endif

#include <ctype.h>
#include <string.h>

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <callback.h>

#include <pcre.h>

typedef const unsigned char *chartables;  /* Type of chartable sets */

/* Contents of callout data */
struct cod {
  value v_substrings;  /* Substrings matched so far */
  value v_cof;         /* Callout function */
  value v_exn;         /* Possible exception raised by callout function */
};

/* Cache for exceptions */
static value *pcre_exc_Not_found     = NULL;  /* Exception [Not_found] */
static value *pcre_exc_BadPattern    = NULL;  /* Exception [BadPattern] */
static value *pcre_exc_BadUTF8       = NULL;  /* Exception [BadUTF8] */
static value *pcre_exc_BadUTF8Offset = NULL;  /* Exception [BadUTF8Offset] */
static value *pcre_exc_InternalError = NULL;  /* Exception [InternalError] */
static value *pcre_exc_MatchLimit    = NULL;  /* Exception [MatchLimit] */
static value *pcre_exc_Backtrack     = NULL;  /* Exception [Backtrack] */

/* Cache for polymorphic variants */
static value var_Start_only;   /* Variant [`Start_only] */
static value var_ANCHORED;     /* Variant [`ANCHORED] */
static value var_Char;         /* Variant [`Char char] */
static value var_Not_studied;  /* Variant [`Not_studied] */
static value var_Studied;      /* Variant [`Studied] */
static value var_Optimal;      /* Variant [`Optimal] */

static value None = Val_int(0);

/* Callout handler */
static int pcre_callout_handler(pcre_callout_block* cb)
{
  struct cod *cod = (struct cod *) cb->callout_data;

  if (cod != NULL) {
    /* Callout is available */
    value v_res;
    const value v_substrings = cod->v_substrings;

    const int capture_top = cb->capture_top;
    int subgroups2 = capture_top << 1;
    const int subgroups2_1 = subgroups2 - 1;

    const int *ovec_src = cb->offset_vector + subgroups2_1;
    long int *ovec_dst = &Field(Field(v_substrings, 1), 0) + subgroups2_1;

    value params[6];
    value *pptr = &params[0];

    /* Copy preliminary substring information */
    while (subgroups2--) {
      *ovec_dst = Val_int(*ovec_src);
      --ovec_src; --ovec_dst;
    }

    /* Set up parameter array */
    *pptr = v_substrings; ++pptr;
    *pptr = Val_int(cb->start_match); ++pptr;
    *pptr = Val_int(cb->current_position); ++pptr;
    *pptr = Val_int(capture_top); ++pptr;
    *pptr = Val_int(cb->capture_last); ++pptr;
    *pptr = Val_int(cb->callout_number);

    /* Perform callout */
    v_res = callbackN_exn(cod->v_cof, 6, params);

    if (Is_exception_result(v_res)) {
      /* Callout raised an exception */
      const value v_exn = Extract_exception(v_res);
      if (Field(v_exn, 0) == *pcre_exc_Backtrack) return 1;
      cod->v_exn = v_exn;
      return PCRE_ERROR_CALLOUT;
    }
  }

  return 0;
}

/* Fetchs the named OCaml-values + caches them and
   calculates + caches the variant hash values */
CAMLprim value pcre_ocaml_init(value unit)
{
  pcre_exc_Not_found     = caml_named_value("Pcre.Not_found");
  pcre_exc_BadPattern    = caml_named_value("Pcre.BadPattern");
  pcre_exc_BadUTF8       = caml_named_value("Pcre.BadUTF8");
  pcre_exc_InternalError = caml_named_value("Pcre.InternalError");
  pcre_exc_MatchLimit    = caml_named_value("Pcre.MatchLimit");
  pcre_exc_Backtrack     = caml_named_value("Pcre.Backtrack");

  var_Start_only         = hash_variant("Start_only");
  var_ANCHORED           = hash_variant("ANCHORED");
  var_Char               = hash_variant("Char");
  var_Not_studied        = hash_variant("Not_studied");
  var_Studied            = hash_variant("Studied");
  var_Optimal            = hash_variant("Optimal");

  pcre_callout = &pcre_callout_handler;

  return Val_unit;
}

/* Finalizing deallocation function for chartable sets */
static void pcre_dealloc_tables(value v_table)
{ (pcre_free)((void *) Field(v_table, 1)); }

/* Finalizing deallocation function for compiled regular expressions */
static void pcre_dealloc_regexp(value v_rex)
{
  void *extra = (void *) Field(v_rex, 2);
  (pcre_free)((void *) Field(v_rex, 1));
  if (extra != NULL) (pcre_free)(extra);
}

/* Raises exceptions which take two arguments */
static void raise_with_two_args(value tag, value arg1, value arg2)
{
  value v_exc;

  /* Protects tag, arg1 and arg2 from being reclaimed by the garbage
     collector when the exception value is allocated */
  Begin_roots3(tag, arg1, arg2);
    v_exc = alloc_small(3, 0);
    Field(v_exc, 0) = tag;
    Field(v_exc, 1) = arg1;
    Field(v_exc, 2) = arg2;
  End_roots();

  mlraise(v_exc);
}

/* Makes OCaml-string from PCRE-version */
CAMLprim value pcre_version_stub(value unit) {
  return copy_string((char *) pcre_version());
}

/* Makes compiled regular expression from compilation options, an optional
   value of chartables and the pattern string */
CAMLprim value pcre_compile_stub(value v_opt, value v_tables, value v_pat)
{
  value v_rex;  /* Final result -> value of type [regexp] */
  const char *error = NULL;  /* pointer to possible error message */
  int error_ofs = 0;  /* offset in the pattern at which error occurred */

  /* If v_tables = [None], then pointer to tables is NULL, otherwise
     set it to the appropriate value */
  chartables tables =
    (v_tables == None) ? NULL : (chartables) Field(Field(v_tables, 0), 1);

  /* Compiles the pattern */
  pcre *regexp = pcre_compile(String_val(v_pat), Int_val(v_opt), &error,
                              &error_ofs, tables);

  /* Raises appropriate exception [BadPattern] if the pattern could not
     be compiled */
  if (regexp == NULL) raise_with_two_args(*pcre_exc_BadPattern,
                                          copy_string((char *) error),
                                          Val_int(error_ofs));

  /* Finalized value: GC will do a full cycle every 500 regexp allocations
     (one regexp consumes in average probably less than 100 bytes ->
     maximum of 50000 bytes unreclaimed regexps) */
  v_rex = alloc_final(4, pcre_dealloc_regexp, 100, 50000);

  /* Field[1]: compiled regular expression (Field[0] is finalizing
     function! See above!) */
  Field(v_rex, 1) = (value) regexp;

  /* Field[2]: extra information about regexp when it has been studied
     successfully */
  Field(v_rex, 2) = (value) NULL;

  /* Field[3]: If 0 -> regexp has not yet been studied
                  1 -> regexp has already been studied */
  Field(v_rex, 3) = 0;

  return v_rex;
}

/* Studies a regexp */
CAMLprim value pcre_study_stub(value v_rex)
{
  /* If it has not yet been studied */
  if (! (int) Field(v_rex, 3)) {
    const char *error = NULL;
    pcre_extra *extra = pcre_study((pcre *) Field(v_rex, 1), 0, &error);
    if (error != NULL) invalid_argument((char *) error);
    Field(v_rex, 2) = (value) extra;
    Field(v_rex, 3) = Val_int(1);
  }
  return v_rex;
}

/* Sets a match limit for a regular expression imperatively */
CAMLprim value pcre_set_imp_match_limit_stub(value v_rex, value v_lim){
  pcre_extra *extra = (pcre_extra *) Field(v_rex, 2);
  if (extra == NULL) {
    extra = pcre_malloc(sizeof(pcre_extra));
    extra->flags = PCRE_EXTRA_MATCH_LIMIT;
    Field(v_rex, 2) = (value) extra;
  }
  else {
    unsigned long int *flags_ptr = &extra->flags;
    *flags_ptr = PCRE_EXTRA_MATCH_LIMIT | *flags_ptr;
  }
  extra->match_limit = Int_val(v_lim);
  return v_rex;
}

/* Gets the match limit of a regular expression if it exists */
CAMLprim value pcre_get_match_limit_stub(value v_rex){
  pcre_extra *extra = (pcre_extra *) Field(v_rex, 2);
  if (extra == NULL) return None;
  if (extra->flags & PCRE_EXTRA_MATCH_LIMIT) {
    value lim = Val_int(extra->match_limit);
    value res = alloc_small(1, 0);
    Field(res, 0) = lim;
    return res;
  }
  return None;
}

/* Performs the call to the pcre_fullinfo function */
static value pcre_fullinfo_stub(value v_rex, int what, void *where)
{
  return pcre_fullinfo((pcre *) Field(v_rex, 1), (pcre_extra *) Field(v_rex, 2),
                       what, where);
}

/* Some stubs for info-functions */

/* Generic macro for getting integer results from pcre_fullinfo */
#define make_int_info(name, option) \
  CAMLprim value pcre_##name##_stub(value v_rex) \
  { \
    int options; \
    const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_##option, &options); \
    if (ret != 0) \
      raise_with_string(*pcre_exc_InternalError, "pcre_##name##_stub"); \
    return Val_int(options); \
  }

make_int_info(options, OPTIONS)
make_int_info(size, SIZE)
make_int_info(studysize, STUDYSIZE)
make_int_info(capturecount, CAPTURECOUNT)
make_int_info(backrefmax, BACKREFMAX)
make_int_info(namecount, NAMECOUNT)
make_int_info(nameentrysize, NAMEENTRYSIZE)

CAMLprim value pcre_firstbyte_stub(value v_rex)
{
  int firstbyte;
  const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_FIRSTBYTE, &firstbyte);

  if (ret != 0) raise_with_string(*pcre_exc_InternalError,
                                  "pcre_firstbyte_stub");

  switch (firstbyte) {
    case -1 : return var_Start_only; break;  /* [`Start_only] */
    case -2 : return var_ANCHORED; break;    /* [`ANCHORED] */
    default :
      if (firstbyte < 0 )  /* Should not happen */
        raise_with_string(*pcre_exc_InternalError, "pcre_firstbyte_stub");
      else {
        value v_firstbyte;
        /* Allocates the non-constant constructor [`Char of char] and fills
           in the appropriate value */
        v_firstbyte = alloc_small(2, 0);
        Field(v_firstbyte, 0) = var_Char;
        Field(v_firstbyte, 1) = Val_int(firstbyte);
        return v_firstbyte;
      }
  }
}

CAMLprim value pcre_firsttable_stub(value v_rex)
{
  const unsigned char *ftable;

  int ret =
    pcre_fullinfo_stub(v_rex, PCRE_INFO_FIRSTTABLE, (void *) &ftable);

  if (ret != 0) raise_with_string(*pcre_exc_InternalError,
                                  "pcre_firsttable_stub");

  if (ftable == NULL) return None;
  else {
    value v_res, v_res_str;
    char *ptr;
    int i;

    Begin_roots1(v_rex);
      v_res_str = alloc_string(32);
    End_roots();

    ptr = String_val(v_res_str);
    for (i = 0; i <= 31; ++i) { *ptr = *ftable; ++ptr; ++ftable; }

    Begin_roots1(v_res_str);
      /* Allocates [Some string] from firsttable */
      v_res = alloc_small(1, 0);
    End_roots();

    Field(v_res, 0) = v_res_str;

    return v_res;
  }
}

CAMLprim value pcre_lastliteral_stub(value v_rex)
{
  int lastliteral;
  const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_LASTLITERAL,
                                        &lastliteral);

  if (ret != 0) raise_with_string(*pcre_exc_InternalError,
                                  "pcre_lastliteral_stub");

  if (lastliteral == -1) return None;
  if (lastliteral < 0) raise_with_string(*pcre_exc_InternalError,
                                         "pcre_lastliteral_stub");
  else {
    /* Allocates [Some char] */
    value v_res = alloc_small(1, 0);
    Field(v_res, 0) = Val_int(lastliteral);
    return v_res;
  }
}

CAMLprim value pcre_study_stat_stub(value v_rex)
{
  /* Generates the appropriate constant constructor [`Optimal] or
     [`Studied] if regexp has already been studied */
  if (Field(v_rex, 3))
    return ((pcre_extra *) Field(v_rex, 2) == NULL) ? var_Optimal : var_Studied;

  return var_Not_studied;  /* otherwise [`Not_studied] */
}

/* Executes a pattern match with runtime options, a regular expression, a
   string offset, a string length, a subject string, a number of subgroup
   offsets, an offset vector and an optional callout function */
CAMLprim value pcre_exec_stub(value v_opt, value v_rex, value v_ofs,
                              value v_subj, value v_subgroups2, value v_ovec,
                              value v_maybe_cof)
{
  const int ofs = Int_val(v_ofs), len = string_length(v_subj);

  if (ofs > len || ofs < 0)
    invalid_argument("Pcre.pcre_exec_stub: illegal offset");

  {
    const pcre *code = (pcre *) Field(v_rex, 1);  /* Compiled pattern */
    const pcre_extra *extra = (pcre_extra *) Field(v_rex, 2);  /* Extra info */
    const char *ocaml_subj = String_val(v_subj);  /* Subject string */
    const int opt = Int_val(v_opt);  /* Runtime options */
    int subgroups2 = Int_val(v_subgroups2);
    const int subgroups2_1 = subgroups2 - 1;
    const int subgroups3 = (subgroups2 >> 1) + subgroups2;

    /* Special case when no callout functions specified */
    if (v_maybe_cof == None) {
      int *ovec = (int *) &Field(v_ovec, 0);

      /* Performs the match */
      const int ret =
        pcre_exec(code, extra, ocaml_subj, len, ofs, opt, ovec, subgroups3);

      if (ret < 0) {
        switch(ret) {
          case PCRE_ERROR_NOMATCH : raise_constant(*pcre_exc_Not_found);
          case PCRE_ERROR_MATCHLIMIT : raise_constant(*pcre_exc_MatchLimit);
          case PCRE_ERROR_BADUTF8 : raise_constant(*pcre_exc_BadUTF8);
          case PCRE_ERROR_BADUTF8_OFFSET :
            raise_constant(*pcre_exc_BadUTF8Offset);
          default :
            raise_with_string(*pcre_exc_InternalError, "pcre_exec_stub");
        }
      }

      else {
        const int *ovec_src = ovec + subgroups2_1;
        long int *ovec_dst = (long int *) ovec + subgroups2_1;

        /* Converts offsets from C-integers to OCaml-Integers
           This is a bit tricky, because there are 32- and 64-bit platforms
           around and OCaml chooses the larger possibility for representing
           integers when available (also in arrays) - not so the PCRE */
        while (subgroups2--) {
          *ovec_dst = Val_int(*ovec_src);
          --ovec_src; --ovec_dst;
        }
      }
    }

    /* There are callout functions */
    else {
      value v_cof = Field(v_maybe_cof, 0);
      value v_substrings;
      char *subj = malloc(sizeof(char) * len);
      int *ovec = malloc(sizeof(int) * subgroups3);
      int ret;
      struct cod cod = { (value) NULL, (value) NULL, (value) NULL };
      struct pcre_extra new_extra = { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL };

      memcpy(subj, ocaml_subj, len);

      Begin_roots2(v_rex, v_cof);
        Begin_roots2(v_subj, v_ovec);
          v_substrings = alloc_small(2, 0);
        End_roots();

        Field(v_substrings, 0) = v_subj;
        Field(v_substrings, 1) = v_ovec;

        cod.v_substrings = v_substrings;
        cod.v_cof = v_cof;
        new_extra.callout_data = &cod;

        if (extra == NULL) {
          ret = pcre_exec(code, &new_extra, subj, len, ofs, opt, ovec,
                          subgroups3);
        }
        else {
          new_extra.flags = PCRE_EXTRA_CALLOUT_DATA | extra->flags;
          new_extra.study_data = extra->study_data;
          new_extra.match_limit = extra->match_limit;

          ret = pcre_exec(code, &new_extra, subj, len, ofs, opt, ovec,
                          subgroups3);
        }

        free(subj);
      End_roots();

      if (ret < 0) {
        free(ovec);
        switch(ret) {
          case PCRE_ERROR_NOMATCH : raise_constant(*pcre_exc_Not_found);
          case PCRE_ERROR_MATCHLIMIT : raise_constant(*pcre_exc_MatchLimit);
          case PCRE_ERROR_BADUTF8 : raise_constant(*pcre_exc_BadUTF8);
          case PCRE_ERROR_BADUTF8_OFFSET :
            raise_constant(*pcre_exc_BadUTF8Offset);
          case PCRE_ERROR_CALLOUT : mlraise(cod.v_exn);
          default :
            raise_with_string(*pcre_exc_InternalError, "pcre_exec_stub");
        }
      }

      else {
        int *ovec_src = ovec + subgroups2_1;
        long int *ovec_dst = &Field(v_ovec, 0) + subgroups2_1;

        while (subgroups2--) {
          *ovec_dst = Val_int(*ovec_src);
          --ovec_src; --ovec_dst;
        }

        free(ovec);
      }
    }
  }

  return Val_unit;
}

/* Byte-code hook for pcre_exec_stub
   Needed, because there are more than 5 arguments */
CAMLprim value pcre_exec_stub_bc(value *argv, int argn)
{
  return pcre_exec_stub(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}

/* Generates a new set of chartables for the current locale (see man
   page of PCRE */
CAMLprim value pcre_maketables_stub(value unit)
{
  /* GC will do a full cycle every 100 table set allocations
     (one table set consumes 864 bytes -> maximum of 86400 bytes
     unreclaimed table sets) */
  const value v_res = alloc_final(2, pcre_dealloc_tables, 864, 86400);
  Field(v_res, 1) = (value) pcre_maketables();
  return v_res;
}

/* Wraps around the isspace-function */
CAMLprim value pcre_isspace_stub(value v_c)
{
  return Val_bool(isspace(Int_val(v_c)));
}

/* Returns number of substring associated with a name */
CAMLprim value pcre_get_stringnumber_stub(value v_rex, value v_name)
{
  const int ret = pcre_get_stringnumber((pcre *) Field(v_rex, 1),
                                        String_val(v_name));
  if (ret == PCRE_ERROR_NOSUBSTRING) invalid_argument("Named string not found");
  return Val_int(ret);
}

/* Generic stub for getting integer results from pcre_config */
static int pcre_config_int(int what)
{
  int ret;
  pcre_config(what, (void *) &ret);
  return ret;
}

/* Some stubs for config-functions */

/* Returns boolean indicating UTF8-support */
CAMLprim value pcre_config_utf8_stub(value unit)
{ return Val_bool(pcre_config_int(PCRE_CONFIG_UTF8)); }

/* Returns character used as newline */
CAMLprim value pcre_config_newline_stub(value unit)
{ return Val_int(pcre_config_int(PCRE_CONFIG_NEWLINE)); }

/* Returns number of bytes used for internal linkage of regular expressions */
CAMLprim value pcre_config_link_size_stub(value unit)
{ return Val_int(pcre_config_int(PCRE_CONFIG_LINK_SIZE)); }

/* Returns default limit for calls to internal matching function */
CAMLprim value pcre_config_match_limit_stub(value unit)
{ return Val_int(pcre_config_int(PCRE_CONFIG_MATCH_LIMIT)); }

/* Returns boolean indicating use of stack recursion */
CAMLprim value pcre_config_stackrecurse_stub(value unit)
{ return Val_bool(pcre_config_int(PCRE_CONFIG_STACKRECURSE)); }
