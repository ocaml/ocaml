/***********************************************************************/
/*                                                                     */
/*                 MLTk, Tcl/Tk interface of Objective Caml            */
/*                                                                     */
/*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    */
/*               projet Cristal, INRIA Rocquencourt                    */
/*            Jacques Garrigue, Kyoto University RIMS                  */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique and Kyoto University.  All rights reserved.         */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file LICENSE found in the Objective Caml source tree. */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdlib.h>

#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "camltk.h"

/* UTF
#if (TCL_MAJOR_VERSION > 8 || \
    (TCL_MAJOR_VERSION == 8 && TCL_MINOR_VERSION >= 1)) /-* 8.1 *-/
# define UTFCONVERSION
#endif
*/

/* The Tcl interpretor */
Tcl_Interp *cltclinterp = NULL;

/* Copy a list of strings from the C heap to Caml */
value copy_string_list(int argc, char **argv)
{
  value res;
  int i;
  value oldres = Val_unit, str = Val_unit;

  Begin_roots2 (oldres, str);
    res = Val_int(0); /* [] */
    for (i = argc-1; i >= 0; i--) {
      oldres = res;
      str = copy_string(argv[i]);
      res = alloc(2, 0);
      Field(res, 0) = str;
      Field(res, 1) = oldres;
    }
  End_roots();
  return res;
}

/*
 * Calling Tcl from Caml
 *   this version works on an arbitrary Tcl command,
 *   and does parsing and substitution
 */
CAMLprim value camltk_tcl_eval(value str)
{
  int code;
  char *cmd = NULL;

  CheckInit();
  
  /* Tcl_Eval may write to its argument, so we take a copy
   * If the evaluation raises a Caml exception, we have a space
   * leak
   */
  Tcl_ResetResult(cltclinterp);
  cmd = string_to_c(str);

/* UTF
#ifdef UTFCONVERSION
  {
    char *utfcmd = NULL;
    Tcl_DString utf;
    int length;

    Tcl_ExternalToUtfDString(NULL, cmd, strlen(cmd), &utf);
    length = Tcl_DStringLength(&utf);
    utfcmd = stat_alloc(length + 1);
    memmove( utfcmd, Tcl_DStringValue(&utf), length+1);
    Tcl_DStringFree(&utf);
//fprintf(stderr,"UTF [%s] -> [%s]\n", cmd, utfcmd);
    code = Tcl_Eval(cltclinterp, utfcmd);
    stat_free(utfcmd);
    stat_free(cmd);
  }
#else
*/
  code = Tcl_Eval(cltclinterp, cmd);
  stat_free(cmd);
/* UTF
#endif
*/

  switch (code) {
  case TCL_OK:
    return copy_string(cltclinterp->result);
  case TCL_ERROR:
    tk_error(cltclinterp->result);
  default:  /* TCL_BREAK, TCL_CONTINUE, TCL_RETURN */
    tk_error("bad tcl result");
  }
}


/* 
 * Calling Tcl from Caml
 *   direct call, argument is TkArgs vect
  type TkArgs =
      TkToken of string
    | TkTokenList of TkArgs list                (* to be expanded *)
    | TkQuote of TkArgs                         (* mapped to Tcl list *)
 * NO PARSING, NO SUBSTITUTION
 */

/* 
 * Compute the size of the argument (of type TkArgs). 
 * TkTokenList must be expanded,
 * TkQuote count for one.
 */
int argv_size(value v)
{
  switch (Tag_val(v)) {
  case 0:                       /* TkToken */
    return 1;
  case 1:                       /* TkTokenList */
    { int n;
      value l;
      for (l=Field(v,0), n=0; Is_block(l); l=Field(l,1))
        n+=argv_size(Field(l,0));
      return n;
    }
  case 2:                       /* TkQuote */
    return 1;
  default:
    tk_error("argv_size: illegal tag");
  }
}

/* 
 * Memory of allocated Tcl lists.
 * We should not need more than MAX_LIST
 */
#define MAX_LIST 256
static char *tcllists[MAX_LIST];

static int startfree = 0;
/* If size is lower, do not allocate */
static char *quotedargv[16];

/* Fill a preallocated vector arguments, doing expansion and all.
 * Assumes Tcl will 
 *  not tamper with our strings
 *  make copies if strings are "persistent"
 */
/* UTF
 * We also UTF convert the strings (if tk8.1 or higher )
 */
int fill_args (char **argv, int where, value v)
{
  switch (Tag_val(v)) {
  case 0:
/* UTF
#ifdef UTFCONVERSION    
    {
      Tcl_DString utf;
      int length;
      Tcl_ExternalToUtfDString(NULL, 
			       (char *)Field(v,0), string_length(Field(v,0)), 
			       &utf);
fprintf(stderr,"UTF %s\n", Tcl_DStringValue(&utf));
      length = Tcl_DStringLength(&utf);
      /-* must freed by stat_free *-/
      argv[where] = (char *)stat_alloc(length * sizeof(char) + 1);
      memmove(argv[where], Tcl_DStringValue(&utf), Tcl_DStringLength(&utf)+1);
      Tcl_DStringFree(&utf);
    }
#else
*/
    argv[where] = string_to_c(Field(v,0)); /* must free by stat_free */
/* UTF
#endif
*/
    return (where + 1);
  case 1:
    { value l;
      for (l=Field(v,0); Is_block(l); l=Field(l,1))
        where = fill_args(argv,where,Field(l,0));
      return where;
    }
  case 2:
    { char **tmpargv;
      int size = argv_size(Field(v,0));
/* WRONG CODE (for complex cases, this can overwrite previous args)
      if (size < 16)
        tmpargv = &quotedargv[0];
      else
*/
      tmpargv = (char **)stat_alloc((size + 1) * sizeof(char *));
      fill_args(tmpargv,0,Field(v,0));
      tmpargv[size] = NULL;
      argv[where] = Tcl_Merge(size,tmpargv);
      tcllists[startfree++] = argv[where]; /* so we can free it by Tcl_Free*/
      stat_free((char *)tmpargv);
      return (where + 1);
    }
  default:
    tk_error("fill_args: illegal tag");
  }
}

/* v is an array of TkArg */
CAMLprim value camltk_tcl_direct_eval(value v)
{
  int i;
  int size;                     /* size of argv */
  char **argv;
  int result;
  Tcl_CmdInfo info;
  int wherewasi,whereami;       /* positions in tcllists array */
/* UTF
  char **utfargv;
*/

  CheckInit();

  /* walk the array to compute final size for Tcl */
  for(i=0,size=0;i<Wosize_val(v);i++)
    size += argv_size(Field(v,i));

  /* +2: one slot for NULL
         one slot for "unknown" if command not found */
  argv = (char **)stat_alloc((size + 2) * sizeof(char *));

  wherewasi = startfree; /* should be zero except when nested calls */
  Assert(startfree < MAX_LIST);

  /* Copy */
  {
    int where;
    for(i=0, where=0;i<Wosize_val(v);i++)
      where = fill_args(argv,where,Field(v,i));
    argv[size] = NULL;
    argv[size + 1] = NULL;
  }

/* needless
  Begin_roots_block ((value *) argv, size + 2);
*/

  whereami = startfree;

    /* Eval */
    Tcl_ResetResult(cltclinterp);
    if (Tcl_GetCommandInfo(cltclinterp,argv[0],&info)) { /* command found */
#if (TCL_MAJOR_VERSION >= 8)
      /* info.proc might be a NULL pointer
       * We should probably attempt an Obj invocation, but the following quick
       * hack is easier.
       */
      if (info.proc == NULL) {
        Tcl_DString buf;
        char *string;
        Tcl_DStringInit(&buf);
        Tcl_DStringAppend(&buf, argv[0], -1);
        for (i=1; i<size; i++) {
          Tcl_DStringAppend(&buf, " ", -1);
          Tcl_DStringAppend(&buf, argv[i], -1);
        }
        /* fprintf(stderr,"80 compat: %s\n", argv[0]); */
        result = Tcl_Eval(cltclinterp, Tcl_DStringValue(&buf));
        Tcl_DStringFree(&buf);
/* UTF
#ifdef UTFCONVERSION
  for(i=0; i< size; i ++){
    stat_free((char *) argv[i]);
  }
#endif
*/
      }
      else
        result = (*info.proc)(info.clientData,cltclinterp,size,argv);
#else
      result = (*info.proc)(info.clientData,cltclinterp,size,argv);
#endif
    } else {/* implement the autoload stuff */
      if (Tcl_GetCommandInfo(cltclinterp,"unknown",&info)) { /* unknown found */
        for (i = size; i >= 0; i--)
          argv[i+1] = argv[i];
        argv[0] = "unknown";
        result = (*info.proc)(info.clientData,cltclinterp,size+1,argv);
/* UTF
#ifdef UTFCONVERSION
  for(i=1; i< size+1; i ++){
    stat_free((char *) argv[i]);
  }
#endif
*/
      } else { /* ah, it isn't there at all */
        result = TCL_ERROR;
        Tcl_AppendResult(cltclinterp, "Unknown command \"", 
                         argv[0], "\"", NULL);
      }
    }
/* needless End_roots (); */

  /* Free the various things we allocated */
  stat_free((char *)argv);
  for (i=wherewasi; i<whereami; i++)
    Tcl_Free(tcllists[i]);
  startfree = wherewasi;
  
  switch (result) {
  case TCL_OK:
    return copy_string (cltclinterp->result);
  case TCL_ERROR:
    tk_error(cltclinterp->result);
  default:  /* TCL_BREAK, TCL_CONTINUE, TCL_RETURN */
    tk_error("bad tcl result");
  }
}

