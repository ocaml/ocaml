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
#include <string.h>

#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "camltk.h"

/* The Tcl interpretor */
Tcl_Interp *cltclinterp = NULL;

/* Copy a list of strings from the C heap to Caml */
value copy_string_list(int argc, char **argv)
{
  CAMLparam0();
  CAMLlocal3( res, oldres, str );
  int i;
  oldres = Val_unit;
  str = Val_unit;

  res = Val_int(0); /* [] */
  for (i = argc-1; i >= 0; i--) {
    oldres = res;
    str = tcl_string_to_caml(argv[i]);
    res = alloc(2, 0);
    Field(res, 0) = str;
    Field(res, 1) = oldres;
  }
  CAMLreturn(res);
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
  cmd = caml_string_to_tcl(str);
  code = Tcl_Eval(cltclinterp, cmd);
  stat_free(cmd);

  switch (code) {
  case TCL_OK:
    return tcl_string_to_caml(Tcl_GetStringResult(cltclinterp));
  case TCL_ERROR:
    tk_error(Tcl_GetStringResult(cltclinterp));
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
    { int n = 0;
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

/* Fill a preallocated vector arguments, doing expansion and all.
 * Assumes Tcl will 
 *  not tamper with our strings
 *  make copies if strings are "persistent"
 */
int fill_args (char **argv, int where, value v)
{
  value l;
  
  switch (Tag_val(v)) {
  case 0:
    argv[where] = caml_string_to_tcl(Field(v,0)); /* must free by stat_free */
    return (where + 1);
  case 1:
    for (l=Field(v,0); Is_block(l); l=Field(l,1))
      where = fill_args(argv,where,Field(l,0));
    return where;
  case 2:
    { char **tmpargv;
      char *merged;
      int i;
      int size = argv_size(Field(v,0));
      tmpargv = (char **)stat_alloc((size + 1) * sizeof(char *));
      fill_args(tmpargv,0,Field(v,0));
      tmpargv[size] = NULL;
      merged = Tcl_Merge(size,tmpargv);
      for(i = 0 ; i<size; i++){ stat_free(tmpargv[i]); }
      stat_free((char *)tmpargv);
      /* must be freed by stat_free */
      argv[where] = (char*)stat_alloc(strlen(merged)+1); 
      strcpy(argv[where], merged);
      Tcl_Free(merged);
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
  char **argv, **allocated;
  int result;
  Tcl_CmdInfo info;

  CheckInit();

  /* walk the array to compute final size for Tcl */
  for(i=0,size=0;i<Wosize_val(v);i++)
    size += argv_size(Field(v,i));

  /* +2: one slot for NULL
         one slot for "unknown" if command not found */
  argv = (char **)stat_alloc((size + 2) * sizeof(char *));
  allocated = (char **)stat_alloc(size * sizeof(char *));

  /* Copy -- argv[i] must be freed by stat_free */
  {
    int where;
    for(i=0, where=0;i<Wosize_val(v);i++){
      where = fill_args(argv,where,Field(v,i));
    }
    if( size != where ){ tk_error("fill_args error!!! Call the CamlTk maintainer!"); }
    for(i=0; i<where; i++){ allocated[i] = argv[i]; } 
    argv[size] = NULL;
    argv[size + 1] = NULL;
  }

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
      Tcl_DStringInit(&buf);
      Tcl_DStringAppend(&buf, argv[0], -1);
      for (i=1; i<size; i++) {
        Tcl_DStringAppend(&buf, " ", -1);
        Tcl_DStringAppend(&buf, argv[i], -1);
      }
      result = Tcl_Eval(cltclinterp, Tcl_DStringValue(&buf));
      Tcl_DStringFree(&buf);
    } else {
      result = (*info.proc)(info.clientData,cltclinterp,size,argv);
    }
#else
    result = (*info.proc)(info.clientData,cltclinterp,size,argv);
#endif
  } else { /* implement the autoload stuff */
    if (Tcl_GetCommandInfo(cltclinterp,"unknown",&info)) { /* unknown found */
      for (i = size; i >= 0; i--)
        argv[i+1] = argv[i];
      argv[0] = "unknown";
      result = (*info.proc)(info.clientData,cltclinterp,size+1,argv);
    } else { /* ah, it isn't there at all */
      result = TCL_ERROR;
      Tcl_AppendResult(cltclinterp, "Unknown command \"", 
                       argv[0], "\"", NULL);
    }
  }

  /* Free the various things we allocated */
  for(i=0; i< size; i ++){
    stat_free((char *) allocated[i]);
  }
  stat_free((char *)argv);
  stat_free((char *)allocated);
  
  switch (result) {
  case TCL_OK:
    return tcl_string_to_caml (Tcl_GetStringResult(cltclinterp));
  case TCL_ERROR:
    tk_error(Tcl_GetStringResult(cltclinterp));
  default:  /* TCL_BREAK, TCL_CONTINUE, TCL_RETURN */
    tk_error("bad tcl result");
  }
}
