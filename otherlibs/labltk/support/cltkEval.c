#include <stdlib.h>

#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "camltk.h"

/* The Tcl interpretor */
Tcl_Interp *cltclinterp = NULL;

/* Copy a list of strings from the C heap to Caml */
value copy_string_list(argc, argv)
     int argc;
     char ** argv;
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
 *   this version works on an arbitrary Tcl command
 */
value camltk_tcl_eval(str) /* ML */
value str; 
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
  code = Tcl_Eval(cltclinterp, cmd);
  stat_free(cmd);

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
    | TkTokenList of TkArgs list		(* to be expanded *)
    | TkQuote of TkArgs 	                (* mapped to Tcl list *)
 * NO PARSING, NO SUBSTITUTION
 */

/* 
 * Compute the size of the argument (of type TkArgs). 
 * TkTokenList must be expanded,
 * TkQuote count for one.
 */
int argv_size(v)
value v;
{
  switch (Tag_val(v)) {
  case 0:			/* TkToken */
    return 1;
  case 1:			/* TkTokenList */
    { int n;
      value l;
      for (l=Field(v,0), n=0; Is_block(l); l=Field(l,1))
	n+=argv_size(Field(l,0));
      return n;
    }
  case 2:			/* TkQuote */
    return 1;
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
int fill_args (argv, where, v) 
char ** argv;
int where;
value v;
{
  switch (Tag_val(v)) {
  case 0:
    argv[where] = String_val(Field(v,0));
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
      if (size < 16)
	tmpargv = &quotedargv[0];
      else
	tmpargv = (char **)stat_alloc((size + 1) * sizeof(char *));
      fill_args(tmpargv,0,Field(v,0));
      tmpargv[size] = NULL;
      argv[where] = Tcl_Merge(size,tmpargv);
      tcllists[startfree++] = argv[where]; /* so we can free it later */
      if (size >= 16) 
	stat_free((char *)tmpargv);
      return (where + 1);
    }
  }
}

/* v is an array of TkArg */
value camltk_tcl_direct_eval(v) /* ML */
value v; 
{
  int i;
  int size;			/* size of argv */
  char **argv;
  int result;
  Tcl_CmdInfo info;
  int wherewasi,whereami;       /* positions in tcllists array */

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

  Begin_roots_block ((value *) argv, size + 2);

  whereami = startfree;

    /* Eval */
    Tcl_ResetResult(cltclinterp);
    if (Tcl_GetCommandInfo(cltclinterp,argv[0],&info)) { /* command found */
      result = (*info.proc)(info.clientData,cltclinterp,size,argv);
    } else {/* implement the autoload stuff */
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
  End_roots ();

  /* Free the various things we allocated */
  stat_free((char *)argv);
  for (i=wherewasi; i<whereami; i++)
    free(tcllists[i]);
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

