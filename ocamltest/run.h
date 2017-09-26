/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sebastien Hinderer, projet Gallium, INRIA Paris            */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Header file for the run library */

#ifndef __RUN_H__

#define __RUN_H__

#include <stdarg.h>
#include <caml/misc.h>

typedef charnat **array;

typedef void Logger(void *, const char *, va_list ap);

typedef struct {
  charnat *program;
  array argv;
  /* array envp; */
  charnat *stdin_filename;
  charnat *stdout_filename;
  charnat *stderr_filename;
  int append;
  int timeout;
  Logger *logger;
  void *loggerData;
} command_settings;

extern int run_command(const command_settings *settings);

#endif /* __RUN_H__ */
