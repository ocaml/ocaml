/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sébastien Hinderer, projet Gallium, INRIA Paris           */
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

typedef char **array;

typedef void Logger(void *, const char *, ...);

typedef struct {
  const char *program;
  array *argv;
  /* array *envp; */
  const char *stdout_filename;
  const char *stderr_filename;
  int append;
  int timeout;
  Logger *logger;
  void *loggerData;
} command_settings;

extern int run_command(const command_settings *settings);

#endif /* __RUN_H__ */
