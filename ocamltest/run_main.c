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

/* Make the run library usable from the command-line */

int main(int argc, char *argv[])
{
  int result;
  if (argc<2) error("Specify which program to execute");
  char *program = argv[1];
  char *args[] = { program, NULL };
  char *env[] = { NULL };
  command_settings settings;
  settings.program = program;
  /* Case 1:
    settings.stdout_filename = NULL;
    settings.stderr_filename = NULL;
  */
  /* Case 2:
    settings.stdout_filename = "/tmp/output";
    settings.stderr_filename = NULL;
  */
  /* Case 3:
    settings.stdout_filename = NULL;
    settings.stderr_filename = "/tmp/error";
  */
  /* Case 4:
    settings.stdout_filename = "/tmp/output";
    settings.stderr_filename = "/tmp/error";
  */
  /* Case 5:
    settings.stdout_filename = "/tmp/log";
    settings.stderr_filename = "/tmp/log";
  */
  settings.stdout_filename = settings.stderr_filename = NULL;
  settings.argv = &args;
  settings.envp = &env;
  settings.timeout = 2;
  result = run_command(&settings);
  fprintf(stderr, "run_command returned %d\n", result);
}
