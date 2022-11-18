/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Based on public-domain code from Berkeley Yacc */

#include <signal.h>
#include <string.h>
#include "defs.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/version.h"

char lflag;
char rflag;
char tflag;
char vflag;
char qflag;
char eflag;
char sflag;
char big_endian;

char_os *file_prefix = 0;
char *myname = "yacc";
char_os temp_form[] = T("yacc.XXXXXXX");

#ifdef _WIN32
wchar_t dirsep = L'\\';
/* mingw provides an implementation of mkstemp, but it's ANSI only */
#undef HAS_MKSTEMP
#else
char dirsep = '/';
#endif

int lineno;
char *virtual_input_file_name = NULL;
int outline;

char_os *action_file_name;
char_os *entry_file_name;
char_os *code_file_name;
char *code_file_name_disp;
char_os *interface_file_name;
char_os *input_file_name = T("");
char *input_file_name_disp;
char_os *output_file_name;
char_os *text_file_name;
char_os *verbose_file_name;

#ifdef HAS_MKSTEMP
int action_fd = -1, entry_fd = -1, text_fd = -1;
#endif

FILE *action_file;      /*  a temp file, used to save actions associated    */
                        /*  with rules until the parser is written          */
FILE *entry_file;
FILE *code_file;        /*  y.code.c (used when the -r option is specified) */
FILE *input_file;       /*  the input file                                  */
FILE *output_file;      /*  y.tab.c                                         */
FILE *text_file;        /*  a temp file, used to save text until all        */
                        /*  symbols have been defined                       */
FILE *verbose_file;     /*  y.output                                        */
FILE *interface_file;

int nitems;
int nrules;
int ntotalrules;
int nsyms;
int ntokens;
int nvars;

int   start_symbol;
char  **symbol_name;
short *symbol_value;
short *symbol_prec;
char  *symbol_assoc;
char **symbol_tag;
char *symbol_true_token;

short *ritem;
short *rlhs;
short *rrhs;
short *rprec;
char  *rassoc;
short **derives;
char *nullable;


void done(int k)
{
#ifdef HAS_MKSTEMP
    if (action_fd != -1)
       unlink(action_file_name);
    if (entry_fd != -1)
       unlink(entry_file_name);
    if (text_fd != -1)
       unlink(text_file_name);
#else
    if (action_file) { fclose(action_file); unlink_os(action_file_name); }
    if (entry_file) { fclose(entry_file); unlink_os(entry_file_name); }
    if (text_file) { fclose(text_file); unlink_os(text_file_name); }
#endif
    if (output_file && k > 0) {
      fclose(output_file); unlink_os(output_file_name);
    }
    if (interface_file && k > 0) {
      fclose(interface_file); unlink_os(interface_file_name);
    }
    exit(k);
}


static void onintr(int dummy)
{
    done(1);
}


static void set_signals(void)
{
#ifdef SIGINT
    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
        signal(SIGINT, onintr);
#endif
#ifdef SIGTERM
    if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
        signal(SIGTERM, onintr);
#endif
#ifdef SIGHUP
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
        signal(SIGHUP, onintr);
#endif
}


void usage(void)
{
    fprintf(stderr, "usage: %s [-v] [--strict] [-q] [-b file_prefix] filename\n",
            myname);
    exit(1);
}

void getargs(int argc, char_os **argv)
{
    int i;
    char_os *s;

    if (argc > 0) myname = caml_stat_strdup_of_os(argv[0]);
    if (!myname) no_space();
    for (i = 1; i < argc; ++i)
    {
        s = argv[i];
        if (*s != '-') break;
        switch (*++s)
        {
        case '\0':
            input_file = stdin;
            file_prefix = T("stdin");
            if (i + 1 < argc) usage();
            return;

        case '-':
            if (!strcmp_os (argv[i], T("--strict"))){
              eflag = 1;
              goto end_of_option;
            }
            ++i;
            goto no_more_options;

        case 'v':
            if (!strcmp_os (argv[i], T("-version"))){
              printf ("The OCaml parser generator, version "
                      OCAML_VERSION_STRING "\n");
              exit (0);
            }else if (!strcmp_os (argv[i], T("-vnum"))){
              printf (OCAML_VERSION_STRING "\n");
              exit (0);
            }else{
              vflag = 1;
            }
            break;

        case 'q':
            qflag = 1;
            break;

        case 'b':
            if (*++s)
                 file_prefix = s;
            else if (++i < argc)
                file_prefix = argv[i];
            else
                usage();
            continue;

        default:
            usage();
        }

        for (;;)
        {
            switch (*++s)
            {
            case '\0':
                goto end_of_option;

            case 'v':
                vflag = 1;
                break;

            case 'q':
                qflag = 1;
                break;

            default:
                usage();
            }
        }
end_of_option:;
    }

no_more_options:;
    if (i + 1 != argc) usage();
    input_file_name = argv[i];
    input_file_name_disp = caml_stat_strdup_of_os(input_file_name);
    if (!input_file_name_disp) no_space();
    if (file_prefix == 0) {
      int len;
      len = strlen_os(argv[i]);
      file_prefix = MALLOC((len + 1) * sizeof(char_os));
      if (file_prefix == 0) no_space();
      strcpy_os(file_prefix, argv[i]);
      while (len > 0) {
        len--;
        if (file_prefix[len] == '.') {
          file_prefix[len] = 0;
          break;
        }
      }
    }
}


char *
allocate(unsigned int n)
{
    char *p;

    p = NULL;
    if (n)
    {
        p = CALLOC(1, n);
        if (!p) no_space();
    }
    return (p);
}


void create_file_names(void)
{
    int i, len;
    char_os *tmpdir;

#ifdef _WIN32
    tmpdir = _wgetenv(L"TEMP");
    if (tmpdir == 0) tmpdir = L".";
#else
    tmpdir = getenv("TMPDIR");
    if (tmpdir == 0) tmpdir = "/tmp";
#endif
    len = strlen_os(tmpdir);
    i = len + sizeof(temp_form);
    if (len && tmpdir[len-1] != dirsep)
        ++i;

    action_file_name = MALLOC(i * sizeof(char_os));
    if (action_file_name == 0) no_space();
    entry_file_name = MALLOC(i * sizeof(char_os));
    if (entry_file_name == 0) no_space();
    text_file_name = MALLOC(i * sizeof(char_os));
    if (text_file_name == 0) no_space();

    strcpy_os(action_file_name, tmpdir);
    strcpy_os(entry_file_name, tmpdir);
    strcpy_os(text_file_name, tmpdir);

    if (len && tmpdir[len - 1] != dirsep)
    {
        action_file_name[len] = dirsep;
        entry_file_name[len] = dirsep;
        text_file_name[len] = dirsep;
        ++len;
    }

    strcpy_os(action_file_name + len, temp_form);
    strcpy_os(entry_file_name + len, temp_form);
    strcpy_os(text_file_name + len, temp_form);

    action_file_name[len + 5] = L'a';
    entry_file_name[len + 5] = L'e';
    text_file_name[len + 5] = L't';

#ifdef HAS_MKSTEMP
    action_fd = mkstemp(action_file_name);
    if (action_fd == -1)
        open_error(action_file_name);
    entry_fd = mkstemp(entry_file_name);
    if (entry_fd == -1)
        open_error(entry_file_name);
    text_fd = mkstemp(text_file_name);
    if (text_fd == -1)
        open_error(text_file_name);
#else
    mktemp_os(action_file_name);
    mktemp_os(entry_file_name);
    mktemp_os(text_file_name);
#endif

    len = strlen_os(file_prefix);

    output_file_name = MALLOC((len + 7) * sizeof(char_os));
    if (output_file_name == 0)
        no_space();
    strcpy_os(output_file_name, file_prefix);
    strcpy_os(output_file_name + len, OUTPUT_SUFFIX);

    code_file_name = output_file_name;
    code_file_name_disp = caml_stat_strdup_of_os(code_file_name);
    if (!code_file_name_disp) no_space();

    if (vflag)
    {
        verbose_file_name = MALLOC((len + 8) * sizeof(char_os));
        if (verbose_file_name == 0)
            no_space();
        strcpy_os(verbose_file_name, file_prefix);
        strcpy_os(verbose_file_name + len, VERBOSE_SUFFIX);
    }

    interface_file_name = MALLOC((len + 8) * sizeof(char_os));
    if (interface_file_name == 0)
        no_space();
    strcpy_os(interface_file_name, file_prefix);
    strcpy_os(interface_file_name + len, INTERFACE_SUFFIX);

}


void open_files(void)
{
    create_file_names();

    if (input_file == 0)
    {
        input_file = fopen_os(input_file_name, T("r"));
        if (input_file == 0)
            open_error(input_file_name);
    }

#ifdef HAS_MKSTEMP
    action_file = fdopen(action_fd, "w");
#else
    action_file = fopen_os(action_file_name, T("w"));
#endif
    if (action_file == 0)
        open_error(action_file_name);

#ifdef HAS_MKSTEMP
    entry_file = fdopen(entry_fd, "w");
#else
    entry_file = fopen_os(entry_file_name, T("w"));
#endif
    if (entry_file == 0)
        open_error(entry_file_name);

#ifdef HAS_MKSTEMP
    text_file = fdopen(text_fd, "w");
#else
    text_file = fopen_os(text_file_name, T("w"));
#endif
    if (text_file == 0)
        open_error(text_file_name);

    if (vflag)
    {
        verbose_file = fopen_os(verbose_file_name, T("w"));
        if (verbose_file == 0)
            open_error(verbose_file_name);
    }

    output_file = fopen_os(output_file_name, T("w"));
    if (output_file == 0)
        open_error(output_file_name);

    if (rflag)
    {
        code_file = fopen_os(code_file_name, T("w"));
        if (code_file == 0)
            open_error(code_file_name);
    }
    else
        code_file = output_file;


    interface_file = fopen_os(interface_file_name, T("w"));
    if (interface_file == 0)
      open_error(interface_file_name);
}

int main_os(int argc, char_os **argv)
{
    set_signals();
    getargs(argc, argv);
    open_files();
    reader();
    lr0();
    lalr();
    make_parser();
    verbose();
    if (eflag && SRtotal + RRtotal > 0) forbidden_conflicts();
    output();
    done(0);
    /*NOTREACHED*/
    return 0;
}
