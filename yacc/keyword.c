#include "defs.h"

/* If subject is a prefix of ptr, returns a pointer to the first character
 * after subject in ptr.
 * Otherwise, returns NULL.
 */
static char *str_match(char *ptr, const char *subject)
{
    for (; *subject; subject++, ptr++)
        if (*subject != *ptr)
            return NULL;
    return ptr;
}

/* pident[i] : name bound to symbol $i, or NULL ).
 * n         : number of symbols (pident[]s range from 1 to n)
 * ptr       : pointer to current character in input stream
 * f         : output file stream
 * note      : pident is 1-indexed, since $0 is not a valid symbol.
 */
static char *copy_kwarg(bucket **pident, int n, char *ptr, FILE *f)
{
    int i = 0;
    char *match = NULL;
    bucket *sym;

    if (*ptr == '$')
    {
        /* The argument is of the form $d.
         * It is translated to a call of the form "(fn    number)".
         * "(fn " has been output by the caller. The three other spaces are there
         * to pad the call to the exact same number of characters used by the
         * keyword in submitted grammar.
         * E.g. "$endpos($10) -> (_'eP    10).
         * */
        ptr++;
        fputs("   ",f);

        /* Directly copy "number)" from the input. */
        for (; isdigit(*ptr); ptr++)
        {
            i = i * 10 + *ptr - '0';
            fputc(*ptr,f);
        }

        /* Check that the argument is valid */
        if (i <= 0 || i > n)
            unknown_rhs(i);
    }
    else if (IS_IDENTCHAR0(*ptr))
    {
        /* Find the ident in bound symbols */
        for (i = 1; i <= n; i++) {
            if ((sym = pident[i]))
                if ((match = str_match(ptr, sym->name)))
                    break;
        }

        if (match == NULL || *match != ')')
            invalid_keyword_arg(lineno, line, ptr);

        fputs("__",f);
        for (; IS_IDENTCHAR(*ptr); ptr++)
            fputc(*ptr,f);
    }

    if (*ptr != ')')
        invalid_keyword_arg(lineno, line, ptr);
    fputc(')',f);
    ptr++;
    return ptr;
}

/* pident[i] : name bound to symbol $i, or NULL ).
 * n         : number of symbols (pident[]s range from 1 to n)
 * ptr       : pointer to current character in input stream
 * f         : output file stream
 * note      : pident is 1-indexed, since $0 is not a valid symbol.
 *
 * Returns the new character pointer if a keyword has been processed, or NULL.
 */
char *keyword_process(bucket **pident, int n, char *ptr, FILE *f)
{
    char *kw1, *kw2;

    if ((kw1 = str_match(ptr, "$start")))
    {
        if ((kw2 = str_match(kw1, "pos")))
        {
            /* $startpos */

            /* The test at the RHS of && below takes care of the case where the
             * user typed:
             *   some_function $startpos(another argument)
             * with the intent to pass two arguments, the first being
             * $startpos. This is not reported as an error. */
            if (kw2[0] == '(' && (kw2[1] == '$' || IS_IDENTCHAR0(kw2[1])))
            {
                /*     $startpos( */
                fputs("(_'stP  ", f);
                return copy_kwarg(pident, n, kw2+1, f);
            }
            else if (!IS_IDENTCHAR(kw2[0]))
            {
                /*     $startpos */
                fputs("(_'stp())", f);
                return kw2;
            }
            else
                invalid_keyword(lineno, line, ptr);
        }
        else if ((kw2 = str_match(kw1, "ofs")))
        {
            /* $startofs */
            if (kw2[0] == '(' && (kw2[1] == '$' || IS_IDENTCHAR0(kw2[1])))
            {
                /*     $startofs( */
                fputs("(_'stO  ", f);
                return copy_kwarg(pident, n, kw2+1, f);
            }
            else if (!IS_IDENTCHAR(*kw2))
            {
                /*     $startofs */
                fputs("(_'sto())", f);
                return kw2;
            }
            else
                invalid_keyword(lineno, line, ptr);
        }
    }
    else if ((kw1 = str_match(ptr, "$end")))
    {
        if ((kw2 = str_match(kw1, "pos")))
        {
            /* $endpos */
            if (kw2[0] == '(' && (kw2[1] == '$' || IS_IDENTCHAR0(kw2[1])))
            {
                /*     $endpos( */
                fputs("(_'eP ", f);
                return copy_kwarg(pident, n, kw2+1, f);
            }
            else if (!IS_IDENTCHAR(*kw2))
            {
                /*     $endpos */
                fputs("(_'P())", f);
                return kw2;
            }
            else
                invalid_keyword(lineno, line, ptr);
        }
        else if ((kw2 = str_match(kw1, "ofs")))
        {
            /* $endofs */
            if (kw2[0] == '(' && (kw2[1] == '$' || IS_IDENTCHAR0(kw2[1])))
            {
                /*     $endofs( */
                fputs("(_'eO ", f);
                return copy_kwarg(pident, n, kw2+1, f);
            }
            else if (!IS_IDENTCHAR(*kw2))
            {
                /*     $endofs */
                fputs("(_'O())", f);
                return kw2;
            }
            else
                invalid_keyword(lineno, line, ptr);
        }
    }
    else if ((kw1 = str_match(ptr, "$syntaxerror")))
    {
        if (!IS_IDENTCHAR(*kw1))
            fputs("(raise _eRR)", f);
        else
            invalid_keyword(lineno, line, ptr);
        return kw1;
    }
    else if ((kw1 = str_match(ptr, "$previouserror")))
    {
        /* $previouserror */
        unimplemented_keyword(lineno, line, ptr);
    }
    else if (IS_IDENTCHAR0(ptr[1]))
        invalid_keyword(lineno, line, ptr);

    return NULL;
}

/* Output definitions expected by keyword processing */
void output_keyword_definitions(FILE* f)
{
    fprintf(f, "\
let _'stP = Parsing.rhs_start_pos\n\
let _'stp = Parsing.symbol_start_pos\n\
let _'stO = Parsing.rhs_start\n\
let _'sto = Parsing.symbol_start\n\
let _'eP  = Parsing.rhs_end_pos\n\
let _'P   = Parsing.symbol_end_pos\n\
let _'eO  = Parsing.rhs_end\n\
let _'O   = Parsing.symbol_end\n\
let _eRR  = Parsing.Parse_error\n\n");
    if (!rflag) outline += 10;
}
