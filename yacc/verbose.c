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

#include "defs.h"


static short *null_rules;


void print_state (int state);
void log_unused (void);
void log_conflicts (void);
void print_conflicts (int state);
void print_core (int state);
void print_nulls (int state);
void print_actions (int stateno);
void print_shifts (register action *p);
void print_reductions (register action *p, register int defred);
void print_gotos (int stateno);

void verbose(void)
{
    register int i;

    if (!vflag) return;

    null_rules = (short *) MALLOC(nrules*sizeof(short));
    if (null_rules == 0) no_space();
    fprintf(verbose_file, "\f\n");
    for (i = 0; i < nstates; i++)
        print_state(i);
    FREE(null_rules);

    if (nunused)
        log_unused();
    if (SRtotal || RRtotal)
        log_conflicts();

    fprintf(verbose_file, "\n\n%d terminals, %d nonterminals\n", ntokens,
            nvars);
    fprintf(verbose_file, "%d grammar rules, %d states\n", nrules - 2, nstates);
}


void log_unused(void)
{
    register int i;
    register short *p;

    fprintf(verbose_file, "\n\nRules never reduced:\n");
    for (i = 3; i < nrules; ++i)
    {
        if (!rules_used[i])
        {
            fprintf(verbose_file, "\t%s :", symbol_name[rlhs[i]]);
            for (p = ritem + rrhs[i]; *p >= 0; ++p)
                fprintf(verbose_file, " %s", symbol_name[*p]);
            fprintf(verbose_file, "  (%d)\n", i - 2);
        }
    }
}


void log_conflicts(void)
{
    register int i;

    fprintf(verbose_file, "\n\n");
    for (i = 0; i < nstates; i++)
    {
        if (SRconflicts[i] || RRconflicts[i])
        {
            fprintf(verbose_file, "State %d contains ", i);
            if (SRconflicts[i] == 1)
                fprintf(verbose_file, "1 shift/reduce conflict");
            else if (SRconflicts[i] > 1)
                fprintf(verbose_file, "%d shift/reduce conflicts",
                        SRconflicts[i]);
            if (SRconflicts[i] && RRconflicts[i])
                fprintf(verbose_file, ", ");
            if (RRconflicts[i] == 1)
                fprintf(verbose_file, "1 reduce/reduce conflict");
            else if (RRconflicts[i] > 1)
                fprintf(verbose_file, "%d reduce/reduce conflicts",
                        RRconflicts[i]);
            fprintf(verbose_file, ".\n");
        }
    }
}


void print_state(int state)
{
    if (state)
        fprintf(verbose_file, "\n\n");
    if (SRconflicts[state] || RRconflicts[state])
        print_conflicts(state);
    fprintf(verbose_file, "state %d\n", state);
    print_core(state);
    print_nulls(state);
    print_actions(state);
}


void print_conflicts(int state)
{
    register int symbol, act, number;
    register action *p;

    symbol = -1;
    act = 0;
    number = 0;
    for (p = parser[state]; p; p = p->next)
    {
        if (p->suppressed == 2)
            continue;

        if (p->symbol != symbol)
        {
            symbol = p->symbol;
            number = p->number;
            if (p->action_code == SHIFT)
                act = SHIFT;
            else
                act = REDUCE;
        }
        else if (p->suppressed == 1)
        {
            if (state == final_state && symbol == 0)
            {
                fprintf(verbose_file, "%d: shift/reduce conflict \
(accept, reduce %d) on $end\n", state, p->number - 2);
            }
            else
            {
                if (act == SHIFT)
                {
                    fprintf(verbose_file, "%d: shift/reduce conflict \
(shift %d, reduce %d) on %s\n", state, number, p->number - 2,
                            symbol_name[symbol]);
                }
                else
                {
                    fprintf(verbose_file, "%d: reduce/reduce conflict \
(reduce %d, reduce %d) on %s\n", state, number - 2, p->number - 2,
                            symbol_name[symbol]);
                }
            }
        }
    }
}


void print_core(int state)
{
    register int i;
    register int k;
    register int rule;
    register core *statep;
    register short *sp;
    register short *sp1;

    statep = state_table[state];
    k = statep->nitems;

    for (i = 0; i < k; i++)
    {
        sp1 = sp = ritem + statep->items[i];

        while (*sp >= 0) ++sp;
        rule = -(*sp);
        fprintf(verbose_file, "\t%s : ", symbol_name[rlhs[rule]]);

        for (sp = ritem + rrhs[rule]; sp < sp1; sp++)
            fprintf(verbose_file, "%s ", symbol_name[*sp]);

        putc('.', verbose_file);

        while (*sp >= 0)
        {
            fprintf(verbose_file, " %s", symbol_name[*sp]);
            sp++;
        }
        fprintf(verbose_file, "  (%d)\n", -2 - *sp);
    }
}


void print_nulls(int state)
{
    register action *p;
    register int i, j, k, nnulls;

    nnulls = 0;
    for (p = parser[state]; p; p = p->next)
    {
        if (p->action_code == REDUCE &&
                (p->suppressed == 0 || p->suppressed == 1))
        {
            i = p->number;
            if (rrhs[i] + 1 == rrhs[i+1])
            {
                for (j = 0; j < nnulls && i > null_rules[j]; ++j)
                    continue;

                if (j == nnulls)
                {
                    ++nnulls;
                    null_rules[j] = i;
                }
                else if (i != null_rules[j])
                {
                    ++nnulls;
                    for (k = nnulls - 1; k > j; --k)
                        null_rules[k] = null_rules[k-1];
                    null_rules[j] = i;
                }
            }
        }
    }

    for (i = 0; i < nnulls; ++i)
    {
        j = null_rules[i];
        fprintf(verbose_file, "\t%s : .  (%d)\n", symbol_name[rlhs[j]],
                j - 2);
    }
    fprintf(verbose_file, "\n");
}


void print_actions(int stateno)
{
    register action *p;
    register shifts *sp;
    register int as;

    if (stateno == final_state)
        fprintf(verbose_file, "\t$end  accept\n");

    p = parser[stateno];
    if (p)
    {
        print_shifts(p);
        print_reductions(p, defred[stateno]);
    }

    sp = shift_table[stateno];
    if (sp && sp->nshifts > 0)
    {
        as = accessing_symbol[sp->shift[sp->nshifts - 1]];
        if (ISVAR(as))
            print_gotos(stateno);
    }
}


void print_shifts(register action *p)
{
    register int count;
    register action *q;

    count = 0;
    for (q = p; q; q = q->next)
    {
        if (q->suppressed < 2 && q->action_code == SHIFT)
            ++count;
    }

    if (count > 0)
    {
        for (; p; p = p->next)
        {
            if (p->action_code == SHIFT && p->suppressed == 0)
                fprintf(verbose_file, "\t%s  shift %d\n",
                            symbol_name[p->symbol], p->number);
        }
    }
}


void print_reductions(register action *p, register int defred)
{
    register int k, anyreds;
    register action *q;

    anyreds = 0;
    for (q = p; q ; q = q->next)
    {
        if (q->action_code == REDUCE && q->suppressed < 2)
        {
            anyreds = 1;
            break;
        }
    }

    if (anyreds == 0)
        fprintf(verbose_file, "\t.  error\n");
    else
    {
        for (; p; p = p->next)
        {
            if (p->action_code == REDUCE && p->number != defred)
            {
                k = p->number - 2;
                if (p->suppressed == 0)
                    fprintf(verbose_file, "\t%s  reduce %d\n",
                            symbol_name[p->symbol], k);
            }
        }

        if (defred > 0)
            fprintf(verbose_file, "\t.  reduce %d\n", defred - 2);
    }
}


void print_gotos(int stateno)
{
    register int i, k;
    register int as;
    register short *to_state;
    register shifts *sp;

    putc('\n', verbose_file);
    sp = shift_table[stateno];
    to_state = sp->shift;
    for (i = 0; i < sp->nshifts; ++i)
    {
        k = to_state[i];
        as = accessing_symbol[k];
        if (ISVAR(as))
            fprintf(verbose_file, "\t%s  goto %d\n", symbol_name[as], k);
    }
}
