int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short yyss[YYSTACKSIZE];
YYSTYPE yyvs[YYSTACKSIZE];
#define yystacksize YYSTACKSIZE
#define YYABORT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab

value yyparse(tables, entrypoint, lexbuf)
     value tables, entrypoint, lexbuf;
{
    register int yym, yyn, yystate;

#define yyact FIELD(tables,0)
#define yytransl FIELD(tables,1)
#define yylhs FIELD(tables, 2)
#define yylen FIELD(tables, 3)
#define yydefred FIELD(tables, 4)
#define yydgoto FIELD(tables, 5)
#define yysindex FIELD(tables, 6)
#define yyrindex FIELD(tables, 7)
#define yygindex FIELD(tables, 8)
#define YYTABLESIZE CINT(FIELD(tables, 9))
#define yytable FIELD(tables, 10)
#define yycheck FIELD(tables, 11)

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

    yychar = CINT(entrypoint);

yyloop:
    if (yyn = yydefred[yystate]) goto yyreduce;
    if (yychar < 0) {
      token = yylex(lexbuf);
      yychar = CINT(yytransl[TAG(token)]);
      yylval = FIELD(token, 0);
    }
    if ((yyn = CINT(yysindex[yystate])) && (yyn += yychar) >= 0 &&
        yyn <= YYTABLESIZE && CINT(yycheck[yyn]) == yychar) 
    {
        if (yyssp >= yyss + yystacksize - 1) grow_stacks();

        *++yyssp = yystate = CINT(yytable[yyn]);
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = CINT(yyrindex[yystate])) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && CINT(yycheck[yyn]) == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;

yynewerror:
    v = alloc(1, EXN_PARSING);
    FIELD(v, 0) = MLINT(yychar);
    mlraise(v);

yyerrlab:
    ++yynerrs;

yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = CINT(yysindex[*yyssp])) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && CINT(yycheck[yyn]) == YYERRCODE)
            {
                if (yyssp >= yyss + yystacksize - 1) grow_stacks();

                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
        yychar = (-1);
        goto yyloop;
    }

yyreduce:
    yym = yylen[yyn];
    yyval = mlapply(FIELD(yyact, yyn), atom(0));
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = CINT(yygindex[yym])) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && CINT(yycheck[yyn]) == yystate)
        yystate = CINT(yytable[yyn]);
    else
        yystate = CINT(yydgoto[yym]);
    if (yyssp >= yyss + yystacksize - 1) grow_stacks();
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
}
