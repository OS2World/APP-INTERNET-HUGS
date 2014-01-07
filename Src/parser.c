/* A YACC parser generated from "parser.y" */

# line 19
#ifndef lint
#define lint
#endif
#define defTycon(n,l,lhs,rhs,w)	 tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)		 ap(SIGDECL,triple(l,vs,t))
#define grded(gs)		 ap(GUARDED,gs)
#define letrec(bs,e)		 (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define yyerror(s)		 /* errors handled elsewhere */
#define YYSTYPE			 Cell

static Cell   local gcShadow     Args((Int,Cell));
static Void   local syntaxError  Args((String));
static String local unexpected   Args((Void));
static Cell   local checkPrec    Args((Cell));
static Void   local fixDefn      Args((Syntax,Cell,Cell,List));
static Void   local setSyntax    Args((Int,Syntax,Cell));
static Cell   local buildTuple   Args((List));
static Cell   local checkClass   Args((Cell));
static Cell   local checkInst	 Args((Cell));
static List   local tupToList    Args((Cell));
static List   local checkContext Args((List));
static Cell   local checkTyLhs	 Args((Cell));
static Cell   local tidyInfix    Args((Cell));

/* For the purposes of reasonably portable garbage collection, it is
 * necessary to simulate the YACC stack on the Hugs stack to keep
 * track of all intermediate constructs.  The lexical analyser
 * pushes a token onto the stack for each token that is found, with
 * these elements being removed as reduce actions are performed,
 * taking account of look-ahead tokens as described by gcShadow()
 * below.
 *
 * Of the non-terminals used below, only start, topDecl & begin do not leave
 * any values on the Hugs stack.  The same is true for the terminals
 * EVALEX and SCRIPT.  At the end of a successful parse, there should only
 * be one element left on the stack, containing the result of the parse.
 */

#define gc0(e)			 gcShadow(0,e)
#define gc1(e)			 gcShadow(1,e)
#define gc2(e)			 gcShadow(2,e)
#define gc3(e)			 gcShadow(3,e)
#define gc4(e)			 gcShadow(4,e)
#define gc5(e)			 gcShadow(5,e)
#define gc6(e)			 gcShadow(6,e)
#define gc7(e)			 gcShadow(7,e)

#define EVALEX 257
#define SCRIPT 258
#define COCO 259
#define INFIXL 260
#define INFIXR 261
#define INFIX 262
#define FUNARROW 263
#define UPTO 264
#define CASEXP 265
#define OF 266
#define IF 267
#define THEN 268
#define ELSE 269
#define WHERE 270
#define TYPE 271
#define DATA 272
#define FROM 273
#define LET 274
#define IN 275
#define VAROP 276
#define VARID 277
#define NUMLIT 278
#define CHARLIT 279
#define STRINGLIT 280
#define REPEAT 281
#define CONOP 282
#define CONID 283
#define TCLASS 284
#define IMPLIES 285
#define TINSTANCE 286
#define DERIVING 287
#define TRUNST 288
#define PRIMITIVE 289
#define DEFAULT 290
#define HIDING 291
#define IMPORT 292
#define INTERFACE 293
#define MODULE 294
#define RENAMING 295
#define TO 296
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

#line 460


static Cell local gcShadow(n,e)		/* keep parsed fragments on stack  */
Int  n;
Cell e; {
    /* If a look ahead token is held then the required stack transformation
     * is:
     *   pushed: n               1     0          1     0
     *           x1  |  ...  |  xn  |  la   ===>  e  |  la
     *                                top()            top()
     *
     * Othwerwise, the transformation is:
     *   pushed: n-1             0        0
     *           x1  |  ...  |  xn  ===>  e
     *                         top()     top()
     */
    if (yychar>=0) {
	pushed(n-1) = top();
        pushed(n)   = e;
    }
    else
	pushed(n-1) = e;
    sp -= (n-1);
    return e;
}

static Void local syntaxError(s)       /* report on syntax error           */
String s; {
    ERROR(row) "Syntax error in %s (unexpected %s)", s, unexpected()
    EEND;
}

static String local unexpected() {	/* find name for unexpected token  */
    static char buffer[100];
    static char *fmt = "%s \"%s\"";
    static char *kwd = "keyword";
    static char *hkw = "(Haskell) keyword";

    switch (yychar) {
	case 0	       : return "end of input";

#define keyword(kw) sprintf(buffer,fmt,kwd,kw); return buffer;
	case INFIXL    : keyword("infixl");
	case INFIXR    : keyword("infixr");
	case INFIX     : keyword("infix");
	case TINSTANCE : keyword("instance");
	case TCLASS    : keyword("class");
	case PRIMITIVE : keyword("primitive");
	case CASEXP    : keyword("case");
	case OF        : keyword("of");
	case IF        : keyword("if");
	case TRUNST    : keyword("runST");
	case THEN      : keyword("then");
	case ELSE      : keyword("else");
	case WHERE     : keyword("where");
	case TYPE      : keyword("type");
	case DATA      : keyword("data");
	case LET       : keyword("let");
	case IN        : keyword("in");
	case DERIVING  : keyword("deriving");
#undef keyword

#define hasword(kw) sprintf(buffer,fmt,hkw,kw); return buffer;
	case DEFAULT   : hasword("default");
	case HIDING    : hasword("hiding");
	case IMPORT    : hasword("import");
	case INTERFACE : hasword("interface");
	case MODULE    : hasword("module");
	case RENAMING  : hasword("renaming");
	case TO	       : hasword("to");
#undef hasword

	case FUNARROW  : return "`->'";
	case '='       : return "`='";
	case COCO      : return "`::'";
	case '-'       : return "`-'";
	case ','       : return "comma";
	case '@'       : return "`@'";
	case '('       : return "`('";
	case ')'       : return "`)'";
	case '|'       : return "`|'";
	case ';'       : return "`;'";
	case UPTO      : return "`..'";
	case '['       : return "`['";
	case ']'       : return "`]'";
	case FROM      : return "`<-'";
	case '\\'      : return "backslash (lambda)";
	case '~'       : return "tilde";
	case '`'       : return "backquote";
	case VAROP     :
	case VARID     :
	case CONOP     :
	case CONID     : sprintf(buffer,"symbol \"%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
	case NUMLIT    : return "numeric literal";
	case CHARLIT   : return "character literal";
	case STRINGLIT : return "string literal";
	case IMPLIES   : return "`=>";
	default	       : return "token";
    }
}

static Cell local checkPrec(p)         /* Check for valid precedence value */
Cell p; {
    if (!isInt(p) || intOf(p)<MIN_PREC || intOf(p)>MAX_PREC) {
        ERROR(row) "Precedence value must be an integer in the range [%d..%d]",
                   MIN_PREC, MAX_PREC
        EEND;
    }
    return p;
}

static Void local fixDefn(a,line,p,ops)/* Declare syntax of operators      */
Syntax a;
Cell   line;
Cell   p;
List   ops; {
    Int l = intOf(line);
    a     = mkSyntax(a,intOf(p));
    map2Proc(setSyntax,l,a,ops);
}

static Void local setSyntax(line,sy,op)/* set syntax of individ. operator  */
Int    line;
Syntax sy;
Cell   op; {
    addSyntax(line,textOf(op),sy);
    opDefns = cons(op,opDefns);
}

static Cell local buildTuple(tup)      /* build tuple (x1,...,xn) from list*/
List tup; {                            /* [xn,...,x1]                      */
    Int  n = 0;
    Cell t = tup;
    Cell x;

    do {                               /*     .                    .       */
        x      = fst(t);               /*    / \                  / \      */
        fst(t) = snd(t);               /*   xn  .                .   xn    */
        snd(t) = x;                    /*        .    ===>      .          */
        x      = t;                    /*         .            .           */
        t      = fun(x);               /*          .          .            */
        n++;                           /*         / \        / \           */
    } while (nonNull(t));              /*        x1  NIL   (n)  x1         */
    fst(x) = mkTuple(n);
    return tup;
}

/* The yacc parser presented above is not sufficiently powerful to
 * determine whether a tuple at the front of a sigType is part of a
 * context:    e.g. (Eq a, Num a) => a -> a -> a
 * or a type:  e.g.  (Tree a, Tree a) -> Tree a
 *
 * Rather than complicate the grammar, both are parsed as tuples of types,
 * using the following checks afterwards to ensure that the correct syntax
 * is used in the case of a tupled context.
 */

static List local tupToList(tps)	/* Convert () | t | (t1,...,tn) to */
Cell tps; {				/* a list of values:		   */
    if (tps==UNIT)			/*        [] | [t] | [t1,...,tn]   */
	return NIL;
    else if (whatIs(getHead(tps))==TUPLE) {
	List qs = NIL;

	while (isAp(tps)) {		/* undo work of buildTuple  :-(    */
	    Cell temp = fun(tps);
	    fun(tps)  = arg(tps);
	    arg(tps)  = qs;
	    qs	      = tps;
	    tps       = temp;
	}
	return qs;
    }
    else
	return singleton(tps);
}

static List local checkContext(con)	/* validate type class context	   */
Type con; {
    mapOver(checkClass, con=tupToList(con));
    return con;
}

static Cell local checkClass(c)		/* check that type expr is a class */
Cell c; {				/* constrnt of the form Class var  */
    Cell cn = getHead(c);

    if (!isCon(cn))
	syntaxError("class expression");
    else if (argCount!=1) {
	ERROR(row) "Class \"%s\" must have exactly one argument",
		   textToStr(textOf(cn))
	EEND;
    }
    else if (whatIs(arg(c))!=VARIDCELL) {
	ERROR(row) "Argument of class \"%s\" must be a variable",
		   /* Ha!  What do you think this is?  Gofer!? :-) */
		   textToStr(textOf(cn))
	EEND;
    }
    return c;
}

static Cell local checkInst(c)		/* check that type expr is a class */
Cell c; {				/* constr of the form Class simple */
    Cell cn = getHead(c);

    if (!isCon(cn))
	syntaxError("class expression");
    else if (argCount!=1) {
	ERROR(row) "Class \"%s\" must have exactly one argument",
		   textToStr(textOf(cn))
	EEND;
    }
    else {
	Cell a  = arg(c);
	Cell tn = getHead(a);
	if (isCon(tn) || tn==ARROW || isTuple(tn) || tn==LIST || tn==UNIT) {
	    for (; isAp(a); a=fun(a))
		if (whatIs(arg(a))!=VARIDCELL) {
		    ERROR(row) "Type variable expected in instance type"
		    EEND;
		}
	}
	else {
	    ERROR(row) "Illegal type expression in instance declaration"
	    EEND;
	}
    }
    return c;
}

static Cell local checkTyLhs(c)		/* check that lhs is of the form   */
Cell c; {				/* T a1 ... a			   */
    Cell tlhs = c;
    while (isAp(tlhs) && whatIs(arg(tlhs))==VARIDCELL)
	tlhs = fun(tlhs);
    if (whatIs(tlhs)!=CONIDCELL) {
	ERROR(row) "Illegal left hand side in datatype definition"
	EEND;
    }
    return c;
}

/* expressions involving a sequence of two or more infix operator symbols
 * are parsed as elements of type:
 *    InfixExpr ::= [Expr]
 *		 |  ap(ap(Operator,InfixExpr),Expr)
 *
 * thus x0 +1 x1 ... +n xn is parsed as: +n (....(+1 [x0] x1)....) xn
 *
 * Once the expression has been completely parsed, this parsed form is
 * `tidied' according to the precedences and associativities declared for
 * each operator symbol.
 *
 * The tidy process uses a `stack' of type:
 *    TidyStack ::= ap(ap(Operator,TidyStack),Expr)
 *		 |  NIL
 * when the ith layer of an InfixExpr has been transferred to the stack, the
 * stack is of the form: +i (....(+n NIL xn)....) xi
 *
 * The tidy function is based on a simple shift-reduce parser:
 *
 *  tidy                :: InfixExpr -> TidyStack -> Expr
 *  tidy [m]   ss        = foldl (\x f-> f x) m ss
 *  tidy (m*n) []        = tidy m [(*n)]
 *  tidy (m*n) ((+o):ss)
 *	       | amb     = error "Ambiguous"
 *	       | shift   = tidy m ((*n):(+o):ss)
 *	       | reduce  = tidy (m*(n+o)) ss
 *			   where sye     = syntaxOf (*)
 *				 (ae,pe) = sye
 *				 sys     = syntaxOf (+)
 *				 (as,ps) = sys
 *				 amb     = pe==ps && (ae/=as || ae==NON_ASS)
 *				 shift   = pe>ps || (ps==pe && ae==LEFT_ASS)
 *				 reduce  = otherwise
 *
 * N.B. the conditions amb, shift, reduce are NOT mutually exclusive and
 * must be tested in that order.
 *
 * As a concession to efficiency, we lower the number of calls to syntaxOf
 * by keeping track of the values of sye, sys throughout the process.  The
 * value APPLIC is used to indicate that the syntax value is unknown.
 */

static Cell local tidyInfix(e)         /* convert InfixExpr to Expr        */
Cell e; {                              /* :: InfixExpr                     */
    Cell   s   = NIL;                  /* :: TidyStack                     */
    Syntax sye = APPLIC;               /* Syntax of op in e (init unknown) */
    Syntax sys = APPLIC;               /* Syntax of op in s (init unknown) */
    Cell   temp;

    while (nonNull(tl(e))) {
        if (isNull(s)) {
            s           = e;
            e           = arg(fun(s));
            arg(fun(s)) = NIL;
            sys         = sye;
            sye         = APPLIC;
        }
        else {
            if (sye==APPLIC) {         /* calculate sye (if unknown)       */
                sye = syntaxOf(textOf(fun(fun(e))));
                if (sye==APPLIC) sye=DEF_OPSYNTAX;
            }
            if (sys==APPLIC) {         /* calculate sys (if unknown)       */
                sys = syntaxOf(textOf(fun(fun(s))));
                if (sys==APPLIC) sys=DEF_OPSYNTAX;
            }

            if (precOf(sye)==precOf(sys) &&                      /* amb    */
                   (assocOf(sye)!=assocOf(sys) || assocOf(sye)==NON_ASS)) {
                ERROR(row) "Ambiguous use of operator \"%s\" with \"%s\"",
                           textToStr(textOf(fun(fun(e)))),
                           textToStr(textOf(fun(fun(s))))
                EEND;
            }
            else if (precOf(sye)>precOf(sys) ||                  /* shift  */
                       (precOf(sye)==precOf(sys) && assocOf(sye)==LEFT_ASS)) {
                temp        = arg(fun(e));
                arg(fun(e)) = s;
                s           = e;
                e           = temp;
                sys         = sye;
                sye         = APPLIC;
            }
            else {                                               /* reduce */
                temp        = arg(fun(s));
                arg(fun(s)) = arg(e);
                arg(e)      = s;
                s           = temp;
                sys         = APPLIC;
                /* sye unchanged */
            }
        }
    }

    e = hd(e);
    while (nonNull(s)) {
        temp        = arg(fun(s));
        arg(fun(s)) = e;
        e           = s;
        s           = temp;
    }

    return e;
}

/*-------------------------------------------------------------------------*/
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 86,
	259, 143,
	44, 143,
	-2, 169,
-1, 95,
	285, 75,
	-2, 74,
-1, 151,
	285, 75,
	-2, 118,
-1, 154,
	285, 75,
	-2, 126,
-1, 236,
	264, 19,
	-2, 41,
-1, 271,
	96, 78,
	282, 78,
	-2, 65,
	};
#define YYNPROD 215
# define YYLAST 821
short yyact[]={

  18, 318,  93,  95, 345, 339,   6, 130, 128,  21,
 310, 268, 196, 233, 365,   5, 269, 364, 261,  34,
  36, 204, 298, 311, 101, 287,  52,  53,  37, 223,
 220, 330, 208,  86, 110,  62, 254, 168, 101,  85,
  68, 104, 134, 384, 137, 370,  89, 105, 105, 360,
 314, 215,  86, 141, 315,  84, 251,  36,  85, 148,
 375, 349, 205,  38, 101, 171,  91,  72, 177,  90,
  91, 136, 309,  90, 258, 102, 230,  70, 222, 232,
 138, 147, 219, 148, 252, 151, 154, 155, 111, 102,
 112, 179, 284,  86, 299, 256, 361,  22,  61,  85,
 367,  39,  58, 161,  71, 173, 178, 299, 281, 148,
 163, 216,   4,   2,   3, 102, 164, 264, 185, 216,
 183,  96, 278,  42, 186, 276, 259, 187,  92,  51,
  86, 188, 182, 189, 181, 148,  85, 201, 193, 248,
 198, 124, 101,  56, 280, 109,  66, 207,  27,  11,
 101, 140,  20,  45, 195, 383, 366, 362, 225, 224,
 363, 157, 125, 307, 200, 273, 164, 235, 214, 157,
  22, 257, 238, 239, 227,  10, 237, 229, 120, 357,
 285, 145, 358,  19,  43,  86, 312, 342, 226, 135,
 307,  85, 201, 102, 255, 282, 382, 246, 283, 330,
 247, 102, 101, 194, 244, 235, 101, 245, 116, 267,
 241, 270, 359, 242, 119, 147, 274, 120, 265, 250,
 356,  27,  11, 174, 277,  20, 297, 279, 306, 117,
 142, 143, 118, 296, 199,  45,  46, 115, 167,  22,
  97, 114, 126, 240,  10, 192, 286, 239, 191, 304,
 262, 288, 166, 102,  97, 131,  19, 102, 295, 294,
  86,  99, 291, 235, 289, 290,  85, 100,  30, 300,
 206, 302, 203, 228,  29,  99, 265,  86, 206,  86,
  97, 100,  86,  85, 235,  85, 331, 172,  85, 293,
  27,  11,  22, 322,  20, 328,  30,  10, 160, 255,
 201,  99, 266,  29, 348, 340, 336, 100, 235, 335,
 346, 319, 337,   7, 341, 270,  72, 350, 334, 270,
  30, 343,  14, 333,  13,  19, 352, 351, 347, 332,
 354,  12, 355,  59,  30,  23,  24,  25,  26,  60,
  29, 329, 217,  27,  11,  16,  30,  20,  22,  91,
 217, 146, 236,  10,  86, 372, 360, 209,  97, 323,
  85, 201, 371, 324, 340, 378, 271, 346, 374, 377,
 381, 380,  30, 341, 376, 379,  22, 131,  19,  99,
 305,  10, 127, 213, 156, 100, 132,  99, 319,  22,
  77,  78,  79, 100, 313,  14, 275,  13, 272,  27,
  11,  75,  76,  20,  12, 303,  15,  30,  23,  24,
  25,  26, 301,  29,  81,  44,  82,  48,  16,  80,
  83,  46,  74, 325, 321, 243,  67,  27,  11, 169,
 234,  20, 253, 131,  19,  35,  30,  49, 210,  99,
  27,  22,  29,  99,  20, 100,  10,  40,  47, 100,
   9,  28, 129,  63, 184,  73, 353, 162,  22,  77,
  78,  79,  19,  10,  14,  48,  13,  88, 292,  87,
  75,  76,  98,  12,  94,  19,  30,  23,  24,  25,
  26,  69,  29,  81,  41,  82,  22,  16,  80,  83,
 159,  74,  27,  11,  22, 190,  20, 158, 317,  10,
 211, 212, 221, 121, 152, 133, 218,  65, 373,  27,
  11,  22, 149,  20, 144, 176,  10,  14, 131,  13,
 131, 175, 131,  22, 114, 369,  12,  19, 368,  30,
  23,  24,  25,  26, 131,  29,   8,  27, 344,  22,
  16,  20, 180, 308,  19,  27,  11, 327, 326,  20,
 338, 139, 263, 231, 260, 202, 150, 153, 165,  64,
  33,  32,  27,  11, 132,  31,  20,   1,   0, 170,
   0,   0,  19,  14,  27,  13,   0, 103,  20,   0,
  19,   0,  12,   0, 106,  30,  23,  24,  25,  26,
  27,  29,   7,   0,  20,   0,  16,  19,   0,   0,
   0,  14,   0,  13,   0,   0,   0,   0,   0,  19,
  12,   0,   0,  30,  23,  24,  25,  26,   0,  29,
   0,   0,   0,   0,  16,  19,  30,  23,  24,  25,
  26,   0,  29,   0,   0,   0,   0,  16,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 170,   0, 132,
   0, 132,   0, 132,   0,   0,   0, 320, 103,   0,
   0,   0,   0,   0,   0, 132,  14,   0,  13,   0,
   0,   0,   0,   0,   7,  12,   0,   0,  30,  23,
  24,  25,  26,  14,  29,  13,   0,   0,   0,  16,
   0,   0, 197,   0,   0,  30,  23,  24,  25,  26,
   0,  29,   0,   0,   0,   0,  16,   0,   0, 107,
   0,   0, 316,   0,   0,   0,   0, 249,   0,  14,
   0,  13,   0,  30,  23,  24,  25,  26,  12,  29,
   0,  30,  23,  24,  25,  26,  14,  29,  13,   0,
   0,   0,  16,   0,   0,  12,   0,   0,  30,  23,
  24,  25,  26,  17,  29,   0,   0,   0,   0,  16,
  30,  23,  24,  25,  26,  50,  29,   0,   0,  54,
  55,  16,   0,  57,   0,   0,  30,  23,  24,  25,
  26,   0,  29,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,  54, 108,   0,   0,   0,   0,   0,   0,
 113,   0,   0,   0,   0,   0,   0,   0,   0, 122,
 123 };
short yypact[]={

-144,-1000, 336,-237,-1000,-207,-158,-1000, 139, 139,
 349, 499,   6, 336, 336, 499, 499,-1000,  79, 499,
-1000,-1000,  57,-1000,-1000,-1000,-1000, 336,-1000,-1000,
-1000,-1000, 199,-274,-1000,-1000,-210,-1000,   5, 102,
 471,-1000,-1000,-1000,-1000,-236,-1000, 471, 499, 446,
-1000, 471,-180,-176,-1000,-1000, 499,-1000, 483, 196,
 167,-1000, 188, 173, 139, 499, 499,  48, 118, 134,
 393,-1000,-1000,-1000,-214,-212, 102,-225,-225,-225,
  95, 102, 102, 102, 125,  42,  79,-1000, 212,-1000,
-1000,-1000, 471,-1000,-248,-1000, 166,-1000,-1000,-1000,
-1000,  24,  -2, 139,  38,  36,-1000, 336,-1000, 395,
-1000, 336,   4,-1000,-1000,-1000,-1000,-1000, 336,-1000,
 336, 454, 207, 204,-1000, 336, 418, 336,-1000, 130,
-1000,-1000,-1000, -19,-1000,   1,-1000,-1000,  86,-253,
 139,-1000, 139, 139, 124,-1000,-1000,-229,  66,-188,
-255,-1000,-192,-256,-1000,-1000, 102,  43,-1000,-207,
-1000, 336,  -8,-1000, 336,-194,  69, 395, 102, 102,
-1000,-1000, 202, 169, 162, 163, 156,-1000,  46,-1000,
 471,-1000,-1000,-1000, 308,-219,-185, 471,-1000,-1000,
-1000,-1000,-1000,-169, 127,-1000,-199,   3,-1000,-1000,
-1000,-1000,-277, 210,  19, 102,-1000, 110,-212, 121,
-1000, 121, 121, 102,  43,-1000, 200, 196,-1000,   2,
 102,-1000,  -1, 102,-1000,-1000,-1000,-1000,-1000,  83,
 -15, 154,-1000,-1000,-172,-1000, 140,-1000,-1000,-1000,
-1000,-1000, 102, -16,-1000,-1000,-1000, 102,-1000,-1000,
-1000, 336, 336, 409,-1000, -30, 336, 418, 336, 471,
-1000, 209,  19, 187, 119,-1000, 140,-203,-101,-1000,
 -46,-1000,  -7, 139,-1000,-1000, 401,-1000, 471,-1000,
 336, 199,-1000,  69,-1000, 159,-1000,-1000,-1000,-1000,
-1000,-1000, 308,-1000,-207, -17, 336,-1000,-1000, 471,
-1000,-1000,-1000, 395,  -9, 146,-1000,  19,-1000,  43,
-1000, 110,  21, 102,-235, 110,-1000, 397,-1000,-1000,
-1000, 395,-1000, 393,-1000, 179, 138, 171,-1000, 117,
  74,-1000,-1000,-1000,-1000,-1000,-167,-219, 116,-1000,
-279,-282,-1000,-1000, 112,-1000,-159,-1000,-1000,-238,
-1000,-101,-1000, 252,-1000,-1000,-1000,-1000,  20,-1000,
 167, 336,-1000,  -9,  43,  20,  43, 102, 155, 111,
-1000,-1000,-1000,-1000,-1000,-233,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-240,-1000 };
short yypgo[]={

   0, 567,  12,  28, 565, 561,  77,   8, 560, 104,
  34, 435, 430, 558, 555, 554, 553,  79,  13, 117,
 552, 550,   5,   0,   9, 548, 547,  55, 189,   3,
 543,  11,  10, 474, 538,   4,   2,  16, 123, 528,
 525, 121, 472, 521, 515, 151, 357, 438, 484, 514,
 181, 512, 506, 504, 502, 498,   1, 145,   6, 497,
 490, 457, 110, 451, 536, 450, 406, 437, 432, 753,
 453, 426,  36, 289, 259, 258,  22, 203, 154,   7 };
short yyr1[]={

   0,   1,   1,   1,   1,   4,   4,   5,   6,   6,
   6,   6,   6,   8,   8,  11,  11,   9,   9,  12,
  12,  13,  13,  16,  16,  17,  17,  14,  14,  14,
  20,  20,  19,  19,  15,  15,  21,  21,  22,  22,
  18,  18,  18,  18,  18,  25,  25,  26,  26,   9,
   9,   9,  28,  28,  28,  30,  30,  34,  34,  35,
  35,  31,  31,  37,  37,  37,  32,  32,  32,  39,
  39,  40,  40,  36,  36,  33,  29,  29,  29,  41,
  41,  42,  42,  42,  42,  42,  42,  42,  42,  42,
  42,  43,  43,  44,  44,   9,   9,   9,  45,  45,
  46,  46,  47,  47,  47,  48,  48,  38,  38,   9,
  49,  49,  49,  50,   9,   9,   9,  51,  51,  52,
  52,  55,  55,  56,  56,  53,  53,  54,  54,  10,
  10,  57,  57,  59,  59,  59,  60,  60,   3,  61,
  61,  62,  27,  27,  23,  23,  63,  63,  24,  24,
   2,   2,   2,  58,  58,  58,  65,  65,  64,  64,
  64,  64,  64,  64,  67,  67,  66,  66,  66,  69,
  69,  69,  69,  69,  69,  69,  69,  69,  69,  69,
  69,  69,  69,  69,  69,  70,  70,  68,  68,  72,
  73,  73,  74,  74,  74,  75,  75,  76,  71,  71,
  71,  71,  71,  71,  71,  71,  77,  77,  78,  78,
  78,   7,   7,  79,  79 };
short yyr2[]={

   0,   2,   3,   2,   1,   3,   1,   1,   3,   3,
   1,   1,   1,   2,   1,   7,   2,   4,   2,   1,
   1,   0,   3,   3,   1,   1,   2,   0,   4,   3,
   0,   1,   3,   1,   0,   4,   3,   1,   3,   3,
   1,   1,   4,   4,   4,   3,   1,   0,   1,   5,
   5,   7,   2,   1,   1,   2,   0,   3,   1,   3,
   1,   3,   1,   3,   1,   1,   0,   2,   4,   0,
   1,   3,   1,   3,   1,   1,   1,   3,   1,   2,
   1,   1,   1,   2,   3,   3,   4,   3,   3,   3,
   2,   2,   1,   3,   3,   3,   3,   3,   1,   0,
   3,   1,   1,   1,   1,   1,   3,   1,   3,   4,
   3,   1,   1,   2,   3,   3,   2,   3,   1,   4,
   0,   3,   1,   1,   1,   3,   1,   4,   0,   3,
   2,   3,   1,   2,   1,   1,   2,   1,   4,   2,
   1,   4,   3,   1,   1,   3,   1,   3,   1,   3,
   3,   1,   1,   1,   3,   1,   3,   5,   2,   4,
   6,   6,   6,   1,   2,   1,   2,   2,   1,   1,
   3,   2,   1,   1,   2,   1,   1,   1,   1,   3,
   3,   3,   4,   4,   4,   3,   3,   3,   1,   2,
   2,   1,   1,   2,   1,   2,   1,   4,   0,   1,
   1,   3,   3,   4,   2,   5,   3,   1,   3,   1,
   4,   2,   1,   1,   1 };
short yychk[]={

-1000,  -1, 257, 258, 256,  -2, -58, 256, -64, -65,
  45,  92, 274, 267, 265, -66, 288, -69, -23, 126,
  95, -24,  40, 278, 279, 280, 281,  91, -63, 283,
 277,  -4,  -5,  -8, 256, -11, 294,  -3, 270, 259,
 -47, -48, -38,  45, 276,  96, 282, -47, -66, -67,
 -69, 123,  -2,  -2, -69, -69,  64, -69,  45, 276,
 282,  41,  -2, -70, -64, -48, -38, -71,  -2, -70,
  -6,  -9, -10, 256, 292, 271, 272, 260, 261, 262,
 289, 284, 286, 290, -27, -58, -23, -11, -12, 256,
 283, 280, 123, -36, -33, -29, -41, 256, -42, 277,
 283,  40,  91, -64, 277, 283, -64, 263, -69, -57,
 -10, 268, 266, -69,  41,  41,  41,  41,  44,  41,
  44, -47, -69, -69,  93,  44, 124, 264,  -7,  59,
 -79, 125, 256, -12, 256, -28, 283, 256, -29, -33,
 -45, 278, -45, -45, -49, -50, 256, -23,  40, -51,
 -33, -29, -53, -33, -29, -29, 259,  44, -59, -60,
 256,  61, -61, -62, 124, -13,  40, -57, 285, 263,
 -42,  41, 263, -29, -41, -43, -44,  44, -29,  93,
 -47,  96,  96,  -2,  59,  -7,  -2, 123,  -2,  -2,
  41,  41,  41,  -2, -77, -78,  -2, 274,  -2,  -9,
 -10, -79, -14, 291,  40,  61, 277,  61, 285, -46,
 -47, -46, -46, 259,  44, 280,  45, 276, -52, 270,
 285, -54, 270, 285, -36, -23,  -3,  -2, -62,  -2,
 270, -16, -17, -18, -12, -23, 283,  -7, -29, -29,
  41,  41,  44, 263,  41,  44,  41,  44,  93, -64,
 -10, 275, 269, -68, -72, -58, 264,  44, 273, 123,
 -15, 295,  40, -20, -19, -18, 283, -29, -31, -37,
 -29, 256, -28,  44, -36, -50, 123, -29, 123, -29,
  61, 123,  41,  44, 264,  40, -29,  41, -29,  -2,
  -2,  -7,  59, -73, -74, -75, 263, 256, -76, 124,
  -2, -78,  -2, -57,  40, -19,  41,  44, -30, 275,
 -32, 124, 287, -38,  96,  61, -47, -55, -56, -10,
 256, -57,  -2,  -6, -17, 264, -25, -26, -24, -27,
  40, -23, -72,  -3, -76,  -2, -58,  -7, -21, -22,
 -23, -24,  41, -18, -34, -35, -23, -37, 283,  40,
 -29, -31,  -7,  59,  -7,  -7,  41,  41,  44,  41,
 282, 263,  41,  44, 296, 296,  44, 259, -39, -40,
 283, -32, -56, 256, -24,  40,  -2, -22, -23, -24,
 -35, -36,  41,  44, 283 };
short yydef[]={

   0,  -2,   0,   0,   4,   1, 151, 152, 153, 155,
   0,   0,   0,   0,   0, 163,   0, 168, 169,   0,
 172, 173,   0, 175, 176, 177, 178, 198, 144, 148,
 146,   3,   0,   6,   7,  14,   0,   2,   0,   0,
   0, 102, 103, 104, 105,   0, 107,   0, 158,   0,
 165,   0,   0,   0, 166, 167,   0, 171,   0, 105,
 107, 174,   0,   0, 153,   0,   0,   0, 199, 200,
   0,  10,  11,  12,   0,   0,   0,  99,  99,  99,
   0,   0,   0,   0,   0,   0,  -2,  13,  21,  16,
  19,  20,   0, 150,   0,  -2,  76,  78,  80,  81,
  82,   0,   0, 154,   0,   0, 156,   0, 164,   0,
 132,   0,   0, 170, 145, 147, 149, 179,   0, 180,
   0,   0,   0,   0, 181,   0,   0, 204,   5,   0,
 212, 213, 214,  27,  18,   0,  53,  54,  75,   0,
   0,  98,   0,   0,   0, 111, 112,   0,   0, 120,
   0,  -2, 128,   0,  -2, 116,   0,   0, 130, 134,
 135,   0, 137, 140,   0,   0,   0,   0,   0,   0,
  79,  83,   0,   0,  76,   0,   0,  92,   0,  90,
   0, 106, 108, 159,   0,   0,   0,   0, 186, 185,
 182, 183, 184, 186, 201, 207, 209,   0, 202,   8,
   9, 211,  34,   0,  30,   0,  52,   0,   0,  95,
 101,  96,  97,   0,   0, 113,   0,   0, 114,   0,
   0, 115,   0,   0, 129, 142, 133, 136, 139,   0,
   0,   0,  24,  25,   0,  40,  -2, 138,  73,  77,
  84,  85,   0,   0,  87,  91,  88,   0,  89, 157,
 131,   0,   0,   0, 188,   0, 203,   0,   0,   0,
  17,   0,   0,   0,  31,  33,  41,  56,  66,  62,
  64,  -2,   0,   0, 109, 110,   0, 117,   0, 125,
   0,   0,  22,   0,  26,  47,  94,  86,  93, 160,
 161, 162,   0, 189, 191, 192,   0, 194, 196,   0,
 205, 206, 208,   0,   0,   0,  29,   0,  49,   0,
  50,   0,   0,   0,   0,   0, 100,   0, 122, 123,
 124,   0, 141,   0,  23,   0,   0,   0,  46,  48,
   0, 143, 187, 190, 195, 193,   0, 210,   0,  37,
   0,   0,  28,  32,  55,  58,  60,  61,  67,  69,
  63,  66, 119,   0, 127,  15,  42,  43,   0,  44,
   0,   0,  35,   0,   0,   0,   0,   0,   0,  70,
  72,  51, 121, 124,  45,   0, 197,  36,  38,  39,
  57,  59,  68,   0,  71 };
#define YYFLAG   -1000
#define YYERROR  goto yyerrlab
#define YYACCEPT return(0)
#define YYABORT  return(1)

/*      parser for yacc output  */

#ifdef YYDEBUG
int yydebug     = 0;     /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar      = -1;    /* current input token number */
int yynerrs     = 0;     /* number of errors */
short yyerrflag = 0;     /* error recovery flag */

int yyparse() {
    short yys[YYMAXDEPTH];
    short yyj, yym;
    register YYSTYPE *yypvt;
    register short yystate, *yyps, yyn;
    register YYSTYPE *yypv;
    register short *yyxi;

    yystate   = 0;
    yychar    = -1;
    yynerrs   = 0;
    yyerrflag = 0;
    yyps      = &yys[-1];
    yypv      = &yyv[-1];

yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
    if (yydebug)
        printf("state %d, char 0%o\n", yystate, yychar);
#endif
    if(++yyps>&yys[YYMAXDEPTH]) {
        yyerror("yacc stack overflow");
        return(1);
    }
    *yyps = yystate;
    ++yypv;
#ifdef UNION
    yyunion(yypv, &yyval);
#else
    *yypv = yyval;
#endif

yynewstate:

    yyn = yypact[yystate];

    if (yyn<=YYFLAG)
        goto yydefault; /* simple state */

    if (yychar<0)
        if ((yychar=yylex())<0)
            yychar=0;
    if ((yyn+=yychar)<0 || yyn>=YYLAST)
        goto yydefault;

    if (yychk[yyn=yyact[yyn]]==yychar) {
        /* valid shift */
        yychar = -1;
#ifdef UNION
        yyunion(&yyval, &yylval);
#else
        yyval = yylval;
#endif
        yystate = yyn;
        if (yyerrflag>0)
            --yyerrflag;
        goto yystack;

    }

yydefault:

    /* default state action */

    if ((yyn=yydef[yystate])== -2) {
        if (yychar<0)
            if ((yychar=yylex())<0)
                yychar = 0;
        /* look through exception table */

        for (yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2)
            ; /* VOID */

        for (yyxi+=2; *yyxi >= 0; yyxi+=2) {
            if (*yyxi==yychar)
                break;
        }
        if ((yyn=yyxi[1])<0)
            return(0);   /* accept */
    }

    if (yyn==0) {
        /* error */
        /* error ... attempt to resume parsing */

        switch (yyerrflag) {
            case 0: /* brand new error */
                    yyerror( "syntax error" );

yyerrlab:           ++yynerrs;

            case 1:
            case 2: /* incompletely recovered error ... try again */

                    yyerrflag = 3;

                    /* find a state where "error" is a legal shift action */

                    while (yyps>=yys) {
                        yyn = yypact[*yyps] + YYERRCODE;
                        if (yyn>=0 && yyn<YYLAST
                                   && yychk[yyact[yyn]]==YYERRCODE) {
                            yystate = yyact[yyn];
                            /* simulate a shift of "error" */
                            goto yystack;
                        }
                        yyn = yypact[*yyps];

                        /* the current yyps has no shift on "error",
                           pop stack */

#ifdef YYDEBUG
                        if (yydebug)
                            printf("error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1]);
#endif

                        --yyps;
                        --yypv;
                    }

                    /* there is no state on the stack with an error shift
                       ... abort */

yyabort:            return(1);


            case 3: /* no shift yet; clobber input char */
#ifdef YYDEBUG
                    if (yydebug)
                        printf("error recovery discards char %d\n", yychar);
#endif

                    if (yychar==0)
                        goto yyabort; /* don't discard EOF, quit */
                    yychar = -1;
                    goto yynewstate;   /* try again in the same state */
        }
    }

    /* reduction by production yyn */

#ifdef YYDEBUG
    if (yydebug)
        printf("reduce %d\n",yyn);
#endif
    yyps -= yyr2[yyn];
    yypvt = yypv;
    yypv -= yyr2[yyn];
#ifdef UNION
    yyunion(&yyval, &yypv[1]);
#else
    yyval = yypv[1];
#endif
    yym=yyn;
    /* consult goto table to find next state */
    yyn = yyr1[yyn];
    yyj = yypgo[yyn] + *yyps + 1;
    if (yyj>=YYLAST || yychk[yystate=yyact[yyj]]!= -yyn)
        yystate = yyact[yypgo[yyn]];
    switch(yym) {
        
case 1:
# line 86
{inputExpr = yypvt[-0];	    sp-=1;} break;
case 2:
# line 87
{inputExpr = letrec(yypvt[-0],yypvt[-1]); sp-=2;} break;
case 3:
# line 88
{valDefns  = yypvt[-0];	    sp-=1;} break;
case 4:
# line 89
{syntaxError("input");} break;
case 5:
# line 102
{yyval = gc2(yypvt[-1]);} break;
case 6:
# line 103
{yyval = yypvt[-0];} break;
case 7:
# line 105
{yyerrok; goOffside(startColumn);} break;
case 8:
# line 107
{yyval = gc2(yypvt[-2]);} break;
case 9:
# line 108
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 10:
# line 109
{yyval = gc0(NIL);} break;
case 11:
# line 110
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 12:
# line 111
{syntaxError("definition");} break;
case 13:
# line 113
{yyval = gc2(appendOnto(yypvt[-0],yypvt[-1]));} break;
case 14:
# line 114
{yyval = yypvt[-0];} break;
case 15:
# line 117
{yyval = gc7(yypvt[-1]);} break;
case 16:
# line 118
{syntaxError("module definition");} break;
case 17:
# line 120
{sp-=4;} break;
case 18:
# line 121
{syntaxError("import declaration");} break;
case 19:
# line 123
{yyval = yypvt[-0];} break;
case 20:
# line 124
{yyval = yypvt[-0];} break;
case 21:
# line 126
{yyval = gc0(NIL);} break;
case 22:
# line 127
{yyval = gc3(NIL);} break;
case 23:
# line 129
{yyval = gc3(NIL);} break;
case 24:
# line 130
{yyval = yypvt[-0];} break;
case 25:
# line 132
{yyval = yypvt[-0];} break;
case 26:
# line 133
{yyval = gc2(NIL);} break;
case 27:
# line 135
{yyval = gc0(NIL);} break;
case 28:
# line 136
{yyval = gc4(NIL);} break;
case 29:
# line 137
{yyval = gc3(NIL);} break;
case 30:
# line 139
{yyval = gc0(NIL);} break;
case 31:
# line 140
{yyval = yypvt[-0];} break;
case 32:
# line 142
{yyval = gc3(NIL);} break;
case 33:
# line 143
{yyval = yypvt[-0];} break;
case 34:
# line 145
{yyval = gc0(NIL);} break;
case 35:
# line 146
{yyval = gc4(NIL);} break;
case 36:
# line 148
{yyval = gc3(NIL);} break;
case 37:
# line 149
{yyval = yypvt[-0];} break;
case 38:
# line 151
{yyval = gc3(NIL);} break;
case 39:
# line 152
{yyval = gc3(NIL);} break;
case 40:
# line 154
{yyval = yypvt[-0];} break;
case 41:
# line 155
{yyval = yypvt[-0];} break;
case 42:
# line 156
{yyval = gc4(NIL);} break;
case 43:
# line 157
{yyval = gc4(NIL);} break;
case 44:
# line 158
{yyval = gc4(NIL);} break;
case 45:
# line 160
{yyval = gc3(NIL);} break;
case 46:
# line 161
{yyval = yypvt[-0];} break;
case 47:
# line 163
{yyval = gc0(NIL);} break;
case 48:
# line 164
{yyval = yypvt[-0];} break;
case 49:
# line 169
{defTycon(5,yypvt[-2],yypvt[-3],yypvt[-1],yypvt[-0]);} break;
case 50:
# line 171
{defTycon(5,yypvt[-2],checkTyLhs(yypvt[-3]),
							rev(yypvt[-1]),yypvt[-0]);} break;
case 51:
# line 174
{defTycon(7,yypvt[-2],yypvt[-3],
						  ap(QUAL,pair(yypvt[-5],rev(yypvt[-1]))),
						  yypvt[-0]);} break;
case 52:
# line 178
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 53:
# line 179
{yyval = yypvt[-0];} break;
case 54:
# line 180
{syntaxError("type defn lhs");} break;
case 55:
# line 182
{yyval = gc2(yypvt[-0]);} break;
case 56:
# line 183
{yyval = gc0(SYNONYM);} break;
case 57:
# line 185
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 58:
# line 186
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 59:
# line 188
{yyval = gc3(sigdecl(yypvt[-1],singleton(yypvt[-2]),
							     yypvt[-0]));} break;
case 60:
# line 190
{yyval = yypvt[-0];} break;
case 61:
# line 192
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 62:
# line 193
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 63:
# line 195
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 64:
# line 196
{if (!isCon(getHead(yypvt[-0])))
					     syntaxError("data constructor");
					 yyval = yypvt[-0];} break;
case 65:
# line 199
{syntaxError("data type definition");} break;
case 66:
# line 201
{yyval = gc0(DATATYPE);} break;
case 67:
# line 202
{yyval = gc2(ap(DERIVE,singleton(yypvt[-0])));} break;
case 68:
# line 203
{yyval = gc4(yypvt[-1]);} break;
case 69:
# line 205
{yyval = gc0(DATATYPE);} break;
case 70:
# line 206
{yyval = gc1(ap(DERIVE,rev(yypvt[-0])));} break;
case 71:
# line 208
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 72:
# line 209
{yyval = gc1(singleton(yypvt[-0]));} break;
case 73:
# line 220
{yyval = gc3(ap(QUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 74:
# line 221
{yyval = yypvt[-0];} break;
case 75:
# line 223
{yyval = gc1(checkContext(yypvt[-0]));} break;
case 76:
# line 225
{yyval = yypvt[-0];} break;
case 77:
# line 226
{yyval = gc3(ap(ap(ARROW,yypvt[-2]),yypvt[-0]));} break;
case 78:
# line 227
{syntaxError("type expression");} break;
case 79:
# line 229
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 80:
# line 230
{yyval = yypvt[-0];} break;
case 81:
# line 232
{yyval = yypvt[-0];} break;
case 82:
# line 233
{yyval = yypvt[-0];} break;
case 83:
# line 234
{yyval = gc2(UNIT);} break;
case 84:
# line 235
{yyval = gc3(ARROW);} break;
case 85:
# line 236
{yyval = gc3(yypvt[-1]);} break;
case 86:
# line 237
{yyval = gc4(ap(ARROW,yypvt[-2]));} break;
case 87:
# line 238
{yyval = gc3(yypvt[-1]);} break;
case 88:
# line 239
{yyval = gc3(buildTuple(yypvt[-1]));} break;
case 89:
# line 240
{yyval = gc3(ap(LIST,yypvt[-1]));} break;
case 90:
# line 241
{yyval = gc2(LIST);} break;
case 91:
# line 243
{yyval = gc2(mkTuple(tupleOf(yypvt[-1])+1));} break;
case 92:
# line 244
{yyval = gc1(mkTuple(2));} break;
case 93:
# line 246
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 94:
# line 247
{yyval = gc3(cons(yypvt[-0],cons(yypvt[-2],NIL)));} break;
case 95:
# line 252
{fixDefn(LEFT_ASS,yypvt[-2],yypvt[-1],yypvt[-0]); sp-=3;} break;
case 96:
# line 253
{fixDefn(RIGHT_ASS,yypvt[-2],yypvt[-1],yypvt[-0]);sp-=3;} break;
case 97:
# line 254
{fixDefn(NON_ASS,yypvt[-2],yypvt[-1],yypvt[-0]);  sp-=3;} break;
case 98:
# line 256
{yyval = gc1(checkPrec(yypvt[-0]));} break;
case 99:
# line 257
{yyval = gc0(mkInt(DEF_PREC));} break;
case 100:
# line 259
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 101:
# line 260
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 102:
# line 262
{yyval = yypvt[-0];} break;
case 103:
# line 263
{yyval = yypvt[-0];} break;
case 104:
# line 264
{yyval = gc1(varMinus);} break;
case 105:
# line 266
{yyval = yypvt[-0];} break;
case 106:
# line 267
{yyval = gc3(yypvt[-1]);} break;
case 107:
# line 269
{yyval = yypvt[-0];} break;
case 108:
# line 270
{yyval = gc3(yypvt[-1]);} break;
case 109:
# line 275
{primDefn(yypvt[-3],yypvt[-2],yypvt[-0]); sp-=4;} break;
case 110:
# line 277
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 111:
# line 278
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 112:
# line 279
{syntaxError("primitive defn");} break;
case 113:
# line 281
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 114:
# line 286
{classDefn(intOf(yypvt[-2]),yypvt[-1],yypvt[-0]); sp-=3;} break;
case 115:
# line 287
{instDefn(intOf(yypvt[-2]),yypvt[-1],yypvt[-0]);  sp-=3;} break;
case 116:
# line 288
{defaultDefn(intOf(yypvt[-1]),
						     tupToList(yypvt[-0])); sp-=2;} break;
case 117:
# line 291
{yyval = gc3(pair(yypvt[-2],checkClass(yypvt[-0])));} break;
case 118:
# line 292
{yyval = gc1(pair(NIL,checkClass(yypvt[-0])));} break;
case 119:
# line 294
{yyval = gc4(yypvt[-1]);} break;
case 120:
# line 295
{yyval = gc0(NIL);} break;
case 121:
# line 297
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 122:
# line 298
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 123:
# line 300
{yyval = gc1(yypvt[-0]);} break;
case 124:
# line 301
{syntaxError("class body");} break;
case 125:
# line 303
{yyval = gc3(pair(yypvt[-2],checkInst(yypvt[-0])));} break;
case 126:
# line 304
{yyval = gc1(pair(NIL,checkInst(yypvt[-0])));} break;
case 127:
# line 306
{yyval = gc4(yypvt[-1]);} break;
case 128:
# line 307
{yyval = gc0(NIL);} break;
case 129:
# line 312
{yyval = gc3(sigdecl(yypvt[-1],yypvt[-2],yypvt[-0]));} break;
case 130:
# line 313
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 131:
# line 315
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 132:
# line 316
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 133:
# line 318
{yyval = gc2(letrec(yypvt[-0],yypvt[-1]));} break;
case 134:
# line 319
{yyval = yypvt[-0];} break;
case 135:
# line 320
{syntaxError("declaration");} break;
case 136:
# line 322
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 137:
# line 323
{yyval = gc1(grded(rev(yypvt[-0])));} break;
case 138:
# line 325
{yyval = gc4(yypvt[-1]);} break;
case 139:
# line 327
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 140:
# line 328
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 141:
# line 330
{yyval = gc4(pair(yypvt[-1],pair(yypvt[-2],yypvt[-0])));} break;
case 142:
# line 332
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 143:
# line 333
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 144:
# line 335
{yyval = yypvt[-0];} break;
case 145:
# line 336
{yyval = gc3(varMinus);} break;
case 146:
# line 338
{yyval = yypvt[-0];} break;
case 147:
# line 339
{yyval = gc3(yypvt[-1]);} break;
case 148:
# line 341
{yyval = yypvt[-0];} break;
case 149:
# line 342
{yyval = gc3(yypvt[-1]);} break;
case 150:
# line 347
{yyval = gc3(ap(ESIGN,pair(yypvt[-2],yypvt[-0])));} break;
case 151:
# line 348
{yyval = yypvt[-0];} break;
case 152:
# line 349
{syntaxError("expression");} break;
case 153:
# line 351
{yyval = yypvt[-0];} break;
case 154:
# line 352
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 155:
# line 353
{yyval = gc1(tidyInfix(yypvt[-0]));} break;
case 156:
# line 355
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 157:
# line 356
{yyval = gc5(ap(ap(yypvt[-1],
							ap(ap(yypvt[-3],singleton(yypvt[-4])),
                                                           yypvt[-2])),yypvt[-0]));} break;
case 158:
# line 360
{if (isInt(yypvt[-0]))
					     yyval = gc2(mkInt(-intOf(yypvt[-0])));
					 else
					     yyval = gc2(ap(varNegate,yypvt[-0]));
					} break;
case 159:
# line 365
{yyval = gc4(ap(LAMBDA,
						     pair(rev(yypvt[-2]),
							  pair(yypvt[-1],yypvt[-0]))));} break;
case 160:
# line 368
{yyval = gc6(letrec(yypvt[-3],yypvt[-0]));} break;
case 161:
# line 369
{yyval = gc6(ap(COND,triple(yypvt[-4],yypvt[-2],yypvt[-0])));} break;
case 162:
# line 370
{yyval = gc6(ap(CASE,pair(yypvt[-4],rev(yypvt[-1]))));} break;
case 163:
# line 371
{yyval = yypvt[-0];} break;
case 164:
# line 373
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 165:
# line 374
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 166:
# line 376
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 167:
# line 377
{yyval = gc2(ap(RUNST,yypvt[-0]));} break;
case 168:
# line 378
{yyval = yypvt[-0];} break;
case 169:
# line 380
{yyval = yypvt[-0];} break;
case 170:
# line 381
{yyval = gc3(ap(ASPAT,pair(yypvt[-2],yypvt[-0])));} break;
case 171:
# line 382
{yyval = gc2(ap(LAZYPAT,yypvt[-0]));} break;
case 172:
# line 383
{yyval = gc1(WILDCARD);} break;
case 173:
# line 384
{yyval = yypvt[-0];} break;
case 174:
# line 385
{yyval = gc2(UNIT);} break;
case 175:
# line 386
{yyval = yypvt[-0];} break;
case 176:
# line 387
{yyval = yypvt[-0];} break;
case 177:
# line 388
{yyval = yypvt[-0];} break;
case 178:
# line 389
{yyval = yypvt[-0];} break;
case 179:
# line 390
{yyval = gc3(yypvt[-1]);} break;
case 180:
# line 391
{yyval = gc3(buildTuple(yypvt[-1]));} break;
case 181:
# line 392
{yyval = gc3(yypvt[-1]);} break;
case 182:
# line 393
{yyval = gc4(ap(yypvt[-1],yypvt[-2]));} break;
case 183:
# line 394
{yyval = gc4(ap(ap(varFlip,yypvt[-2]),yypvt[-1]));} break;
case 184:
# line 395
{yyval = gc4(ap(ap(varFlip,yypvt[-2]),yypvt[-1]));} break;
case 185:
# line 397
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 186:
# line 398
{yyval = gc3(cons(yypvt[-0],cons(yypvt[-2],NIL)));} break;
case 187:
# line 400
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 188:
# line 401
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 189:
# line 403
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 190:
# line 405
{yyval = gc2(letrec(yypvt[-0],yypvt[-1]));} break;
case 191:
# line 406
{yyval = yypvt[-0];} break;
case 192:
# line 408
{yyval = gc1(grded(rev(yypvt[-0])));} break;
case 193:
# line 409
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 194:
# line 410
{syntaxError("case expression");} break;
case 195:
# line 412
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 196:
# line 413
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 197:
# line 415
{yyval = gc4(pair(yypvt[-1],pair(yypvt[-2],yypvt[-0])));} break;
case 198:
# line 420
{yyval = gc0(nameNil);} break;
case 199:
# line 421
{yyval = gc1(ap(FINLIST,cons(yypvt[-0],NIL)));} break;
case 200:
# line 422
{yyval = gc1(ap(FINLIST,rev(yypvt[-0])));} break;
case 201:
# line 423
{yyval = gc3(ap(COMP,pair(yypvt[-2],rev(yypvt[-0]))));} break;
case 202:
# line 424
{yyval = gc3(ap(ap(varFromTo,yypvt[-2]),yypvt[-0]));} break;
case 203:
# line 425
{yyval = gc4(ap(ap(varFromThen,yypvt[-3]),yypvt[-1]));} break;
case 204:
# line 426
{yyval = gc2(ap(varFrom,yypvt[-1]));} break;
case 205:
# line 427
{yyval = gc5(ap(ap(ap(varFromThenTo,
                                                               yypvt[-4]),yypvt[-2]),yypvt[-0]));} break;
case 206:
# line 430
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 207:
# line 431
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 208:
# line 433
{yyval = gc3(ap(FROMQUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 209:
# line 434
{yyval = gc1(ap(BOOLQUAL,yypvt[-0]));} break;
case 210:
# line 435
{yyval = gc4(ap(QWHERE,yypvt[-1]));} break;
case 211:
# line 441
{yyval = gc2(yypvt[-0]);} break;
case 212:
# line 442
{yyval = yypvt[-0];} break;
case 213:
# line 444
{yyval = yypvt[-0];} break;
case 214:
# line 445
{yyerrok;
                                         if (canUnOffside()) {
					     unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
                                         else
                                             syntaxError("definition");
                                        } break;/* End of actions */
    }
    goto yystack;  /* stack new state and value */

}
