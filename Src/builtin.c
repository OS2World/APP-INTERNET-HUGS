/* --------------------------------------------------------------------------
 * builtin.c:   Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.0 August 1994, derived from Gofer 2.30a
 *
 * Primitive functions, input output etc...
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <ctype.h>
#include <math.h>
#include <float.h>
#if (BCC | BCC32)
#include <io.h>
#endif

Name nameFatbar,  nameFail;		/* primitives reqd for translation */
Name nameIf,	  nameSel;
Name namePmInt,   namePmFlt;		/* primitives for pattern matching */
Name namePmInteger;
#if NPLUSK
Name namePmNpk,   namePmSub;		/* primitives for (n+k) patterns   */
#endif
Name nameConCmp,  nameEnRange;		/* primitives used for deriv inst  */
Name nameEnIndex, nameEnInRng;
Name nameEnFrom,  nameEnFrTh;
Name nameEnFrTo;

Name nameUndefMem;			/* undefined member primitive	   */
Name nameMakeMem;			/* makeMember primitive		   */
Name nameBlackHole;			/* for GC-detected black hole	   */
Name nameAnd,     nameOr;		/* built-in logical connectives	   */
Name nameOtherwise;			/* another name for True	   */
Name nameError;				/* error primitive function	   */
Name nameComp;				/* function composition		   */
Name nameApp;				/* list append			   */
Name nameShowParen;			/* wrap with parens		   */
Name nameRangeSize;			/* calculate size of index range   */

Name namePrint,  nameNPrint;		/* primitives for printing	   */

#if    HASKELL_ARRAYS
static Name nameEltUndef;		/* undefined element in array	   */
static Name nameOutBounds;		/* value of of bounds		   */
#endif
#if    IO_MONAD
Name   nameSTRun;			/* encapsulation operator for IO   */
static Name nameFst;			/* fst primitive		   */
static Name nameSnd;			/* snd primitive		   */
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

#define PROTO_PRIM(name)	static Void name Args((StackPtr))
#define primFun(name)		static Void name(root) StackPtr root;
#define primArg(n)		stack(root+n)

/* IMPORTANT: the second element of an update must be written first.
 * this is to deal with the case where an INDIRECT tag is written into
 * a Cell before the second value has been set.  If a garbage collection
 * occurs before the second element was set then the INDIRECTion will be
 * (wrongly) elided and result in chaos.  I know.  It happened to me.
 */

#define update(l,r)		((snd(stack(root))=r),(fst(stack(root))=l))
#define updateRoot(c)		update(INDIRECT,c)
#define updapRoot(l,r)		update(l,r)
#define cantReduce()		evalFails(root)

PROTO_PRIM(primFatbar);
PROTO_PRIM(primFail);
PROTO_PRIM(primSel);
PROTO_PRIM(primIf);
PROTO_PRIM(primStrict);
PROTO_PRIM(primTrace);
PROTO_PRIM(primMakeMem);
PROTO_PRIM(primConCmp);
PROTO_PRIM(primEnRange);
PROTO_PRIM(primEnIndex);
PROTO_PRIM(primEnInRng);
PROTO_PRIM(primEnFrom);
PROTO_PRIM(primEnFrTh);
PROTO_PRIM(primEnFrTo);


#if    HASKELL_ARRAYS
static Int  local getSize	Args((Cell, Cell));
static List local addAssocs	Args((Cell, Int, Cell, List));
static List local foldAssocs	Args((Cell, Int, Cell, Cell, List));

PROTO_PRIM(primArray);
PROTO_PRIM(primUpdate);
PROTO_PRIM(primAccum);
PROTO_PRIM(primAccumArray);
PROTO_PRIM(primAmap);
PROTO_PRIM(primSubscript);
PROTO_PRIM(primBounds);
PROTO_PRIM(primElems);
#endif

PROTO_PRIM(primMinInt);
PROTO_PRIM(primMaxInt);
PROTO_PRIM(primPlusInt);
PROTO_PRIM(primMinusInt);
PROTO_PRIM(primMulInt);
PROTO_PRIM(primDivInt);
PROTO_PRIM(primQuotInt);
PROTO_PRIM(primModInt);
PROTO_PRIM(primRemInt);
PROTO_PRIM(primQrmInt);
PROTO_PRIM(primNegInt);
PROTO_PRIM(primEvenInt);

#if BIGNUMS
PROTO_PRIM(primPlusInteger);
PROTO_PRIM(primMinusInteger);
PROTO_PRIM(primMulInteger);
PROTO_PRIM(primQrmInteger);
PROTO_PRIM(primNegInteger);
PROTO_PRIM(primEvenInteger);
PROTO_PRIM(primIntToInteger);
PROTO_PRIM(primIntegerToInt);
PROTO_PRIM(primIntegerToFloat);
PROTO_PRIM(primEqInteger);
PROTO_PRIM(primCmpInteger);
#endif

PROTO_PRIM(primCharToInt);
PROTO_PRIM(primIntToChar);
PROTO_PRIM(primIntToFloat);
PROTO_PRIM(primDummyCvt);

PROTO_PRIM(primPlusFloat);
PROTO_PRIM(primMinusFloat);
PROTO_PRIM(primMulFloat);
PROTO_PRIM(primDivFloat);
PROTO_PRIM(primNegFloat);

#if HAS_FLOATS
PROTO_PRIM(primPiFloat);
PROTO_PRIM(primSinFloat);
PROTO_PRIM(primCosFloat);
PROTO_PRIM(primTanFloat);
PROTO_PRIM(primAsinFloat);
PROTO_PRIM(primAcosFloat);
PROTO_PRIM(primAtanFloat);
PROTO_PRIM(primAtan2Float);
PROTO_PRIM(primExpFloat);
PROTO_PRIM(primLogFloat);
PROTO_PRIM(primLog10Float);
PROTO_PRIM(primSqrtFloat);
PROTO_PRIM(primFloatToInt);
PROTO_PRIM(primFloatRadix);
PROTO_PRIM(primFloatDigits);
PROTO_PRIM(primFloatRange);
PROTO_PRIM(primFloatDecode);
PROTO_PRIM(primFloatEncode);
#endif

PROTO_PRIM(primEqInt);
PROTO_PRIM(primCmpInt);

PROTO_PRIM(primEqChar);
PROTO_PRIM(primLeChar);

PROTO_PRIM(primEqFloat);
PROTO_PRIM(primLeFloat);

#if GENERIC_CMP
PROTO_PRIM(primCmp);
PROTO_PRIM(primGenericEq);
PROTO_PRIM(primGenericLe);
PROTO_PRIM(primGenericLt);
PROTO_PRIM(primGenericGe);
PROTO_PRIM(primGenericGt);
PROTO_PRIM(primGenericNe);
#endif

PROTO_PRIM(primPrint);
PROTO_PRIM(primNPrint);

static Void   local printer		Args((StackPtr,Name,Int,Cell));
static Void   local startList		Args((StackPtr,Cell));
static Void   local startNList		Args((StackPtr,Cell));

PROTO_PRIM(primLPrint);
PROTO_PRIM(primNLPrint);
PROTO_PRIM(primSPrint);
PROTO_PRIM(primNSPrint);

static Cell   local textAsVar		Args((Text,Cell));
static Cell   local textAsOp		Args((Text,Cell));
static Cell   local stringOutput	Args((String,Cell));
static Cell   local printBadRedex	Args((Cell,Cell));
static Cell   local printDBadRedex	Args((Cell,Cell));
static Cell   local outputInst		Args((Inst,List));

#if IO_DIALOGUE
static String local evalName		Args((Cell));
#endif
#if IO_DIALOGUE
static Void   local abandonDialogue	Args((Cell));
static Cell   local readFile		Args((Void));
static Cell   local writeFile		Args((Void));
static Cell   local appendFile		Args((Void));
static Cell   local readChan		Args((Void));
static Cell   local appendChan		Args((Void));
static FILE  *local validOutChannel	Args((String));
static Cell   local echo		Args((Void));
static Cell   local getCLArgs		Args((Void));
static Cell   local getProgName		Args((Void));
static Cell   local getEnv		Args((Void));
static Cell   local outputDString	Args((FILE *));

PROTO_PRIM(primInput);
PROTO_PRIM(primFopen);
#endif

#if IO_MONAD
PROTO_PRIM(primSTRun);
PROTO_PRIM(primFst);
PROTO_PRIM(primSnd);
PROTO_PRIM(primSTReturn);
PROTO_PRIM(primIOBind);
PROTO_PRIM(primSTBind);
PROTO_PRIM(primSTInter);
PROTO_PRIM(primSTNew);
PROTO_PRIM(primSTAssign);
PROTO_PRIM(primSTDeref);
PROTO_PRIM(primSTMutVarEq);
PROTO_PRIM(primIOGetch);
PROTO_PRIM(primIOPutchar);
#if HASKELL_ARRAYS
PROTO_PRIM(primSTNewArr);
PROTO_PRIM(primSTReadArr);
PROTO_PRIM(primSTWriteArr);
PROTO_PRIM(primSTFreeze);
#endif
#endif

/* --------------------------------------------------------------------------
 * Table of primitive/built-in values:
 * ------------------------------------------------------------------------*/

struct primitive primitives[] = {
  {"primFatbar",	2, primFatbar},
  {"primFail",		0, primFail},
  {"primUndefMem",	1, primFail},
  {"primGCBhole",	0, primFail},
  {"primError",		1, primFail},
  {"primSel",		3, primSel},
  {"primIf",		3, primIf},
  {"primTrace",		2, primTrace},
  {"primMakeMem",	2, primMakeMem},
  {"primConCmp",	3, primConCmp},
  {"primEnRange",	1, primEnRange},
  {"primEnIndex",	2, primEnIndex},
  {"primEnInRng",	2, primEnInRng},
  {"primEnFrom",	1, primEnFrom},
  {"primEnFrTh",	2, primEnFrTh},
  {"primEnFrTo",	2, primEnFrTo},

#if    HASKELL_ARRAYS
  {"primArray",		3, primArray},
  {"primUpdate",	3, primUpdate},
  {"primAccum",		4, primAccum},
  {"primAccumArray",	5, primAccumArray},
  {"primAmap",		2, primAmap},
  {"primSubscript",	3, primSubscript},
  {"primBounds",	1, primBounds},
  {"primElems",		1, primElems},
  {"primEltUndef",	0, primFail},
  {"primOutBounds",	2, primFail},
#endif

  {"primPrint",		3, primPrint},
  {"primNprint",	3, primNPrint},
  {"primLprint",	2, primLPrint},
  {"primNlprint",	2, primNLPrint},
  {"primSprint",	2, primSPrint},
  {"primNsprint",	2, primNSPrint},

  {"primMinInt",	0, primMinInt},
  {"primMaxInt",	0, primMaxInt},
  {"primPlusInt",	2, primPlusInt},
  {"primMinusInt",	2, primMinusInt},
  {"primMulInt",	2, primMulInt},
  {"primDivInt",	2, primDivInt},
  {"primQuotInt",	2, primQuotInt},
  {"primModInt",	2, primModInt},
  {"primRemInt",	2, primRemInt},
  {"primNegInt",	1, primNegInt},
  {"primEvenInt",	1, primEvenInt},
  {"primQrmInt",	2, primQrmInt},

#if BIGNUMS				/* Bignum primitive functions	   */
  {"primPlusInteger",	2, primPlusInteger},
  {"primMinusInteger",	2, primMinusInteger},
  {"primMulInteger",	2, primMulInteger},
  {"primQrmInteger",	2, primQrmInteger},
  {"primNegInteger",	1, primNegInteger},
  {"primEvenInteger",	1, primEvenInteger},
  {"primIntToInteger",  1, primIntToInteger},
  {"primIntegerToInt",  1, primIntegerToInt},
  {"primIntegerToFloat",1, primIntegerToFloat},
  {"primIntegerToDouble",1,primIntegerToFloat},
  {"primEqInteger",	2, primEqInteger},
  {"primCmpInteger",	3, primCmpInteger},
#else					/* Implement Integer as Int	   */
  {"primPlusInteger",	2, primPlusInt},
  {"primMinusInteger",	2, primMinusInt},
  {"primMulInteger",	2, primMulInt},
  {"primQrmInteger",	2, primQrmInt},
  {"primNegInteger",	1, primNegInt},
  {"primIntToInteger",  1, primDummyCvt},
  {"primIntegerToInt",  1, primDummyCvt},
  {"primIntegerToFloat",1, primIntToFloat},
  {"primIntegerToDouble",1,primIntToFloat},
  {"primEqInteger",	2, primEqInt},
  {"primCmpInteger",	3, primCmpInt},
#endif

  {"primPlusFloat",	2, primPlusFloat},
  {"primMinusFloat",	2, primMinusFloat},
  {"primMulFloat",	2, primMulFloat},
  {"primDivFloat",	2, primDivFloat},
  {"primNegFloat",	1, primNegFloat},

  {"primPlusDouble",	2, primPlusFloat},	/* Currently Float */
  {"primMinusDouble",	2, primMinusFloat},	/* Currently Float */
  {"primMulDouble",	2, primMulFloat},	/* Currently Float */
  {"primDivDouble",	2, primDivFloat},	/* Currently Float */
  {"primNegDouble",	1, primNegFloat},	/* Currently Float */

#if HAS_FLOATS
  {"primPiFloat",	0, primPiFloat},
  {"primSinFloat",	1, primSinFloat},
  {"primCosFloat",	1, primCosFloat},
  {"primTanFloat",	1, primTanFloat},
  {"primAsinFloat",	1, primAsinFloat},
  {"primAcosFloat",	1, primAcosFloat},
  {"primAtanFloat",	1, primAtanFloat},
  {"primAtan2Float",	2, primAtan2Float},
  {"primExpFloat",	1, primExpFloat},
  {"primLogFloat",	1, primLogFloat},
  {"primLog10Float",	1, primLog10Float},
  {"primSqrtFloat",	1, primSqrtFloat},
  {"primFloatToInt",	1, primFloatToInt},
  {"primFloatRadix",	1, primFloatRadix},
  {"primFloatDigits",	1, primFloatDigits},
  {"primFloatRange",	1, primFloatRange},
  {"primFloatDecode",	1, primFloatDecode},
  {"primFloatEncode",	2, primFloatEncode},

  {"primPiDouble",	0, primPiFloat},	/* Currently Float */
  {"primSinDouble",	1, primSinFloat},	/* Currently Float */
  {"primCosDouble",	1, primCosFloat},	/* Currently Float */
  {"primTanDouble",	1, primTanFloat},	/* Currently Float */
  {"primAsinDouble",	1, primAsinFloat},	/* Currently Float */
  {"primAcosDouble",	1, primAcosFloat},	/* Currently Float */
  {"primAtanDouble",	1, primAtanFloat},	/* Currently Float */
  {"primAtan2Double",	2, primAtan2Float},	/* Currently Float */
  {"primExpDouble",	1, primExpFloat},	/* Currently Float */
  {"primLogDouble",	1, primLogFloat},	/* Currently Float */
  {"primLog10Double",	1, primLog10Float},	/* Currently Float */
  {"primSqrtDouble",	1, primSqrtFloat},	/* Currently Float */
  {"primDoubleToInt",	1, primFloatToInt},	/* Currently Float */
  {"primDoubleRadix",	1, primFloatRadix},	/* Currently Float */
  {"primDoubleDigits",	1, primFloatDigits},	/* Currently Float */
  {"primDoubleRange",	1, primFloatRange},	/* Currently Float */
  {"primDoubleDecode",	1, primFloatDecode},	/* Currently Float */
  {"primDoubleEncode",	2, primFloatEncode},	/* Currently Float */
#endif

  {"primIntToChar",	1, primIntToChar},
  {"primCharToInt",	1, primCharToInt},
  {"primIntToFloat",	1, primIntToFloat},
  {"primIntToDouble",	1, primIntToFloat},	/* Currently Float */
  {"primDoubleToFloat", 1, primDummyCvt},	/* dummy	   */

  {"primEqInt",		2, primEqInt},
  {"primCmpInt",	3, primCmpInt},
  {"primEqChar",	2, primEqChar},
  {"primLeChar",	2, primLeChar},
  {"primEqFloat",	2, primEqFloat},
  {"primLeFloat",	2, primLeFloat},
  {"primEqDouble",	2, primEqFloat},	/* Currently Float */
  {"primLeDouble",	2, primLeFloat},	/* Currently Float */

#if GENERIC_CMP
  {"primCompare",	1, primCmp},
  {"primGenericEq",	2, primGenericEq},
  {"primGenericNe",	2, primGenericNe},
  {"primGenericGt",	2, primGenericGt},
  {"primGenericLe",	2, primGenericLe},
  {"primGenericGe",	2, primGenericGe},
  {"primGenericLt",	2, primGenericLt},
#endif

  {"primShowsInt",	3, primPrint},
  {"primShowsInteger",	3, primPrint},
  {"primShowsFloat",	3, primPrint},
  {"primShowsDouble",	3, primPrint},

  {"primStrict",	2, primStrict},

#if IO_DIALOGUE
  {"primInput",		1, primInput},
  {"primFopen",		3, primFopen},
#endif

#if IO_MONAD
  {"primSTRun",		1, primSTRun},
  {"primFst",		1, primFst},
  {"primSnd",		1, primSnd},
  {"primSTReturn",	1, primSTReturn},
  {"primIOBind",	3, primIOBind},
  {"primSTBind",	3, primSTBind},
  {"primSTInter",	2, primSTInter},
  {"primSTNew",		2, primSTNew},
  {"primSTAssign",	3, primSTAssign},
  {"primSTDeref",	2, primSTDeref},
  {"primSTMutVarEq",	2, primSTMutVarEq},
  {"primIOGetch",	1, primIOGetch},
  {"primIOPutchar",	2, primIOPutchar},
#if HASKELL_ARRAYS
  {"primSTNewArr",	4, primSTNewArr},
  {"primSTReadArr",	4, primSTReadArr},
  {"primSTWriteArr",	5, primSTWriteArr},
  {"primSTFreeze",	2, primSTFreeze},
#endif
#endif

  {0,			0, 0}
};

/* --------------------------------------------------------------------------
 * Primitive functions:
 * ------------------------------------------------------------------------*/

primFun(primFatbar) {			/* Fatbar primitive		   */
    Cell l    = primArg(2);		/* _FAIL [] r = r		   */
    Cell r    = primArg(1);		/* l     [] r = l  -- otherwise	   */
    Cell temp = evalWithNoError(l);
    if (nonNull(temp))
	if (temp==nameFail)
	    updateRoot(r);
	else {
	    updateRoot(temp);
	    cantReduce();
	}
    else
	updateRoot(l);
}

primFun(primFail) {		       /* Failure primitive		   */
    cantReduce();
}

primFun(primSel) {		       /* Component selection		   */
    Cell c = primArg(3);	       /* _sel c e n   return nth component*/
    Cell e = primArg(2);	       /*	       in expression e	   */
    Cell n = intOf(primArg(1));        /*	       built using cfun c  */

    eval(e);
    if (whnfHead==c &&	((isName(whnfHead) && name(whnfHead).arity==whnfArgs)
		      || (isTuple(whnfHead) && tupleOf(whnfHead)==whnfArgs)))
	updateRoot(pushed(n-1));
    else
	cantReduce();
}

primFun(primIf) {		       /* Conditional primitive 	   */
    eval(primArg(3));
    if (whnfHead==nameTrue)
	updateRoot(primArg(2));
    else
	updateRoot(primArg(1));
}

primFun(primStrict) {		       /* Strict application primitive	   */
    eval(primArg(1));		       /* evaluate 2nd argument 	   */
    updapRoot(primArg(2),primArg(1));  /* and apply 1st argument to result */
}

primFun(primTrace) {			/* an unsound trace primitive for  */
    fflush(stdout);			/* debugging purposes		   */
    eval(pop());			/*  :: String -> a -> a		   */
    while (whnfHead==nameCons) {
	eval(pop());
	putchar(charOf(whnfHead));
	eval(pop());
    }
    updateRoot(pop());
}

primFun(primMakeMem) {			/* construct member function	   */
    Int  di = intOf(primArg(2));	/* Assume that makeMember redexes  */
    Cell i  = primArg(1);		/* appear only in dictionary blocks*/
    List ds = name(i).type;		/* and need no further evaluation  */

    while (nonNull(tl(ds))) {		/* makeMember is only used when	   */
	i  = ap(i,makeDictFor(hd(ds),di));/* list of evidence is nonNull   */
	ds = tl(ds);
    }
    updapRoot(i,makeDictFor(hd(ds),di));
}

primFun(primConCmp) {			/* compare constructors		   */
    eval(primArg(3));			/*  :: a -> a -> Bool -> Bool	   */
    if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	Int l = name(whnfHead).number;
	eval(primArg(2));
	if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	    Int r = name(whnfHead).number;
	    updateRoot(l<r ? nameFalse : (l>r ? nameTrue : primArg(1)));
	    return;
	}
    }
    cantReduce();
}

primFun(primEnRange) {			/* derived range for enum type	   */
    eval(primArg(1));			/* :: (a,a) -> [a]		   */
    updapRoot(ap(nameEnFrTo,primArg(3)),primArg(2));
}

primFun(primEnIndex) {			/* derived indec for enum type	   */
    eval(primArg(2));			/*  :: (a,a) -> a -> Int	   */
    eval(primArg(4));			/* evaluate lower bound		   */
    if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	Int l = name(whnfHead).number;
	eval(primArg(3));		/* evaluate upper bound		   */
	if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	    Int h = name(whnfHead).number;
	    eval(primArg(1));		/* evaluate index		   */
	    if (l<=name(whnfHead).number && name(whnfHead).number<=h) {
		updateRoot(mkInt(name(whnfHead).number-l));
		return;
	    }
	}
    }
    cantReduce();
}

primFun(primEnInRng) {			/* derived inRange for enum type   */
    eval(primArg(2));			/*  :: (a,a) -> a -> Bool	   */
    eval(primArg(4));			/* evaluate lower bound		   */
    if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	Int l = name(whnfHead).number;
	eval(primArg(3));		/* evaluate upper bound		   */
	if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	    Int h = name(whnfHead).number;
	    eval(primArg(1));		/* evaluate index		   */
	    if (l<=name(whnfHead).number && name(whnfHead).number<=h)
		updateRoot(nameTrue);
	    else
		updateRoot(nameFalse);
	    return;
	}
    }
    cantReduce();
}

primFun(primEnFrom) {			/* derived enumFrom for enum type  */
    eval(primArg(1));			/* :: a -> [a] 			   */
    if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	Name cfs  = succCfun(whnfHead);
	Cell cont = isNull(cfs) ? nameNil : ap(nameEnFrom,cfs);
	updapRoot(ap(nameCons,whnfHead),cont);
    }
    else
	cantReduce();
}

primFun(primEnFrTo) {			/* derived enumFromTo for enum type*/
    eval(primArg(2));			/* :: a -> a -> [a]		   */
    if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	Name l = whnfHead;
	eval(primArg(1));
	if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	    if (name(l).number < name(whnfHead).number)
		updapRoot(ap(nameCons,l),
			  ap(ap(nameEnFrTo,succCfun(l)),whnfHead));
	    else if (l==whnfHead)
		updapRoot(ap(nameCons,l),nameNil);
	    else
		updateRoot(nameNil);
	    return;
	}
    }
    cantReduce();
}

primFun(primEnFrTh) {			/* derived enumFromThen for enum ty*/
   eval(primArg(2));			/* :: a -> a -> [a]		   */
   if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	Name f = whnfHead;
	eval(primArg(1));
	if (isName(whnfHead) && name(whnfHead).defn==CFUN) {
	    Name n    = nextCfun(f,whnfHead);
	    Cell cont = isNull(n) ? ap(ap(nameCons,whnfHead),nameNil)
				  : ap(ap(nameEnFrTh,whnfHead),n);
	    updapRoot(ap(nameCons,f),cont);
	    return;
	}
    }
    cantReduce();
}

/* --------------------------------------------------------------------------
 * Array primitives:
 * ------------------------------------------------------------------------*/

#if    HASKELL_ARRAYS
static Int local getSize(bounds,range)
Cell bounds, range; {
    Int lo;
    eval(bounds);			/* get components of bounds pair   */
    eval(ap(range,pop()));		/* get lower bound as an integer   */
    lo = whnfInt;
    eval(ap(range,pop()));		/* get upper bound as an integer   */
    whnfInt -= lo;
    return (whnfInt<0 ? 0 : whnfInt+1);
}

static List local addAssocs(r,s,as,vs)	/* add assocs in as to array	   */
Cell r;					/* list vs, using r for the range  */
Int  s;					/* and s for array size		   */
Cell as;
List vs; {
    for (;;) {				/* loop through assocs		   */
	eval(as);
	if (whnfHead==nameNil && whnfArgs==0)
	    break;
	else if (whnfHead==nameCons && whnfArgs==2) {
	    eval(pop());
	    /* at this point, the top of the stack looks like:
	     *
	     *      pushed(0) == index  (first component in assoc)
	     *      pushed(1) == value for assoc
	     *	    pushed(2) == rest of assocs
	     */
	    eval(ap(r,top()));
	    if (whnfInt<0 || whnfInt>=s)
		return UNIT;
	    else {
		List us = NIL;
		drop();
		for (us=vs; whnfInt>0; --whnfInt)
		    us = tl(us);
		hd(us) = (isNull(hd(us)) ? top() : nameEltUndef);
		drop();
		as = pop();
	    }
	}
	else
	    internal("runtime type error");
    }
    return vs;
}

static List local foldAssocs(r,s,f,as,vs)
Cell r;					/* fold assocs as into array list  */
Int  s;					/* vs using function f, with r for */
Cell f;					/* range and s for size		   */
Cell as;				/* bounds.			   */
List vs; {
    for (;;) {				/* loop through assocs		   */
	eval(as);
	if (whnfHead==nameNil && whnfArgs==0)
	    break;
	else if (whnfHead==nameCons && whnfArgs==2) {
	    eval(pop());
	    /* at this point, the top of the stack looks like:
	     *
	     *      pushed(0) == index  (first component in assoc)
	     *      pushed(1) == value for assoc
	     *	    pushed(2) == rest of assocs
	     */
	    eval(ap(r,top()));
	    if (whnfInt<0 || whnfInt>s)
		return UNIT;
	    else {
		List us = NIL;
		drop();
		for (us=vs; whnfInt>0; --whnfInt)
		    us = tl(us);
		hd(us) = ap(ap(f,hd(us)),pop());
		as = pop();
	    }
	}
	else
	    internal("runtime type error");
    }
    return vs;
}

primFun(primArray) {			/* Array creation		   */
    Cell range  = primArg(3);		/*  :: (a -> Int) -> 		   */
    Cell bounds = primArg(2);		/*	(a,a) ->		   */
    Cell assocs = primArg(1);		/*	 [Assoc a b] -> Array a b  */
    List vs     = NIL;
    List us	= NIL;
    Int  size;

    size = getSize(bounds,range);			/* check bounds	   */
    vs   = copy(size,NIL);				/* initialize elems*/
    vs   = addAssocs(range,size,assocs,vs);		/* process assocs  */
    if (vs==UNIT) {
	updapRoot(ap(nameOutBounds,bounds),top());
	cantReduce();
    }
    for (us=vs; nonNull(us); us=tl(us))			/* set undef elts  */
	if (isNull(hd(us)))
	    hd(us) = nameEltUndef;

    updapRoot(ARRAY,ap(bounds,vs));
}

primFun(primUpdate) {			/* Array update			   */
    Cell range  = primArg(3);		/*  :: (a -> Int) ->		   */
    Cell oldArr = primArg(2);		/*	Array a b ->		   */
    Cell assocs = primArg(1);		/*	 [Assoc a b] -> Array a b  */
    Cell bounds = NIL;
    Cell elems  = NIL;
    List vs     = NIL;
    List us	= NIL;
    Int  size;

    eval(oldArr);					/* find bounds	   */
    bounds = fst(snd(whnfHead));
    elems  = snd(snd(whnfHead));
    size   = getSize(bounds,range);
    vs     = copy(size,NIL);				/* initialize elems*/
    vs     = addAssocs(range,size,assocs,vs);		/* process assocs  */
    if (vs==UNIT) {
        updapRoot(ap(nameOutBounds,bounds),top());
        cantReduce();
    }
    for (us=vs; nonNull(us) && nonNull(elems); us=tl(us), elems=tl(elems))
	if (isNull(hd(us)))				/* undef values    */
	    hd(us) = hd(elems);				/* replaced by the */
							/* old array vals  */
    updapRoot(ARRAY,ap(bounds,vs));
}

primFun(primAccum) {			/* Array accum			   */
    Cell range  = primArg(4);		/*  :: (a -> Int) ->		   */
    Cell f      = primArg(3);		/*	(b -> c -> b) ->	   */
    Cell orig   = primArg(2);		/*	 Array a b ->		   */
    Cell assocs = primArg(1);		/*	  [Assoc a c] -> Array a b */
    Cell bounds = NIL;
    List vs     = NIL;
    Int  size;

    eval(orig);						/* find bounds	   */
    bounds = fst(snd(whnfHead));
    vs     = dupList(snd(snd(whnfHead)));		/* elements of orig*/
    size   = getSize(bounds,range);
    vs     = foldAssocs(range,size,f,assocs,vs);	/* process assocs  */
    if (vs==UNIT) {
        updapRoot(ap(nameOutBounds,bounds),top());
        cantReduce();
    }
    updapRoot(ARRAY,ap(bounds,vs));
}

primFun(primAccumArray) {		/* Array accumArray		   */
    Cell range  = primArg(5);		/*  :: (a -> Int) ->		   */
    Cell f      = primArg(4);		/*	(b -> c -> b) ->	   */
    Cell z      = primArg(3);		/*	 b ->			   */
    Cell bounds = primArg(2);		/*	  (a,a) ->		   */
    Cell assocs = primArg(1);		/*	   [Assoc a c] -> Array a b*/
    List vs     = NIL;
    Int  size;

    size = getSize(bounds,range);			/* check size	   */
    vs   = copy(size,z);				/* initialize elems*/
    vs   = foldAssocs(range,size,f,assocs,vs);		/* process assocs  */
    if (vs==UNIT) {
        updapRoot(ap(nameOutBounds,bounds),top());
        cantReduce();
    }
    updapRoot(ARRAY,ap(bounds,vs));
}

primFun(primAmap) {			/* map function over array	   */
    Cell f  = primArg(2);		/*  :: (b -> c) ->		   */
    Cell a  = primArg(1);		/*	Array a b -> Array a c	   */
    List us = NIL;
    List vs = NIL;

    eval(a);		
    a = whnfHead;
    for (us=snd(snd(a)); nonNull(us); us=tl(us))
	vs = cons(ap(f,hd(us)),vs);
    updapRoot(ARRAY,ap(fst(snd(a)),rev(vs)));
}

primFun(primSubscript) {		/* Subscript primitive		   */
    Int  index = 0;			/*  :: (a -> Int) ->		   */
    List vs = NIL;			/*	Array a b ->		   */
					/*	 a -> b			   */

    eval(ap(primArg(3),primArg(1)));	/* find required position	   */
    if ((index=whnfInt) < 0)
	cantReduce();
    eval(primArg(2));			/* evaluate array		   */
    if (whatIs(whnfHead)!=ARRAY)
	internal("primBounds");
    for (vs=snd(snd(whnfHead)); nonNull(vs) && index>0; vs=tl(vs))
	--index;
    if (isNull(vs))
	cantReduce();
    updateRoot(hd(vs));
}

primFun(primBounds) {			/* Bounds primitive		   */
    eval(primArg(1));			/*  :: Array a b -> (a,a)	   */
    if (whatIs(whnfHead)!=ARRAY)
	internal("primBounds");
    updateRoot(fst(snd(whnfHead)));
}

primFun(primElems) {			/* elems primitive		   */
    Cell vs = NIL;
    Cell us = NIL;
    eval(primArg(1));			/* evaluate array to whnf	   */
    if (whatIs(whnfHead)!=ARRAY)
	internal("primElems");
    for (us=snd(snd(whnfHead)); nonNull(us); us=tl(us))
	vs = ap(ap(nameCons,hd(us)),vs);
    updateRoot(revOnto(vs,nameNil));
}
#endif

/* --------------------------------------------------------------------------
 * Integer arithmetic primitives:
 * ------------------------------------------------------------------------*/

primFun(primMinInt) {			/* minimum integer CAF		   */
    push(mkInt((-MAXPOSINT)-1));
}

primFun(primMaxInt) {			/* maximum integer CAF		   */
    push(mkInt(MAXPOSINT));
}

primFun(primPlusInt) {		       /* Integer addition primitive	   */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x+whnfInt));
}

primFun(primMinusInt) { 	       /* Integer subtraction primitive    */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x-whnfInt));
}

primFun(primMulInt) {		       /* Integer multiplication primitive */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x*whnfInt));
}

primFun(primQrmInt) {			/* Integer quotient and remainder  */
    Int x;				/* truncated towards zero	   */
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    updapRoot(ap(nameCons,mkInt(x/whnfInt)),mkInt(x%whnfInt));
}

primFun(primQuotInt) {			/* Integer division primitive	   */
    Int x;				/* truncated towards zero	   */
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    updateRoot(mkInt(x/whnfInt));
}

primFun(primDivInt) {			/* Integer division primitive	   */
    Int x,r;				/* truncated towards -ve infinity  */
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    r = x%whnfInt;
    x = x/whnfInt;
    if ((whnfInt<0 && r>0) || (whnfInt>0 && r<0))
	x--;
    updateRoot(mkInt(x));
}

primFun(primModInt) {		       /* Integer modulo primitive	   */
    Int x,y;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    y = x%whnfInt;		       /* "... the modulo having the sign  */
    if ((y<0 && whnfInt>0) ||	       /*	       of the divisor ..." */
	(y>0 && whnfInt<0))	       /* See definition on p.91 of Haskell*/
	updateRoot(mkInt(y+whnfInt));  /* report...			   */
    else
	updateRoot(mkInt(y));
}

primFun(primRemInt) {		       /* Integer remainder primitive	   */
    Int x;
    eval(primArg(2));		       /* quot and rem satisfy:		   */
    x = whnfInt;		       /* (x `quot` y)*y + (x `rem` y) == x*/
    eval(primArg(1));		       /* which is exactly the property    */
    if (whnfInt==0)		       /* described in K&R 2:		   */
	cantReduce();		       /*      (a/b)*b + a%b == a	   */
    updateRoot(mkInt(x%whnfInt));
}

primFun(primNegInt) {		       /* Integer negation primitive	   */
    eval(primArg(1));
    updateRoot(mkInt(-whnfInt));
}

primFun(primEvenInt) {		       /* Integer even predicate	   */
    eval(primArg(1));
    updateRoot((whnfInt&1) ? nameFalse : nameTrue);
}

/* --------------------------------------------------------------------------
 * Haskell Integer (bignum) primitives:
 * ------------------------------------------------------------------------*/

#if BIGNUMS
#include "bignums.c"

primFun(primPlusInteger) {
    Bignum x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(bigAdd(x,whnfHead));
}

primFun(primMinusInteger) {
    Bignum x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(bigSub(x,whnfHead));
}

primFun(primMulInteger) {
    Bignum x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(bigMul(x,whnfHead));
}

primFun(primQrmInteger) {
    Bignum x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    x = bigQrm(x,whnfHead);
    if (isNull(x))
	cantReduce();
    else
	updateRoot(x);
}

primFun(primNegInteger) {
    eval(primArg(1));
    updateRoot(bigNeg(whnfHead));
}

primFun(primEvenInteger) {
    eval(primArg(1));
    updateRoot(bigEven(whnfHead) ? nameTrue : nameFalse);
}

primFun(primIntToInteger) {
    eval(primArg(1));
    updateRoot(bigInt(whnfInt));
}

primFun(primIntegerToInt) {
    eval(primArg(1));
    whnfHead = bigToInt(whnfHead);
    if (nonNull(whnfHead))
	updateRoot(whnfHead);
    else
	cantReduce();
}

primFun(primIntegerToFloat) {
    eval(primArg(1));
    updateRoot(bigToFloat(whnfHead));
}

primFun(primEqInteger) {
    Bignum x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(bigCmp(x,whnfHead)==0 ? nameTrue : nameFalse);
}

primFun(primCmpInteger) {
    Bignum x;
    eval(primArg(3));
    x = whnfHead;
    eval(primArg(2));
    switch (bigCmp(x,whnfHead)) {
	case (-1) : x = nameFalse;  break;
	case   0  : x = primArg(1); break;
	case   1  : x = nameTrue;   break;
    }
    updateRoot(x);
}
#endif

/* --------------------------------------------------------------------------
 * Coercion primitives:
 * ------------------------------------------------------------------------*/

primFun(primCharToInt) {	       /* Character to integer primitive   */
    eval(primArg(1));
    updateRoot(mkInt(charOf(whnfHead)));
}

primFun(primIntToChar) {	       /* Integer to character primitive   */
    eval(primArg(1));
    if (whnfInt<0  || whnfInt>MAXCHARVAL)
	cantReduce();
    updateRoot(mkChar(whnfInt));
}

primFun(primIntToFloat) {		/* Integer to Float primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat((Float)(whnfInt)));
}

primFun(primDummyCvt) {			/* dummy (identity) conversion	   */
    updateRoot(primArg(1));
}

/* --------------------------------------------------------------------------
 * Float arithmetic primitives:
 * ------------------------------------------------------------------------*/

primFun(primPlusFloat) {	       /* Float addition primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x+whnfFloat));
}

primFun(primMinusFloat) { 	       /* Float subtraction primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x-whnfFloat));
}

primFun(primMulFloat) {		       /* Float multiplication primitive   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x*whnfFloat));
}

primFun(primDivFloat) {		       /* Float division primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    if (whnfFloat==0)
	cantReduce();
    updateRoot(mkFloat(x/whnfFloat));
}

primFun(primNegFloat) {		       /* Float negation primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(-whnfFloat));
}

#if HAS_FLOATS
primFun(primPiFloat) {			/* Float pi primitive		   */
    push(mkFloat(3.1415926535));
}

primFun(primSinFloat) {			/* Float sin (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(sin(whnfFloat)));
}

primFun(primCosFloat) {			/* Float cos (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(cos(whnfFloat)));
}

primFun(primTanFloat) {			/* Float tan (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(tan(whnfFloat)));
}

primFun(primAsinFloat) {		/* Float arc sin (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(asin(whnfFloat)));
}

primFun(primAcosFloat) {		/* Float arc cos (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(acos(whnfFloat)));
}

primFun(primAtanFloat) {		/* Float arc tan (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(atan(whnfFloat)));
}

primFun(primAtan2Float) {		/* Float arc tan with quadrant info*/
    Float t;				/* 		 (trig) primitive  */
    eval(primArg(2));
    t = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(atan2(t,whnfFloat)));
}

primFun(primExpFloat) {			/* Float exponential primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(exp(whnfFloat)));
}

primFun(primLogFloat) {			/* Float logarithm primitive	   */
    eval(primArg(1));
    if (whnfFloat<=0)
	cantReduce();
    updateRoot(mkFloat(log(whnfFloat)));
}

primFun(primLog10Float) {		/* Float logarithm (base 10) prim  */
    eval(primArg(1));
    if (whnfFloat<=0)
	cantReduce();
    updateRoot(mkFloat(log10(whnfFloat)));
}

primFun(primSqrtFloat) {		/* Float square root primitive	   */
    eval(primArg(1));
    if (whnfFloat<0)
	cantReduce();
    updateRoot(mkFloat(sqrt(whnfFloat)));
}

primFun(primFloatToInt) {		/* Adhoc Float --> Int conversion  */
    eval(primArg(1));
    updateRoot(mkInt((Int)(whnfFloat)));
}

primFun(primFloatRadix) {		/* Float radix primitive	   */
#if BIGNUMS				/*  :: a -> Integer		   */
    updateRoot(bigInt(FLT_RADIX));	/* from K&R2, I hope it's portable */
#else
    updateRoot(mkInt(FLT_RADIX));
#endif
}

primFun(primFloatDigits) {		/* Float sig. digits primitive	   */
    updateRoot(mkInt(FLT_MANT_DIG));	/*  :: a -> Int			   */
}					/* again, courtesy K&R2		   */

primFun(primFloatRange) {		/* Float exponent range primitive  */
    updapRoot(ap(mkTuple(2),mkInt(FLT_MIN_EXP)),mkInt(FLT_MAX_EXP));
}

primFun(primFloatDecode) {		/* Float decode primitive	   */
    double f;				/*  :: Float -> (Integer,Int)	   */
    Int    n;				/* another gruesome hack	   */
    eval(primArg(1));
    f  = frexp((double)(whnfFloat),&n);	/* 0.5   <= f < 1		   */
    f  = ldexp(f,FLT_MANT_DIG);		/* 2^m-1 <= f < 2^m, m=FLT_MANT_DIG*/
    n -= FLT_MANT_DIG;
#if BIGNUMS
    updapRoot(ap(mkTuple(2),bigDouble(f)),mkInt(n));
#else
    updapRoot(ap(mkTuple(2),mkInt(((Int)f))),mkInt(n));
#endif
}

primFun(primFloatEncode) {		/* Float encode primitive	   */
    Int n;				/*  :: Integer -> Int -> a	   */
    Cell f;				/* Ugly hack, don't use Hugs for   */
    eval(primArg(1));			/* numerical work		   */
    n = whnfInt;
    eval(primArg(2));			/* get integer			   */
    f = bigToFloat(whnfHead);		/* and turn it into a float	   */
    updateRoot(mkFloat(ldexp(floatOf(f),n)));
}
#endif

/* --------------------------------------------------------------------------
 * Comparison primitives:
 * ------------------------------------------------------------------------*/

primFun(primEqInt) {		       /* Integer equality primitive	   */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(x==whnfInt ? nameTrue : nameFalse);
}

primFun(primCmpInt) {		       /* Integer compare primitive	   */
    Int x;
    eval(primArg(3));
    x = whnfInt;
    eval(primArg(2));
    updateRoot(x>whnfInt ? nameTrue  :
                          (x<whnfInt ? nameFalse : primArg(1)));
}

primFun(primEqChar) {		       /* Character equality primitive	   */
    Cell x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(x==whnfHead ? nameTrue : nameFalse);
}

primFun(primLeChar) {		       /* Character <= primitive	   */
    Cell x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(x<=whnfHead ? nameTrue : nameFalse);
}

primFun(primEqFloat) {		       /* Float equality primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(x==whnfFloat ? nameTrue : nameFalse);
}

primFun(primLeFloat) {		       /* Float <= primitive		   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(x<=whnfFloat ? nameTrue : nameFalse);
}

#if GENERIC_CMP
/* Generic comparisons implemented using the internal primitive function:
 *
 * primCmp []			= EQ
 *         ((C xs, D ys):rs)
 *	   | C < D		= LT
 *	   | C == D		= primCmp (zip xs ys ++ rs)
 *	   | C > D		= GT
 *	   ((Int n, Int m):rs)
 *	   | n < m		= LT
 *	   | n == m		= primCmp rs
 *	   | n > m		= GT
 *	   etc ... similar for comparison of characters:
 *
 * The list argument to primCmp is represented as an `internal list';
 * i.e. no (:)/[] constructors - use internal cons and NIL instead!
 *
 * To compare two values x and y, evaluate primCmp [(x,y)] and use result.
 */

#define LT            1
#define EQ            2
#define GT            3
#define compResult(x) updateRoot(mkInt(x))

static Name namePrimCmp;

primFun(primCmp) {			/* generic comparison function	   */
    Cell rs = primArg(1);

    if (isNull(rs)) {
	compResult(EQ);
	return;
    }
    else {
	Cell x = fst(hd(rs));
	Cell y = snd(hd(rs));
	Int  whnfArgs1;
	Cell whnfHead1;

	rs = tl(rs);
	eval(x);
	whnfArgs1 = whnfArgs;
	whnfHead1 = whnfHead;

	switch (whatIs(whnfHead1)) {
	    case INTCELL  : if (whnfArgs==0) {		/* compare ints    */
				eval(y);
				if (!isInt(whnfHead) || whnfArgs!=0)
				    break;
				if (intOf(whnfHead1) > whnfInt)
				    compResult(GT);
				else if (intOf(whnfHead1) < whnfInt)
				    compResult(LT);
				else
				    updapRoot(namePrimCmp,rs);
				return;
			    }
			    break;

	    case FLOATCELL: if (whnfArgs==0) {		/* compare floats  */
				eval(y);
				if (!isFloat(whnfHead) || whnfArgs!=0)
				    break;
				if (floatOf(whnfHead1) > whnfFloat)
				    compResult(GT);
				else if (floatOf(whnfHead1) < whnfFloat)
				    compResult(LT);
				else
				    updapRoot(namePrimCmp,rs);
				return;
			    }
			    break;

	    case CHARCELL : if (whnfArgs==0) {		/* compare chars   */
				eval(y);
				if (!isChar(whnfHead) || whnfArgs!=0)
				    break;
				if (charOf(whnfHead1) > charOf(whnfHead))
				    compResult(GT);
				else if (charOf(whnfHead1) < charOf(whnfHead))
				    compResult(LT);
				else
				    updapRoot(namePrimCmp,rs);
				return;
			    }
			    break;

#if HASKELL_ARRAYS
	    case ARRAY    : break;
#endif
#if IO_MONAD
	    case MUTVAR   : break;
#endif

	    default	  : eval(y);			/* compare structs */
			    if (whnfHead1==whnfHead &&
				whnfArgs1==whnfArgs &&
				(whnfHead==UNIT    ||
				 isTuple(whnfHead) ||
				 (isName(whnfHead) &&
				  name(whnfHead).defn==CFUN))) {
				while (whnfArgs1-- >0)
				    rs = cons(pair(pushed(whnfArgs+whnfArgs1),
						   pushed(whnfArgs1)),rs);
				updapRoot(namePrimCmp,rs);
				return;
			    }
			    if (isName(whnfHead1)	    &&
				 name(whnfHead1).defn==CFUN &&
				 isName(whnfHead)	    &&
				 name(whnfHead).defn==CFUN) {
				if (name(whnfHead1).number
						> name(whnfHead).number)
				    compResult(GT);
				else if (name(whnfHead1).number
						< name(whnfHead).number)
				    compResult(LT);
				else
				    break;
				return;
			    }
                            break;
	}
        /* we're going to fail because we can't compare x and y; modify    */
	/* the root expression so that it looks reasonable before failing  */
	/* i.e. output produced will be:  {_compare x y}		   */
	updapRoot(ap(namePrimCmp,x),y);
    }
    cantReduce();
}

primFun(primGenericEq) {		/* Generic equality test	   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt==EQ ? nameTrue : nameFalse);
}

primFun(primGenericLe) {		/* Generic <= test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt<=EQ ? nameTrue : nameFalse);
}

primFun(primGenericLt) {		/* Generic < test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt<EQ ? nameTrue : nameFalse);
}

primFun(primGenericGe) {		/* Generic >= test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt>=EQ ? nameTrue : nameFalse);
}

primFun(primGenericGt) {		/* Generic > test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt>EQ ? nameTrue : nameFalse);
}

primFun(primGenericNe) {		/* Generic /= test		   */
    Cell c = ap(namePrimCmp,singleton(pair(primArg(2),primArg(1))));
    eval(c);
    updateRoot(whnfInt!=EQ ? nameTrue : nameFalse);
}
#endif

/* --------------------------------------------------------------------------
 * Print primitives:
 * ------------------------------------------------------------------------*/

static Cell consOpen,	consSpace,  consComma,	consClose;
static Cell consObrace, consCbrace, consOsq,	consCsq;
static Cell consBack,	consMinus,  consQuote,  consDQuote;

static Name nameLPrint, nameNLPrint;	/* list printing primitives	   */
static Name nameSPrint, nameNSPrint;	/* string printing primitives	   */

#define print(pr,d,e,ss)    ap(ap(ap(pr,mkInt(d)),e),ss)
#define lprint(pr,xs,ss)    ap(ap(pr,xs),ss)
#define printString(s,ss)   revOnto(stringOutput(s,NIL),ss)
#define printSChar(c,ss)    printString(unlexChar(c,'\"'),ss)

primFun(primPrint) {			/* evaluate and print term	   */
    Int  d    = intOf(primArg(3));	/*    :: Int->Expr->[Char]->[Char] */
    Cell e    = primArg(2);
    Cell ss   = primArg(1);
    Cell temp = evalWithNoError(e);
    if (nonNull(temp))
	updateRoot(printBadRedex(temp,ss));
    else
	printer(root,namePrint,d,ss);
}

primFun(primNPrint) {			/* print term without evaluation   */
    Int    d	  = intOf(primArg(3)); /*     :: Int->Expr->[Char]->[Char] */
    Cell   e	  = primArg(2);
    Cell   ss	  = primArg(1);
    unwind(e);
    printer(root,nameNPrint,d,ss);
}

static Void local printer(root,pr,d,ss)	/* Main part: primPrint/primNPrint */
StackPtr root;				/* root or print redex		   */
Name	 pr;				/* printer to use on components	   */
Int	 d;				/* precedence level		   */
Cell	 ss; {				/* rest of output		   */
    Int  used	= 0;
    Cell output = NIL;

    switch(whatIs(whnfHead)) {

	case NAME     : {   Syntax sy = syntaxOf(name(whnfHead).text);

			    if (name(whnfHead).defn!=CFUN ||
				    name(whnfHead).arity>whnfArgs)
				pr = nameNPrint;

			    if (whnfHead==nameCons && whnfArgs==2) {/*list */
				if (pr==namePrint)
				    startList(root,ss);
				else
				    startNList(root,ss);
				return;
			    }
			    if (whnfArgs==1 && sy!=APPLIC) {	  /* (e1+) */
				used   = 1;
				output = ap(consClose,
					  textAsOp(name(whnfHead).text,
					   ap(consSpace,
					    print(pr,FUN_PREC-1,pushed(0),
					     ap(consOpen,NIL)))));
			    }
			    else if (whnfArgs>=2 && sy!=APPLIC) { /* e1+e2 */
				Syntax a = assocOf(sy);
				Int    p = precOf(sy);
				used     = 2;
				if (whnfArgs>2 || d>p)
				     output = ap(consOpen,output);
				output = print(pr,(a==RIGHT_ASS?p:1+p),
					      pushed(1),
					  ap(consSpace,
					   textAsOp(name(whnfHead).text,
					    ap(consSpace,
					     print(pr,(a==LEFT_ASS? p:1+p),
						  pushed(0),
					      output)))));
				if (whnfArgs>2 || d>p)
				    output = ap(consClose,output);
			    }
			    else				  /* f ... */
				output = textAsVar(name(whnfHead).text,NIL);
			}
			break;

#if BIGNUMS
	case NEGNUM   :
	case ZERONUM  :
	case POSNUM   : output = rev(bigOut(whnfHead,output,d>=FUN_PREC));
			pr     = nameNPrint;
			break;
#endif

	case INTCELL  : {   Int digit;

			    if (intOf(whnfHead)<0 && d>=FUN_PREC)
				output = ap(consClose,output);

			    do {
				digit = whnfInt%10;
				if (digit<0)
				    digit= (-digit);
				output = ap(consChar('0'+digit),output);
			    } while ((whnfInt/=10)!=0);

			    if (intOf(whnfHead)<0) {
				output = ap(consMinus,output);
				if (d>=FUN_PREC)
				    output = ap(consOpen,output);
			    }

			    output = rev(output);
			    pr	   = nameNPrint;
			}
			break;

	case UNIT     : output = ap(consClose,ap(consOpen,NIL));
			pr     = nameNPrint;
			break;

	case TUPLE    : {   Int  tn   = tupleOf(whnfHead);
                            Cell punc = consOpen;
			    Int  i;

			    used      = tn<whnfArgs ? tn : whnfArgs;
			    output    = NIL;
			    for (i=0; i<used; ++i) {
				output = print(pr,MIN_PREC,pushed(i),
					  ap(punc,
					   output));
				punc   = consComma;
			    }
			    for (; i<tn; ++i) {
				output = ap(punc,output);
				punc   = consComma;
			    }
			    output = ap(consClose,output);
			}
			pr = nameNPrint;
			break;

	case CHARCELL : output = ap(consQuote,
                                  stringOutput(unlexChar(charOf(whnfHead),
                                                         '\''),
				   ap(consQuote,
				    output)));
			pr     = nameNPrint;
			break;

	case FLOATCELL: if (whnfFloat<0.0 && d>=FUN_PREC)
			    output = ap(consOpen,output);
			output = stringOutput(floatToString(whnfFloat),output);
			if (whnfFloat<0.0 && d>=FUN_PREC)
			    output = ap(consClose,output);
			pr = nameNPrint;
			break;

#if HASKELL_ARRAYS
	case ARRAY    : output = stringOutput("{array}",output);
			pr     = nameNPrint;
			break;
#endif

#if IO_MONAD
	case MUTVAR   : output = stringOutput("{mutable variable}",output);
			pr     = nameNPrint;
			break;
#endif

        case DICTCELL : output = stringOutput("{dict}",output);
			pr     = nameNPrint;
			break;

	case FILECELL : output = stringOutput("{file}",output);
			pr     = nameNPrint;
			break;

	case INSTANCE : output = outputInst(whnfHead,output);
			pr     = nameNPrint;
			break;

	default       : internal("Error in graph");
			break;
    }

    if (used<whnfArgs) {		/* Add remaining args to output	   */
	do
	    output = print(pr,FUN_PREC,pushed(used),ap(consSpace,output));
	while (++used<whnfArgs);

	if (d>=FUN_PREC) {		/* Determine if parens are needed  */
	    updapRoot(consOpen,revOnto(output,ap(consClose,ss)));
	    return;
	}
    }

    updateRoot(revOnto(output,ss));
}

/* --------------------------------------------------------------------------
 * List printing primitives:
 * ------------------------------------------------------------------------*/

static Void local startList(root,ss)	/* start printing evaluated list   */
StackPtr root;
Cell     ss; {
    Cell x    = pushed(0);
    Cell xs   = pushed(1);
    Cell temp = evalWithNoError(x);
    if (nonNull(temp))
	updapRoot(consOsq,
		   printBadRedex(temp,
		    lprint(nameLPrint,xs,ss)));
    else if (isChar(whnfHead) && whnfArgs==0)
	updapRoot(consDQuote,
		   printSChar(charOf(whnfHead),
		    lprint(nameSPrint,xs,ss)));
    else
	updapRoot(consOsq,
		   print(namePrint,MIN_PREC,x,
		    lprint(nameLPrint,xs,ss)));
}

static Void local startNList(root,ss)	/* start printing unevaluated list */
StackPtr root;
Cell     ss; {
    Cell x    = pushed(0);
    Cell xs   = pushed(1);
    unwind(x);
    if (isChar(whnfHead) && whnfArgs==0)
	updapRoot(consDQuote,
		   printSChar(charOf(whnfHead),
		    lprint(nameNSPrint,xs,ss)));
    else
	updapRoot(consOsq,
		   print(nameNPrint,MIN_PREC,x,
		    lprint(nameNLPrint,xs,ss)));
}

primFun(primLPrint) {			/* evaluate and print list	   */
    Cell e    = primArg(2);
    Cell ss   = primArg(1);
    Cell temp = evalWithNoError(e);

    if (nonNull(temp))
	updateRoot(printString("] ++ ",printBadRedex(temp,ss)));
    else if (whnfHead==nameCons && whnfArgs==2)
	updapRoot(consComma,
		   ap(consSpace,
		    print(namePrint,MIN_PREC,pushed(0),
		     lprint(nameLPrint,pushed(1),ss))));
    else if (whnfHead==nameNil && whnfArgs==0)
	updapRoot(consCsq,ss);
    else
	updateRoot(printString("] ++ ",printBadRedex(e,ss)));
}

primFun(primNLPrint) {			/* print list without evaluation   */
    Cell e  = primArg(2);
    Cell ss = primArg(1);
    unwind(e);
    if (whnfHead==nameCons && whnfArgs==2)
	updapRoot(consComma,
		   ap(consSpace,
		    print(nameNPrint,MIN_PREC,pushed(0),
		     lprint(nameNLPrint,pushed(1),ss))));
    else if (whnfHead==nameNil && whnfArgs==0)
	updapRoot(consCsq,ss);
    else
	updateRoot(printString("] ++ ",print(nameNPrint,FUN_PREC-1,e,ss)));
}

primFun(primSPrint) {			/* evaluate and print string	   */
    Cell e    = primArg(2);
    Cell ss   = primArg(1);
    Cell temp = evalWithNoError(e);

    if (nonNull(temp))
	updateRoot(printString("\" ++ ",printBadRedex(temp,ss)));
    else if (whnfHead==nameCons && whnfArgs==2) {
	Cell x  = pushed(0);
	Cell xs = pushed(1);
	temp    = evalWithNoError(x);
	if (nonNull(temp))
	    updateRoot(printString("\" ++ [",
			printBadRedex(temp,
			 lprint(nameLPrint,xs,ss))));
	else if (isChar(whnfHead) && whnfArgs==0)
	    updateRoot(printSChar(charOf(whnfHead),
		        lprint(nameSPrint,xs,ss)));
	else
	    updateRoot(printString("\" ++ [",
			printBadRedex(x,
			 lprint(nameLPrint,xs,ss))));
    }
    else if (whnfHead==nameNil && whnfArgs==0)
	updapRoot(consDQuote,ss);
    else
	updateRoot(printString("\" ++ ",printBadRedex(e,ss)));
}

primFun(primNSPrint) {			/* print string without eval	   */
    Cell e  = primArg(2);
    Cell ss = primArg(1);
    unwind(e);
    if (whnfHead==nameCons && whnfArgs==2) {
	Cell x  = pushed(0);
	Cell xs = pushed(1);
	unwind(x);
	if (isChar(whnfHead) && whnfArgs==0)
	    updateRoot(printSChar(charOf(whnfHead),
		        lprint(nameNSPrint,xs,ss)));
	else
	    updateRoot(printString("\" ++ [",
			print(nameNPrint,MIN_PREC,x,
			 lprint(nameNLPrint,xs,ss))));
    }
    else if (whnfHead==nameNil && whnfArgs==0)
	updapRoot(consDQuote,ss);
    else
	updateRoot(printString("\" ++ ",print(nameNPrint,FUN_PREC-1,e,ss)));
}

/* --------------------------------------------------------------------------
 * Auxiliary functions for printer(s):
 * ------------------------------------------------------------------------*/

static Cell local textAsVar(t,ss)	/* reverse t as function symbol	   */
Text t;					/* onto output ss		   */
Cell ss; {
    String s = textToStr(t);
    if ((isascii(s[0]) && isalpha(s[0])) || s[0]=='_' || strcmp(s,"[]")==0)
	return stringOutput(s,ss);
    else
	return ap(consClose,stringOutput(s,ap(consOpen,ss)));
}

static Cell local textAsOp(t,ss)	/* reverse t as op. symbol onto ss */
Text t;
Cell ss; {
    String s = textToStr(t);
    if (isascii(s[0]) && isalpha(s[0]))
	return ap(consBack,stringOutput(s,ap(consBack,ss)));
    else
	return stringOutput(s,ss);
}

static Cell local stringOutput(s,ss)	/* reverse string s onto output ss */
String s;
Cell   ss; {
    while (*s)
	ss = ap(consChar(*s++),ss);
    return ss;
}

static Cell local printBadRedex(rx,rs)	/* Produce expression to print bad */
Cell rx, rs; {				/* redex and then print rest ...   */
    return ap(consObrace,
	    print(nameNPrint,MIN_PREC,rx,
	     ap(consCbrace,
	      rs)));
}

static Cell local printDBadRedex(rx,rs) /* Produce expression for bad redex*/
Cell rx, rs; {				/* within a Dialogue, with special */
    if (isAp(rx) && fun(rx)==nameError) /* handling of {error str} redexes */
	return arg(rx);
    else
	return printBadRedex(rx,rs);
}

Void abandon(what,rx)			/* abandon computation		   */
String what;
Cell   rx; {
    outputString(errorStream,
		 revOnto(stringOutput("\n",NIL),
		 revOnto(stringOutput(what,NIL),
		 revOnto(stringOutput(" error: ",NIL),
			 printDBadRedex(rx,nameNil)))));
    errAbort();
}

static Cell local outputInst(in,out)	/* produce string representation   */
Inst in;				/* of instance			   */
List out; {
    out = ap(consMinus,stringOutput(textToStr(class(inst(in).c).text),out));
    switch (whatIs(inst(in).t)) {
	case LIST  : return stringOutput("[]",out);
	case UNIT  : return stringOutput("()",out);
	case TUPLE : {   Int n = tupleOf(inst(in).t);
			 for (out=ap(consOpen,out); n>0; --n)
			     out = ap(consComma,out);
			 return ap(consClose,out);
		     }
	case ARROW : return stringOutput("(->)",out);
	case TYCON : return stringOutput(textToStr(tycon(inst(in).t).text),
					 out);
    }
    return stringOutput("???",out);
}

/* --------------------------------------------------------------------------
 * Evaluate name, obtaining a C string from a Hugs string:
 * ------------------------------------------------------------------------*/

#if IO_DIALOGUE
static String local evalName(es)	/* evaluate es :: [Char] and save  */
Cell es; {				/* in char array... return ptr to  */
    static char buffer[FILENAME_MAX+1];	/* string or 0, if error occurs	   */
    Int         pos    = 0;
    StackPtr    saveSp = sp;

    while (isNull(evalWithNoError(es)))
	if (whnfHead==nameCons && whnfArgs==2) {
	    Cell e = pop();		/* avoid leaving anything on stack */
	    es	   = pop();
	    if (isNull(evalWithNoError(e))
			&& isChar(whnfHead) && whnfArgs==0
			&& pos<FILENAME_MAX)
		buffer[pos++] = charOf(whnfHead);
	    else
		break;
	}
	else if (whnfHead==nameNil && whnfArgs==0) {
	    buffer[pos] = '\0';
	    return buffer;
	}
	else
	    break;

    sp = saveSp;			/* stack pointer must be the same  */
    return 0;				/* as it was on entry		   */
}
#endif

/* --------------------------------------------------------------------------
 * Dialogue based input/output:
 *
 * N.B. take care when modifying this code - it is rather delicate and even
 * the simplest of changes might create a nasty space leak... you have been
 * warned (please let me know if you think there already is a space leak!).
 * ------------------------------------------------------------------------*/

#if IO_DIALOGUE
static Name nameInput;			/* For reading from stdin	   */
static Bool echoChanged;		/* TRUE => echo changed in dialogue*/
static Bool stdinUsed;			/* TRUE => ReadChan stdin has been */
					/*	   seen in dialogue	   */
static FILE *writingFile = 0;		/* points to file open for writing */

Void dialogue(prog)			/* carry out dialogue ...	   */
Cell prog; {				/* :: Dialog=[Response]->[Request] */
    static String ioerr = "Attempt to read response before request complete";
    Cell tooStrict      = mkStr(findText(ioerr));
    Cell resps		= prog = ap(prog,NIL);
    Cell temp;

    echoChanged = FALSE;
    stdinUsed   = FALSE;
    for (;;) {				/* Keep Responding to Requests	   */
	resps = snd(resps) = ap(nameError,tooStrict);
        clearStack();
	if (nonNull(temp=evalWithNoError(prog)))
	    abandonDialogue(temp);
	else if (whnfHead==nameCons && whnfArgs==2) {
	    if (nonNull(temp=evalWithNoError(pushed(0))))
		abandonDialogue(temp);

	    prog = pushed(1+whnfArgs);

	    if (whnfHead==nameReadFile && whnfArgs==1)
		fst(resps) = ap(nameCons,readFile());
	    else if (whnfHead==nameWriteFile && whnfArgs==2)
		fst(resps) = ap(nameCons,writeFile());
	    else if (whnfHead==nameAppendFile && whnfArgs==2)
		fst(resps) = ap(nameCons,appendFile());
	    else if (whnfHead==nameReadChan && whnfArgs==1)
		fst(resps) = ap(nameCons,readChan());
	    else if (whnfHead==nameAppendChan && whnfArgs==2)
		fst(resps) = ap(nameCons,appendChan());
	    else if (whnfHead==nameEcho && whnfArgs==1)
		fst(resps) = ap(nameCons,echo());
	    else if (whnfHead==nameGetArgs && whnfArgs==0)
		fst(resps) = ap(nameCons,getCLArgs());
	    else if (whnfHead==nameGetProgName && whnfArgs==0)
		fst(resps) = ap(nameCons,getProgName());
	    else if (whnfHead==nameGetEnv && whnfArgs==1)
		fst(resps) = ap(nameCons,getEnv());
	    else
		abandonDialogue(pushed(whnfArgs));
	}
	else if (whnfHead==nameNil && whnfArgs==0) {
	    normalTerminal();
	    return;
	}
	else
	    internal("Type error during Dialogue");
    }
}

static Void local abandonDialogue(rx)	/* abandon dialogue after failure  */
Cell rx; {				/* to reduce redex rx		   */
    abandon("Dialogue",rx);
}

static Cell local readFile() {		/* repond to ReadFile request	   */
    String s    = evalName(pushed(0));	/* pushed(0) = file name string	   */
    Cell   temp = NIL;			/* pushed(1) = ReadFile request	   */
					/* pushed(2) = rest of program	   */

    if (!s)				/* problem with filename?	   */
	abandonDialogue(pushed(1));
    if (access(s,0)!=0)			/* can't find file		   */ 
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
    if (isNull(temp = openFile(s)))	/* can't open file		   */
	return ap(nameFailure,ap(nameReadError,pushed(0)));
    return ap(nameStr,temp);		/* otherwise we got a file!	   */
}

static Cell local writeFile() {		/* respond to WriteFile req.	   */
    String s    = evalName(pushed(0));	/* pushed(0) = file name string	   */
    FILE   *fp;				/* pushed(1) = output string	   */
    Cell   temp;			/* pushed(2) = output request	   */
					/* pushed(3) = rest of program	   */

    if (!s)				/* problem with filename?          */
        abandonDialogue(pushed(2));
    pushed(2) = NIL;			/* eliminate space leak!	   */
    if ((fp=fopen(s,FOPEN_WRITE))==0)	/* problem with output file?	   */
	return ap(nameFailure,ap(nameWriteError,pushed(0)));
    drop();
    temp = outputDString(writingFile = fp);
    fclose(fp);
    writingFile = 0;
    if (nonNull(temp))
	return ap(nameFailure,ap(nameWriteError,temp));
    else
	return nameSuccess;
}

static Cell local appendFile() {	/* respond to AppendFile req.	   */
    String s    = evalName(pushed(0));	/* pushed(0) = file name string	   */
    FILE   *fp;				/* pushed(1) = output string	   */
    Cell   temp;			/* pushed(2) = output request	   */
					/* pushed(3) = rest of program	   */

    if (!s)				/* problem with filename?          */
        abandonDialogue(pushed(2));
    pushed(2) = NIL;			/* eliminate space leak!	   */
    if (access(s,0)!=0)			/* can't find file?		   */
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
    if ((fp=fopen(s,FOPEN_APPEND))==0)	/* problem with output file?	   */
	return ap(nameFailure,ap(nameWriteError,pushed(0)));
    drop();
    temp = outputDString(writingFile = fp);
    fclose(fp);
    writingFile = 0;
    if (nonNull(temp))
	return ap(nameFailure,ap(nameWriteError,temp));
    else
	return nameSuccess;
}

static Cell local readChan() {		/* respond to readChan req.	   */
    String s    = evalName(pushed(0));	/* pushed(0) = channel name string */
					/* pushed(1) = output request	   */
					/* pushed(2) = rest of program	   */

    if (!s)				/* problem with filename?	   */
	abandonDialogue(pushed(1));
    if (strcmp(s,"stdin")!=0)		/* only valid channel == stdin	   */
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
    if (stdinUsed)			/* can't reuse stdin channel!      */
	return ap(nameFailure,ap(nameReadError,pushed(0)));
    stdinUsed = TRUE;
    return ap(nameStr,ap(nameInput,UNIT));
}

static Cell local appendChan() {	/* respond to AppendChannel req.   */
    String s = evalName(pushed(0));	/* pushed(0) = channel name string */
    FILE   *fp;				/* pushed(1) = output string	   */
    Cell   temp;			/* pushed(2) = output request	   */
					/* pushed(3) = rest of program	   */
    if (!s)				/* problem with filename?          */
        abandonDialogue(pushed(2));
    pushed(2) = NIL;			/* eliminate space leak!	   */
    if ((fp = validOutChannel(s))==0)	/* problem with output channel?	   */
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
    drop();
    if (nonNull(temp=outputDString(fp)))
	return ap(nameFailure,ap(nameWriteError,temp));
    else
	return nameSuccess;
}

static FILE *local validOutChannel(s)	/* return FILE * for valid output  */
String s; {				/* channel name or 0 otherwise...  */
    if (strcmp(s,"stdout")==0)
	return stdout;
    if (strcmp(s,"stderr")==0)
	return stderr;
    if (strcmp(s,"stdecho")==0)		/* in Hugs, stdecho==stdout	   */
	return stdout;
    return 0;
}

static Cell local echo() {		/* respond to Echo request	   */
    					/* pushed(0) = boolean echo status */
					/* pushed(1) = echo request	   */
					/* pushed(2) = rest of program	   */
    static String inUse  = "stdin already in use";
    static String repeat = "repeated Echo request";

    if (isNull(evalWithNoError(pushed(0)))) {
	if (stdinUsed)
	    return ap(nameFailure,ap(nameOtherError,mkStr(findText(inUse))));
	if (echoChanged)
	    return ap(nameFailure,ap(nameOtherError,mkStr(findText(repeat))));
	if (whnfHead==nameFalse && whnfArgs==0) {
	    echoChanged = TRUE;
	    noechoTerminal();
	    return nameSuccess;
	}
	if (whnfHead==nameTrue && whnfArgs==0) {
	    echoChanged = TRUE;
	    return nameSuccess;
	}
    }
    abandonDialogue(pushed(1));
    return NIL;/*NOTREACHED*/
}

static Cell local getCLArgs() {		/* get command args -- always []   */
    return ap(nameStrList,nameNil);
}

static Cell local getProgName() {	/* get program name -- an error!   */
    return ap(nameFailure,ap(nameOtherError,nameNil));
}

static Cell local getEnv() {		/* get environment variable	   */
    String s = evalName(pushed(0));	/* pushed(0) = variable name string*/
    String r = 0;			/* pushed(1) = output request	   */
					/* pushed(2) = rest of program	   */
    if (!s)
        abandonDialogue(pushed(1));
    if (r=getenv(s))
	return ap(nameStr,revOnto(stringOutput(r,NIL),nameNil));
    else
	return ap(nameFailure,ap(nameSearchError,pushed(0)));
}

primFun(primInput) {			/* read single character from stdin*/
    Int c = readTerminalChar();

    if (c==EOF || c<0 || c>=NUM_CHARS) {
	clearerr(stdin);
	updateRoot(nameNil);
    }
    else
	updapRoot(consChar(c),ap(nameInput,UNIT));
}

primFun(primFopen) {			/* open file for reading as str	   */
    Cell   succ = primArg(1);		/*  :: String->a->(String->a)->a   */
    Cell   fail = primArg(2);
    String s    = evalName(primArg(3));

    if (s){
	Cell file = openFile(s);
	if (nonNull(file)) {
	    updapRoot(succ,file);
	    return;
	}
    }
    updateRoot(fail);
}

static Cell local outputDString(fp)	/* Evaluate string cs and print	   */
FILE *fp; {				/* on specified output stream fp   */
    Cell temp = NIL;
    for (;;) {				/* keep reducing and printing head */
	temp = evalWithNoError(pop());	/* character			   */
	if (nonNull(temp))
	    return printDBadRedex(temp,nameNil);
	else if (whnfHead==nameCons && whnfArgs==2) {
	    if (nonNull(temp=evalWithNoError(pop())))
		return printDBadRedex(temp,top());
	    else if (isChar(whnfHead) && whnfArgs==0) {
		fputc(charOf(whnfHead),fp);
		if (!writingFile)
		    fflush(fp);
	    }
	    else
		break;
	}
	else if (whnfHead==nameNil && whnfArgs==0) {
	    if (writingFile)
		fflush(fp);
	    return NIL;
	}
	else
	    break;
    }
    internal("runtime type error");
    return nameNil;/*NOTREACHED*/
}
#endif

/* --------------------------------------------------------------------------
 * Top-level printing mechanism:
 * ------------------------------------------------------------------------*/

Cell outputString(fp,cs)		/* Evaluate string cs and print	   */
FILE *fp;				/* on specified output stream fp   */
Cell cs; {
    Cell temp;

    for (;;) {				/* keep reducing and printing head */
	clearStack();			/* character			   */
	temp = evalWithNoError(cs);
	if (nonNull(temp))
	    cs = printBadRedex(temp,nameNil);
	else if (whnfHead==nameCons && whnfArgs==2) {
	    Cell c = pushed(0);
	    cs     = pushed(1);

	    if (nonNull(temp=evalWithNoError(c)))
		cs = printBadRedex(temp,cs);
	    else if (isChar(whnfHead) && whnfArgs==0) {
		fputc(charOf(whnfHead),fp);
		    fflush(fp);
	    }
	    else
		break;
	}
	else if (whnfHead==nameNil && whnfArgs==0)
	    return NIL;
	else
	    break;
    }
    internal("runtime type error");
    return nameNil;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * IO monad implementation,  based on `Lazy State Threads' by Launchbury and
 * Peyton Jones, PLDI 94.
 *
 * type ST s a = State s -> (a, State s)
 * ------------------------------------------------------------------------*/

#if IO_MONAD
Void ioExecute(prog)			/* execute IO monad program of type*/
Cell prog; {				/* IO ()			   */
    Cell temp;
    noechoTerminal();
    if (nonNull(temp=evalWithNoError(ap(prog,UNIT))) ||
        nonNull(temp=evalWithNoError(pushed(1))))
	abandon("Program execution",temp);
}

primFun(primSTRun) {			/* ST monad encapsulate		   */
    updapRoot(nameFst,			/*  :: all s.(ST s a) -> a	   */
	      ap(primArg(1),UNIT));
}

primFun(primFst) {			/* fst primitive		   */
    eval(primArg(1));			/*  :: (a,s) -> a		   */
    updateRoot(top());
}

primFun(primSnd) {			/* snd primitive		   */
    eval(primArg(1));			/*  :: (a,s) -> s		   */
    updateRoot(pushed(1));
}

primFun(primSTReturn) {			/* ST monad return		   */
    updapRoot(mkTuple(2),primArg(1));	/* return    :: a -> ST s a	   */
}					/* return a   = \s -> (a,s)	   */

primFun(primIOBind) {			/* IO monad bind		   */
    Cell m = primArg(3);		/* :: ST s a ->			   */
    Cell f = primArg(2);		/*     (a -> ST s b) ->		   */
    Cell s = primArg(1);		/*	ST s b			   */
    eval(ap(m,s));
    updapRoot(ap(f,top()),pushed(1));	/* A strict bind operation on ST   */
}

primFun(primSTBind) {			/* ST monad bind		   */
    Cell m = primArg(3);		/* :: ST s a ->			   */
    Cell f = primArg(2);		/*     (a -> ST s b) ->		   */
    Cell s = primArg(1);		/*	ST s b			   */
    Cell r = ap(m,s);			/* lazy version of bind on ST	   */
    updapRoot(ap(f,ap(nameFst,r)),ap(nameSnd,r));
}

primFun(primSTInter) {			/* ST monad interleave		   */
    Cell m = primArg(2);		/*  :: ST s a ->		   */
    Cell s = primArg(1);		/*      ST s a			   */
    updapRoot(ap(mkTuple(2),ap(nameFst,ap(m,s))),s);
}

primFun(primSTNew) {			/* ST monad variable allocator	   */
    Cell i = primArg(2);		/*  :: a ->			   */
    Cell s = primArg(1);		/* 	ST s (MutVar s a)	   */
    eval(s);				/* force evaluation of state	   */
    updapRoot(ap(mkTuple(2),ap(MUTVAR,i)),s);
}

primFun(primSTAssign) {			/* ST monad assignment		   */
    Cell v = primArg(3);		/*  :: MutVar s a ->		   */
    Cell e = primArg(2);		/*	a ->			   */
    Cell s = primArg(1);		/*	 ST s ()		   */
    eval(s);				/* force evaluation of state	   */
    eval(v);
    if (!isPair(whnfHead) || fst(whnfHead)!=MUTVAR)
	internal("type error in assign");
    snd(whnfHead) = e;			/* Arrgh! impurity! :-)		   */
    updapRoot(ap(mkTuple(2),UNIT),s);
}

primFun(primSTDeref) {			/* ST monad dereference		   */
    Cell v = primArg(2);		/*  :: MutVar s a ->		   */
    Cell s = primArg(1);		/*	ST s a			   */
    eval(s);				/* force evaluation of state	   */
    eval(v);
    if (!isPair(whnfHead) || fst(whnfHead)!=MUTVAR)
	internal("type error in deref");
    updapRoot(ap(mkTuple(2),snd(whnfHead)),s);
}

primFun(primSTMutVarEq) {		/* ST monad variable equality	   */
    Cell x = primArg(2);		/*  :: MutVar s a -> 		   */
    Cell y = primArg(1);		/*      MutVar s a -> Bool	   */
    eval(x);
    x = whnfHead;
    eval(y);
    updateRoot(x==whnfHead ? nameTrue : nameFalse);
}

primFun(primIOGetch) {			/* get character from stdin	   */
    Cell s = primArg(1);		/*  :: IO Char			   */
    eval(s);
    updapRoot(ap(mkTuple(2),mkChar(readTerminalChar())),s);
}

primFun(primIOPutchar) {		/* print character on stdout	   */
    Cell c = primArg(2);		/*  :: Char ->			   */
    Cell s = primArg(1);		/*	IO ()			   */
    eval(s);
    eval(c);
    putchar(charOf(whnfHead));
    fflush(stdout);
    updapRoot(ap(mkTuple(2),UNIT),s);
}

#if HASKELL_ARRAYS
primFun(primSTNewArr) {			/* allocate mutable array	   */
    Cell range  = primArg(4);		/*  :: (a -> Int) ->		   */
    Cell bounds = primArg(3);		/*      (a,a) ->		   */
    Cell z	= primArg(2);		/*	 b ->			   */
    Cell s	= primArg(1);		/*	  ST s (MutArr s a b)	   */
    Int  size;
    eval(s);
    size = getSize(bounds,range);
    updapRoot(ap(mkTuple(2), ap(ARRAY, ap(bounds,copy(size,z)))), s);
}

primFun(primSTReadArr) {		/* read element in mutable array   */
    Cell index = primArg(4);		/*  :: ((a,a) -> a -> Int) ->	   */
    Cell a     = primArg(3);		/*	MutArr s a b ->		   */
    Cell i     = primArg(2);		/*       a ->			   */
    Cell s     = primArg(1);		/*	  ST s b		   */
    Cell vs    = NIL;
    eval(s);
    eval(a);
    vs = snd(whnfHead);
    eval(ap(ap(index,fst(vs)),i));
    while (whnfInt-- > 0)
	vs = snd(vs);
    updapRoot(ap(mkTuple(2),fst(snd(vs))),s);
}

primFun(primSTWriteArr) {		/* write element in mutable array  */
    Cell index = primArg(5);		/*  :: ((a,a) -> a -> Int) ->	   */
    Cell a     = primArg(4);		/*	MutArr s a b ->		   */
    Cell i     = primArg(3);		/*       a ->			   */
    Cell v     = primArg(2);		/*	  b ->			   */
    Cell s     = primArg(1);		/*	   ST s ()		   */
    Cell vs    = NIL;
    eval(s);
    eval(a);
    vs = snd(whnfHead);
    eval(ap(ap(index,fst(vs)),i));
    while (whnfInt-- > 0)
	vs = snd(vs);
    fst(snd(vs)) = v;
    updapRoot(ap(mkTuple(2),UNIT),s);
}

primFun(primSTFreeze) {			/* freeze mutable array		   */
    Cell arr = primArg(2);		/*  :: MutArr s a b ->		   */
    Cell s   = primArg(1);		/*	ST s (Array a b)	   */
    eval(s);
    eval(arr);
    updapRoot(ap(mkTuple(2),ap(ARRAY,dupList(snd(whnfHead)))),s);
}
#endif
#endif

/* --------------------------------------------------------------------------
 * Build array of character conses:
 * ------------------------------------------------------------------------*/

static Cell consCharArray[NUM_CHARS];

Cell consChar(c)			/* return application (:) c	   */
Char c; {
    if (c<0)
	c += NUM_CHARS;
    return consCharArray[c];
}

/* --------------------------------------------------------------------------
 * Built-in control:
 * ------------------------------------------------------------------------*/

Void builtIn(what)
Int what; {
    Int i;

    switch (what) {
#if IO_DIALOGUE
	case RESET   : if (writingFile) {
			   fclose(writingFile);
			   writingFile = 0;
		       }
		       break;
#endif

	case MARK    : for (i=0; i<NUM_CHARS; ++i)
			   mark(consCharArray[i]);
		       break;

	case INSTALL : for (i=0; i<NUM_CHARS; ++i)
			   consCharArray[i] = ap(nameCons,mkChar(i));

		       consOpen       = consCharArray['('];
		       consSpace      = consCharArray[' '];
		       consComma      = consCharArray[','];
		       consClose      = consCharArray[')'];
		       consObrace     = consCharArray['{'];
		       consCbrace     = consCharArray['}'];
		       consOsq	      = consCharArray['['];
		       consCsq	      = consCharArray[']'];
		       consBack       = consCharArray['`'];
		       consMinus      = consCharArray['-'];
		       consQuote      = consCharArray['\''];
		       consDQuote     = consCharArray['\"'];

#define pFun(n,s,t)    addPrim(0,n=newName(findText(s)),t,NIL)
		       pFun(nameFatbar,	   "_FATBAR", "primFatbar");
		       pFun(nameFail,	   "_FAIL",   "primFail");
		       pFun(nameIf,	   "_IF",     "primIf");
		       pFun(nameSel,	   "_SEL",    "primSel");

#if GENERIC_CMP
		       pFun(namePrimCmp,   "_compare", "primCompare");
#endif
		       pFun(namePrint,	   "_print",   "primPrint");
		       pFun(nameNPrint,	   "_nprint",  "primNprint");
		       pFun(nameLPrint,	   "_lprint",  "primLprint");
		       pFun(nameNLPrint,   "_nlprint", "primNlprint");
		       pFun(nameSPrint,	   "_sprint",  "primSprint");
		       pFun(nameNSPrint,   "_nsprint", "primNsprint");

		       pFun(nameConCmp,	   "_concmp",  "primConCmp");
		       pFun(nameEnRange,   "_range",   "primEnRange");
		       pFun(nameEnIndex,   "_index",   "primEnIndex");
		       pFun(nameEnInRng,   "_inRange", "primEnInRng");
		       pFun(nameEnFrom,    "_From",    "primEnFrom");
		       pFun(nameEnFrTo,	   "_FromTo",  "primEnFrTo");
		       pFun(nameEnFrTh,	   "_FromThen","primEnFrTh");

#if IO_DIALOGUE
		       pFun(nameInput,	   "_input",   "primInput");
#endif
		       pFun(nameUndefMem,  "_undefined_member", "primUndefMem");
		       pFun(nameMakeMem,   "_makeMember",   "primMakeMem");
		       pFun(nameBlackHole, "Gc Black Hole", "primGCBhole");
#if    HASKELL_ARRAYS
		       pFun(nameEltUndef,  "_undefined_array_element",
							"primEltUndef");
		       pFun(nameOutBounds, "_out_of_bounds","primOutBounds");
#endif
#if    IO_MONAD
		       pFun(nameSTRun,	   "runST",	"primSTRun");
		       pFun(nameFst,	   "_fst",	"primFst");
		       pFun(nameSnd,	   "_snd",	"primSnd");
#endif
#undef pFun
#define predef(nm,str) nm=newName(findText(str)); name(nm).defn=PREDEFINED
		       predef(nameAnd,		"&&");
		       predef(nameOr,		"||");
		       predef(nameOtherwise,	"otherwise");
		       predef(nameError,	"error");
		       predef(nameComp,		".");
		       predef(nameApp,		"++");
		       predef(nameShowParen,	"showParen");
		       predef(nameRangeSize,	"rangeSize");
		       predef(namePmInt,	"primPmInt");
		       predef(namePmInteger,	"primPmInteger");
		       predef(namePmFlt,	"primPmFlt");
#if NPLUSK
		       predef(namePmNpk,	"primPmNpk");
		       predef(namePmSub,	"primPmSub");
#endif
#undef  predef
		       break;
    }
}

/*-------------------------------------------------------------------------*/
