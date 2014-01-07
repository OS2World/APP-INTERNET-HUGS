/* --------------------------------------------------------------------------
 * preds.c:     Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.0 August 1994, derived from Gofer 2.30a
 *
 * Part of type checker dealing with predicates and entailment.
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Cell   local assumeEvid        Args((Cell,Int));
static List   local makeEvidArgs      Args((List,Int));
static Void   local markPred          Args((Cell));
static List   local copyPreds         Args((List));
static Cell   local copyPred	      Args((Cell,Int));
static Void   local qualify           Args((List,Cell));
static Void   local qualifyBinding    Args((List,Cell));

static List   local simplify          Args((List));
static Void   local overEvid          Args((Cell,Cell));
static Cell   local tidyEvid	      Args((Cell));
static Cell   local simpleEntails     Args((List,Cell,Int));
static List   local superSels	      Args((Class,Class));

static List   local elimConstPreds    Args((Int,String,List));
static Cell   local haskSimp	      Args((Class,Type,Int));
static Cell   local haskSpecific      Args((Class,Int,Type,Int));

static Class  local classConstraining Args((Int,Cell,Int));
static Bool   local resolveDefs	      Args((List,List));
static Bool   local resolveVar	      Args((Int,List));

static Void   local makePendingDicts  Args((Void));
static Void   local installMembers    Args((Inst,Dict));
static Cell   local makeMember	      Args((Name,Int));

/* --------------------------------------------------------------------------
 * Predicate sets:
 *
 * A predicate set is represented by a list of triples (pi, o, used)
 * where o is the offset for types in pi, with evidence required at the
 * node pointed to by used (which is taken as a dictionary parameter if
 * no other evidence is available).  Note that the used node will be
 * overwritten at a later stage if evidence for that predicate is found
 * subsequently.
 * ------------------------------------------------------------------------*/

static List preds;			/* current predicate list	   */
static List evids;			/* evidence for preds		   */
static List hpreds;			/* Haskell form predicates	   */

static Cell local assumeEvid(pi,o)	/* add predicate pi (offset o) to  */
Cell pi;				/* preds with new dict var nd	   */
Int  o; {
    Cell nd = inventDictVar();
    preds   = cons(triple(pi,mkInt(o),nd),preds);
    return nd;
}

static List local makeEvidArgs(qs,o)	/* make list of predicate assumps. */
List qs;				/* from qs (offset o), w/ new dict */
Int  o; {				/* vars for each predicate	   */
    List result;
    for (result=NIL; nonNull(qs); qs=tl(qs))
	result = cons(triple(hd(qs),mkInt(o),inventDictVar()),result);
    return rev(result);
}

static Void local markPred(pi)		/* marked fixed type vars in pi	   */
Cell pi; {
    Cell cl = fst3(pi);
    Int  o  = intOf(snd3(pi));

    for (; isAp(cl); cl=fun(cl))
	markType(arg(cl),o);
}

static List local copyPreds(qs)		/* copy list of predicates         */
List qs; {
    List result;
    for (result=NIL; nonNull(qs); qs=tl(qs)) {
	Cell pi = hd(qs);
	result  = cons(copyPred(fst3(pi),intOf(snd3(pi))),result);
    }
    return rev(result);
}

static Cell local copyPred(pi,o)	/* copy single predicate (or part  */
Cell pi;				/* thereof) ...			   */
Int  o; {
    if (isAp(pi)) {
	Cell temp = copyPred(fun(pi),o);/* to ensure correct order of eval.*/
	return ap(temp,copyType(arg(pi),o));
    }
    else
	return pi;
}

static Void local qualify(qs,alt)	/* Add extra dictionary args to	   */
List qs;				/* qualify alt by predicates in qs */
Cell alt; {				/* :: ([Pat],Rhs)		   */
    List ds;
    for (ds=NIL; nonNull(qs); qs=tl(qs))
	ds = cons(thd3(hd(qs)),ds);
    fst(alt) = revOnto(ds,fst(alt));
}

static Void local qualifyBinding(qs,b)	/* Add extra dict args to each	   */
List qs;				/* alternative in function binding */
Cell b ; {
    if (!isVar(fst(b)))			/* check for function binding	   */
	internal("qualifyBinding");
    map1Proc(qualify,qs,snd(snd(b)));
}

/* --------------------------------------------------------------------------
 * Predicate set Simplification:
 *
 * Calculate a minimal equivalent subset of a given set of predicates.
 * ------------------------------------------------------------------------*/

static List local simplify(qs)		/* Simplify predicates in qs,      */
List qs; {				/* returning equiv minimal subset  */
    Int n = length(qs);

    while (0<n--) {
	Cell pi = hd(qs);
	Cell ev = simpleEntails(tl(qs),fst3(pi),intOf(snd3(pi)));
	if (nonNull(ev)) {
	    overEvid(thd3(pi),ev);
	    qs = tl(qs);
	}
	else {
	    Cell tmp = tl(qs);
	    tl(qs)   = NIL;
	    qs       = appendOnto(tmp,qs);
	}
    }
    return qs;
}

static Void local overEvid(c,ev)	/* overwrite evidence (possibly	   */
Cell c;					/* including indirection; select0) */
Cell ev; {
    if (isPair(ev) && isSelect(fst(ev)))
	overwrite(c,ev);		/* overwrite with dict selection   */
    else {
	fst(c) = mkSelect(0);		/* indirect to dict variable	   */
	snd(c) = ev;
    }
}

static Cell local tidyEvid(ev)		/* eliminate indirections in evid  */
Cell ev; {
    if (isAp(ev)) {
	arg(ev) = tidyEvid(arg(ev));
	if (fun(ev)==mkSelect(0))
	    return arg(ev);
	else if (!isSelect(fun(ev)))
	    fun(ev) = tidyEvid(fun(ev));
    }
    return ev;
}

static Cell local simpleEntails(ps,pi,o)/* Calculate evidence for (pi,o)   */
List ps;				/* from ps, using only equality and*/
Cell pi;				/* superclasses, returning NIL if  */
Int  o; {				/* no such evidence can be found   */
    Class c = fun(pi);
    Type  t = arg(pi);

    for (; nonNull(ps); ps=tl(ps)) {
	Cell  p  = hd(ps);
	if (sameType(arg(fst3(p)),intOf(snd3(p)),t,o)) {
	    Cell ev = superEvid(thd3(p),fun(fst3(p)),c);
	    if (nonNull(ev))
		return ev;
	}
    }
    return NIL;
}

Cell superEvid(v,c,d)			/* Assuming that v is evidence for */
Cell  v;				/* some predicate c t, find evid.  */
Class c;				/* for d t using superclasses, or  */
Class d; {				/* return NIL if this is impossible*/
    List dSels = superSels(c,d);
    return (dSels!=UNIT) ?  revOnto(dSels,v) : NIL;
}

static List local superSels(c,d)	/* auxiliary function, calculate   */
Class c,d; {				/* list of dict sels from c to d   */
    if (c==d)
	return NIL;
    else if (class(c).level > class(d).level) {
	List cs = class(c).supers;
	Int  n  = class(c).numMembers+1;
	for (; nonNull(cs); cs=tl(cs), ++n) {
	    Cell dSels = superSels(hd(cs),d);
	    if (dSels!=UNIT)
		return cons(mkSelect(n),dSels);
	}
    }
    return UNIT;			/* use UNIT to indicate failure	   */
}

/* --------------------------------------------------------------------------
 * Deal with constant and locally constant predicates
 * ------------------------------------------------------------------------*/

static Int    numFixedVars;		/* number of fixed vars found	   */
static Int    numGenericVars;		/* number of generic vars found	   */

static Int    lineProve;
static String whereProve;

static List local elimConstPreds(l,wh,ps)
Int    l;				/* Cycles through current preds	   */
String wh;				/* eliminating constant predicates,*/
List   ps; {				/* adding locally constant preds to*/
    List qs   = NIL;			/* ps (which is used as the return */
    hpreds    = NIL;			/* value), simplifying and then    */
    evids     = NIL;			/* calculating Haskell equiv of any*/
    lineProve = l;			/* predicates that remain.	   */

    for (preds=simplify(preds); nonNull(preds);) {
	Cell pi = hd(preds);
	Cell nx = tl(preds);
	Cell ev;

	numFixedVars = numGenericVars = 0;
	whereProve   = wh;
	ev           = haskSimp(fun(fst3(pi)),arg(fst3(pi)),intOf(snd3(pi)));

	if (numGenericVars>0) {				/* contains generic*/
	    tl(preds) = qs;
	    qs        = preds;
	    evids     = cons(ev,evids);
	}
	else if (numFixedVars>0) {			/* only fixed vars */
	    tl(preds) = ps;
	    ps        = preds;
	}
	else						/* constant types  */
	    overwrite(thd3(pi),makeDictFor(ev,0));

	preds = nx;
    }
    preds  = qs;
    hpreds = simplify(hpreds);
    return ps;
}

static Cell local haskSimp(c,t,o)	/* Simplify unary pred with class c*/
Class c;				/* and type (t,o) to Haskell form  */
Type  t;				/* (i.e. class variable pairs),    */
Int   o; {				/* and returning appropriate evid. */
    Tyvar *tyv;

    for (;;) {
	deRef(tyv,t,o);
	if (tyv) {			/* variable			   */
	    Cell nd = inventDictVar();
	    if (tyv->offs == FIXED_TYVAR)
		++numFixedVars;
	    else {			/* add to hpreds for generic vars  */
		Int  vn = tyvNum(tyv);
		Cell pi = ap(c,mkInt(vn));
		hpreds  = cons(triple(pi,mkInt(0),nd),hpreds);
		numGenericVars++;
	    }
	    return nd;
	}
	else {				/* constructor			   */
	    Type h = getDerefHead(t,o);
	    Int  a = argCount;
	    if (isSynonym(h) && a>=tycon(h).arity) {
		expandSyn(h,a,&t,&o);
		continue;
	    }
	    else {
		Cell in = findInst(c,h);/* locate instance decl.	   */
		if (isNull(in)) {
		    ERROR(lineProve) "Cannot construct instance "
		    ETHEN ERRPRED(ap(c,copyType(t,o)));
		    ERRTEXT " in %s", whereProve
		    EEND;
		}
		else if (a!=inst(in).arity)
		    internal("arity mismatch");
		else if (nonNull(inst(in).specifics)) {
		    List sps = inst(in).specifics;
		    for (; nonNull(sps); sps=tl(sps))
			in = ap(in,haskSpecific(fun(hd(sps)),
						a-offsetOf(arg(hd(sps)))-1,
						t,
						o));
		}
		return in;
	    }
	}
    }
}

static Cell local haskSpecific(c,n,t,o)	/* take n args down from the root   */
Class c;				/* of (t,o) and construct corresp.  */
Int   n;				/* instance of class c		    */
Type  t;
Int   o; {
    for (; 0<n--; t=fun(t)) {
	Tyvar *tyv;
	deRef(tyv,t,o);
	if (tyv || !isAp(t))
	    internal("haskSpecific");
    }
    if (!isAp(t))
	internal("haskSpecific2");
    return haskSimp(c,arg(t),o);
}

/* --------------------------------------------------------------------------
 * Mechanisms for dealing with defaults:
 * ------------------------------------------------------------------------*/

Bool mtInst(c,mt)			/* Determine whether a specified   */
Class c;				/* monotype mt (no synonyms) is an */
Type  mt; {				/* instance of the class c.	   */
    Type h  = getHead(mt);
    Int  a  = argCount;
    Inst in = findInst(c,h);

    if (nonNull(in)) {
	if (a!=inst(in).arity)
	    internal("arity mismatch");
	else if (nonNull(inst(in).specifics)) {
	    List sps = inst(in).specifics;
	    for (; nonNull(sps); sps=tl(sps)) {
		Int  i = a - offsetOf(arg(hd(sps))) - 1;
                Cell t = mt;
                while (0<i--)
		    t = fun(t);
                if (!mtInst(fun(hd(sps)),arg(t)))
		    return FALSE;
	    }
	}
	return TRUE;
    }
    return FALSE;
}

static Class local classConstraining(vn,pi,o)
Int  vn;				/* return class constraining var*/
Cell pi;				/* vn in predicate pi, or NIL if*/
Int  o; { 				/* vn is not involved		*/
    lookingFor = tyvar(vn);
    if (doesntOccurIn(arg(pi),o))
	return NIL;
    else
	return fun(pi);
}

static Bool local resolveDefs(vs,ps)	/* attempt to resolve defaults  */
List vs;				/* for variables vs subject to  */
List ps; {				/* constraints ps		*/
    List pvs       = NIL;
    List qs        = ps;
    Bool defaulted = FALSE;
 
#ifdef DEBUG_DEFAULTS
    printf("Attempt to resolve variables ");
    printExp(stdout,vs);
    printf(" with context ");
    printContext(stdout,ps);
    printf("\n");
#endif

    resetGenericsFrom(0);		/* find type variables in ps	*/
    for (; nonNull(qs); qs=tl(qs)) {
	Cell pi = fst3(hd(qs));
	Int  o  = intOf(snd3(hd(ps)));
	for (; isAp(pi); pi=fun(pi))
	    pvs = genvarType(arg(pi),o,pvs);
    }

    for (; nonNull(pvs); pvs=tl(pvs)) {	/* now try defaults		*/
	Int vn = intOf(hd(pvs));

#ifdef DEBUG_DEFAULTS
	printf("is var %d included in ",vn);
	printExp(stdout,vs);
	printf("?\n");
#endif

	if (!intIsMember(vn,vs))
	    defaulted |= resolveVar(vn,ps);
#ifdef DEBUG_DEFAULTS
	else
	    printf("Yes, so no ambiguity!\n");
#endif
    }

    return defaulted;
}

static Bool local resolveVar(vn,ps)	/* determine whether an ambig.  */
Int  vn;				/* variable vn can be resolved  */
List ps; {				/* by default in the context of */
    List cs	   = NIL;		/* the predicates in ps		*/
    Bool aNumClass = FALSE;

    /* According to the Haskell definition, we can only default an ambiguous
     * variable if the set of classes that constrain it:
     *   (a) includes at least one numeric class.
     *   (b) includes only numeric or standard classes.
     */

#ifdef DEBUG_DEFAULTS
    printf("Trying to default variable %d\n",vn);
#endif

    for (; nonNull(ps); ps=tl(ps)) {
	Class c = classConstraining(vn,fst3(hd(ps)),intOf(snd3(hd(ps))));
	if (nonNull(c) && !cellIsMember(c,cs)) {
	    if (c==classRealFrac   || c==classRealFloat ||
		c==classFractional || c==classFloating  ||
		c==classReal	   || c==classIntegral  || c==classNum)
		aNumClass = TRUE;
	    else if (c!=classEq     && c!=classOrd && c!=classText &&
		     c!=classBinary && c!=classIx  && c!=classEnum)
		return FALSE;
	    cs = cons(c,cs);
	}
    }

    /* Now find the first class (if any) in the list of defaults that
     * is an instance of all of the required classes.
     */

    if (aNumClass) {
	List ds = defaultDefns;
#ifdef DEBUG_DEFAULTS
	printf("Default conditions met, looking for type\n");
#endif
	for (; nonNull(ds); ds=tl(ds)) {
	    List cs1 = cs;
	    while (nonNull(cs1) && mtInst(hd(cs1),hd(ds)))
		cs1 = tl(cs1);
	    if (isNull(cs1)) {
		bindTv(vn,hd(ds),0);
#ifdef DEBUG_DEFAULTS
		printf("Default type for variable %d is ",vn);
		printType(stdout,hd(ds));
		printf("\n");
#endif
		return TRUE;
	    }
	}
    }

#ifdef DEBUG_DEFAULTS
    printf("No default permitted/found\n");
#endif
    return FALSE;
}

/* -----------------------------------------------------------------------
 * Dictionary construction:
 *
 * 0 | class(c).numMembers | class(c).numSupers | inst(in).numSpecifics |
 *
 * Proof of termination of makeDictFor(ev,off) relies on the use of the
 * lexicographic ordering on (Type,Class); recursive calls for instance
 * specific dictionaries decrease the first (type) component, recursive
 * calls for superclass dictionaries leave the first component unchanged
 * but decrease the second (class) component.  These facts are consequences
 * of (1) the syntactic restrictions on the form of instance declarations,
 * and (2) the restriction to an acyclic class hierarchy.
 *
 * This section of code doesn't attempt to construct the dictionaries
 * required by specific member functions, which would cause non-termination
 * in some cases.  Instead, it uses the makeMember primitive to delay the
 * construction of dictionaries.
 * ----------------------------------------------------------------------- */

static List dictsPending;		/* List of dicts to be initialized */

Cell makeDictFor(ev,off)		/* Construct dictionary for given  */
Cell ev;				/* evidence expression		   */
Dict off; {
    List  args = NIL;
    Int   n    = 0;
    List  ds;
    Dict  dc, di;
    Class c;

    switch (whatIs(ev)) {				/* Special cases:  */
	case DICTCELL : return ev;			/* already eval'd? */
	case OFFSET   : return dict(off+offsetOf(ev));	/* dict param.	   */
	case AP	      : if (isSelect(fun(ev)))		/* superclass?	   */
			    return dict(dictOf(makeDictFor(arg(ev),off))
					+ selectOf(fun(ev)));
    }

    for (; isAp(ev); ev=fun(ev), ++n)	/* Otherwise, eval args to dicts   */
	args = cons(makeDictFor(arg(ev),off),args);
    if (!isInst(ev) || n!=inst(ev).numSpecifics)
	internal("mkDict");

    c = inst(ev).c;			/* scan prev. constructed dicts	   */
    n = 1 + class(c).numMembers + class(c).numSupers;
    for (ds=inst(ev).dicts; nonNull(ds); ds=tl(ds)) {
	Dict d  = dictOf(hd(ds)) + n;
	List as = args;
	for (;; as=tl(as), ++d)
	    if (isNull(as))
		return hd(ds);
	    else if (dict(d)!=hd(as))
		break;
    }

    /* Time to build a new dictionary ... */

    dc		   = newDict(n + inst(ev).numSpecifics);
    inst(ev).dicts = cons(dict(dc)=mkDict(dc),inst(ev).dicts);

#ifdef DEBUG_DICTS
    printf("Building dictionary %d = %s-",dc,textToStr(class(c).text));
    printType(stdout,inst(ev).t);
    for (ds=args; nonNull(ds); ds=tl(ds))
	printf(" %d",dictOf(hd(ds)));
    putchar('\n');
#endif

    for (di=dc+n; nonNull(args); args=tl(args), ++di)	/* save specifics  */
	dict(di) = hd(args);

    for (di=dc+1+class(c).numMembers, ds=inst(ev).superBuild;	/* supers  */
	nonNull(ds); ++di, ds=tl(ds))
	dict(di) = makeDictFor(hd(ds),dc+n);

    if (dictsPending==UNIT)
	installMembers(ev,dc);
    else
	dictsPending = cons(pair(ev,mkInt(dc)),dictsPending);

    return dict(dc);
}

static Void local makePendingDicts() {	/* Build pending dictionaries	   */

    /* The installation of member function implementations in dictionaries */
    /* has to be delayed until the types (and hence the evid parameters) of*/
    /* the implementations are known.  Dictionaries that are built before  */
    /* these types are known are added to the pending list, and the member */
    /* function implementations are installed later by calling this func.  */

    for (; nonNull(dictsPending); dictsPending=tl(dictsPending))
	installMembers(fst(hd(dictsPending)),intOf(snd(hd(dictsPending))));
    dictsPending = UNIT;
}

static Void local installMembers(in,dc)	/* Install member functions in dict*/
Inst in;
Dict dc; {
    Class c  = inst(in).c;
    Int   n  = 1 + class(c).numMembers + class(c).numSupers;
    List  ms = class(c).members;
    List  is = inst(in).implements;
    List  ds = class(c).defaults;
    Dict  di = 1;
    for (; nonNull(ms); ms=tl(ms)) {
	if (nonNull(is) && nonNull(hd(is)))
	    dict(dc + di++) = makeMember(hd(is),dc+n);
	else if (nonNull(ds) && nonNull(hd(ds)))
	    dict(dc + di++) = makeMember(hd(ds),dc);
	else
	    dict(dc + di++) = ap(nameUndefMem,pair(in,hd(ms)));

	if (nonNull(is)) is=tl(is);
	if (nonNull(ds)) ds=tl(ds);
    }

#ifdef DEBUG_DICTS
    {   Int i = n + inst(in).numSpecifics;
	printf("DICTIONARY:------\n");
	for (di=dc; i>0; --i, ++di) {
	    printf("dict(%d) = ",di);
	    printExp(stdout,dict(di));
	    putchar('\n');
	}
    }
#endif
}

static Cell local makeMember(n,off)	/* set initial value of member fn   */
Name n;
Int  off; {
    if (nonNull(name(n).type))
	return ap(ap(nameMakeMem,mkInt(off)),n);
    else
	return n;
}

/*-------------------------------------------------------------------------*/
