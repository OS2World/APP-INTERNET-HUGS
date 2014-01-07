/* --------------------------------------------------------------------------
 * connect.h:	Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *		See NOTICE for details and conditions of use etc...
 *              Hugs version 1.0 August 1994, derived from Gofer 2.30a
 *
 * Connections between components of the Hugs system
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Standard data:
 * ------------------------------------------------------------------------*/

extern Name  nameFalse,	  nameTrue;	/* primitive constructor functions */
extern Name  nameNil,	  nameCons;
extern Name  nameJust,	  nameMaybe;
extern Name  nameFatbar,  nameFail;	/* primitives reqd for translation */
extern Name  nameIf,	  nameSel;
extern Name  namePmInt,	  namePmFlt;	/* primitives for pattern matching */
extern Name  namePmInteger;
#if NPLUSK
extern Name  namePmNpk,	  namePmSub;	/* primitives for (n+k) patterns   */
#endif
extern Name  nameUndefMem;	 	/* undefined member primitive	   */
extern Name  nameMakeMem;		/* makeMember primitive		   */
extern Name  nameError;			/* for runtime error messages	   */
extern Name  nameBlackHole;		/* for GC-detected black hole	   */
extern Name  nameAnd,	  nameOr;	/* for optimisation of && and ||   */
extern Name  nameOtherwise;
extern Name  nameFromInt, nameFromDouble;/*coercion of numerics		   */
extern Name  nameFromInteger;
extern Name  nameEq,	  nameOrdcmp;	/* derivable names		   */
extern Name  nameIndex,	  nameInRange;
extern Name  nameRange;
extern Name  nameLe,	  nameShowsPrec;
extern Name  nameMult,	  namePlus;
extern Name  nameConCmp,  nameEnRange;
extern Name  nameEnIndex, nameEnInRng;
extern Name  nameEnFrom,  nameEnFrTh;
extern Name  nameEnFrTo;
extern Name  nameComp,	  nameApp;	/* composition and append	   */
extern Name  nameShowParen;		/* wrap with parens		   */
extern Name  nameRangeSize;		/* calculate size of index range   */

extern Name  namePrint;			/* printing primitive		   */

#if IO_DIALOGUE
extern Name  nameReadFile,   nameWriteFile;/* I/O name primitives	   */
extern Name  nameAppendFile, nameReadChan;
extern Name  nameAppendChan, nameEcho;
extern Name  nameGetArgs,    nameGetProgName;
extern Name  nameGetEnv;
extern Name  nameSuccess,    nameStr;
extern Name  nameFailure,    nameStrList;
extern Name  nameWriteError;
extern Name  nameReadError,  nameSearchError;
extern Name  nameFormatError,nameOtherError;
#endif
#if    IO_MONAD
extern Type   typeIO, typeProgIO;	/* for the IO monad, IO and IO ()  */
extern Type   typeWorld, typeST;	/* built on top of IO = ST World   */
extern Void   ioExecute Args((Cell));	/* IO monad executor		   */
extern Name   nameSTRun;		/* encapsulator			   */
extern Type   typeMutVar;		/* type constr for mutable vars	   */
#if    HASKELL_ARRAYS
extern Type   typeMutArr;		/* type constr for mutable arrays  */
#endif
#endif
#if    HASKELL_ARRAYS
extern Type typeArray;			/* type constr for arrays	   */
#endif

extern Text  textPlus;			/* used to recognise n+k patterns  */

extern String repeatStr;		/* repeat last command string	   */

extern Type  typeString;		/* String  == [Char]		   */
extern Type  typeDialogue;		/* Dialogue== [Response]->[Request]*/
extern Type  typeBool;
extern Type  typeInt;
extern Type  typeFloat;
extern Type  typeChar;		
extern Type  typeBin;		
extern Type  typeDouble;
extern Type  typeInteger;
extern Type  typeMaybe;

extern List  stdDefaults;		/* list of standard default types  */

extern Class classEq;			/* `standard' classes		   */
extern Class classOrd;
extern Class classText;
extern Class classBinary;
extern Class classIx;
extern Class classEnum;

extern Class classReal;			/* `numeric' classes		   */
extern Class classIntegral;
extern Class classRealFrac;
extern Class classRealFloat;
extern Class classFractional;
extern Class classFloating;
extern Class classNum;

extern Cell  *CStackBase;		/* pointer to base of C stack	   */

extern List  tyconDefns;		/* list of type constructor defns  */
extern List  typeInDefns;		/* list of synonym restrictions	   */
extern List  valDefns;			/* list of value definitions       */
extern List  opDefns;			/* list of operator definitions    */
extern List  classDefns;		/* list of class definitions       */
extern List  instDefns;			/* list of instance definitions    */
extern List  overDefns;			/* list of overloaded member defns */
extern List  primDefns;			/* list of primitive definitions   */
extern List  defaultDefns;		/* default definitions (if any)	   */
extern Int   defaultLine;		/* line in which default defs occur*/
extern List  evalDefaults;		/* defaults for evaluator	   */
extern Cell  inputExpr;			/* evaluator input expression      */
extern Addr  inputCode;			/* Code for compiled input expr    */

extern Int   whnfArgs;			/* number of args of term in whnf  */
extern Cell  whnfHead;			/* head of term in whnf            */
extern Int   whnfInt;			/* integer value of term in whnf   */
extern Float whnfFloat;			/* float value of term in whnf	   */
extern Long  numReductions;		/* number of reductions used       */
extern Long  numCells;			/* number of cells allocated       */
extern Int   numberGcs;			/* number of garbage collections   */
extern Int   cellsRecovered;		/* cells recovered by last gc	   */
extern Bool  broken;			/* indicates interrupt received    */

extern Bool  gcMessages;		/* TRUE => print GC messages	   */
extern Bool  literateScripts;		/* TRUE => default lit scripts     */
extern Bool  literateErrors;		/* TRUE => report errs in lit scrs */
extern Bool  failOnError;		/* TRUE => error produces immediate*/
					/*	   termination		   */
extern Bool  kindExpert;		/* TRUE => display kind errors in  */
					/* 	   full detail		   */

/* --------------------------------------------------------------------------
 * Function prototypes etc...
 * ------------------------------------------------------------------------*/

extern Void   everybody        Args((Int));

#define RESET   1		/* reset subsystem                         */
#define MARK    2		/* mark parts of graph in use by subsystem */
#define INSTALL 3		/* install subsystem (executed once only)  */
#define EXIT	4		/* Take action immediately before exit()   */
#define BREAK   5		/* Take action after program break	   */

typedef long   Target;
extern  Void   setGoal          Args((String, Target));
extern  Void   soFar            Args((Target));
extern  Void   done             Args((Void));
extern  String fromEnv		Args((String,String));

extern  Void   storage          Args((Int));
extern  Void   setLastExpr	Args((Cell));
extern  Cell   getLastExpr	Args((Void));
extern	List   addNamesMatching Args((String,List));

extern  Void   input            Args((Int));
extern  Void   consoleInput     Args((String));
extern  Void   projInput	Args((String));
extern  Void   parseScript      Args((String,Long));
extern  Void   parseExp         Args((Void));
extern  String readFilename     Args((Void));
extern  String readLine		Args((Void));
extern  Syntax defaultSyntax    Args((Text));
extern  String unlexChar        Args((Char,Char));

extern  Void   staticAnalysis	Args((Int));
extern  Void   tyconDefn	Args((Int,Cell,Cell,Cell));
extern  Void   setTypeIns	Args((List));
extern  Void   clearTypeIns	Args((Void));
extern  Bool   isAmbiguous	Args((Type));
extern  Void   ambigError	Args((Int,String,Cell,Type));
extern  Void   classDefn	Args((Int,Cell,Cell));
extern  Void   instDefn		Args((Int,Cell,Cell));
extern  Void   addTupInst	Args((Class,Int));
extern  Void   primDefn		Args((Cell,List,Cell));
extern  Void   defaultDefn	Args((Int,List));
extern  Void   checkExp		Args((Void));
extern  Void   checkDefns	Args((Void));

extern  Void   typeChecker	Args((Int));
extern  Type   typeCheckExp	Args((Bool));
extern  Void   typeCheckDefns	Args((Void));
extern  Cell   rhsExpr		Args((Cell));
extern  Int    rhsLine		Args((Cell));
extern  Bool   typeMatches	Args((Type,Type));
extern  Cell   superEvid	Args((Cell,Class,Class));
extern  Bool   mtInst		Args((Class,Type));
extern  Cell   makeDictFor      Args((Cell,Dict));
extern  Void   linkPreludeCore  Args((Void));

extern  Void   kindTCGroup	Args((List));
extern  Void   kindSigType	Args((Int,Type));
extern  Void   kindInst		Args((Inst,Cell));
extern  Void   kindDefaults	Args((Int,List));

extern  Void   compiler         Args((Cell));
extern  Void   compileDefns     Args((Void));
extern  Void   compileExp       Args((Void));
extern  Bool   refutable	Args((Cell));
extern  Int    discrArity       Args((Cell));

extern  Void   machine          Args((Int));
extern  Addr   codeGen          Args((Name,Int,Cell));
extern  Void   addCfunTable	Args((Tycon));
extern  Name   succCfun		Args((Name));
extern  Name   nextCfun		Args((Name,Name));
extern  Void   externalPrim	Args((Name,String));
extern  Void   unwind           Args((Cell));
extern  Void   eval             Args((Cell));
extern  Cell   evalWithNoError  Args((Cell));
extern  Void   evalFails        Args((StackPtr));
extern  Cell   graphForExp	Args((Void));

extern  Void   builtIn          Args((Int));
extern  Void   abandon		Args((String,Cell));
extern  Cell   outputString	Args((FILE *,Cell));
extern  Void   dialogue		Args((Cell));
extern  Cell   consChar		Args((Char));
#if BIGNUMS
extern  Bignum bigStr		Args((String));
extern  Cell   bigOut		Args((Bignum,Cell,Bool));
#endif

extern  Void   machdep          Args((Int));
extern  String timeString	Args((Void));
extern  Int    shellEsc		Args((String));
extern  Int    getTerminalWidth Args((Void));
extern  Void   normalTerminal	Args((Void));
extern  Void   noechoTerminal	Args((Void));
extern  Int    readTerminalChar Args((Void));
extern  Void   gcStarted	Args((Void));
extern  Void   gcScanning	Args((Void));
extern  Void   gcRecovered	Args((Int));
extern  Void   gcCStack		Args((Void));

/*-------------------------------------------------------------------------*/
