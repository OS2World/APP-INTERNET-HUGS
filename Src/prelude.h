/*
#----------------------------------------------------------------------------
#
# This file was chanced for compilation under OS/2 Warp with GCC+emx
#
# Michael Zimmermann, 15.Apr.96, Michael.Zimmermann@post.rwth-aachen.de
#
#----------------------------------------------------------------------------
*/

/* --------------------------------------------------------------------------
 * prelude.h:   Copyright (c) Mark P Jones 1991-1994.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.0 August 1994, derived from Gofer 2.30a
 *
 * Basic data type definitions, prototypes and standard macros including
 * machine dependent variations...
 * ------------------------------------------------------------------------*/

#define const             /* const is more trouble than it's worth,...     */
#include <stdio.h>

/*---------------------------------------------------------------------------
 * To select a particular machine/compiler, just place a 1 in the appropriate
 * position in the following list and ensure that 0 appears in all other
 * positions:
 *
 * The letters UN in the comment field indicate that I have not personally
 * been able to test this configuration yet and I have not heard from anybody
 * else that has tried it.  If you run Hugs on one of these systems and it
 * works (or needs patches) please let me know so that I can fix it and
 * update the source.
 *-------------------------------------------------------------------------*/

#define BCC      0      /* For IBM PC, using Borland C++ 3.1            UN */
#define BCC32    0      /* For IBM PC, using Borland C++ 4.02 powerpack    */
#define DJGPP    0      /* For DJGPP version 1.09 (gcc2.2.2) and DOS 5.0UN */
#define WATCOM   0      /* For IBM PC, using WATCOM C/C++32 v9.5c          */
#define OS2      1      /* For IBM OS/2 2.0 using EMX GCC               UN */
#define SUNOS    0      /* For Sun 3/Sun 4 running SunOs 4.x               */
#define SOLARIS  0      /* For Solaris 2.4                                 */
#define MIPS     0      /* For MIPS RC6280/Sony machine NWS-3870        UN */
#define NEXTSTEP 0      /* For NeXTstep 3.0 using NeXT cc               UN */
#define NEXTGCC  0      /* For NeXTstep with gcc 2.x, doesn't work w/ NS3.2*/
#define MINIX68K 0      /* For Minix68k with gcc                        UN */
#define AMIGA    0      /* For Amiga using gcc 2.2.2                    UN */
#define HPUX     0      /* For HPUX using gcc                           UN */
#define LINUX    0      /* For Linux using gcc                          UN */
#define RISCOS   0      /* For Acorn DesktopC and RISCOS2 or 3          UN */
#define SVR4     0      /* For SVR4 using GCC2.2                        UN */
#define ULTRIX   0      /* For DEC Ultrix 4.x using GCC2.3.3            UN */
#define AIX      0      /* For IBM AIX on RS/6000 using GCC             UN */
#define ATARI    0      /* For Atari ST/STE/TT/Falcon w/ Lattice C 5.52 UN */
#define SGI4     0      /* For SiliconGraphics Indigo, IRIX v*4*.0.5    UN */
#define NETBSD   0      /* For NetBSD-current                           UN */

/*---------------------------------------------------------------------------
 * To add a new machine/compiler, add a new macro line above, add the new
 * to the appropriate flags below and add a `machine specific' section in the
 * following section of this file.  Please send me details of any new machines
 * or compilers that you try so that I can pass them onto others!
 *
 *   UNIX           if the machine runs fairly standard Unix
 *   SMALL_HUGS     for 16 bit operation on a limited memory PC
 *   REGULAR_HUGS   for 32 bit operation using largish default table sizes
 *   LARGE_HUGS     for 32 bit operation using larger default table sizes
 *   JMPBUF_ARRAY   if jmpbufs can be treated like arrays.
 *   DOS_IO         to use DOS style IO for terminal control
 *   TERMIO_IO      to use Unix termio for terminal control
 *   SGTTY_IO       to use Unix sgtty for terminal control
 *   TERMIOS_IO     to use posix termios for terminal control
 *   BREAK_FLOATS   to use two integers to store a float (or double)
 *                  if SMALL_HUGS, then you *must* use BREAK_FLOATS == 1
 *                  (assumes sizeof(int)==2, sizeof(float)==4).
 *                  Otherwise, assuming sizeof(int)==sizeof(float)==4,
 *                  BREAK_FLOATS == 0 will give you floats  for floating pt,
 *                  BREAK_FLOATS == 1 will give you doubles for floating pt.
 *   HAS_FLOATS     to indicate support for floating point
 *   HASKELL_ARRAYS to include support for Haskell array primitives
 *   IO_MONAD       to include the IO and ST monad primitives and support
 *   IO_DIALOGUE    to include old style Haskell Dialogue based I/O
 *   NPLUSK         to include support for (n+k) and (c*n) patterns
 *   BIGNUMS        to include support for Integer bignums
 *   FIXED_SUBST    to force a fixed size for the current substitution
 *-------------------------------------------------------------------------*/

#define UNIX            (SUNOS  | NEXTSTEP | HPUX | NEXTGCC | LINUX | AMIGA | \
                         MINIX68K | OS2 | SVR4 | ULTRIX | AIX | MIPS |\
                         SGI4 | NETBSD | SOLARIS)
#define SMALL_HUGS      (BCC)
#define REGULAR_HUGS    (RISCOS | DJGPP | ATARI | BCC32 | WATCOM)
#define LARGE_HUGS      (UNIX)
#define JMPBUF_ARRAY    (UNIX   | DJGPP | RISCOS | ATARI)
#define DOS_IO          (BCC | DJGPP | BCC32 | ATARI | WATCOM)
#define TERMIO_IO       (LINUX  | HPUX | OS2 | SVR4 | SGI4)
#define SGTTY_IO        (SUNOS  | NEXTSTEP | NEXTGCC | AMIGA | MINIX68K | \
                         ULTRIX | AIX | MIPS)
#define TERMIOS_IO      (NETBSD | SOLARIS)
#define BREAK_FLOATS    (BCC)
#define HAS_FLOATS      (REGULAR_HUGS | LARGE_HUGS | BREAK_FLOATS)

#define HASKELL_ARRAYS  1 /*(REGULAR_HUGS | LARGE_HUGS)*/
#define IO_MONAD        (REGULAR_HUGS | LARGE_HUGS)
#define IO_DIALOGUE     1 /* Warning: This may become 0 in future versions */
#define NPLUSK          1 /* Warning: This may become 0 in future versions */
#define BIGNUMS         1 /* Experimental bignum implementation            */
#define GENERIC_CMP     0 /* generic comparison primitives                 */
#define FIXED_SUBST     0 /* Warning: This may not be appropriate for PCs  */

/*---------------------------------------------------------------------------
 * The following flags should be set automatically according to builtin
 * compiler flags, but you might want to set them manually to avoid default
 * behaviour in some situations:
 *-------------------------------------------------------------------------*/

#ifdef  __GNUC__                        /* look for GCC 2.x extensions     */
#if     __GNUC__ >= 2 && !NEXTSTEP      /* NeXT cc lies and says it's 2.x  */
#define GCC_THREADED 1

/* WARNING: if you use the following optimisations to assign registers for
 * particular global variables, you should be very careful to make sure that
 * storage(RESET) is called after a longjump (usually resulting from an error
 * condition) and before you try to access the heap.  The current version of
 * main deals with this using everybody(RESET) at the head of the main read,
 * eval, print loop
 */

#ifdef  m68k                            /* global registers on an m68k     */
#define GLOBALfst       asm("a4")
#define GLOBALsnd       asm("a5")
#define GLOBALsp        asm("a3")
#endif

#ifdef  sparc                           /* global registers on a sparc     */
/* sadly, although the gcc documentation suggests that the following reg   */
/* assignments should be ok, experience shows (at least on Suns) that they */
/* are not -- it seems that atof() and friends spoil things.               */
/*#define GLOBALfst     asm("g5")*/
/*#define GLOBALsnd     asm("g6")*/
/*#define GLOBALsp      asm("g7")*/
#endif

#endif
#endif

#ifndef GCC_THREADED
#define GCC_THREADED 0
#endif

/*---------------------------------------------------------------------------
 * Machine specific sections:
 * Include any machine specific declarations and define macros:
 *   local              prefix for locally defined functions
 *   far                prefix for far pointers
 *   allowBreak()       call to allow user to interrupt computation
 *   FOPEN_WRITE        fopen *text* file for writing
 *   FOPEN_APPEND       fopen *text* file for append
 *
 * N.B. `far' must be explicitly defined (usually to the empty string)
 *-------------------------------------------------------------------------*/

#ifdef __STDC__           /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#if (BCC)                 /* K&R 1 does not permit `defined(__STDC__)' ... */
#define Args(x) x
#else
#define Args(x) ()
#endif
#endif

#if     BCC
#include <alloc.h>
#define local           near pascal
extern  int  kbhit      Args((void));
#define allowBreak()    kbhit()
#define FOPEN_WRITE     "wt"
#define FOPEN_APPEND    "at"
#define farCalloc(n,s)  farcalloc((unsigned long)n,(unsigned long)s)
#define sigProto(nm)    int nm(void)
#define sigRaise(nm)    nm()
#define sigHandler(nm)  int nm()
#define sigResume       return 1
#endif

#if     (SUNOS | SOLARIS)
#include <malloc.h>
#define far
#define farCalloc(n,s)  (Void *)valloc(((unsigned)n)*((unsigned)s))
#endif

#if     MIPS
#define far
#define farCalloc(n,s)  (Void *)valloc(((unsigned)n)*((unsigned)s))
#endif

#if     (NEXTSTEP | NEXTGCC | MINIX68K | ULTRIX)
#include <stdlib.h>
#define far
#define farCalloc(n,s)  (Void *)valloc(((unsigned)n)*((unsigned)s))
#endif

#if     AMIGA
#include <stdlib.h>
#define Main            int
#define far
#define farCalloc(n,s)  (Void *)valloc(((unsigned)n)*((unsigned)s))
#endif

#if     (HPUX | DJGPP | LINUX | OS2 | SVR4 | AIX | SGI4 | NETBSD)
#include <stdlib.h>
#define  far
#endif

#if     BCC32
#include <stdlib.h>
#define  far
#define  Main           int
#define  MainDone       return 0;/*NOTUSED*/
#define sigProto(nm)    int nm(void)
#define sigRaise(nm)    nm()
#define sigHandler(nm)  int nm()
#define sigResume       return 1
extern  sigProto(breakHandler);
extern  int  kbhit      Args((void));
#define allowBreak()    if (kbhit(),broken) {broken=FALSE; breakHandler();}
#endif

#if     WATCOM
#include <stdlib.h>
#undef   far
#define  far
extern   void breakHandler(int);
#define  allowBreak()   if (broken) {broken=FALSE; sigRaise(breakHandler);}
#endif

#if     RISCOS
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#define  far
#define  isascii(c)     (((unsigned)(c))<128)
#define  Main           int
#define  MainDone       return 0;/*NOTUSED*/
extern   int access     Args((char *, int));
extern   int namecmp    Args((char *, char *));
#endif

#ifndef USE_READLINE
#define USE_READLINE  0
#endif
#ifndef allowBreak
#define allowBreak()
#endif
#ifndef local
#define local
#endif
#ifndef farCalloc
#define farCalloc(n,s)     (Void *)calloc(((unsigned)n),((unsigned)s))
#endif
#ifndef FOPEN_WRITE
#define FOPEN_WRITE        "w"
#endif
#ifndef FOPEN_APPEND
#define FOPEN_APPEND       "a"
#endif
#ifndef sigProto
#define sigProto(nm)       Void nm Args((int))
#define sigRaise(nm)       nm(1)
#define sigHandler(nm)     Void nm(sig_arg) int sig_arg;
#define sigResume          return
#endif
#ifndef Main                    /* to cope with systems that don't like    */
#define Main               Void /* main to be declared as returning Void   */
#endif
#ifndef MainDone
#define MainDone
#endif

#if (UNIX | DJGPP | RISCOS | ATARI | WATCOM)
#define ctrlbrk(bh)        signal(SIGINT,bh)
#endif

/*---------------------------------------------------------------------------
 * General settings:
 *-------------------------------------------------------------------------*/

#define Void     void   /* older compilers object to: typedef void Void;   */
typedef unsigned Bool;
#define TRUE     1
#define FALSE    0
typedef char    *String;
typedef int      Int;
typedef long     Long;
typedef int      Char;
typedef unsigned Unsigned;

#ifndef STD_PRELUDE
#if     RISCOS
#define STD_PRELUDE        "prelude"
#else
#define STD_PRELUDE        "hugs.prelude"
#endif
#endif

#define NUM_SYNTAX         100
#define NUM_SELECTS        100
#define NUM_FILES          20
#define NUM_MODULES        64
#define NUM_FIXUPS         100
#define NUM_TUPLES         100
#define NUM_OFFSETS        1024
#define NUM_CHARS          256

#if     SMALL_HUGS                      /* the McDonalds mentality :-)     */
#define Pick(s,r,l)        s
#endif
#if     REGULAR_HUGS
#define Pick(s,r,l)        r
#endif
#if     LARGE_HUGS
#define Pick(s,r,l)        l
#endif

#define NUM_TYCON          Pick(60,    160,        160)
#define NUM_NAME           Pick(625,   2000,       16000)
#define NUM_CLASSES        Pick(20,    40,         40)
#define NUM_INSTS          Pick(150,   200,        400)
#define NUM_DICTS          Pick(500,   10000,      32000)
#define NUM_TEXT           Pick(7000,  20000,      80000)
#define NUM_TEXTH          Pick(1,     10,         10)
#define NUM_TYVARS         Pick(800,   2000,       4000)
#define NUM_STACK          Pick(1800,  12000,      16000)
#define NUM_ADDRS          Pick(28000, 60000,      320000)
#define MINIMUMHEAP        Pick(7500,  7500,       7500)
#define MAXIMUMHEAP        Pick(32765, 0,          0)
#define DEFAULTHEAP        Pick(16000, 50000,      100000)
#define MAXPOSINT          Pick(32767, 2147483647, 2147483647)
#define NUM_DTUPLES        Pick(3,     5,          5)
#define BIGBASE            Pick(100,   10000,      10000)
#define BIGEXP             Pick(2,     4,          4)

#define minRecovery        Pick(1000,  1000,       1000)
#define bitsPerWord        Pick(16,    32,         32)
#define wordShift          Pick(4,     5,          5)
#define wordMask           Pick(15,    31,         31)

#define bitArraySize(n)    ((n)/bitsPerWord + 1)
#define placeInSet(n)      ((-(n)-1)>>wordShift)
#define maskInSet(n)       (1<<((-(n)-1)&wordMask))

#ifndef __GNUC__
#if !RISCOS
extern Int      strcmp     Args((String, String));
extern Int      strlen     Args((String));
extern char     *strcpy    Args((String,String));
extern char     *strcat    Args((String,String));
#endif
#endif
/* --- hier ge„ndert --- */
#if !LINUX  &&  !OS2
extern char     *getenv    Args((char *));
extern int      system     Args((const char *));
extern double   atof       Args((char *));
#endif
extern char     *strchr    Args((char *,int));  /* test membership in str  */
#if !DJGPP
extern Void     exit       Args((Int));
#endif
extern Void     internal   Args((String));
extern Void     fatal      Args((String));

#if     HAS_FLOATS

#if     (REGULAR_HUGS | LARGE_HUGS) & BREAK_FLOATS
#define FloatImpType       double
#define FloatPro           double
#define FloatFMT           "%.9g"
#else
#define FloatImpType       float
#define FloatPro           double  /* type to use in prototypes            */
                                   /* strictly ansi (i.e. gcc) conforming  */
                                   /* but breaks data hiding :-(           */
#define FloatFMT           "%g"
#endif
#else
#define FloatImpType       int     /*dummy*/
#define FloatPro           int
#define FloatFMT           "%d"
#endif

#ifndef FILENAME_MAX       /* should already be defined in an ANSI compiler*/
#define FILENAME_MAX 256
#else
#if     FILENAME_MAX < 256
#undef  FILENAME_MAX
#define FILENAME_MAX 256
#endif
#endif

/* --- hier auch ge„ndert: --- */
#ifdef  OS2
#define DEF_EDITOR         "tedit"
#define DEF_EDITLINE       "tedit"
#else
#define DEF_EDITOR         "vi"                 /* replace with ((char *)0)*/
#define DEF_EDITLINE       "vi +%d %s"          /* if no default editor rqd*/
#endif

/*-------------------------------------------------------------------------*/
