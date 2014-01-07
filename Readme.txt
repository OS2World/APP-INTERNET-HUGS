______________________________________________________________________________
     ___    ___   ___    ___   __________   __________
    /  /   /  /  /  /   /  /  /  _______/  /  _______/  Version 1.01
   /  /___/  /  /  /   /  /  /  / _____   /  /______
  /  ____   /  /  /   /  /  /  / /_   /  /______   /    Copyright (c)
 /  /   /  /  /  /___/  /  /  /___/  /  _______/  /     Mark P Jones
/__/   /__/  /_________/  /_________/  /_________/      1994, 1995

 The Haskell User's Gofer System.   Derived from Gofer 2.30b.

______________________________________________________________________________

This is the Readme file for Hugs version 1.01, the Haskell User's Gofer
System.  It provides a functional programming environment much like
that of Gofer but with a much greater degree of compatibility with
Haskell.  Almost all of the features of Haskell 1.2 are implemented
with the exception of the module system (like Gofer, module headers and
import declarations are parsed, but are otherwise ignored).  For
example, Hugs supports Haskell style type classes, a full prelude,
derived instances, defaults, overloaded numeric literals and pattern
matching, and bignum arithmetic.

Some of the fancier features of Gofer are not supported by Hugs.  In some
cases, this is necessary because they are incompatible with the Haskell
type system (overlapping instances, multiple parameter type classes, the
ability to use custom preludes,...).  Others have been removed because they
are experiments in language design that were appropriate for Gofer, but not
for Hugs (c*n patterns, do notation, overloaded monad comprehensions,
Orwell style guards,...).  If you're wondering, constructor classes *are*
supported.

Hugs was written on a PC while I was between jobs at Yale and Nottingham.
I'd been meaning to put together a system like this for some time, and have
had strong encouragement from certain quarters, but hadn't had the time to
work on it until then.  Gofer has been popular and has played a useful role
for me and for others.  But as time has passed and Haskell has become more
established as a standard, the differences and incompatibilities between
the two languages has become something of a pain and an embarrassment,
particularly as one of the goals in the design of Haskell was to `reduce
unnecessary diversity in functional programming languages'.

Hugs is derived from Gofer and, as a result, benefits from many of the
bug-fixes and enhancements that have been made to Gofer over the past
few years, a significant proportion of which have been suggested to me
by Gofer users.  However, there is also quite a lot of new code, so
please be prepared to find bugs, and please report them to me if you
do.  Bearing in mind that this is the first release, you should probably
expect to see some things change in future distributions.

I've held back the release of Hugs until now because I didn't have the
time to package it up and release it before.  Hugs has benefited from
the delay with the addition of things like finishing off the support
for bignum arithmetic, but there are still a couple of other things I'd
like to add in due course, the most obvious being 1.3 style monadic
IO.  There are also a couple of things that I've removed from this
distribution because they weren't quite ready for public consumption
and I didn't want to hold up the distribution any longer.  Watch this
space.

Other things that might be FAQs:

  - Since I wrote Hugs, I've discovered how to handle multiple
    parameter classes in a way that is both useful and fully compatible
    with the rest of the type system.  I'll probably hack that into the
    system when I get a chance.  (See my work on simplifying and
    improving qualified types for details.)

  - There are no plans at the moment for a hugsc compiler, analogous to
    gofc; I don't think that would be particularly difficult, but I
    don't have the time.

  - Documentation for Hugs is in preparation.  For the time being, if
    you are familiar with the Gofer system, then you aren't likely
    to have much difficulty with Hugs.  If you're interested in the
    implementation, then much of the report on the implementation of
    Gofer still applies.  The type checker is quite different though;
    the main technical achievement of Hugs is to make the old Gofer
    style approach to type classes (on which the implementation still
    depends) look like the Haskell type system.  I started to write a
    paper about this, but there's a long way to go before it's finished.

  - For the time being, I intend to continue maintaining both Gofer and
    Hugs, but I haven't decided what I'll do about future development.
    It is unlikely that I will be able to keep Gofer and Hugs in step
    with one another in terms of future development.

Your feedback, comments, suggestions and bug reports are most welcome!
However, this is a very busy time for me and I will almost certainly
be unable to respond quickly to messages about Hugs or Gofer during the
next month or two.

______________________________________________________________________________

You can obtain a copy of Hugs by anonymous ftp from:
     ftp://ftp.cs.nott.ac.uk/pub/haskell/hugs
 or: ftp://ftp.cs.nott.ac.uk/pub/nott-fp/hugs

Or, using the World Wide Web, from my home page:
     http://www.cs.nott.ac.uk/Department/Staff/mpj/index.html

______________________________________________________________________________
Mark P. Jones                                                mpj@cs.nott.ac.uk
______________________________________________________________________________
