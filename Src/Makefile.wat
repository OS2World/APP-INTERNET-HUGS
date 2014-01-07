IOBJECTS	= hugs.obj builtin.obj machine.obj output.obj $(OBJECTS)
OBJECTS		= storage.obj input.obj static.obj type.obj compiler.obj
CC		= wcl386
CFLAGS		= /l=dos4g /zp4 /5r /zq /oilr
#CFLAGS		= /l=dos4g /oilr /zp4 /5r /zq
#CFLAGS		= /l=win386/bt=windows

hugs.exe	: $(IOBJECTS)
		  wlink @wathugs.lnk

.c.obj		:
		  $(CC) -c $(CFLAGS) $<

hugs.obj	: prelude.h storage.h connect.h errors.h command.h machdep.c commonui.c hugs.c
storage.obj	: prelude.h storage.h connect.h errors.h storage.c
input.obj	: prelude.h storage.h connect.h errors.h parser.c command.h input.c
static.obj	: prelude.h storage.h connect.h errors.h scc.c static.c
type.obj	: prelude.h storage.h connect.h errors.h preds.c kind.c subst.c type.c
output.obj	: prelude.h storage.h connect.h errors.h output.c
compiler.obj	: prelude.h storage.h connect.h errors.h compiler.c
machine.obj	: prelude.h storage.h connect.h errors.h machine.c
builtin.obj	: prelude.h storage.h connect.h errors.h bignums.c builtin.c

