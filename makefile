#DMD makefile.

DC = dmd
DFLAGS = -wi -I./src -I..
GDB = -unittest -debug #-gc
DLDLIBS =

VPATH = src:src/ASM:utils

OBJS = main.obj interpreter.obj parser.obj AST.obj lexical.obj testing.obj exception.obj ctfe.obj

all : asm.exe

asm.exe : $(OBJS)
	$(DC) $^ $(DLDLIBS) -of$@

%.obj : %.d
	$(DC) $(DFLAGS) $(GDB) $< -c

clean:
	rm -f *.obj

