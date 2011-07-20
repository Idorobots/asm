#DMD makefile.

DC = dmd
DFLAGS = -wi -I./src -I..
GDB = -unittest -debug #-gc
DLDLIBS =

VPATH = src:src/ASM:utils

TARGET = asm.exe

OBJS = main.obj interpreter.obj parser.obj AST.obj lexical.obj testing.obj exception.obj ctfe.obj

all : $(TARGET)

$(TARGET) : $(OBJS)
	$(DC) $^ $(DLDLIBS) -of$@

%.obj : %.d
	$(DC) $(DFLAGS) $(GDB) $< -c

clean:
	rm -f *.obj
	rm -f $(TARGET)

