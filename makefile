#DMD makefile.

DC = dmd
DFLAGS = -wi -I./src
GDB = -unittest -debug #-gc
DLDLIBS =

VPATH = src:src/ASM:src/utils

TARGET = asm.exe

OBJS = main.obj interpreter.obj kit.obj parser.obj AST.obj lexical.obj testing.obj exception.obj ctfe.obj

all : $(TARGET)

$(TARGET) : $(OBJS)
	$(DC) $^ $(DLDLIBS) -of$@

%.obj : %.d
	$(DC) $(DFLAGS) $(GDB) $< -c

clean:
	rm -f *.obj
	rm -f $(TARGET)

