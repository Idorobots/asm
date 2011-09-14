#GDC makefile.

DC = gdc
DFLAGS = -Wall -Wextra -pedantic -I../src
GDB = -funittest -fdebug -ggdb3
DLDLIBS = 

VPATH = ../src/utils

OBJS = lexer.o testing.o exception.o ctfe.o

TARGET = lexer

all : $(TARGET)

$(TARGET) : $(OBJS)
	$(DC) $^ $(DLDLIBS) -o$@

%.o : %.d
	$(DC) $(DFLAGS) $(GDB) $< -c

clean:
	rm -f $(TARGET)
	rm -f *.o