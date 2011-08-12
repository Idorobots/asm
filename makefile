#makefile for ASM language interpreter
#The default compiler is GDC, for DMD build `make dmd`

GDCMAKE = Makefile.gdc
DMDMAKE = Makefile.dmd

default: gdc

gdc:
	$(MAKE) -f $(GDCMAKE)

dmd:
	$(MAKE) -f $(DMDMAKE)

clean: gdc-clean

gdc-clean:
	$(MAKE) -f $(GDCMAKE) clean

dmd-clean:
	$(MAKE) -f $(DMDMAKE) clean

