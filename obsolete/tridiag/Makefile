# ./common/Makefile

TARGET = libtridiag.a

include ../make.inc

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = tridiag.f

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.f=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

default: $(OBJECTS)
	@$(ECHOCMD) -n "\nCreating $(TARGET)..."
	@$(ARCH) $(ARCHFLAGS) $(TARGET) $(OBJECTS)
# 	$(RANLIB) $(COMMONLIBNAME)
	@$(ECHOCMD) " done.\n"

%.o: %.f
	$(F90C) -c $(F90OPTS) -I../$(COMMONDIR) $<

.PHONY: clean

clean:
	@rm -f *.o
	@rm -f *.mod
# 	@rm -f $(COMMONLIBNAME)

realclean: clean
	@rm -f $(TARGET)
