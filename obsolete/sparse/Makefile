# ./common/Makefile

TARGET = libsparse.a

include ../make.inc

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = sparsemod.f90 \
          itsolvmod.f90

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.f90=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

default: $(OBJECTS)
	@$(ECHOCMD) -n "\nCreating $(TARGET)..."
	@$(ARCH) $(ARCHFLAGS) $(TARGET) $(OBJECTS)
# 	$(RANLIB) $(COMMONLIBNAME)
	@$(ECHOCMD) " done.\n"

%.o: %.f90
	$(F90C) -c $(F90OPTS) -I../$(COMMONDIR) $<

.PHONY: clean

clean:
	@rm -f *.o
	@rm -f *.mod
# 	@rm -f $(COMMONLIBNAME)
