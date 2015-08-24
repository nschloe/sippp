# ./rdparabol/Makefile

include ../make.inc

TARGET:=main.out

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = parameters.f \
          tridiag.f    \
          operator.f   \
          main.f

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.f=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


LIBPARABOLDIR := ../libparabol
LIBPARABOL    := -lparabol

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# 

default: main

main: $(OBJECTS)
	@$(ECHOCMD) -n "\nCreating $(TARGET) ..."
	@$(LINKER) $(LINKOPTS) -o $(TARGET) $(OBJECTS) -L$(LIBPARABOLDIR) $(LIBPARABOL) -L$(LAPACKDIR) $(LIBLAPACK) -L$(BLASDIR)
	@$(ECHOCMD) " done.\n"

%.o: %.f
	$(F90C) $(F90OPTS) -c -I$(LIBPARABOLDIR) $<

.PHONY: clean

clean:
	@rm -f $(OBJECTS)
	@rm -f *.mod

realclean: clean
	@rm -f $(TARGET)
	@rm -f *~
