# ./elliptic/Makefile

include ../make.inc

TARGET:=ellipticSolve.out

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = params.f90         \
          solve_elliptic.f90 \
          put_elliptic.f90   \
          main_elliptic.f90

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.f90=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

elliptic: common $(OBJECTS)
	@$(ECHOCMD) -n "\nCreating $(TARGET) ..."
	@$(LINKER) $(LINKOPTS) $(OBJECTS) ../$(COMMONDIR)/*.o -L$(LAPACKDIR) $(LIBLAPACK) -o $(TARGET)
	@$(ECHOCMD) " done.\n"

common:
	@( cd ../$(COMMONDIR); $(MAKE) )

%.o: %.f90
	$(F90C) $(F90OPTS) -c -I../$(COMMONDIR) $<

.PHONY: clean

clean: commonclean
	@rm -f $(OBJECTS)
	@rm -f *.mod

commonclean:
	@( cd ../$(COMMONDIR); $(MAKE) clean )