# ./rdparabol/Makefile

include ../make.inc

TARGET:=rdparabol2D.out

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = params_generic.f90     \
          meshes.f90             \
          solve_rdparabol.f90    \
          put_parabol.f90        \
          main_rdparabol.f90

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.f90=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SPARSEDIR := ../sparse
LIBSPARSE := -lsparse

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# 
# default lib: rdparabol2D
# 
rdparabol: common sparse $(OBJECTS)
	@$(ECHOCMD) -n "\nCreating $(TARGET) ..."
	@$(LINKER) $(LINKOPTS) $(OBJECTS) ../$(COMMONDIR)/*.o -L$(SPARSEDIR) $(LIBSPARSE) -L$(LAPACKDIR) $(LIBLAPACK) -o $(TARGET)
	@$(ECHOCMD) " done.\n"

common:
	@( cd ../$(COMMONDIR); $(MAKE) )

sparse:
	@( cd $(SPARSEDIR); $(MAKE) )

%.o: %.f90
	$(F90C) $(F90OPTS) -c -I../$(COMMONDIR) -I$(SPARSEDIR)  $<

.PHONY: clean

clean:
	@rm -f $(OBJECTS)
	@rm -f *.mod

realclean: clean commonclean

commonclean:
	@( cd ../$(COMMONDIR); $(MAKE) clean )