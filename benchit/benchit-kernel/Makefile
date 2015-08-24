# ./rdparabol/Makefile

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = kindmod.F         \
          errormod.F        \
	  rk_parameters.F   \
	  tridiag.F         \
	  params_generic.F  \
	  meshes.F          \
	  operators.F       \
	  preconditioners.F \
	  solvers.F         \
          kernel_main.F
# #           atimes.f90          \
# #           gmres_test.f90
#           main.f90

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

F90C       := ifort
F90OPTS    := -free -g -O2 -check all -warn all
# F90OPTS    := -free -O4
# LINKER     := ifort
# LINKOPTS   := -O2 -check all -warn all

ECHOCMD  := /bin/echo -e

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.F=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# 
# default lib: parabolic-1D
# 
default: $(OBJECTS)
	@$(ECHOCMD) -n "\nAll done.\n"

%.o: %.F
	$(F90C) $(F90OPTS) -c $<

.PHONY: clean

clean:
	@rm -f $(OBJECTS)
	@rm -f *.mod
