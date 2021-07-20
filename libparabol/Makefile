# ./libparabol/Makefile

include ../make.inc

TARGET:=libparabol.a

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = kindmod.f90         \
          errormod.f90        \
          intermod.f90      \
          meshes.f90          \
          rk_parameters.f90   \
          preconditioners.f90 \
          solvers.f90         \
          runge-kutta.f90     \
          put.f90

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.f90=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# 
# default lib: parabolic
# 

default: parabolic

parabolic: $(OBJECTS)
	@$(ECHOCMD) -n "\nCreating $(TARGET) ..."
	@$(ARCH) $(ARCHFLAGS) $(TARGET) $(OBJECTS)
	@$(ECHOCMD) " done.\n"

%.o: %.f90
	$(F90C) $(F90OPTS) -c $<

.PHONY: clean

clean:
	@rm -f $(OBJECTS)
	@rm -f *.mod

realclean: clean
	@rm -f $(TARGET)
	@rm -f *~
