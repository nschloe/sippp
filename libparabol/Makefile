# ./libparabol/Makefile

include ../make.inc

TARGET:=libparabol.a

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = kindmod.f         \
          errormod.f        \
          intermod.f      \
          meshes.f          \
          rk_parameters.f   \
          preconditioners.f \
          solvers.f         \
          runge-kutta.f     \
          put.f

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.f=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# 
# default lib: parabolic
# 

default: parabolic

parabolic: $(OBJECTS)
	@$(ECHOCMD) -n "\nCreating $(TARGET) ..."
	@$(ARCH) $(ARCHFLAGS) $(TARGET) $(OBJECTS)
	@$(ECHOCMD) " done.\n"

%.o: %.f
	$(F90C) $(F90OPTS) -c $<

.PHONY: clean

clean:
	@rm -f $(OBJECTS)
	@rm -f *.mod

realclean: clean
	@rm -f $(TARGET)
	@rm -f *~
