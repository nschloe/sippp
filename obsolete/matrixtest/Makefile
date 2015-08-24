# ./matrixnorm/Makefile

include ../make.inc

TARGET:=mtest.out

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

SOURCES = params_generic.f90 \
          meshes.f90         \
          matrices.f90       \
          inorm.f90          \
          boundfun.f90       \
          mtest.f90

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

OBJECTS := $(SOURCES:.f90=.o)

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

mtest: $(OBJECTS)
	@$(ECHOCMD) -n "\nCreating $(TARGET) ..."
	@$(LINKER) $(LINKOPTS) -o $(TARGET) $(OBJECTS) -L../$(COMMONDIR) $(LIBCOMMON) -L$(LAPACKDIR) $(LIBLAPACK) -L$(BLASDIR) $(LIBBLAS)
	@$(ECHOCMD) " done.\n"

%.o: %.f90
	$(F90C) $(F90OPTS) -c -I../$(COMMONDIR) $<

.PHONY: clean

clean:
	@rm -f $(OBJECTS)
	@rm -f $(TARGET)
	@rm -f *.mod