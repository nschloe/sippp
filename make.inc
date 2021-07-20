# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#  LAPACKDIR:=/usr/local/lib
#  LIBLAPACK:=-llapack-ifort

#  BLASDIR:=/usr/local/lib
#  LIBBLAS:=-lblas-ifort

#  STOPWATCHDIR:=/usr/local/lib
#  LIBSTOPWATCH:=-lstopwatch

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  F90C       := gfortran
  F90OPTS    := -g -O2
#  F90OPTS    := -O4
  LINKER     := gfortran
  LINKOPTS   := -O2

  ARCH      := ar
  ARCHFLAGS := cr

  RANLIB    := ranlib

  ECHOCMD  := /bin/echo -e

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
