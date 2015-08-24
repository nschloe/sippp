# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#  LAPACKDIR:=/usr/local/lib
#  LIBLAPACK:=-llapack-ifort

#  BLASDIR:=/usr/local/lib
#  LIBBLAS:=-lblas-ifort

#  STOPWATCHDIR:=/usr/local/lib
#  LIBSTOPWATCH:=-lstopwatch

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  F90C       := ifort
  F90OPTS    := -free -g -O2 -check all -warn all
#  F90OPTS    := -free -O4
  LINKER     := ifort
  LINKOPTS   := -O2 -check all -warn all

  ARCH      := ar
  ARCHFLAGS := cr

  RANLIB    := ranlib

  ECHOCMD  := /bin/echo -e

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
