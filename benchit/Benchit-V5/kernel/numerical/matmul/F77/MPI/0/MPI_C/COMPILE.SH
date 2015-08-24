#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: william $
# $Revision: 1.3 $
# $Date: 2006/01/10 17:56:26 $
#####################################################################

# If kernel name string is set, this COMPILE.SH is sourced
# from the global COMPILE.SH. If it's not set, the user
# executed this script directly so we need to call configure.
if [ -z "${BENCHIT_KERNELNAME}" ]; then
	# Go to COMPILE.SH directory
	cd `dirname ${0}` || exit 1

	# Set configure mode
	CONFIGURE_MODE="COMPILE"

	# Load the definitions
	. ../../../../../../../tools/configure
fi

# check wether compilers and libraries are available
if [ -z "${BENCHIT_CC}" ]; then
	echo "No C compiler specified - check \$BENCHIT_CC"
	exit 1
fi

if [ -z "${BENCHIT_F77}" ]; then
	echo "No F77 compiler specified - check \$BENCHIT_F77"
	exit 1
fi

BENCHIT_COMMENT="Matrix Multiplication in F77 with MPI (C)"
export BENCHIT_COMMENT

# set up c-compiler
BENCHIT_C_COMPILER="${BENCHIT_MPICC}"
BENCHIT_C_COMPILERFLAGS="${BENCHIT_MPICC_C_FLAGS} ${BENCHIT_MPICC_C_FLAGS_STD} ${BENCHIT_MPICC_C_FLAGS_HIGH}"
BENCHIT_C_COMPILE="${BENCHIT_C_COMPILER} ${BENCHIT_C_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"

# set up f77-compiler
BENCHIT_F_COMPILER="${BENCHIT_MPIF77}"
BENCHIT_F_COMPILERFLAGS="${BENCHIT_MPIF77_C_FLAGS} ${BENCHIT_MPIF77_C_FLAGS_STD} ${BENCHIT_MPIF77_C_FLAGS_HIGH}"
BENCHIT_F_COMPILE="${BENCHIT_F_COMPILER} ${BENCHIT_F_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"


export BENCHIT_COMPILER BENCHIT_COMPILERFLAGS
export BENCHIT_COMMENT

#
# ENVIRONMENT HASHING - creates bienvhash.c
#
cd "${BENCHITROOT}/tools/"
set > "tmp.env"
./fileversion >> tmp.env
./envhashbuilder
cd "${KERNELDIR}"



#
# FIRST STAGE: COMPILE
#
printf "${BENCHIT_F_COMPILE}: \n" ;
printf "   matmul.f \n"
${BENCHIT_F_COMPILE} matmul.f

printf "${BENCHIT_C_COMPILE}: \n" ;
printf "   matmul1.c \n"
${BENCHIT_C_COMPILE} matmul1.c
printf "   matmul1_funcs.c \n"
${BENCHIT_C_COMPILE} matmul1_funcs.c
printf "   benchit.c ${BENCHIT_CPP_MPI} \n"
${BENCHIT_C_COMPILE} "${BENCHITROOT}/benchit.c" ${BENCHIT_CPP_MPI}



#
# SECOND STAGE: LINK
# note that the executable main is located in the main directory
#
printf "${BENCHIT_C_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_MPICC_L_FLAGS}\n"
${BENCHIT_C_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_MPICC_L_FLAGS}

#
# REMOVE *.o FILES
#
rm -f "${KERNELDIR}/"*.o

#
# CHANGE TO THE MAIN DIRECTORY
#
cd "${BENCHITROOT}"


#####################################################################
# Log-History
#
# $Log: COMPILE.SH,v $
# Revision 1.3  2006/01/10 17:56:26  william
# changed variables to new names
#
# Revision 1.2  2005/12/14 21:42:46  hackenb
# modified/unified header and footer
#
# Revision 1.1  2005/12/06 15:48:22  william
# created two matmul-fortran-kernels for mpi - first implements mpi via fortran - second via c - hence the names
#
####################################################################
