#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: william $
# $Revision: 1.4 $
# $Date: 2006/01/03 14:00:06 $
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
	echo "no C compiler found - check your LOCALDEF"
	exit 1
fi
if [ -z "${BENCHIT_F77}" ]; then
	echo "no Fortran77 compiler found - check your LOCALDEF"
	exit 1
fi

# set up c-compiler
BENCHIT_COMPILER="${BENCHIT_CC}"
BENCHIT_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH}"
BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_DEFINES}"
C_COMPILE="${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} -I. -I${BENCHITROOT} -c"

# set up fortran-compiler
BENCHIT_COMPILER="${BENCHIT_F77}"
BENCHIT_COMPILERFLAGS="${BENCHIT_F77_C_FLAGS} ${BENCHIT_F77_C_FLAGS_HIGH}"
BENCHITF_COMPILER="${BENCHIT_F77} ${BENCHIT_F77_C_FLAGS} ${BENCHIT_F77_C_FLAGS_STD} ${BENCHIT_DEFINES}"
F_COMPILE="${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} -I. -I${BENCHITROOT} -c"

C_LINK="${BENCHITC_COMPILER} ${BENCHIT_COMPILERFLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS}"

export BENCHIT_COMPILER BENCHIT_COMPILERFLAGS LIBS

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
printf "${F_COMPILE}: \n" ;
printf "   vecadd.f \n"
${F_COMPILE} vecadd.f

printf "${C_COMPILE}: \n" ;
printf "   helper.c \n"
${C_COMPILE} helper.c
printf "   work.c \n"
${C_COMPILE} work.c
printf "   benchit.c \n"
${C_COMPILE} "${BENCHITROOT}/benchit.c"


#
# SECOND STAGE: LINK
# note that the executable main is located in the main directory
#
printf "${C_LINK}\n"
${C_LINK}

#
# REMOVE *.o FILES
#
rm -f "${KERNELDIR}/"*.o

#
# CHANGE TO THE MAIN DIRECTORY
#
cd "${BENCHITROOT}"

###############################################################################
#  Log-History
#
#  $Log: COMPILE.SH,v $
#  Revision 1.4  2006/01/03 14:00:06  william
#  fixed issues due to the different interfaceremoved BENCHIT_COMMENT
#
#  Revision 1.3  2005/12/19 12:46:23  william
#  fixed issues due to the different interface
#
#  Revision 1.2  2005/12/07 18:36:09  william
#  added cvs header and finished the README
#
#  Revision 1.1  2005/12/06 16:23:01  william
#  created several versions of kernels doing vector add
#
###############################################################################
