#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: rschoene $
# $Revision: 1.3 $
# $Date: 2006/05/09 09:39:05 $
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

BENCHIT_C_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD}"
C_COMPILE="${BENCHIT_CC} ${BENCHIT_C_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"

BENCHIT_F_COMPILERFLAGS="${BENCHIT_F77_C_FLAGS} ${BENCHIT_F77_C_FLAGS_HIGH}"
F_COMPILE="${BENCHIT_F77} ${BENCHIT_F_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"

BENCHIT_COMPILER=${BENCHIT_F77}
BENCHIT_COMPILERFLAGS=${BENCHIT_F_COMPILERFLAGS}

export BENCHIT_C_COMPILER BENCHIT_C_COMPILERFLAGS
export BENCHIT_COMPILER BENCHIT_COMPILERFLAGS

#
# ENVIRONMENT HASHING - creates bienvhash.c
#
cd ${BENCHITROOT}/tools/
set > tmp.env
./fileversion >> tmp.env
./envhashbuilder
cd ${KERNELDIR}

#
# FIRST STAGE: COMPILE
#
printf "${F_COMPILE} -c matmul_f_core.f\n"
${F_COMPILE} -c matmul_f_core.f
printf "${C_COMPILE} -c matmul_sub.c\n"
${C_COMPILE} -c matmul_sub.c

printf "${C_COMPILE} -c ${BENCHITROOT}/benchit.c\n"
${C_COMPILE} -c ${BENCHITROOT}/benchit.c

#
# SECOND STAGE: LINK
# note that the executable main is located in the main directory
#
printf "${C_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_F77_L_FLAGS}\n"
${C_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_F77_L_FLAGS}

#
# REMOVE *.o FILES
#
rm -f ${KERNELDIR}/*.o

#####################################################################
# Log-History
#
# $Log: COMPILE.SH,v $
# Revision 1.3  2006/05/09 09:39:05  rschoene
# now Compiler and Flags are set
#
# Revision 1.2  2006/04/24 12:07:54  rschoene
# changed flags, removed comment...
#
# Revision 1.1  2006/04/24 11:16:11  rschoene
# initial commit for matmul-f77-int (mostly taken from matmul-f77-double)
#
#
#####################################################################
