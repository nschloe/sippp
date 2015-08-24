#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: rschoene $
# $Revision: 1.9 $
# $Date: 2006/10/10 15:09:13 $
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
if [ -z "${BENCHIT_MPICC}" ]; then
	echo "No C compiler specified - check \$BENCHIT_MPICC"
	exit 1
fi

BENCHIT_COMMENT=""
export BENCHIT_COMMENT

BENCHIT_COMPILER="${BENCHIT_MPICC}"
BENCHIT_COMPILERFLAGS="${BENCHIT_MPICC_C_FLAGS} ${BENCHIT_MPICC_C_FLAGS_HIGH}"
BENCHITC_COMPILER="${BENCHIT_MPICC} ${BENCHIT_MPICC_C_FLAGS} ${BENCHIT_MPICC_C_FLAGS_STD} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES}"

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
echo ${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES} -c randomaccess_init.c randomaccess_entry.c
${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES} -c randomaccess_init.c randomaccess_entry.c

echo ${BENCHITC_COMPILER} -DLINEAR_MEASUREMENT -c ${BENCHITROOT}/benchit.c
${BENCHITC_COMPILER} -DLINEAR_MEASUREMENT -c ${BENCHITROOT}/benchit.c
#
# SECOND STAGE: LINK
# note that the executable main is located in the main directory
#
echo ${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_MPICC_L_FLAGS}
${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_MPICC_L_FLAGS}

#
# REMOVE *.o FILES
#
rm -f ${KERNELDIR}/*.o

#
# CHANGE TO THE MAIN DIRECTORY
#
cd ${BENCHITROOT}


#####################################################################
# Log-History
#
# $Log: COMPILE.SH,v $
# Revision 1.9  2006/10/10 15:09:13  rschoene
# changed linker flag
#
# Revision 1.6  2006/01/03 18:25:09  hackenb
# modified/unified header and footer
# new interface
# now using bi_gettime()
#
# Revision 1.5  2006/01/03 10:42:58  mickler
# # Unified header and footer
# # New environment variable names
#
#####################################################################
