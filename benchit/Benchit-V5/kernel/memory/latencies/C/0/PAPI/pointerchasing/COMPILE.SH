#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: mickler $
# $Revision: 1.6 $
# $Date: 2006/01/03 10:42:58 $
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

BENCHIT_COMMENT=""
export BENCHIT_COMMENT

BENCHIT_COMPILER="${BENCHIT_CC}"
BENCHIT_COMPILERFLAGS="-DLINEAR_MEASUREMENT ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} ${BENCHIT_CPP_PAPI}"
BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES}"

BENCHIT_CC_L_FLAGS="${BENCHIT_CC_L_FLAGS} ${BENCHIT_LIB_PAPI}"

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
echo ${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES} -c  pointerchasing_init.c pointerchasing_entry.c pointerchasing_accesses.c
${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES} -c pointerchasing_init.c pointerchasing_entry.c pointerchasing_accesses.c

echo ${BENCHITC_COMPILER} -DERROR_CORRECTION=5 -c ${BENCHITROOT}/benchit.c
${BENCHITC_COMPILER} -DERROR_CORRECTION=5 -c ${BENCHITROOT}/benchit.c
#
# SECOND STAGE: LINK
# note that the executable main is located in the main directory
#
echo ${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS}
${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS}

#
# REMOVE *.o FILES
#
rm -f ${KERNELDIR}/*.o

#
# CHANGE TO THE MAIN DIRECTORY
#
cd ${BENCHITROOT}

###############################################################################
#  Log-History
#
#  $Log: COMPILE.SH,v $
#  Revision 1.6  2006/01/03 10:42:58  mickler
#  # Unified header and footer
#  # New environment variable names
#
#  Revision 1.5  2005/11/04 16:21:23  mickler
#  - benchit.c was being compiled with $BENCHIT_COMPILER and -O3.
#    Changed that, now extra entry $BENCHITC_COMPILER with correct
#    flags included
#  - Fixed missing $BENCHIT_DEFINES in some kernel compilations
#
#  Revision 1.4  2005/09/23 10:58:34  mark
#  *** empty log message ***
#
#  Revision 1.3  2005/09/16 05:42:25  mark
#  *** empty log message ***
#
#  Revision 1.2  2005/09/13 12:49:25  mark
#  PAPI counter in PARAMETERS file
#  near stepsize
#
#  Revision 1.1  2005/09/02 12:14:47  mark
#  *** empty log message ***
#
#  Revision 1.4  2005/09/02 11:44:10  mark
#  *** empty log message ***
#
#  Revision 1.3  2005/08/24 12:23:20  juckel
#  added NUMBER_OF_JUMPS to PARAMETERS
#
#  Revision 1.2  2005/08/16 16:20:02  juckel
#  some bugfixing
#
#  Revision 1.1  2005/08/16 14:20:53  juckel
#  - included kernel from old benchit into new structure
#  - changed MPI-functionality
#    old: only root process runs memory test -> generates 1 data set
#    new: for i=1 to numproc do ruu mesurement -> generates numproc data sets
#
#
