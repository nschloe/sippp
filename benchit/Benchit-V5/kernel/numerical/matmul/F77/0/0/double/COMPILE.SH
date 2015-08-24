#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: rschoene $
# $Revision: 1.11 $
# $Date: 2006/10/04 09:46:08 $
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
F_COMPILE="${BENCHIT_F77} ${BENCHIT_F_COMPILERFLAGS}"
BENCHIT_COMPILER="${BENCHIT_F77}"
BENCHIT_COMPILERFLAGS="${BENCHIT_F_COMPILERFLAGS}"
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
${C_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS}

#
# REMOVE *.o FILES
#
rm -f ${KERNELDIR}/*.o


#####################################################################
# Log-History
#
# $Log: COMPILE.SH,v $
# Revision 1.11  2006/10/04 09:46:08  rschoene
# changed BENCHIT_COMPILER(FLAGS)
#
# Revision 1.10  2006/05/10 13:43:04  william
# changes made to make the kernel compile on jump
#
# Revision 1.9  2006/05/09 09:39:05  rschoene
# now Compiler and Flags are set
#
# Revision 1.8  2006/04/24 12:47:27  rschoene
# setted compiler, removed comment...
#
# Revision 1.7  2006/01/10 15:56:18  william
# reformatted the output
#
# Revision 1.6  2006/01/10 15:33:29  william
# cleaned up variable-mess
#
# Revision 1.5  2006/01/03 15:30:16  mickler
# # Changed for MIN, MAX, INCREMENT
#
# Revision 1.4  2005/12/14 21:42:46  hackenb
# modified/unified header and footer
#
# Revision 1.3  2005/11/04 16:21:24  mickler
# - benchit.c was being compiled with $BENCHIT_COMPILER and -O3.
#   Changed that, now extra entry $BENCHITC_COMPILER with correct
#   flags included
# - Fixed missing $BENCHIT_DEFINES in some kernel compilations
#
# Revision 1.2  2005/09/12 14:00:05  mickler
# - Fixed: Use new variable names
#
# Revision 1.1  2005/08/20 15:48:22  rschoene
# Matrix Multiply for F77 checkin
# (merged from src3-matmulc and src2-matmul-f77)
#
# Revision 1.2  2005/07/29 11:53:51  wloch
# removed setting of kernelname in bi getinfo and equalized parameter variables
#
# Revision 1.1  2005/07/20 21:38:26  mickler
# # Added /numerical/matmul/C/0/0/double/ kernel
#
# Revision 1.2  2005/01/13 10:19:38  rschoene
# setted linker-flags
#
# Revision 1.1.1.1  2004/12/14 21:22:57  william
# Release 3.0 - created new cvs-tree src2
#
#####################################################################
