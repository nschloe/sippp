#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: hackenb $
# $Revision: 1.5 $
# $Date: 2006/01/03 16:37:29 $
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
BENCHIT_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH}"
BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES}"

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
echo ${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES} -c memory_latencies_init.c memory_latencies_entry.c
${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES} -c memory_latencies_init.c memory_latencies_entry.c

echo ${BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c
${BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c
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
# Log-History
#
# $Log: COMPILE.SH,v $
# Revision 1.5  2006/01/03 16:37:29  hackenb
# modified/unified header and footer
# new interface
#
# Revision 1.4  2006/01/03 10:42:57  mickler
# # Unified header and footer
# # New environment variable names
#
# Revision 1.3  2005/11/04 16:21:22  mickler
# - benchit.c was being compiled with $BENCHIT_COMPILER and -O3.
#   Changed that, now extra entry $BENCHITC_COMPILER with correct
#   flags included
# - Fixed missing $BENCHIT_DEFINES in some kernel compilations
#
# Revision 1.2  2005/08/30 22:23:59  mickler
# + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
#   BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
#   main BenchIT files (benchit.c, JBI.java, ...)
# - FIXED: user settings for e.g. linear measurement were not used,
#   added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
#
# Revision 1.1  2005/08/04 11:59:05  mark
# memory latencies
#
# Revision 1.5  2005/07/29 11:53:51  wloch
# removed setting of kernelname in bi getinfo and equalized parameter variables
#
# Revision 1.4  2005/07/22 10:35:04  wloch
# replaced common block by minimal settings
#
# Revision 1.3  2005/07/20 14:24:36  mickler
# # Moved PARAMETERS loading from configure to COMPILE.SH
# + Finished cmdlineparams logic
#
# Revision 1.2  2005/07/19 13:35:06  mickler
# + Moved unnecessary things to configure script
# + Kernel name finding in configure
#
# Revision 1.1.1.1  2005/07/18 13:03:20  wloch
# the final restructured benchit source tree
#
# Revision 1.6  2005/06/21 14:20:30  wloch
# fine tuning of new interface usage
#
# Revision 1.5  2005/06/21 12:59:00  wloch
# moved common code to configure
#
# Revision 1.4  2005/06/21 12:38:22  wloch
# debugged bin path
#
# Revision 1.3  2005/06/21 12:27:24  wloch
# adapted to new interface in JBI and benchit c
#
# Revision 1.2  2005/06/16 15:16:40  wloch
# adapted environment hashing changes to bi getenv
#
# Revision 1.1  2005/02/11 10:29:40  wloch
# code skeleton for c kernels
#
# Revision 2.0  2003/12/09 11:18:52  juckel
# build of version 2.0
#
#####################################################################
