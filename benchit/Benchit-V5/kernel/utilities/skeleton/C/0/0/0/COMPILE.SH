#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: hackenb $
# $Revision: 1.14 $
# $Date: 2006/10/11 11:39:54 $
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

# check whether compilers and libraries are available
if [ -z "${BENCHIT_CC}" ]; then
	echo "No C compiler specified - check \$BENCHIT_CC"
	exit 1
fi

BENCHIT_COMMENT="C skeleton kernel"
export BENCHIT_COMMENT


# the compiler and its flags for the benchit.c
LOCAL_BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_DEFINES}"
# the compiler for the measurement kernel
LOCAL_KERNEL_COMPILER="${BENCHIT_CC}"
# the compilerflags for the measurement kernel
LOCAL_KERNEL_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} ${BENCHIT_INCLUDES}"
# the linkerflags
LOCAL_LINKERFLAGS="${BENCHIT_CC_L_FLAGS}"

# COMPILER-variables should appear in resultfile...
export LOCAL_BENCHITC_COMPILER LOCAL_KERNEL_COMPILER LOCAL_KERNEL_COMPILERFLAGS LOCAL_LINKERFLAGS

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
printf "${LOCAL_KERNEL_COMPILER} ${LOCAL_KERNEL_COMPILERFLAGS} -c work.c kernel_main.c\n"
${LOCAL_KERNEL_COMPILER} ${LOCAL_KERNEL_COMPILERFLAGS} -c work.c kernel_main.c

printf "${LOCAL_BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c\n"
${LOCAL_BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c

#
# SECOND STAGE: LINK
#
printf "${LOCAL_KERNEL_COMPILER} -o ${BENCHIT_KERNELBINARY} *.o ${LOCAL_LINKERFLAGS}\n"
${LOCAL_KERNEL_COMPILER} -o ${BENCHIT_KERNELBINARY} *.o ${LOCAL_LINKERFLAGS}


#
# REMOVE *.o FILES
#
rm -f ${KERNELDIR}/*.o



#####################################################################
# Log-History
#
# $Log: COMPILE.SH,v $
# Revision 1.14  2006/10/11 11:39:54  hackenb
# COMPILE.SH now in new style and hopefully less confusing...
#
# Revision 1.13  2006/01/03 09:39:04  mickler
# # Unified header and footer
# # New environment variable names
#
# Revision 1.12  2005/12/15 15:44:18  hackenb
# modified/unified header and footer
#
# Revision 1.11  2005/12/15 10:22:43  rschoene
# works with new variables
#
# Revision 1.10  2005/11/22 01:26:53  mickler
# + Using BI_GET_CALL_OVERHEAD_FUNC macro now
# + Using bi_timer() function for time measuring
#
# Revision 1.9  2005/11/04 16:21:24  mickler
# - benchit.c was being compiled with $BENCHIT_COMPILER and -O3.
#   Changed that, now extra entry $BENCHITC_COMPILER with correct
#   flags included
# - Fixed missing $BENCHIT_DEFINES in some kernel compilations
#
# Revision 1.8  2005/09/11 22:52:28  mickler
# + Support for special characters in pathnames to compilers
# + Output of compiler call
#
# Revision 1.7  2005/08/30 22:24:00  mickler
# + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
#   BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
#   main BenchIT files (benchit.c, JBI.java, ...)
# - FIXED: user settings for e.g. linear measurement were not used,
#   added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
#
# Revision 1.6  2005/08/30 13:27:26  mickler
# - Kernelname without the comment for the database
# + Renamed BENCHIT_KERNEL_NAME_STRING to BENCHIT_KERNELNAME
# + Sanity-check for the comment
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
