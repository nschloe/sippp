#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: william $
# $Revision: 1.1 $
# $Date: 2006/12/14 12:54:15 $
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

if [ -z "${BENCHIT_LIB_PTHREAD}" ]; then
	echo "No PTHREAD-Lib specified - check \$BENCHIT_LIB_PTHREAD"
	exit 1
fi

BENCHIT_COMMENT="IO Write Kernel"
export BENCHIT_COMMENT

# the compiler and its flags for the benchit.c
LOCAL_BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_DEFINES}"
# the compiler for the measurement kernel
LOCAL_KERNEL_COMPILER="${BENCHIT_CC}"
# the compilerflags for the measurement kernel
LOCAL_KERNEL_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} ${BENCHIT_INCLUDES}"
# the linkerflags
LOCAL_LINKERFLAGS="${BENCHIT_CC_L_FLAGS} ${BENCHIT_LIB_PTHREAD}"

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
printf "${LOCAL_KERNEL_COMPILER} ${LOCAL_KERNEL_COMPILERFLAGS} -c tpool.c kernel_main.c\n"
${LOCAL_KERNEL_COMPILER} ${LOCAL_KERNEL_COMPILERFLAGS} -c tpool.c kernel_main.c

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

#
# CHANGE TO THE MAIN DIRECTORY
#
cd ${BENCHITROOT}


#####################################################################
# Log-History
#
# $Log: COMPILE.SH,v $
# Revision 1.1  2006/12/14 12:54:15  william
# changed the algorithm a bit for mor informative resultfilesmade chenges to reflect new format of COMPILE.SH and a like
#
# Revision 1.2  2006/04/27 12:01:16  william
# altered, cleaned up and successfully tested the kernel
#
#
#####################################################################
