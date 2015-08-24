#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: mueller $
# $Revision: 1.2 $
# $Date: 2006/05/23 08:10:47 $
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

BENCHIT_COMMENT="C skeleton kernel"
export BENCHIT_COMMENT

BENCHIT_COMPILER="${BENCHIT_CC}"
BENCHIT_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH}"
BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"
BENCHIT_LINKER_FLAGS="${BENCHIT_CC_L_FLAGS} ${BENCHIT_LIB_PTHREAD}"

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
#printf "${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES} -c work.c\n"
#${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES} -c work.c
printf "${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES} -c kernel_main.c\n"
${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES} -c kernel_main.c

printf "${BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c\n"
${BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c

printf "${BENCHITC_COMPILER} -c ${BENCHITROOT}/tools/benchscript.c\n"
${BENCHITC_COMPILER} -c ${BENCHITROOT}/tools/benchscript.c

#
# SECOND STAGE: LINK
# note that the executable main is located in the main directory
#
printf "${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS}\n"
${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_LINKER_FLAGS}

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
# Revision 1.2  2006/05/23 08:10:47  mueller
# - changed location of benchscript files to tools/
#
# Revision 1.1  2006/05/22 10:15:04  mueller
# - adding perl select kernel
#
#
#####################################################################
