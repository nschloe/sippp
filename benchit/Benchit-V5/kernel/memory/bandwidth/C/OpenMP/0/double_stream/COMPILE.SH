#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: developer@benchit.org
#
# Last change by: $Author: rschoene $
# $Revision: 1.2 $
# $Date: 2007/02/13 13:03:26 $
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

LOCAL_KERNEL_COMPILER="${BENCHIT_CC}"
LOCAL_KERNEL_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} ${BENCHIT_CC_C_FLAGS_OMP}"
LOCAL_BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"

export LOCAL_KERNEL_COMPILER BENCHIT_COMPILERFLAGS

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
printf "${LOCAL_KERNEL_COMPILER} ${LOCAL_KERNEL_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES} -c work.c\n"
${LOCAL_KERNEL_COMPILER} ${LOCAL_KERNEL_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES} -c work.c


printf "${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES} -c kernel_main.c\n"
${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES} -c kernel_main.c

printf "${LOCAL_BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c\n"
${LOCAL_BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c

#
# SECOND STAGE: LINK
# note that the executable main is located in the main directory
#
printf "${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS} ${BENCHIT_CC_C_FLAGS_OMP}\n"
${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS} ${BENCHIT_CC_C_FLAGS_OMP}

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
# Revision 1.2  2007/02/13 13:03:26  rschoene
# changed work.f to work.c
#
# Revision 1.1  2007/02/13 12:46:12  rschoene
# STREAM Benchmar C + OpenMP
#
#
#####################################################################
