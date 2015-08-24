#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: william $
# $Revision: 1.3 $
# $Date: 2006/01/12 11:34:05 $
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

BENCHIT_COMMENT="pingpong pairwise allt-to-all"
export BENCHIT_COMMENT

BENCHIT_COMPILER="${BENCHIT_MPICC}"
BENCHIT_COMPILERFLAGS="${BENCHIT_MPICC_C_FLAGS} ${BENCHIT_MPICC_C_FLAGS_STD} ${BENCHIT_MPICC_C_FLAGS_HIGH}"
BENCHIT_COMPILE="${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"

export BENCHIT_COMPILER BENCHIT_COMPILERFLAGS BENCHIT_COMPILE

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
printf "${BENCHIT_COMPILE} -c pingpong.c\n"
${BENCHIT_COMPILE} -c pingpong.c
printf "${BENCHIT_COMPILE} -c kernel_main.c\n"
${BENCHIT_COMPILE} -c kernel_main.c

printf "${BENCHIT_COMPILE} -c ${BENCHITROOT}/benchit.c ${BENCHIT_CPP_MPI}\n"
${BENCHIT_COMPILE} -c ${BENCHITROOT}/benchit.c ${BENCHIT_CPP_MPI}

#
# SECOND STAGE: LINK
# note that the executable main is located in the main directory 
#
printf "${BENCHIT_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_MPICC_L_FLAGS}\n"
${BENCHIT_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_MPICC_L_FLAGS}

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
# Revision 1.3  2006/01/12 11:34:05  william
# cvs-keyword-problems with binary/ASCII flags
#
# Revision 1.5  2006/01/10 07:31:48  hackenb
# *** keyword substitution change ***
#
#
#####################################################################
