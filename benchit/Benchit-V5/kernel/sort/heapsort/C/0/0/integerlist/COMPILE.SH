#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: william $
# $Revision: 1.1 $
# $Date: 2006/07/24 16:19:11 $
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

BENCHIT_COMMENT="Simple C kernel"
export BENCHIT_COMMENT

# set up c-compiler
BENCHIT_COMPILER="${BENCHIT_CC}"
BENCHIT_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH}"
BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"
C_COMPILE="${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} ${BENCHIT_INCLUDES} -c"

L_FLAGS="${BENCHIT_CC_L_FLAGS}"
C_LINK="${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS}"

export BENCHIT_COMPILER BENCHIT_COMPILERFLAGS L_FLAGS

#
# ENVIRONMENT HASHING - creates bienvhash.c
#
cd ${BENCHITROOT}/tools/
set > tmp.env
./fileversion >> tmp.env
./envhashbuilder
cd ${KERNELDIR}

#
# COMPILE
#
printf "${C_COMPILE} parser.c\n"
${C_COMPILE} parser.c
printf "${C_COMPILE} simple.c\n"
${C_COMPILE} simple.c
printf "${C_COMPILE} kernel_main.c\n"
${C_COMPILE} kernel_main.c
printf "${BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c\n"
${BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c

#
# SECOND STAGE: LINK
# note that the executable is located in the main directory
#
printf "${C_LINK}\n"
${C_LINK}

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
# Revision 1.1  2006/07/24 16:19:11  william
# wrote, tested and submitted 4 different heapsort-versions
#
# Revision 1.7.2.1  2006/03/09 11:35:22  william
# ups - a missing "-c" in the line where benchit.c is compiled caused the whole compilation to fail with linker errors
#
# Revision 1.7  2006/01/10 07:31:48  hackenb
# *** keyword substitution change ***
#
#
#####################################################################
