#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: william $
# $Revision: 1.1 $
# $Date: 2007/06/05 18:07:11 $
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


BENCHIT_COMMENT="Simple Fortran kernel"
export BENCHIT_COMMENT

BENCHIT_COMPILER="${BENCHIT_F77}"
BENCHIT_COMPILERFLAGS="${BENCHIT_F_COMPILERFLAGS}"
export BENCHIT_COMPILER BENCHIT_COMPILERFLAGS



##########################

BENCHIT_F_COMPILER="${BENCHIT_F77}"
BENCHIT_F_COMPILER="${BENCHIT_F_COMPILER} ${BENCHIT_F77_C_FLAGS}"
BENCHIT_F_COMPILER="${BENCHIT_F_COMPILER} ${BENCHIT_F77_C_FLAGS_HIGH}"
BENCHIT_F_COMPILER="${BENCHIT_F_COMPILER} ${BENCHIT_INCLUDES}"
BENCHIT_F_COMPILER="${BENCHIT_F_COMPILER} ${BENCHIT_DEFINES}"

BENCHIT_C_COMPILER="${BENCHIT_CC}" 
BENCHIT_C_COMPILER="${BENCHIT_C_COMPILER} ${BENCHIT_CC_C_FLAGS}"   
BENCHIT_C_COMPILER="${BENCHIT_C_COMPILER} ${BENCHIT_CC_C_FLAGS_HIGH}"
BENCHIT_C_COMPILER="${BENCHIT_C_COMPILER} ${BENCHIT_INCLUDES}"
BENCHIT_C_COMPILER="${BENCHIT_C_COMPILER} ${BENCHIT_DEFINES}"

##########################



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
printf "\n"
printf "###########"
printf "\n"
printf "# COMPILE #"
printf "\n"
printf "###########"
printf "\n"
sync

printf "${BENCHIT_F_COMPILER} -c simple.f\n"
${BENCHIT_F_COMPILER} -c simple.f
printf "\n"

#printf "${BENCHIT_C_COMPILER} -c simple.c\n"
#${BENCHIT_C_COMPILER} -c simple.c
#printf "\n"

printf "${BENCHIT_C_COMPILER} -c kernel_main.c\n"
${BENCHIT_C_COMPILER} -c kernel_main.c
printf "\n"

printf "${BENCHIT_C_COMPILER} -c \"${BENCHITROOT}/benchit.c\"\n"
${BENCHIT_C_COMPILER} -c ${BENCHITROOT}/benchit.c
printf "\n"

#
# SECOND STAGE: LINK
# note that the executable is located in the main directory
#
printf "\n"
printf "########"
printf "\n"
printf "# LINK #"
printf "\n"
printf "########"
printf "\n"
sync

printf "${BENCHIT_F_COMPILER} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_F77_L_FLAGS}\n"
${BENCHIT_F_COMPILER} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_F77_L_FLAGS}
printf "\n"


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
# Revision 1.1  2007/06/05 18:07:11  william
# This Skeleton shows how to compile Fortran Codeagainst the BenchIT-Interface written in CFor more information regarding Fortran have a lookat our wiki:http://www.benchit.org/wiki/index.php/Fortran
#
# Revision 1.8  2006/07/01 15:42:45  william
# a missing -c prevented the benchit.c from compiling correctly
# - this bug was already fixed for the stable version but was still existent
#   in the developerversion
#
# Revision 1.7  2006/01/10 07:31:48  hackenb
# *** keyword substitution change ***
#
#
#####################################################################
