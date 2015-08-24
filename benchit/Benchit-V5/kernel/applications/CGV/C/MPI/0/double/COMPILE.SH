#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: developer@benchit.org
#
# Last change by: $Author: hackenb $
# $Revision: 1.1 $
# $Date: 2007/01/22 14:54:19 $
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
if [ -z "${BENCHIT_MPICC}" ]; then
	echo "No C compiler specified - check \$BENCHIT_MPICC"
	exit 1
fi


# the compiler and its flags for the benchit.c
LOCAL_BENCHITC_COMPILER="${BENCHIT_MPICC} ${BENCHIT_MPICC_C_FLAGS} ${BENCHIT_MPICC_C_FLAGS_STD} ${BENCHIT_DEFINES}"
# the compiler for the measurement kernel
LOCAL_KERNEL_COMPILER="${BENCHIT_MPICC}"
# the compilerflags for the measurement kernel
LOCAL_KERNEL_COMPILERFLAGS="${BENCHIT_MPICC_C_FLAGS} ${BENCHIT_MPICC_C_FLAGS_HIGH} ${BENCHIT_INCLUDES}"
# the linkerflags
LOCAL_LINKERFLAGS="${BENCHIT_MPICC_L_FLAGS}"

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
printf "${LOCAL_KERNEL_COMPILER} -Drestrict= ${LOCAL_KERNEL_COMPILERFLAGS} -c cgv.c kernel_main.c\n"
${LOCAL_KERNEL_COMPILER} -Drestrict= ${LOCAL_KERNEL_COMPILERFLAGS} -c cgv.c kernel_main.c

printf "${LOCAL_BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c ${BENCHIT_CPP_MPI}\n"
${LOCAL_BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c ${BENCHIT_CPP_MPI}

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
# Revision 1.1  2007/01/22 14:54:19  hackenb
# + initial commit :-)
#
#
#####################################################################
