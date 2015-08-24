#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: hackenb $
# $Revision: 1.11 $
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


# the compiler and its flags for the benchit.c
LOCAL_BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_DEFINES}"
# the compiler for the measurement kernel
LOCAL_KERNEL_COMPILER="${BENCHIT_CC}"
# the compilerflags for the measurement kernel
LOCAL_KERNEL_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} ${BENCHIT_CPP_ACML} ${BENCHIT_INCLUDES}"
# the linkerflags
LOCAL_LINKERFLAGS="${BENCHIT_CC_L_FLAGS} -lacml -lg2c ${BENCHIT_LIB_ACML}"

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
printf "${LOCAL_KERNEL_COMPILER} ${LOCAL_KERNEL_COMPILERFLAGS} -c dgemm_init.c dgemm_entry.c\n"
${LOCAL_KERNEL_COMPILER} ${LOCAL_KERNEL_COMPILERFLAGS} -c dgemm_init.c dgemm_entry.c

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



###############################################################################
# Log-History
#
# $Log: COMPILE.SH,v $
# Revision 1.11  2006/10/11 11:39:54  hackenb
# COMPILE.SH now in new style and hopefully less confusing...
#
# Revision 1.10  2006/01/03 11:57:39  mickler
# # Changes for new variable names
#
# Revision 1.9  2005/12/15 08:57:40  hackenb
# (hopefully) fixed footer
#
# Revision 1.1  2005/08/03 09:57:20  mark
# Attention: The ACML functions are different from normal BLAS-routines
# (row-column-order). Being unimportant for measurements it has be ignored.
#
###############################################################################
