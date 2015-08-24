#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: rschoene $
# $Revision: 1.4 $
# $Date: 2007/06/20 15:00:28 $
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


# the compiler and its flags for the benchit.c
LOCAL_BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_DEFINES}"
# the compiler for the measurement kernel
LOCAL_KERNEL_COMPILER="${BENCHIT_CC}"
# the compilerflags for the measurement kernel
LOCAL_KERNEL_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH}"
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


#SSE_ALIGNED=1
#export SSE_ALIGNED
#
# FIRST STAGE: COMPILE
#
printf "${LOCAL_KERNEL_COMPILER} ${BENCHIT_INCLUDES} ${LOCAL_KERNEL_COMPILERFLAGS} -c work.c kernel_main.c\n"
${LOCAL_KERNEL_COMPILER} ${BENCHIT_INCLUDES} ${LOCAL_KERNEL_COMPILERFLAGS} -c work.c kernel_main.c

printf "${LOCAL_BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c\n"
${LOCAL_BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c


#
# SECOND STAGE: LINK
#
printf "${LOCAL_KERNEL_COMPILER} ${BENCHIT_INCLUDES} -o ${BENCHIT_KERNELBINARY} *.o ${LOCAL_LINKERFLAGS}\n"
${LOCAL_KERNEL_COMPILER} ${BENCHIT_INCLUDES} -o ${BENCHIT_KERNELBINARY} *.o ${LOCAL_LINKERFLAGS}


#
# REMOVE *.o FILES
#
rm -f ${KERNELDIR}/*.o



###############################################################################
#  Log-History
#
#  $Log: COMPILE.SH,v $
#  Revision 1.4  2007/06/20 15:00:28  rschoene
#  moved BENCHIT_INCLUDE
#
#  Revision 1.3  2006/10/11 11:39:54  hackenb
#  COMPILE.SH now in new style and hopefully less confusing...
#
#  Revision 1.2  2006/04/10 08:25:54  rschoene
#  changed header/footer
#
#  Revision 1.1  2006/04/05 07:28:23  rschoene
#  gemv, single precision, sse unaligned
#
###############################################################################
