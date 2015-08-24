#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: rschoene $
# $Revision: 1.11 $
# $Date: 2007/06/20 14:58:17 $
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



#
# FIRST STAGE: COMPILE
#
printf "${LOCAL_KERNEL_COMPILER} ${BENCHIT_INCLUDES} ${LOCAL_KERNEL_COMPILERFLAGS} -c matmul_c_core.c matmul_sub.c\n"
${LOCAL_KERNEL_COMPILER} ${BENCHIT_INCLUDES} ${LOCAL_KERNEL_COMPILERFLAGS} -c matmul_c_core.c matmul_sub.c

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
#  Revision 1.11  2007/06/20 14:58:17  rschoene
#  moved BENCHIT_INCLUDE
#
#  Revision 1.10  2006/10/11 11:39:54  hackenb
#  COMPILE.SH now in new style and hopefully less confusing...
#
#  Revision 1.9  2006/01/03 11:57:39  mickler
#  # Changes for new variable names
#
#  Revision 1.8  2005/12/15 01:23:12  mickler
#  # Converted to new variable names
#
#  Revision 1.7  2005/12/14 21:42:46  hackenb
#  modified/unified header and footer
#
#  Revision 1.6  2005/11/22 01:26:51  mickler
#  + Using BI_GET_CALL_OVERHEAD_FUNC macro now
#  + Using bi_timer() function for time measuring
#
#  Revision 1.5  2005/11/04 16:21:23  mickler
#  - benchit.c was being compiled with $BENCHIT_COMPILER and -O3.
#    Changed that, now extra entry $BENCHITC_COMPILER with correct
#    flags included
#  - Fixed missing $BENCHIT_DEFINES in some kernel compilations
#
#  Revision 1.4  2005/09/12 13:59:34  mickler
#  + Added output of compile command
#
#  Revision 1.3  2005/08/30 22:23:59  mickler
#  + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
#    BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
#    main BenchIT files (benchit.c, JBI.java, ...)
#  - FIXED: user settings for e.g. linear measurement were not used,
#    added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
#
#  Revision 1.2  2005/07/29 11:53:51  wloch
#  removed setting of kernelname in bi getinfo and equalized parameter variables
#
#  Revision 1.1  2005/07/20 21:38:26  mickler
#  # Added /numerical/matmul/C/0/0/double/ kernel
#
#  Revision 1.2  2005/01/13 10:19:38  rschoene
#  setted linker-flags
#
#  Revision 1.1.1.1  2004/12/14 21:22:57  william
#  Release 3.0 - created new cvs-tree src2
#
###############################################################################
