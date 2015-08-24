#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: hackenb $
# $Revision: 1.6 $
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
LOCAL_KERNEL_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH} ${BENCHIT_CPP_SCSL} ${BENCHIT_INCLUDES}"
# the linkerflags
LOCAL_LINKERFLAGS="${BENCHIT_CC_L_FLAGS} -lscs ${BENCHIT_LIB_SCSL}"

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
#  Log-History
#
#  $Log: COMPILE.SH,v $
#  Revision 1.6  2006/10/11 11:39:54  hackenb
#  COMPILE.SH now in new style and hopefully less confusing...
#
#  Revision 1.5  2006/01/04 11:10:02  hackenb
#  removed OMP flag
#
#  Revision 1.4  2006/01/03 11:57:40  mickler
#  # Changes for new variable names
#
#  Revision 1.3  2005/12/15 01:23:12  mickler
#  # Converted to new variable names
#
#  Revision 1.2  2005/12/14 21:42:45  hackenb
#  modified/unified header and footer
#
#  Revision 1.1  2005/11/28 12:30:07  rschoene
#  initial version for DGEMM with SCSL (SGI)
#
#  Revision 1.5  2005/11/04 16:21:23  mickler
#  - benchit.c was being compiled with $BENCHIT_COMPILER and -O3.
#    Changed that, now extra entry $BENCHITC_COMPILER with correct
#    flags included
#  - Fixed missing $BENCHIT_DEFINES in some kernel compilations
#
#  Revision 1.4  2005/08/30 22:24:00  mickler
#  + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
#    BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
#    main BenchIT files (benchit.c, JBI.java, ...)
#  - FIXED: user settings for e.g. linear measurement were not used,
#    added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
#
#  Revision 1.3  2005/08/10 06:37:35  mark
#  minor changes
#
#  Revision 1.0  2005/08/04 11:07:00  mark
#  
###############################################################################
