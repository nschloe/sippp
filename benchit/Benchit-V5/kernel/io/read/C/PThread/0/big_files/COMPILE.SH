#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: william $
# $Revision: 1.1 $
# $Date: 2007/04/26 02:33:04 $
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

if [ -z "${BENCHIT_LIB_PTHREAD}" ]; then
	echo "No PTHREAD-Lib specified - check \$BENCHIT_LIB_PTHREAD"
	exit 1
fi

BENCHIT_COMMENT="IO Read Kernel (big-files) using PThreads"
export BENCHIT_COMMENT

# set up c-compiler
BENCHIT_COMPILER="${BENCHIT_CC}"
BENCHIT_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_HIGH}"
BENCHITC_COMPILER="${BENCHIT_CC} ${BENCHIT_CC_C_FLAGS} \
   ${BENCHIT_CC_C_FLAGS_STD} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"
C_COMPILE="${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} ${BENCHIT_DEFINES} \
   ${BENCHIT_INCLUDES} -c"

L_FLAGS="${BENCHIT_CC_L_FLAGS} ${BENCHIT_LIB_PTHREAD}"
C_LINK="${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} -o" 
#   "${BENCHIT_KERNELBINARY}"

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
printf "${C_COMPILE} tpool.c\n"
${C_COMPILE} tpool.c
printf "${C_COMPILE} eval.c\n"
${C_COMPILE} eval.c
printf "${C_COMPILE} iocreate.c\n"
${C_COMPILE} iocreate.c
printf "${C_COMPILE} ioremove.c\n"
${C_COMPILE} ioremove.c
printf "${C_COMPILE} iobig_writefct.c\n"
${C_COMPILE} iobig_writefct.c
printf "${C_COMPILE} iobig_readfct.c\n"
${C_COMPILE} iobig_readfct.c
printf "${C_COMPILE} kernel_main.c\n"
${C_COMPILE} kernel_main.c
printf "${BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c\n"
${BENCHITC_COMPILER} -c ${BENCHITROOT}/benchit.c

#
# SECOND STAGE: LINK
# note that the executable is located in the main directory
#
printf "${C_LINK} ${BENCHIT_KERNELBINARY}\n"
${C_LINK} ${BENCHIT_KERNELBINARY}_execute tpool.o eval.o iobig_readfct.o iobig_writefct.o kernel_main.o benchit.o ${L_FLAGS}
${C_LINK} ${BENCHIT_KERNELBINARY}_create tpool.o eval.o iocreate.o iobig_writefct.o ${L_FLAGS}
${C_LINK} ${BENCHIT_KERNELBINARY}_remove ioremove.o eval.o ${L_FLAGS}
cp ${KERNELDIR}/do.sh ${BENCHIT_KERNELBINARY}
chmod +x ${BENCHIT_KERNELBINARY}

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
# Revision 1.1  2007/04/26 02:33:04  william
# fixed errors due to old layout of Kernel (no more RUN.SH)
#
# Revision 1.3  2006/04/20 23:23:23  william
# let the bughuntseason begin
#
# Revision 1.2  2006/04/19 15:42:59  william
# fixed PARAMETERS-file and started work on bi_init + bi_entry
# rewrote all the Environment-Variables-stuff
#
# Revision 1.1.1.1  2006/04/18 10:03:50  william
# import version 0.1
#
#####################################################################
