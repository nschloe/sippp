#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: rschoene $
# $Revision: 1.10 $
# $Date: 2006/05/09 11:22:11 $
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
CAN_COMPILE_JAVA="0"
if [ -n "${BENCHIT_JAVAC}" ]; then
	if [ -n "${CLASSPATH}" ]; then
		CAN_COMPILE_JAVA="1"
	else
		CAN_COMPILE_JAVA="1"
		echo "Running without CLASSPATH is set!"
	fi
fi

if [ "${CAN_COMPILE_JAVA}" = "0" ]; then
   echo "No java compiler found or set in LOCALDEFS. Exiting."
   exit 1
fi

BENCHIT_COMPILER="${BENCHIT_JAVAC}"
BENCHIT_COMPILERFLAGS="${BENCHIT_JAVAC_FLAGS}"
BENCHIT_JAVAKERNELCLASS="KernelMain"

export BENCHIT_COMPILER BENCHIT_COMPILERFLAGS BENCHIT_JAVAKERNELCLASS

#
# ENVIRONMENT HASHING - creates jbi/BIEnvHash.java
#
cd "${BENCHITROOT}/tools/"
set > "tmp.env"
./fileversion >> tmp.env
./envhashbuilder
cd "${KERNELDIR}"

DEST="${BENCHIT_KERNELBINARY}/"
if [ ! -d "${DEST}" ]
   then
      mkdir "${DEST}"
fi

rm -f "${DEST}/"*.class
rm -f "${DEST}/"*.java

if [ "${CAN_COMPILE_JAVA}" = "1" ]; then
   cp -f DataObject.java "${DEST}"
   cp -f MatMul.java "${DEST}"
   cp -f "${BENCHIT_JAVAKERNELCLASS}.java" "${DEST}"
   cp -f "${BENCHITROOT}/jbi/"*.java "${DEST}"
cp -f run.sh "${DEST}/RUN.SH"
fi

cd "${DEST}"

# compile
if [ "${CAN_COMPILE_JAVA}" = "1" ]; then
   "${BENCHIT_COMPILER}" ${BENCHIT_COMPILERFLAGS} *.java
   rm -f *.java
fi

###############################################################################
#  Log-History
#
#  $Log: COMPILE.SH,v $
#  Revision 1.10  2006/05/09 11:22:11  rschoene
#  inserted a fi
#
#  Revision 1.8  2006/05/09 10:22:32  hackenb
#  fixed some obvious bugs
#
#  Revision 1.7  2005/12/14 21:42:46  hackenb
#  modified/unified header and footer
#
#  Revision 1.6  2005/09/11 22:57:32  mickler
#  - Fixed: corrected name of kernel binary
#  + Support for special charaters in pathnames
#
#  Revision 1.5  2005/08/30 22:24:00  mickler
#  + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
#    BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
#    main BenchIT files (benchit.c, JBI.java, ...)
#  - FIXED: user settings for e.g. linear measurement were not used,
#    added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
#
#  Revision 1.4  2005/07/29 11:53:51  wloch
#  removed setting of kernelname in bi getinfo and equalized parameter variables
#
#  Revision 1.3  2005/07/22 15:24:06  wloch
#  fixed dot bug
#
#  Revision 1.2  2005/07/22 10:35:04  wloch
#  replaced common block by minimal settings
#
#  Revision 1.1  2005/07/19 11:51:17  wloch
#  matmul java kernel for src3 structure
#
#  Revision 1.3  2005/06/21 14:24:44  wloch
#  fine tuning of new interface usage
#
#  Revision 1.2  2005/01/03 09:47:44  wloch
#  fixed PARAMETER script search
#
#  Revision 1.1  2005/01/03 09:18:04  wloch
#  the matmul kernel in java
#
#  Revision 2.4  2004/09/17 09:05:43  roeding
###############################################################################
