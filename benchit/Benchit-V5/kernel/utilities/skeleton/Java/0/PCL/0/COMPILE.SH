#!/bin/sh
#####################################################################
# BenchIT - Performance Measurement for Scientific Applications
# Contact: benchit@zih.tu-dresden.de
#
# Last change by: $Author: mickler $
# $Revision: 1.8 $
# $Date: 2006/01/03 09:39:05 $
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
if [ -z "${BENCHIT_JAVAC}" ]; then
	echo "No Java compiler specified - check \$BENCHIT_JAVAC"
	exit 1
fi

BENCHIT_COMPILER="${BENCHIT_JAVAC}"
BENCHIT_COMPILERFLAGS="${BENCHIT_JAVAC_FLAGS_HIGH}"
BENCHIT_JAVAKERNELCLASS="SkeletonPCLKernelMain"

BENCHIT_COMMENT="Plain skeleton for Java kernels using the PCL"
export BENCHIT_COMMENT

export BENCHIT_COMPILER BENCHIT_COMPILERFLAGS BENCHIT_JAVAKERNELCLASS

#
# ENVIRONMENT HASHING - creates jbi/BIEnvHash.java
#
cd ${BENCHITROOT}/tools/
set > tmp.env
./fileversion >> tmp.env
./envhashbuilder
cd ${KERNELDIR}

DEST="${BENCHIT_KERNELBINARY}"
if [ ! -d ${DEST} ]; then
	mkdir ${DEST}
fi

rm -f ${DEST}/*.class
rm -f ${DEST}/*.java

cp -f SkeletonPCLDataObject.java ${DEST}
cp -f SkeletonPCLWork.java ${DEST}
cp -f ${BENCHIT_JAVAKERNELCLASS}.java ${DEST}
cp -f ${BENCHITROOT}/jbi/*.java ${DEST}

cd ${DEST}

# compile
printf "${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} *.java\n"
${BENCHIT_COMPILER} ${BENCHIT_COMPILERFLAGS} *.java
rm -f *.java


###############################################################################
#  Log-History
#
#  $Log: COMPILE.SH,v $
#  Revision 1.8  2006/01/03 09:39:05  mickler
#  # Unified header and footer
#  # New environment variable names
#
#  Revision 1.7  2005/12/22 19:20:36  mickler
#  # Modified/unified header and footer
#  # New environment variable names
#
#  Revision 1.6  2005/09/11 23:01:38  mickler
#  - Fixed: corrected kernel binary name
#  + Aded support for special chars in pathnames
#
#  Revision 1.5  2005/08/30 22:24:01  mickler
#  + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
#    BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
#    main BenchIT files (benchit.c, JBI.java, ...)
#  - FIXED: user settings for e.g. linear measurement were not used,
#    added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
#
#  Revision 1.4  2005/07/29 11:53:52  wloch
#  removed setting of kernelname in bi getinfo and equalized parameter variables
#
#  Revision 1.3  2005/07/22 15:24:06  wloch
#  fixed dot bug
#
#  Revision 1.2  2005/07/22 10:35:04  wloch
#  replaced common block by minimal settings
#
#  Revision 1.1.1.1  2005/07/18 13:03:21  wloch
#  the final restructured benchit source tree
#
#  Revision 1.5  2005/06/21 14:21:46  wloch
#  fine tuning of new interface usage
#
#  Revision 1.4  2005/06/21 12:59:01  wloch
#  moved common code to configure
#
#  Revision 1.3  2005/06/21 12:38:22  wloch
#  debugged bin path
#
#  Revision 1.2  2005/06/21 12:27:25  wloch
#  adapted to new interface in JBI and benchit c
#
#  Revision 1.1  2005/02/14 17:31:34  wloch
#  skeleton for java kernel using the PCL
#
#
