#!/bin/sh
########################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#****h* BenchIT/COMPILE.SH
# NAME
#  COMPILE.SH -- Shellscript compiling kernels
#
# FUNCTION
#  The script sets the environment-variable CONFIGURE_MODE to COMPILE
#  and then starts tools/configure which sets up the variables
#  needed by the COMPILE.SH's of the kernels.
#
# DIAGNODSTICS
#  Author: Thomas William (william@zhr.tu-dresden.de)
#  Last change by: $Author: molka $
#  $Revision: 1.9 $
#  $Date: 2007/07/10 14:45:52 $
#******
########################################################################

# Go to COMPILE.SH directory
cd "`dirname ${0}`" || exit 1

# Set configure mode
CONFIGURE_MODE="COMPILE"

# Switch for cmdlineparams to enable scan for missing BENCHIT_KERNELBINARY
COMPILE_GLOBAL=1

# Load the definitions
. ./tools/configure

if [ "${KERNELDIR}" = "${BENCHITROOT}" ]; then
	# Global COMPILE.SH was called without a target
	printf "\nERROR: No kernel for compile specified.\n"
	printf "Use COMPILE.SH -h to view the syntax of this command.\n\n"
	exit 1
fi

# figure out compiler versions
if [ ${BENCHIT_CC} ]; then
  ${BENCHIT_CC} -o printVersion tools/compilerversion/printCompilerVersion.c
  BENCHIT_CC_COMPILER_VERSION="`./printVersion`"
  export BENCHIT_CC_COMPILER_VERSION
fi

if [ ${BENCHIT_CXX} ] && [ "${BENCHIT_CXX}" != "" ]; then

  ${BENCHIT_CXX} -o printVersion tools/compilerversion/printCompilerVersion.cpp
  BENCHIT_CXX_COMPILER_VERSION="`./printVersion`"
  export BENCHIT_CXX_COMPILER_VERSION
fi

if [ ${BENCHIT_F77} ] && [ "${BENCHIT_F77}" != "" ]; then

  ${BENCHIT_F77} -o printVersion tools/compilerversion/printCompilerVersion.f
  BENCHIT_F77_COMPILER_VERSION="`./printVersion`"
  export BENCHIT_F77_COMPILER_VERSION
fi

if [ ${BENCHIT_F90} ] && [ "${BENCHIT_F90}" != "" ]; then

  ${BENCHIT_F90} -o printVersion tools/compilerversion/printCompilerVersion.f
  BENCHIT_F90_COMPILER_VERSION="`./printVersion`"
  export BENCHIT_F90_COMPILER_VERSION
fi

if [ ${BENCHIT_F95} ] && [ "${BENCHIT_F95}" != "" ]; then

  ${BENCHIT_F95} -o printVersion tools/compilerversion/printCompilerVersion.f
  BENCHIT_F95_COMPILER_VERSION="`./printVersion`"
  export BENCHIT_F95_COMPILER_VERSION
fi

# Source kernel's COMPILE.SH
. "${KERNELDIR}/COMPILE.SH"

###############################################################################
#
# $Log: COMPILE.SH,v $
# Revision 1.9  2007/07/10 14:45:52  molka
# added compiler version information
#
# Revision 1.8  2006/09/29 16:36:23  william
# added documentation for robodoc
#
# Revision 1.7  2005/12/15 02:41:13  mickler
# # Converted to new variable names
#
# Revision 1.6  2005/12/06 18:15:25  mickler
# - Fixed local COMPILE.SH aborting with error about missing kernel argument
#
# Revision 1.5  2005/08/30 22:23:59  mickler
# + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
#   BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
#   main BenchIT files (benchit.c, JBI.java, ...)
# - FIXED: user settings for e.g. linear measurement were not used,
#   added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
#
# Revision 1.4  2005/08/29 22:53:10  mickler
# - SCRIPT NOW CONFORMS 100% TO POSIX.1 STANDARD
#
# - FIXED: Former head and tail usage replaced by shell functionality
# - FIXED: Integer comparisons in [] replaced by correct syntax (-eq, -lt, ...)
# - FIXED: Run of global COMPILE.SH without specifying kernel caused infinite loop
#
# Revision 1.3  2005/08/23 13:11:02  william
# added a new ENVIRONMENT - corrected some nasty copy&paste-errors
#
# Revision 1.2  2005/08/11 19:40:10  mickler
# - Fixed " in all shell scripts
#
# Revision 1.1  2005/07/29 08:19:48  wloch
# moved global scripts to benchit root and changed configure for this
#
#
########################################################################
