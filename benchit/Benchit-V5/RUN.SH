#!/bin/sh
########################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#****h* BenchIT/RUN.SH
# NAME
#  RUN.SH -- Shellscript runing kernels
#
# FUNCTION
#  The script sets the environment-variable CONFIGURE_MODE to RUN
#  and then starts tools/configure which sets up the variables
#  needed to run the binary located in the ./bin/ folder.
#
# DIAGNODSTICS
#  Author: Thomas William (william@zhr.tu-dresden.de)
#  Last change by: $Author: molka $
#  $Revision: 1.13 $
#  $Date: 2007/01/12 10:44:35 $
#******
########################################################################

# Go to RUN.SH directory
cd "`dirname ${0}`" || exit 1

# Set configure mode
CONFIGURE_MODE="RUN"

# Load the definitions
. ./tools/configure

# Check if BENCHIT_KERNELBINARY is a directory
# -> Java kernel -> cd and set BENCHIT_KERNELBINARY to "$JVM JBI"
if [ -d "${BENCHIT_KERNELBINARY}" ]; then
   cd "${BENCHIT_KERNELBINARY}"
   export CLASSPATH="${CLASSPATH}:`pwd`"
   # add BENCHIT_LD_LIBRARY_PATH to LD_LIBRARY_PATH to allow Java-kernels to use the JNI-Libraries
   export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${BENCHIT_LD_LIBRARY_PATH}"
   BENCHIT_KERNELBINARY="${BENCHIT_JAVA} JBI"
fi

# Check for existence of selected environment
if [ ! -f ${BENCHITROOT}/tools/environments/${BENCHIT_ENVIRONMENT} ]; then
   printf "BenchIT: Your selected environment \"${BENCHIT_ENVIRONMENT}\" can not be found.\n"
   printf "         Execution of the kernel aborted.\n\n"
   exit 1
fi

# Execute kernel
. ${BENCHITROOT}/tools/environments/${BENCHIT_ENVIRONMENT}

###############################################################################
#
# $Log: RUN.SH,v $
# Revision 1.13  2007/01/12 10:44:35  molka
# replaced //-comments and tabs
#
# Revision 1.12  2006/12/07 15:03:50  molka
# Added C-Timer for Java-kernels
#
# Revision 1.11  2006/09/29 16:36:23  william
# added documentation for robodoc
#
# Revision 1.10  2006/01/03 14:45:48  mickler
# - Fix for Java CLASSPATH
#
# Revision 1.9  2005/12/15 02:41:14  mickler
# # Converted to new variable names
#
# Revision 1.8  2005/12/12 10:22:55  mickler
# + Added BENCHIT_RUN_QUEUENAME
# # Cosmetical changes for output
#
# Revision 1.7  2005/11/24 03:26:09  mickler
# + Split run_benchit up into different environment files
#
# Revision 1.6  2005/09/11 22:44:03  mickler
# - Bugfix for running Java kernels: CLASSPATH
#
# Revision 1.5  2005/08/11 19:40:10  mickler
# - Fixed " in all shell scripts
#
# Revision 1.4  2005/08/05 06:17:51  mickler
# - voreilig eingecheckt
#
# Revision 1.3  2005/08/05 06:15:57  mickler
# - Parameter fixing
#
# Revision 1.2  2005/08/04 22:06:01  mickler
# - Fixed parameter handling
#
# Revision 1.1  2005/07/29 08:19:48  wloch
# moved global scripts to benchit root and changed configure for this
#
# Revision 1.6  2005/07/22 11:21:55  mickler
# # Fixed variable for argument passing to run_benchit
#
# Revision 1.5  2005/07/22 10:44:18  wloch
# add code for all run_benchit arguments
#
# Revision 1.4  2005/07/22 10:32:22  wloch
# generalized RUN.SH
#
#
########################################################################
