#!/bin/sh
###############################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#  Shellscript running java kernels
#
#  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
#  Last change by: $Author: mickler $
#  $Revision: 1.4 $
#  $Date: 2005/09/11 23:04:32 $
#
###############################################################################
if [ -z "${BENCHITROOT}" ]; then
	# running stand-alone
	java JBI $@
else
	"${JVM}" JBI $@
fi
###############################################################################
#  Log-History
#
#  $Log: run.sh,v $
#  Revision 1.4  2005/09/11 23:04:32  mickler
#  - Fixed: use slected JVM instead of "java"
#
#  Revision 1.3  2005/07/22 10:33:50  wloch
#  made run script independend of benchit
#
#  Revision 1.2  2005/07/21 18:56:04  wloch
#  simplified run.sh made it undependend from configure
#
#  Revision 1.1.1.1  2005/07/18 13:03:21  wloch
#  the final restructured benchit source tree
#
#  Revision 1.1  2005/06/21 12:36:31  wloch
#  reduced run scripts for java kernels
#
