#!/bin/sh
###############################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#  Shellscript running java kernels
#
#  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
#  Last change by: $Author: mickler $
#  $Revision: 1.5 $
#  $Date: 2005/09/11 22:54:43 $
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
#  Revision 1.5  2005/09/11 22:54:43  mickler
#  - Fixed: use selected JVM instead of "java"
#
#  Revision 1.4  2005/07/22 10:33:50  wloch
#  made run script independend of benchit
#
#  Revision 1.3  2005/07/21 18:56:04  wloch
#  simplified run.sh made it undependend from configure
#
#  Revision 1.2  2005/07/19 12:10:36  wloch
#  added cvs footers
#
#  Revision 1.1  2005/07/19 11:51:17  wloch
#  matmul java kernel for src3 structure
#
#  Revision 1.1.1.1  2005/07/18 13:03:20  wloch
#  the final restructured benchit source tree
#
#  Revision 1.1  2005/06/21 12:36:31  wloch
#  reduced run scripts for java kernels
#
###############################################################################
