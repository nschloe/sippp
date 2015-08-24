#!/bin/sh
########################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#  Check if files in other than leaf directories already exist
#
#  Author: Peter Gottschling (gottschling@zhr.tu-dresden.de)
#
#  Last change by $Author: william $
#
#  $Revision: 1.1 $
#
#  $Date: 2006/11/01 20:22:51 $
#
########################################################################

# Check syntax

if [ $# != 3 ] ; then
   echo "syntax: $0 <RootDirectoryOfTree> <TreeDepth> <FilesPerLeafDir>"
   exit 127
fi

# If status file does not exit
if [ ! -r $1/reg_files_interval_state ] ; then
    exit 2
fi

# Check depth interval
depthInt=`grep Depth $1/reg_files_interval_state|sed -e 's/.*Depth is \([0-9]*-[0-9]*\)\..*/\1/'`
if [ "1-`expr $2 - 1`" != "$depthInt" ]; then
    exit 3
fi

exit 0
