#!/bin/sh
########################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#  Check if directory tree already exist
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

# Check if directory can be used to write tree

if [ ! -d $1 ] ; then
   echo $1 does not exist or is not a directory
   exit 127
fi

if [ ! -r $1 ] ; then
   echo $1 is not readable
   exit 127
fi

if [ ! -x $1 ] ; then
   echo $1 is not executable
   exit 127
fi

if [ ! -w $1 ] ; then
   echo $1 is not writable
   exit 127
fi

# Check if tree already exist

if [ ! -d $1/d0 -o ! -d $1/d1 ] ; then
    exit 1
fi

# Check if status files are readable

if [ ! -r $1/dir_state -o ! -r $1/reg_files_state ] ; then
    exit 2
fi

# Check if depth of tree is okay

storedDepth=`awk '/^Depth is [0-9]*/ {print substr($3,1,length($3)-1)}' $1/dir_state`
if [ "x$storedDepth" != "x$2" ] ; then
    exit 3
fi

# Check number of files at tree leafs

storedFPD=`awk '/^[0-9]* files per dir/ {print $1}' $1/reg_files_state`
if [ $storedFPD != $3 ] ; then
    exit 4
fi

exit 0

###############################################################################
#  Log-History (CVS)
#
#  $Log: CHECK_DIR_TREE.SH,v $
#  Revision 1.1  2006/11/01 20:22:51  william
#  first checkin after complete codereview and simple tests
#
#  Revision 1.1.1.1  2006/04/18 10:03:49  william
#  import version 0.1
#
#  Revision 1.5  2004/09/23 06:34:34  pgottsch
#  Script debugged
#
#  Revision 1.3  2004/07/08 16:55:08  pgottsch
#  Files not only written in the leaf dir + Deletion of files at any depth of the tree
#
#  Revision 1.2  2004/07/02 14:34:55  pgottsch
#  A few cleanings
#
#  Revision 1.1  2004/07/02 14:24:35  pgottsch
#  First version (is running)
#
#
