#! /bin/sh
###############################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
# MainDir/QUICKVIEW.SH
# DESCRIPTION
#  Shell script using gnuplot to display resultfile(s)
#
#  $Author: william $
#  $Revision: 1.3 $
#  $Date: 2006/09/29 16:35:11 $
#  $State: Exp $
#***
#
###############################################################################

#cd `dirname ${0}` || exit 1
#cd ../output

# remove eps-files and *.gp-files without corresponding *.bit-file
#rm -f *.eps

if [ "$1" = "" ]
then
   echo "QUICKVIEW: No file to view entered"
   echo "           Usage: ./QUICKVIEW <outputfile1.bit>"
   exit
fi

while [ "$1" != "" ]
do
   FOLDER=`dirname $1`
   FILE=`basename $1`
   cd $FOLDER
   gnuplot "${FILE}.gp"
   { gv "${FILE}.gp.eps" || gview "${FILE}.gp.eps" ||  echo "QUICKVIEW: Ghostview not found. Could not display ${FILE}.gp.eps"; } &
   shift
done


###############################################################################
#  Log-History
#
#  $Log: QUICKVIEW.SH,v $
#  Revision 1.3  2006/09/29 16:35:11  william
#  fixed wrong robodoc-entries
#
#  Revision 1.2  2005/07/19 13:41:46  william
#  fixed QUICKTIME to work with new output-dir
#
#  Revision 1.1.1.1  2005/07/18 13:03:19  wloch
#  the final restructured benchit source tree
#
#  Revision 1.2  2005/02/03 13:22:46  mark
#  *** empty log message ***
#
#  Revision 1.1.1.1  2004/12/14 21:23:03  william
#  Release 3.0 - created new cvs-tree src2
#
#  Revision 2.1  2004/03/04 12:42:50  william
#  I've started with the documentation
#
#  Revision 2.0  2003/12/09 11:17:57  juckel
#  build of version 2.0
#
#  Revision 1.2  2003/01/21 17:29:06  juckel
#  added: cvs header
#
#
###############################################################################

