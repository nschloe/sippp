#!/bin/sh
###############################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#  Shell script, installing the gui
#
#  Author: SWTP Nagel 1
#  Last change by: $Author: rschoene $
#  $Revision: 1.1 $
#  $Date: 2005/10/20 13:08:58 $
#
###############################################################################
echo "###### installing ######"

cd ../src
jar uf ../bin/BenchIT.jar *.class */*.class
rm *.class
rm */*.class
cd ../bin

echo "###### done       ######"
###############################################################################
#  Log-History
#
###############################################################################
