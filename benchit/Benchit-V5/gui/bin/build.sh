#!/bin/sh
###############################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#  Shell script, compiling the gui
#
#  Author: SWTP Nagel 1
#  Last change by: $Author: rschoene $
#  $Revision: 1.1 $
#  $Date: 2005/10/20 13:08:58 $
#
###############################################################################
echo "###### compiling  ######"

OLDDIR="${PWD}"
cd `dirname ${0}`
javac ${1} -classpath BenchIT.jar ../src/*.java ../src/system/*.java ../src/gui/*.java ../src/admin/*.java ../src/plot/*.java ../src/conn/*.java

./install.sh

cd "${PWD}"
echo "###### done       ######"
###############################################################################
#  Log-History
#
###############################################################################
