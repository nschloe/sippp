#!/bin/sh
###############################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#  Shell script, generating the docs of the gui
#
#  Author: SWTP Nagel 1
#  Last change by: $Author: rschoene $
#  $Revision: 1.1 $
#  $Date: 2005/10/20 13:08:58 $
#
###############################################################################
echo "### documenting ###"

OLDDIR="${PWD}"
cd `dirname ${0}`

#javadoc -quiet  BenchIT.jar -d doc -author -notree -link /usr/local/doc/java-1.4.2_03-doc/api/ -breakiterator -classpath jar/jcommon-0.8.0.jar:jar/jfreechart-0.9.8.jar:lib -link http://www.jfree.org/jcommon/javadoc/ -link http://www.jfree.org/jfreechart/javadoc/
javadoc -quiet -d ../doc ../src/*/* ../src/BI* -author -notree -link /usr/local/doc/java-1.4.2_03-doc/api/ -breakiterator -classpath ../BenchIT.jar -link http://www.jfree.org/jcommon/javadoc/ -link http://www.jfree.org/jfreechart/javadoc/
cd "${PWD}"

echo "### done ###"
###############################################################################
#  Log-History
#
###############################################################################
