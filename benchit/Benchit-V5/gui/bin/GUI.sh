#!/bin/sh
###############################################################################
#
#  B e n c h I T - Performance Measurement for Scientific Applications
#
#  Shell script, starting the gui
#
#  Author: SWTP Nagel 1
#  Last change by: $Author: rschoene $
#  $Revision: 1.7 $
#  $Date: 2006/09/11 06:00:37 $
#
###############################################################################

### this prevents the message "Cannot convert string "MetaCtrl<Key>Insert" to type VirtualBinding"
xprop -root -remove _MOTIF_DEFAULT_BINDINGS

OSNAME=`uname`
cd `dirname ${0}`
cd ..
GUIDIR=`pwd`
cd ..
BENCHITDIR=`pwd`
cd "${GUIDIR}/cfg"
CFGDIR=`pwd`


cd "$GUIDIR/bin"
### get hostname if not given
if test -z "$NODENAME" ; then
   #NODENAME=`${BENCHITDIR}/tools/uname_minus_a | cut -d ' ' -f2`
   NODENAME="`hostname`" || NODENAME="`\"${BENCHITROOT}/tools/uname_minus_a\" | cut -d ' ' -f2`"
     NODENAME=${NODENAME%%.*}
   
fi
#hostname

 set +e
# if we are new on the machine (LOCALDEFS don't exist) run script FIRSTTIME
# uname_minus_n=`../tools/uname_minus_a | cut -d ' ' -f2`
if test -f "${BENCHITDIR}/LOCALDEFS/${NODENAME}" ; then
   echo "transfer all money to" > /dev/null
else
   echo "No LOCALDEFS for host ${NODENAME} found. Starting FIRSTTIME..."
  export BENCHIT_INTERACTIVE="0"
   ${BENCHITDIR}/tools/FIRSTTIME
#	../tools/FIRSTTIME
#   if [ $? -ne 0 ]; then
#      echo "BenchIT: ERROR: First time setup did not run successfully. Aborting..."
#      exit 127
#   fi
fi
# set -e

### empty postproc.sh
echo " " > postproc.sh

### test for gnuplot
gnuplot /dev/null > /dev/null 2> /dev/null
if test $? = "127" ; then
   echo "gnuplot seems not to be installed - Quickview may not work"
fi


### test for gv
gv -v > /dev/null 2> /dev/null
if test $? = "127" ; then
   echo "gv seems not to be installed - Quickview may not work"
fi


### execute java GUI

ARGS="-host=$NODENAME $*"
java -Xmx128m -Xms128m -classpath BenchIT.jar -Dusessl=true -Djavax.net.ssl.trustStore=../cfg/client.trusts -Djavax.net.ssl.trustStorePassword=BenchIT BIGMain $ARGS
#java -classpath lib BIGMain -hostname=$HOSTNAME $*

RETURNCODE=$?
echo "Returncode was $RETURNCODE"

#[ "$RETURNCODE" == "3" ] && echo "returncode was 3"
#[ "$RETURNCODE" == "0" ] && ../../uni/swp/swt-prak/MAINDIREXEC.SH

### execute postproc.sh
echo "Execute postproc.sh"
if test -f postproc.sh ; then
   chmod u+x postproc.sh
   ./postproc.sh
fi
###############################################################################
#  Log-History
#
###############################################################################
