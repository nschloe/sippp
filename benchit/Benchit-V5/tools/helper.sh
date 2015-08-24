#!/bin/sh
######################################################################
#
# B e n c h I T - Performance Measurement for Scientific Applications
#
#  Seldom used more complex functions provided for assisting
#  configuring and running BenchIT.
#
#  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
#
#  Last change by $Author: mickler $
#
#  $Revision: 1.14 $
#
#  $Date: 2005/12/12 10:27:20 $
#
######################################################################

case $1 in
	0 )
		# do nothing
		;;


	${menuitem_selkernel} )
		# [1] generates "availablekernel.txt" to chose from
		if [ ! -f ${BENCHITDIR}/availablekernel.txt ]; then
			cd ${BENCHITDIR}
			if [ ${OSNAME} = "Linux" ]; then
				find kernel -name PARAMETERS -fprintf ${BENCHITDIR}/availablekernel.txt %h\\n
			else
				find kernel -name PARAMETERS | sed -e "s/\/PARAMETERS//" > ${BENCHITDIR}/availablekernel.txt
			fi
		fi

		cd ${BENCHITDIR}

		echo "Please delete the lines of the kernels"
		echo "you don't want to measure."
		echo ""
		echo "Press enter to continue."
		read dummy
		${BENCHIT_EDITOR} ${BENCHITDIR}/availablekernel.txt
		;;


	${menuitem_setparams} )
		# generate a global benchit-PARAMETERS
		if [ -f "availablekernel.txt" ]; then
			oldcrc=`grep "#CRC" ${BENCHITDIR}/benchit-PARAMETERS 2>/dev/null`
			crc="#CRC `cksum availablekernel.txt`"
		else
			oldcrc="#CRC no"
			crc="#CRC"
		fi
		if [ ! -f ${BENCHITDIR}/benchit-PARAMETERS ] || [ "${oldcrc}" != "${crc}" ]; then
			cd ${BENCHITDIR}
			echo "${crc}" >./benchit-PARAMETERS
			echo "" >>./benchit-PARAMETERS
			echo "# This is benchit's PARAMETERS file containing" >>./benchit-PARAMETERS
			echo "# all the parameters needed for your set of kernels" >>./benchit-PARAMETERS
			# cp ./benchit-PARAMETERS ./benchit-PARAMETERS1.txt
			printf "\n\n\n" >>${BENCHITDIR}/benchit-PARAMETERS
			LISTOFKERNEL=`cat ${BENCHITDIR}/availablekernel.txt`
			cd ${BENCHITDIR}
			for KERNEL in ${LISTOFKERNEL}; do
				cd ${KERNEL}
				if [ -f ./PARAMETERS ]; then
					KNAME="`printf \"${KERNEL#kernel/}\" | tr / .`"
					printf "\n################################################################################\n" >>${BENCHITDIR}/benchit-PARAMETERS
					printf "#BEGINOF ${KNAME}\n" >>${BENCHITDIR}/benchit-PARAMETERS
					printf "################################################################################\n" >>${BENCHITDIR}/benchit-PARAMETERS
					cat ./PARAMETERS  | grep ^[^#] >>${BENCHITDIR}/benchit-PARAMETERS
					printf "\n################################################################################\n" >>${BENCHITDIR}/benchit-PARAMETERS
					printf "#ENDOF ${KNAME}\n" >>${BENCHITDIR}/benchit-PARAMETERS
					printf "################################################################################\n\n" >>${BENCHITDIR}/benchit-PARAMETERS
				fi
				cd ${BENCHITDIR}
				# we only need the BENCHIT-VARIABLES
				#   cat ${BENCHITDIR}/benchit-PARAMETERS | grep "^\ BENCHIT" >>kernelparameters1.txt
			done
		fi
		echo "You may now modify the parameters of the selected kernels"
		echo "according to your needs."
		echo "The parameters of the kernels have been combined into"
		echo "one file which will be opened now."
		echo ""
		echo "Press enter to continue."
		read dummy
		${BENCHIT_EDITOR} ${BENCHITDIR}/benchit-PARAMETERS

		# generate "compileall" script
		cd ${BENCHITDIR}
		printf "#!/bin/sh\n\n" >./compileall
		chmod +x ./compileall
		LISTOFKERNEL=`cat ${BENCHITDIR}/availablekernel.txt`
		for KERNEL in ${LISTOFKERNEL} ; do
			KNAME="`printf \"${KERNEL#kernel/}\" | tr / .`"
			echo "printf \"################################################################################\\n\"" >>./compileall
			echo "printf \"\\n################################################################################\\n\" >&2" >>./compileall
			echo "printf \"Compiling Kernel ${KNAME} ...\\n\"" >>./compileall
			echo "printf \"Compiling Kernel ${KNAME} ...\\n\" >&2" >>./compileall
			echo "cd ${BENCHITDIR}" >>./compileall
			echo "par_begin=\`grep -F -n \"#BEGINOF ${KNAME}\" benchit-PARAMETERS\`" >>./compileall
			echo "par_begin=\${par_begin%%:*}" >>./compileall
			echo "par_end=\`grep -F -n \"#ENDOF ${KNAME}\" benchit-PARAMETERS\`" >>./compileall
			echo "par_end=\${par_end%%:*}" >>./compileall
			echo "head -n \${par_end} benchit-PARAMETERS | tail -n \$((\${par_end} - \${par_begin})) > \"par_${KNAME}\"" >>./compileall
			echo "cd ${KERNEL}" >>./compileall
			echo "./COMPILE.SH -p ${BENCHITDIR}/par_${KNAME}" >>./compileall
			echo "rm -f \"par_${KNAME}\"" >>./compileall
			echo "printf \"\\n\"" >>./compileall
		done

		# generate runall
		cd ${BENCHITDIR}
		echo "#!/bin/sh" >./runall
		chmod +x ./runall
		echo "" >>./runall
		echo "cd ${BENCHITDIR} " >>./runall
		LISTOFKERNEL=`cat ${BENCHITDIR}/availablekernel.txt`
		for KERNEL in ${LISTOFKERNEL}; do
			KNAME="`printf \"${KERNEL#kernel/}\" | tr / .`"
			echo "printf \"################################################################################\\n\"" >>./runall
			echo "printf \"Running Kernel ${KNAME} ...\\n\"" >>./runall
			echo "par_begin=\`grep -F -n \"#BEGINOF ${KNAME}\" benchit-PARAMETERS\`" >>./runall
			echo "par_begin=\${par_begin%%:*}" >>./runall
			echo "par_end=\`grep -F -n \"#ENDOF ${KNAME}\" benchit-PARAMETERS\`" >>./runall
			echo "par_end=\${par_end%%:*}" >>./runall
			echo "head -n \${par_end} benchit-PARAMETERS | tail -n \$((\${par_end} - \${par_begin})) > \"par_${KNAME}\"" >>./runall
			if [ -z "${KERNEL##*Java*}" ]; then
				KERNEL="bin/`echo ${KERNEL#*kernel/} | tr / .`.0"
			else
				KERNEL="`echo ${KERNEL#*kernel/} | tr / .`.0"
			fi
			echo "./RUN.SH -p ${BENCHITDIR}/par_${KNAME} ${KERNEL}" >>./runall
			echo "rm -f \"par_${KNAME}\"" >>./runall
		done

		# generate cleanupall (kernel-execs and so on)
		cd ${BENCHITDIR}
		printf "#!/bin/sh\n" >./cleanupall
		chmod +x ./cleanupall
		printf "cd ${BENCHITDIR}\n\
			rm -f availablekernel.txt\n\
			rm -f benchit-PARAMETERS\n\
			rm -f cleanupall\n\
			rm -f compileall\n\
			rm -f error.log\n\
			rm -f par_*\n\
			rm -f runall\n\
			" >>./cleanupall
		clear
		echo "All scripts needed to do the measurements have been generated."
		echo "If you want to start them manually, exit benchit and invoke:"
		echo "  ./compileall   --   compile selected kernels"
		echo "  ./runall       --   run selected kernels"
		echo "  ./cleanupall   --   remove all settings"
		echo ""
		echo "Press enter to continue."
		read dummy
		;;


	${menuitem_compile} )
		cd ${BENCHITDIR}
		./compileall 2>${BENCHITDIR}/error.log
		echo "Press enter to continue."
		read dummy
		clear
		echo "Press enter to view the error.log..."
		read dummy
		${BENCHIT_EDITOR} ${BENCHITDIR}/error.log
		cd ${BENCHITDIR}
		exit 80
		;;

	${menuitem_run} )
		cd ${BENCHITDIR}
		./runall
		cd ${BENCHITDIR}
		printf "\nAll kernels were executed, press enter to return to the menu.\n"
		read dummy
		;;


	${menuitem_viewresults} )
		# use QUICKVIEW.SH to display the resultfiles
		cd ${BENCHITDIR}
		LISTOFKERNEL=`cat availablekernel.txt`
		for KERNEL in ${LISTOFKERNEL}; do
			KERNEL="`echo ${KERNEL#*kernel/}`"
			# get the most recent result
			results=`ls -t1 "${BENCHITDIR}/output/${KERNEL}/"*.bit 2>/dev/null | head -n 1 | tr '\n' ' '`
			[ -n "${results}" ] && tools/QUICKVIEW.SH ${results}
		done
		;;


	${menuitem_cleanup} )
		# Cleanup with "cleanupall"
		${BENCHITDIR}/cleanupall
		;;


	${menuitem_gui} )
		cd ${BENCHITDIR}
		./gui/bin/GUI.sh
		cd ${BENCHITDIR}
		;;
	${menuitem_exit} )
		# this is the exit :-)
		;;


	* )
		echo "wrong argument"
		;;
esac

###############################################################################
#  Log-History
#
#  $Log: helper.sh,v $
#  Revision 1.14  2005/12/12 10:27:20  mickler
#  # Changed menu entry "view results" to show only the most recent result
#    file of each kernel (instead of 3)
#
#  Revision 1.13  2005/11/25 10:19:48  mickler
#  - Fixed cleanupall not correctly built up
#
#  Revision 1.12  2005/11/24 03:29:13  mickler
#  - Fixed kernel lookup bug
#  # Removed ool & obsolete cleanup section
#  # Added splitted parameter file generation to runall file as well
#
#  Revision 1.11  2005/11/22 01:38:53  mickler
#  + Better output for compiling and running Kernels
#  + Create individual parameter files for the kernels for compiling and
#    running (-> settings of the different kernels do not overwrite each other)
#  + Check for changed availablekernels.txt
#
#  Revision 1.10  2005/11/04 16:16:18  mickler
#  # Minor output text change
#
#  Revision 1.9  2005/10/27 18:32:28  mickler
#  - Fixed leading dot in Kernelnames on Mac system
#
#  Revision 1.8  2005/10/26 14:15:33  mickler
#  # cosmetic changes in screen output
#  # changed kernelparameters.txt to benchit-PARAMETERS
#
#  Revision 1.7  2005/10/25 11:52:37  mickler
#  # Removed global PARAMETERS, changed benchit script accordingly
#
#  Revision 1.6  2005/09/12 16:03:29  mickler
#  - Fixed: Did not run correctly on all platforms
#
#  Revision 1.5  2005/09/12 12:48:54  mickler
#  - Fixed: Starting GUI from within benchit
#
#  Revision 1.4  2005/09/11 22:41:04  mickler
#  # Adapted script to work in new environment
#
#  Revision 1.3  2005/08/30 22:24:01  mickler
#  + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
#    BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
#    main BenchIT files (benchit.c, JBI.java, ...)
#  - FIXED: user settings for e.g. linear measurement were not used,
#    added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
#
#  Revision 1.2  2005/07/19 12:17:59  wloch
#  added cvs footer
#
#
###############################################################################



