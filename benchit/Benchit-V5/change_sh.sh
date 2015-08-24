#!/bin/sh
#/****h* BenchIT/change_sh.sh
# DESCRIPTION
#  BenchIT depends on a POSIX-conform shell. All our shell-scripts use 
#  /bin/sh as the standard-shell. If you want or have to change that
#  you can use this script
#******

#/****f* change_sh.sh/usage
# NAME
#  usage()
# FUNCTION
#  print some helping infos to standard-out
#******
usage () {
	printf "\nChanges the shell interpreter that BenchIT uses in its scripts.\n\n"
	printf "  USAGE:\n    ${0} <Old shell> <New shell>\n"
	printf "    ${0} -restore\n\n"
	printf "If you just installed BenchIT, use /bin/sh for <Old shell>.\n"
	printf "  Examples:\n    ${0} /bin/sh /bin/bash  - Use the bash shell\n"
	exit 1
}

#/****f* change_sh.sh/change
# NAME
#  change()
# FUNCTION
#  change the shell
#******
change () {
	for myfile in `find . -type f ! -name "*.orig" -print`; do
		[ "${myfile}" = './change_sh.sh' ] && continue
		mod="`grep -F -l -e ${1} ${myfile}`"
		if [ -n "${mod}" ]; then
			if [ ! -f "${myfile}.orig" ]; then
				printf "\nCreating backup file ${myfile}.orig..."
				cp "${myfile}" "${myfile}.orig"
				printf "done.\n"
			fi
			printf "Modifying ${myfile}..."
			sed s.${1}.${2}.g "${mod}" > "change_sh.tmp"
			cat "change_sh.tmp" > "${mod}"
			#rm -f "${mod}"
			#mv change_sh.tmp "${mod}"
			printf "done.\n"
		fi
	done
	rm -f "change_sh.tmp"
}

#/****f* change_sh.sh/restore
# NAME
#  restore()
# FUNCTION
#  restore an old version from backup'd files
#******
restore () {
	backups_present=""
	for backupfile in `find . -name "*.orig" -print`; do
		myfile=`echo "${backupfile}"|sed s/.orig//`
		printf "Restoring ${myfile}..."
		cp -p "${backupfile}" "${myfile}"
		printf "done.\n"
		backups_present="yes"
	done
	if [ -z "${backups_present}" ]; then
		printf "\nCannot find any backup files.\n\n"
	fi
}

if [ "${1}" = "-restore" ]; then
	restore
else
	[ -z "${1}" ] || [ -z "${2}" ] && usage
	change "${1}" "${2}"
fi

