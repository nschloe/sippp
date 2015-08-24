# this script compiles an additional C-Library for better precision or performance when testing Java-kernels
#
# FUNCTION
# the script tests if all necessary tools are available (javac,javah,cc or gcc, the include-directory of the Java-SDK)
# the script trys to detect the SDK-location automatically if it is not specified
# if everything is found the C-Library is compiled
#
# Usage
# ./compile.sh [SDK-LOCATION]
#
# ! Java-kernels should not rely on C-libraries being available, it is supposed to use the Java-wrapper-methods 
#   in class JBI as they provide fallbackimplementations in Java if the libraries are missing
#
# ! if you add new native methods to ${BENCHITROOT}/jbi/JBI.java you have to recreate the JBI.h by:
# cd ${BENCHITROOT}/jbi/
# javac JBI.java
# javah JBI
# cp JBI.h jni


# Compiles C-Library
compile(){
	if [ -z ${JNI_CC} ]; then
		echo "         Warning: could not find C-Compiler (cc or gcc). Optional C-Libraries won't be available."
		compile_classes ${JNI_DIR}
		exit -1
	fi
	OLDDIR="`pwd`"
	cd ${2}
	printf "         compiling JNI-Libraries...\n"
	DIRECTORIES="`find \"${1}/include\" -type d`"
	DIRECTORIES="`echo -n ${DIRECTORIES}`"
	INCLUDE=""
	IFS=' '
	for D in ${DIRECTORIES}
	do
		INCLUDE="${INCLUDE} -I${D}"
	done

	${JNI_CC} -shared -o libjbi.so ${INCLUDE} JBI.c

	cd ${OLDDIR}
	
}


# try to find Java-SDK if not specified or invalid 
find_java(){
	echo "         Trying to detect Java-SDK automatically"

	#first we check if JAVA_HOME environment variable is set and actually points to a SDK
	printf "         Checking for JAVA_HOME ->"
	if [ -n ${JAVA_HOME} ] && [ -d "${JAVA_HOME}/include" ]
	then 
		printf " OK: ${JAVA_HOME}\n"
		compile "${JAVA_HOME}" "${JNI_DIR}"
		exit 0
	else
		printf " Failed\n"
	fi

	#next attempt is to read BENCHIT_JAVA_HOME from Localdefs
	printf "         Checking for LOCALDEFS -> " 
	HOSTNAME="`hostname`"
	printf "Hostname = \"${HOSTNAME}\"\n"

	if  [ -f "${JNI_DIR}/../../LOCALDEFS/${HOSTNAME}" ]; then
		echo "         Loading LOCALDEFS/${HOSTNAME}"
		set -a
		. "${JNI_DIR}/../../LOCALDEFS/${HOSTNAME}"
		set +a
		
		if [ -n ${BENCHIT_JAVA_HOME} ]; then
		
			if [ -d "${BENCHIT_JAVA_HOME}/include" ]
			then 
				printf "          - OK: ${BENCHIT_JAVA_HOME}\n"
				compile "${BENCHIT_JAVA_HOME}" "${JNI_DIR}"
				exit 0
			else
				printf "          - Failed\n"
			fi
		
		else
			echo "         Could not find BENCHIT_JAVA_HOME in LOCALDEFS"
		fi

	else
		echo "         No LOCALDEFS found for your system."
	fi

	#last chance is to specify SDK-location manually
	printf "         Automatic detection failed. Would you like to specify SDK-Location on your own? (y/n) "
	read e1
  	if [ "${e1}" = "y" ]; then	
		printf "         SDK-Location: "
		read JAVA_DIR
		if [ -n ${JAVA_DIR} ] && [ -d "${JAVA_DIR}/include" ]
		then 
			printf "          - OK\n"
			compile "${JAVA_DIR}" "${JNI_DIR}"
			exit 0
		else
			printf "          - Failed. ${JAVA_DIR} does not seem to contain a Java-SDK \n"
			echo "         No Java-SDK could be found. Can't compile C-Libraries."
			# Java-fallbacks will be used
			exit -1
		fi
		
	else
		echo "         No Java-SDK could be found. Can't compile C-Libraries."
		# Java-fallbacks will be used
		exit -1
	fi
}

#Main

JNI_DIR="`dirname \"${0}\"`"
BENCHITROOT="${JNI_DIR}/../.."
export BENCHITROOT

# test if javac is available
if ! "${BENCHITROOT}/tools/features" have javac ;then
	echo "         Error: javac not found.Aborting."
	exit -2
fi

# choose C-Compiler
if "${BENCHITROOT}/tools/features" have cc ;then
	JNI_CC="cc"
elif "${BENCHITROOT}/tools/features" have gcc ;then
	JNI_CC="gcc"
fi

if [ ${1} ]
then
	
	# if a parameter is specified it is supposed to be a path to a Java-SDK	
	if [ -d "${1}/include" ]; then
		compile "${1}" "${JNI_DIR}"
	else	# well supposed to be
		echo "         ${1} does not seem to contain a Java-SDK"
		find_java
	fi
			
	
else
	echo "         No path to Java-SDK specified"
	find_java
fi