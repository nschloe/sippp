#!/bin/sh

write_to_file() {
	if [ -z "$1" ]; then
		printf "\nInternal error.\n\n"
		exit -1
	fi
	echo "\
#####################################################################
############    BenchIT local config file    ########################
#####################################################################

#######################################
# Section 1 Architectural Information #
#######################################

# Short specification of your CPU - will be used for the output-filename
# ex. \"AmK7\" for AMD Athlon, \"InP4\" for Intel Pentium IV, \"MIPS\", \"SX6\"...
BENCHIT_ARCH_SHORT=\"${BENCHIT_ARCH_SHORT}\"

# CPU clock rate in Megahertz (e.g. 700M, 2000M, ...)
BENCHIT_ARCH_SPEED=\"${BENCHIT_ARCH_SPEED}\"

# To debug the application in case of problems, set the parameter DEBUGLEVEL
# to one of the following values:
# 0 for the standard ouput (default)
# 1 additionally a message for each function call (on entry and exit)
# 2 additionally a message for each loop (on entry, cycle, and exit)
# 3 additionally a message for each code section (use this if you encounter
#   problems when the output-file is written)
BENCHIT_DEBUGLEVEL=$BENCHIT_DEBUGLEVEL

# Batch or runtime environment on your system
# Look into <BENCHITDIR>/tools/environments for available environments
BENCHIT_ENVIRONMENT=\"${BENCHIT_ENVIRONMENT}\"

BENCHIT_FILENAME_COMMENT=\"$BENCHIT_FILENAME_COMMENT\"

# If you don't want to load settings from the kernel's PARAMETERS file, set this
# option to 1.
BENCHIT_IGNORE_PARAMETER_FILE=$BENCHIT_IGNORE_PARAMETER_FILE

# If you don't want to configure the measurement run in an interactive mode
# change this parameter to 0.
BENCHIT_INTERACTIVE=$BENCHIT_INTERACTIVE

# Number of processors to use
BENCHIT_NUM_CPUS="${BENCHIT_NUM_CPUS}"

# Number of processes
# Leave unset if you have no clue
BENCHIT_NUM_PROCESSES=$BENCHIT_NUM_PROCESSES

# Number of threads per process
# HINT: OpenMP uses only 1 process but many threads
# Leave unset if you have no clue
BENCHIT_NUM_THREADS_PER_PROCESS=$BENCHIT_NUM_THREADS_PER_PROCESS

# BenchIT allows for different accuracies of the measurements. This means how
# often BenchIT will repeat a measurement run to eliminate outliers. Set the
# parameter ACCURACY to any value from 0 (no repetitions) to any number
# you like (usually 10 repetitions should be more than enough).
# Defaults to 2 if not set.
BENCHIT_RUN_ACCURACY=$BENCHIT_RUN_ACCURACY

# Maximum size of the coredump in MB, disabled by default.
BENCHIT_RUN_COREDUMPLIMIT=$BENCHIT_RUN_COREDUMPLIMIT

# Some batch systems need an email address to send output to.
BENCHIT_RUN_EMAIL_ADDRESS=\"$BENCHIT_RUN_EMAIL_ADDRESS\"

# BenchIT offers two ways how the measurement problem sizes are arranged.
# Usually BenchIT cuts the problemsize in half and measures for that point.
# In the next iteration the new intervals are again cut in half and so on.
# To overide this and run the measurement linearly from problemsize 1 to
# maxproblemsize set LINEAR=1 (default=0)
BENCHIT_RUN_LINEAR=$BENCHIT_RUN_LINEAR

# Maximum memory to use, in MB. No 'M' suffix here, e.g. 128, 512, ...
BENCHIT_RUN_MAX_MEMORY=${BENCHIT_RUN_MAX_MEMORY}

# Name of the queue you want to use on batch systems
# Normally you do not need this, as the queue is selected by the batch system
BENCHIT_RUN_QUEUENAME=\"$BENCHIT_RUN_QUEUENAME\"

# Do not actually run kernels, instead print what the call would be.
# Useful for troubleshooting batch systems
BENCHIT_RUN_TEST=$BENCHIT_RUN_TEST

# Maximum time a kernel may run, in seconds
# 0 means no timelimit, and the kernel will run until all measurements have
# completed
BENCHIT_RUN_TIMELIMIT=${BENCHIT_RUN_TIMELIMIT}

# The Vampir suite allows the generation of trace files that can be displayed
# by vampir as well to help you debug your kernel. If you want to use vampir
# change the parameter USE_VAMPIR_TRACE to 1. (default=0)
BENCHIT_USE_VAMPIR_TRACE=$BENCHIT_USE_VAMPIR_TRACE


#####################################
# Section 2 Library Linking Options #
#####################################

# pThreads
BENCHIT_CPP_PTHREADS=\"${BENCHIT_CPP_PTHREADS}\"
BENCHIT_LIB_PTHREAD=\"${BENCHIT_LIB_PTHREAD}\"

# Performance Counter Library
BENCHIT_CPP_PCL=\"${BENCHIT_CPP_PCL} -DUSE_PCL\"
BENCHIT_LIB_PCL=\"${BENCHIT_LIB_PCL}\"

# Performance Application Programming Interface
BENCHIT_CPP_PAPI=\"${BENCHIT_CPP_PAPI} -DUSE_PAPI\"
BENCHIT_LIB_PAPI=\"${BENCHIT_LIB_PAPI}\"

# BLAS-Routines
BENCHIT_CPP_BLAS=\"${BENCHIT_CPP_BLAS}\"
BENCHIT_LIB_BLAS=\"${BENCHIT_LIB_BLAS}\"

# MPI-Library
BENCHIT_CPP_MPI=\"${BENCHIT_CPP_MPI} -DUSE_MPI\"
BENCHIT_LIB_MPI=\"${BENCHIT_LIB_MPI}\"

# PVM-Library
BENCHIT_CPP_PVM=\"${BENCHIT_CPP_PVM}\"
BENCHIT_LIB_PVM=\"${BENCHIT_LIB_PVM}\"


##############################
# Section 3 Compiler Options #
##############################

# Common include paths
BENCHIT_INCLUDES=\"$BENCHIT_INCLUDES\"

##################################
# Section 3.1 C Compiler Options #
##################################

# name of C Compiler
BENCHIT_CC=\"${BENCHIT_CC}\"

# Compilerflags
BENCHIT_CC_C_FLAGS=\"$BENCHIT_CC_C_FLAGS\"

# Additional Compilerflags - used for normal files
BENCHIT_CC_C_FLAGS_STD=\"${BENCHIT_CC_C_FLAGS_STD}\"

# Additional Compilerflags - used for the kernels
BENCHIT_CC_C_FLAGS_HIGH=\"${BENCHIT_CC_C_FLAGS_HIGH}\"

# Additional Compilerflags - used for OpenMP kernel
BENCHIT_CC_C_FLAGS_OMP=\"$BENCHIT_CC_C_FLAGS_OMP\"

# Standard Linkerflags
BENCHIT_CC_L_FLAGS=\"$BENCHIT_CC_L_FLAGS\"

######################################
# Section 3.2 MPI C Compiler Options #
######################################

# name of C Compiler
BENCHIT_MPICC=\"$BENCHIT_MPICC\"

# Compilerflags
BENCHIT_MPICC_C_FLAGS=\"$BENCHIT_MPICC_C_FLAGS\"

# Additional Compilerflags - used for normal files
BENCHIT_MPICC_C_FLAGS_STD=\"${BENCHIT_CC_C_FLAGS_STD}\"

# Additional Compilerflags - used for the kernels
BENCHIT_MPICC_C_FLAGS_HIGH=\"${BENCHIT_CC_C_FLAGS_HIGH}\"

# Additional Compilerflags - used for OpenMP kernel
BENCHIT_MPICC_C_FLAGS_OMP=\"$BENCHIT_MPICC_C_FLAGS_OMP\"

# Standard Linkerflags
BENCHIT_MPICC_L_FLAGS=\"$BENCHIT_MPICC_L_FLAGS\"

####################################
# Section 3.3 C++ Compiler Options #
####################################

# Name of C++ Compiler
BENCHIT_CXX=\"${BENCHIT_CXX}\"

# Compilerflags
BENCHIT_CXX_C_FLAGS=\"$BENCHIT_CXX_C_FLAGS\"

# Additional Compilerflags - used for normal files
BENCHIT_CXX_C_FLAGS_STD=\"${BENCHIT_CXX_C_FLAGS_STD}\"

# Additional Compilerflags - used for the kernels
BENCHIT_CXX_C_FLAGS_HIGH=\"${BENCHIT_CXX_C_FLAGS_HIGH}\"

# Additional Compilerflags - used for OpenMP kernel
BENCHIT_CXX_C_FLAGS_OMP=\"$BENCHIT_CXX_C_FLAGS_OMP\"

# Standard Linkerflags
BENCHIT_CXX_L_FLAGS=\"$BENCHIT_CXX_L_FLAGS\"

##########################################
# Section 3.4 Fortran77 Compiler Options #
##########################################

# Name of Fortran77 Compiler
BENCHIT_F77=\"${BENCHIT_F77}\"

# Compilerflags
BENCHIT_F77_C_FLAGS=\"$BENCHIT_F77_C_FLAGS\"

# Additional Compilerflags - used for normal files
BENCHIT_F77_C_FLAGS_STD=\"${BENCHIT_F77_C_FLAGS_STD}\"

# Additional Compilerflags - used for the kernels
BENCHIT_F77_C_FLAGS_HIGH=\"${BENCHIT_F77_C_FLAGS_HIGH}\"

# Additional Compilerflags - used for OpenMP kernel
BENCHIT_F77_C_FLAGS_OMP=\"$BENCHIT_F77_C_FLAGS_OMP\"

# Standard Linkerflags
BENCHIT_F77_L_FLAGS=\"$BENCHIT_F77_L_FLAGS\"

##########################################
# Section 3.5 Fortran90 Compiler Options #
##########################################

# Name of Fortran90 Compiler
BENCHIT_F90=\"${BENCHIT_F90}\"

# Compilerflags
BENCHIT_F90_C_FLAGS=\"$BENCHIT_F90_C_FLAGS\"

# Additional Compilerflags - used for normal files
BENCHIT_F90_C_FLAGS_STD=\"${BENCHIT_F90_C_FLAGS_STD}\"

# Additional Compilerflags - used for the kernels
BENCHIT_F90_C_FLAGS_HIGH=\"${BENCHIT_F90_C_FLAGS_HIGH}\"

# Additional Compilerflags - used for OpenMP kernel
BENCHIT_F90_C_FLAGS_OMP=\"$BENCHIT_F90_C_FLAGS_OMP\"

# Additional Compilerflags - choose the source format (e.g -free for freeform under ifort)
BENCHIT_F90_SOURCE_FORMAT_FLAG=\"\"

# Standard Linkerflags
BENCHIT_F90_L_FLAGS=\"$BENCHIT_F90_L_FLAGS\"

##########################################
# Section 3.6 Fortran95 Compiler Options #
##########################################

# Name of Fortran95 Compiler
BENCHIT_F95=\"\"

# Compilerflags
BENCHIT_F95_C_FLAGS=\"\"

# Additional Compilerflags - used for normal files
BENCHIT_F95_C_FLAGS_STD=\"-O2\"

# Additional Compilerflags - used for the kernels
BENCHIT_F95_C_FLAGS_HIGH=\"-O3\"

# Additional Compilerflags - used for OpenMP kernel
BENCHIT_F95_C_FLAGS_OMP=\"\"

# Additional Compilerflags - choose the source format (e.g -free for freeform under ifort)
BENCHIT_F95_SOURCE_FORMAT_FLAG=\"\"

# Standard Linkerflags
BENCHIT_F95_L_FLAGS=\"\"

############################
# Section 3.7 Java Options #
############################

# Name of Java Compiler
BENCHIT_JAVAC=\"${BENCHIT_JAVAC}\"
 
# Compilerflags
BENCHIT_JAVAC_FLAGS=\"$BENCHIT_JAVAC_FLAGS\"

# Additional Compilerflags - used for the kernels
BENCHIT_JAVAC_FLAGS_HIGH=\"${BENCHIT_JAVAC_FLAGS_HIGH}\"

# Name of Java Virtual Machine
BENCHIT_JAVA=\"${BENCHIT_JAVA}\"

# Flags for the Java Virtual Machine
BENCHIT_JAVA_FLAGS=\"\"


################################
# Section 4 Additional Options #
################################

OMP_DYNAMIC=FALSE
OMP_NESTED=FALSE
">$1
}

conv() {
	[ "`basename $1`" = "PROTOTYPE" ] && return 0
	printf "$1\n"

	proto="${my_dir}/_proto"
	locdef="${my_dir}/_newldef"

	sed 's/\$/\\\$/g' $proto_file > $proto
	# Load new PROTOTYPE for standard values
	. $proto
	if [ -z "$BENCHIT_INTERACTIVE" ]; then
		printf "\nYour PROTOTYPE file does not define the BENCHIT_INTERACTIVE variable.\n"
		printf "Are you sure it is an actual PROTOTYPE file?\n"
		printf "Enter y to continue, anything other to quit.\n"
		read cont
		[ "$cont" != "y" ] && printf "\n*** ABORTED ***\n\n"; exit -1
	fi

	sed 's/\$/\\\$/g' $1 > $locdef
	mv $locdef ${locdef}_
	sed -f $my_dir/LOC_repl ${locdef}_ > $locdef
	rm ${locdef}_
	# Load old LOCALDEFS
	. $locdef
	[ ! -f ${1}_old ] && cp $1 ${1}_old
	write_to_file $1
}

usage() {
	printf "\nGive the directory of the LOCALDEFS that shall be converted\n"
	printf "as argument 1, e.g.\n"
	printf "    $0 /home/BenchIT/LOCALDEFS\n\n"
	printf "This directory MUST contain a NEW PROTOTYPE file, else the conversion\n"
	printf "will NOT work!!\n\n"
	exit -1
}

if [ -z "$1" ]; then
	usage
fi

if [ ! -d "$1" ]; then
	printf "\n\"$1\" is not a directory!\n\n"
	exit -1
fi

start_dir=`pwd`
cd `dirname $0`
my_dir=`pwd`

cd $start_dir
cd $1
ldef_dir=`pwd`
_list=`ls -1`

proto_file="${ldef_dir}/PROTOTYPE"
if [ ! -r $proto_file ]; then
	printf "Cannot read \"$proto_file\"\n\n"
	exit -1
fi

for myfile in ${_list}; do
	[ -n "${myfile%%*_*}" -a -f ${myfile} ] && conv $myfile
done

exit 0




################################################################################
	# Assign values
	# BENCHIT_NUM_CPUS="$NUMCPUS"
	# BENCHIT_RUN_MAX_MEMORY="${MEMUSED%%[!0-9]*}"
	# #is same
	# #BENCHIT_ARCH_SHORT
	# #is same
	# #BENCHIT_ARCH_SPEED
	# BENCHIT_RUN_TIMELIMIT="$BENCHIT_TOTAL_TIMELIMIT"
	# BENCHIT_ENVIRONMENT="$ENVIRONMENT"
# 
	# BENCHIT_CPP_PTHREADS="$CPP_PTHREADS"
	# BENCHIT_LIB_PTHREAD="$LIB_PTHREAD"
	# BENCHIT_CPP_PCL="$CPP_PCL -DBENCHIT_USE_PCL"
	# BENCHIT_LIB_PCL="$LIB_PCL"
	# BENCHIT_CPP_PAPI="$CPP_PAPI -DBENCHIT_USE_PAPI"
	# BENCHIT_LIB_PAPI="$LIB_PAPI"
# 
	# BENCHIT_LIB_BLAS="$LIB_BLAS"
# 
	# BENCHIT_CPP_MPI="$CPP_MPI"
	# BENCHIT_LIB_MPI="$LIB_MPI"
# 
	# BENCHIT_CPP_PVM="$CPP_PVM"
	# BENCHIT_LIB_PVM="$LIB_PVM"
# 
	# BENCHIT_CC="$CC"
	# BENCHIT_CC_C_FLAGS="$CC_C_FLAGS"
	# BENCHIT_CC_C_FLAGS_STD="$CC_C_FLAGS_STD"
	# BENCHIT_CC_C_FLAGS_HIGH="$CC_C_FLAGS_HIGH"
	# BENCHIT_CC_C_FLAGS_OMP="$CC_C_FLAGS_OMP"
	# BENCHIT_CC_L_FLAGS="$CC_L_FLAGS"
# 
	# BENCHIT_CXX="$CXX"
	# BENCHIT_CXX_C_FLAGS="$CXX_C_FLAGS"
	# BENCHIT_CXX_C_FLAGS_STD="$CXX_C_FLAGS_STD"
	# BENCHIT_CXX_C_FLAGS_HIGH="$CXX_C_FLAGS_HIGH"
	# BENCHIT_CXX_L_FLAGS="$CXX_L_FLAGS"
# 
	# BENCHIT_F77="$F77"
	# BENCHIT_F77="$F77_C_FLAGS"
	# BENCHIT_F77="$F77_C_FLAGS_STD"
	# BENCHIT_F77="$F77_C_FLAGS_HIGH"
	# BENCHIT_F77="$F77_C_FLAGS_OMP"
	# BENCHIT_F77="$F77_L_FLAGS"
# 
	# BENCHIT_F90="$F90"
	# BENCHIT_F90="$F90_C_FLAGS"
	# BENCHIT_F90="$F90_C_FLAGS_STD"
	# BENCHIT_F90="$F90_C_FLAGS_HIGH"
	# BENCHIT_F90="$F90_C_FLAGS_OMP"
	# BENCHIT_F90="$F90_L_FLAGS"
# 
	# BENCHIT_JAVAC="$JAVAC"
	# BENCHIT_JAVAC_FLAGS="$JAVAC_C_FLAGS"
	# BENCHIT_JAVAC_FLAGS_HIGH="$JAVAC_C_FLAGS_HIGH"
	# BENCHIT_JAVA="$JVM"

