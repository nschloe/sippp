#!/bin/sh

# If kernel name string is set, this COMPILE.SH is sourced
# from the global COMPILE.SH. If it's not set, the user
# executed this script directly so we need to call configure.
if [ -z "${BENCHIT_KERNELNAME}" ]; then
   # Go to COMPILE.SH directory
   cd `dirname ${0}` || exit 1

   # Set configure mode
   CONFIGURE_MODE="COMPILE"

   # Load the definitions
   . ../../../../../../../tools/configure
fi

# check wether compilers and libraries are available
if [ -z "${BENCHIT_CC}" ]; then
	echo "No C compiler specified - check \$BENCHIT_CC"
	exit 1
fi

if [ -z "${BENCHIT_F95}" ]; then
	echo "No F95 compiler specified - check \$BENCHIT_F95"
	exit 1
fi

BENCHIT_C_COMPILERFLAGS="${BENCHIT_CC_C_FLAGS} ${BENCHIT_CC_C_FLAGS_STD}"
C_COMPILE="${BENCHIT_CC} ${BENCHIT_C_COMPILERFLAGS} ${BENCHIT_INCLUDES} ${BENCHIT_DEFINES}"
export BENCHIT_C_COMPILERFLAGS

BENCHIT_F_COMPILERFLAGS="${BENCHIT_F95_C_FLAGS} ${BENCHIT_F95_SOURCE_FORMAT_FLAG} ${BENCHIT_F95_C_FLAGS_HIGH}"
F_COMPILE="${BENCHIT_F95} ${BENCHIT_F_COMPILERFLAGS}"

BENCHIT_COMPILER="${BENCHIT_F95}"
export BENCHIT_COMPILER

BENCHIT_COMPILERFLAGS="${BENCHIT_F_COMPILERFLAGS}"
export BENCHIT_COMPILERFLAGS

# ENVIRONMENT HASHING - creates bienvhash.c
#
cd ${BENCHITROOT}/tools/
set > tmp.env
./fileversion >> tmp.env
./envhashbuilder
cd ${KERNELDIR}

# FIRST STAGE: COMPILE
#

printf "${F_COMPILE} -c kindmod.F\n"
${F_COMPILE} -c kindmod.F

printf "${F_COMPILE} -c errormod.F\n"
${F_COMPILE} -c errormod.F

printf "${F_COMPILE} -c errormod.F\n"
${F_COMPILE} -c rk_parameters.F

printf "${F_COMPILE} -c tridiag.F\n"
${F_COMPILE} -c tridiag.F

printf "${F_COMPILE} -c params_generic.F\n"
${F_COMPILE} -c params_generic.F

printf "${F_COMPILE} -c meshes.F\n"
${F_COMPILE} -c meshes.F

printf "${F_COMPILE} -c operators.F\n"
${F_COMPILE} -c operators.F

printf "${F_COMPILE} -c preconditioners.F\n"
${F_COMPILE} -c preconditioners.F

printf "${F_COMPILE} -c solvers.F\n"
${F_COMPILE} -c solvers.F

printf "${F_COMPILE} -c kernel_main.F\n"
${F_COMPILE} -c kernel_main.F

printf "${C_COMPILE} -c kernel_wrapper.c\n"
${C_COMPILE} -c kernel_wrapper.c

printf "${C_COMPILE} -c ${BENCHITROOT}/benchit.c\n"
${C_COMPILE} -c ${BENCHITROOT}/benchit.c

# SECOND STAGE: LINK
# note that the executable main is located in the main directory
#
printf "${C_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o ${BENCHIT_CC_L_FLAGS} ${BENCHIT_F95_L_FLAGS}\n"
${C_COMPILE} -o ${BENCHIT_KERNELBINARY} *.o \
                ${BENCHIT_CC_L_FLAGS}       \
                ${BENCHIT_F95_L_FLAGS}

# REMOVE *.o FILES
#
rm -f ${KERNELDIR}/*.o
