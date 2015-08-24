/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.2 $
 *  $Date: 2005/12/07 18:36:11 $
 *******************************************************************/
#ifndef __work_h
#define __work_h

/*prototypes for work.c are in interface.h*/
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
/*
#include "helper.h"
*/

#ifdef USE_MPI
  #include <mpi.h>
#endif



#include "interface.h"




/** here we define a minimum time that our kernel needs
 *  we do this to avoid a divide by zero
 */
#if(!defined(MINTIME))
#define MINTIME 1.0e-22
#endif

#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif




/* structure saves both vectors that are added
 * result is saved in pdb
 ***/
typedef struct float_data_struct
   {
   double *pda, *pdb;
   int VECADD_START;
int VECADD_INCREMENT;
int VECADD_STEPS;
int VECADD_PRECISION;

   } fds;

#endif
