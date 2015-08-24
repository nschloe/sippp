/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.3 $
 *  $Date: 2006/01/03 14:00:06 $
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


/** The data structure that holds all the data.
 *  Please use this construct instead of global variables.
 *  Global Variables seem to have large access times (?).
 */
typedef struct mydata
{
   double *pda, *pdb, *pdc;
   myinttype min;
   myinttype max;
   myinttype increment;
   myinttype steps;
   myinttype precision;
} mydata_t;


#endif
