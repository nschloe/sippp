/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2006/07/03 22:32:24 $
 *******************************************************************/
 
#ifndef __work_h
#define __work_h

#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <time.h>

#include "interface.h"


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
   myinttype min;
   myinttype max;
   myinttype increment;
   myinttype steps;
   double *doublearray;
} mydata_t;

extern double simple( myinttype * );

void bubblesortd( double *sort, long number );
int  verifyd( double *probe, long elements );


#endif


/********************************************************************
 * Log-History
 * 
 *******************************************************************/
