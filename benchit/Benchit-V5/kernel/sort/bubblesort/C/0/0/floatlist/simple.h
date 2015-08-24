/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.2 $
 * $Date: 2007/07/11 10:36:30 $
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
/*#include "parser.h"*/


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
/*
	myinttype min;
   myinttype max;
   myinttype increment;
   myinttype steps;
 */
   char * plistenv; /* env variable einlesen */
   bi_list_t *plist; /* parsed list of elements which will be sorted */
} mydata_t;

extern double simple( myinttype * );
extern void bubblesorti( int *pisort, long lnumber );
extern int verifyi( int *piprobe, long lelements );
void bubblesortf( float *sort, long number );
int  verifyf( float *probe, long elements );

#endif


/********************************************************************
 * Log-History
 * 
 *******************************************************************/
