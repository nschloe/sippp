/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2007/06/19 12:57:44 $
 *******************************************************************/
 
#ifndef __work_h
#define __work_h

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
   unsigned char empty_list;
   unsigned int * problemlist;
   unsigned int problemlistsize;
} mydata_t;

extern double simple( myinttype * );

#endif


/********************************************************************
 * Log-History
 * 
 * $Log: simple.h,v $
 * Revision 1.1  2007/06/19 12:57:44  william
 * Skeleton takes values as komma separated list and executes them in the provided order . . .
 *
 * 
 *******************************************************************/
