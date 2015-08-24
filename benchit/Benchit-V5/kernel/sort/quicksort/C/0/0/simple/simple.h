/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2006/07/03 22:34:28 $
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
   myinttype * intarray;
   float * floatarray;
   double * doublearray;
} mydata_t;

extern double simple( myinttype * );
extern int quicksort_clib_myinttype(  const void *pvelement1, const void *pvelement2 );
extern int quicksort_clib_flt(  const void *pvelement1, const void *pvelement2 );
extern int quicksort_clib_dbl(  const void *pvelement1, const void *pvelement2 );

#endif


/********************************************************************
 * Log-History
 * 
 * $Log: simple.h,v $
 * Revision 1.1  2006/07/03 22:34:28  william
 * quicksort up and running
 *
 * Revision 1.4  2006/01/09 16:24:20  william
 * updated the cvs-header
 *
 * Revision 1.3  2006/01/09 15:57:01  william
 * cvs-keyword-problems
 *
 * Revision 1.2  2005/12/15 15:44:17  hackenb
 * modified/unified header and footer
 *
 * Revision 1.1  2005/12/14 22:37:12  william
 * A simple Version of the skeleton for easy first time development
 * 
 *******************************************************************/
