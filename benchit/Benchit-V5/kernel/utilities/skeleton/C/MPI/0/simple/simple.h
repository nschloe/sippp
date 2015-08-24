/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton with MPI
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.3 $
 * $Date: 2006/01/09 16:24:20 $
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
   myinttype commsize;
   myinttype commrank;
   myinttype min;
   myinttype max;
   myinttype increment;
   myinttype steps;
} mydata_t;

extern double simple( myinttype * );

#endif


/********************************************************************
 * Log-History
 * 
 * $Log: simple.h,v $
 * Revision 1.3  2006/01/09 16:24:20  william
 * updated the cvs-header
 *
 * Revision 1.2  2006/01/09 16:01:43  william
 * cvs-keyword-problems
 *
 * Revision 1.1  2006/01/05 19:37:36  william
 * A simple Version of the skeleton with mpi for easy first time development
 *
 * 
 *******************************************************************/
