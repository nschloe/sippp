/**************************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: measure Bandwidth inspired by STREAM benchmark (FORTRAN-version)
 *
 * according to the rules, reffer this Benchmark as:
 * "BenchIT kernel based on a variant of the STREAM benchmark code"
 * when publishing results
 *
 * This file is a header for the work-part of the kernel (work.f)
 * 
 * Contact: developer@benchit.org
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.1 $
 * $Date: 2007/02/13 12:48:43 $
 ************************************************************************/

#ifndef __work_h
#define __work_h
#endif

/** The data structure that holds all the data.
 *  Please use this construct instead of global variables.
 *  Global Variables seem to have large access times (?).
 */
typedef struct mydata
{
   double* a;
   double* b;
   double* c;
} mydata_t;

extern void copy_( double *a, double *b, int *size);
extern void scale_( double *a, double *b, double *scalar, int *size);
extern void add_( double *a, double *b, double *c, int *size);
extern void triad_( double *a, double *b, double *c, double *scalar, int *size);


/********************************************************************
 * Log-History
 * 
 * $Log: work.h,v $
 * Revision 1.1  2007/02/13 12:48:43  rschoene
 * STREAM Benchmark F77
 *
 * 
 *******************************************************************/
