/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: DGEMV (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.2 $
 * $Date: 2006/04/10 08:23:50 $
 *******************************************************************/
#ifndef __work_h
#define __work_h
#endif
/**  to make it easier to print some messages to stdout depending
 *   on a selectable debug level
 */
#if(!defined(DEBUGLEVEL))
#define DEBUGLEVEL (0)
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
   double* x;
   double* y;
   double* a;
/*
   <variabletype1> <variablename1>;
   <variabletype2> <variablename2>;
*/
} mydata_t;

void ij_(int sizeVector,int sizeAusgabe,double alpha,double beta, double* a, double *x, double *y);
void ji_(int sizeVector,int sizeAusgabe,double alpha,double beta, double* a, double *x, double *y);

/*****************************************************************************

LOG-History

$Log: work.h,v $
Revision 1.2  2006/04/10 08:23:50  rschoene
changed header/footer

Revision 1.1  2006/04/05 07:24:49  rschoene
gemv, double precision

(based on Robert Wlochs c kernel skeleton)
*****************************************************************************/
