/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: SSE2 Matrix Multiply (C), unaligned data
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.3 $
 * $Date: 2005/12/15 09:28:29 $
 *******************************************************************/

#ifndef __work_h
#define __work_h
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
   double* a;
   double* b;
   double* c;
/*
   <variabletype1> <variablename1>;
   <variabletype2> <variablename2>;
*/
} mydata_t;

void multasseijk_( double *a, double *b, double *c, int *size );
void multasseikj_( double *a, double *b, double *c, int *size );
void multassejik_( double *a, double *b, double *c, int *size );
void multassejki_( double *a, double *b, double *c, int *size );
void multassekji_( double *a, double *b, double *c, int *size );
void multassekij_( double *a, double *b, double *c, int *size );


/***************************************************************************** 
LOG-History
 
$Log: work.h,v $
Revision 1.3  2005/12/15 09:28:29  hackenb
new variable names
modified/unified header and footer
MIN/MAX/INCREMENT iterating strategy

Revision 1.2  2005/11/09 11:59:52  rschoene
commenting ... bah

Revision 1.1  2005/11/09 09:12:23  rschoene
initial SSE2 matmul (double precision, unaligned data)

*****************************************************************************/
