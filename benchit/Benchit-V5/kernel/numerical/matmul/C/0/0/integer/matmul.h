/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply (C) integer
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.2 $
 * $Date: 2006/05/11 11:40:04 $
 *******************************************************************/

#ifndef BENCHIT_MATMUL_H
#define BENCHIT_MATMUL_H

#endif /* BENCHIT_MATMUL_H */

#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif



/********************************************************************
 * Log-History
 * 
 * $Log: matmul.h,v $
 * Revision 1.2  2006/05/11 11:40:04  hackenb
 * bugfix (myinttype)
 *
 * Revision 1.1  2006/04/24 12:21:46  rschoene
 * initial commit matmul-c-int, based on matmul-c-double
 *
 * 
 *******************************************************************/
