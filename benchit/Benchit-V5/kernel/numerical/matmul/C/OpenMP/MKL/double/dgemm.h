/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, BLAS, MKL (C) - OpenMP version
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.1 $
 * $Date: 2006/07/19 12:10:58 $
 *******************************************************************/

#ifndef DGEMM__

#define DGEMM__
	typedef struct floating_data_struct {
	  double *feld1, *feld2, *feld3;
	} fds;

	long MIN, MAX, INCREMENT; 

#else
	extern long MIN, MAX, INCREMENT; 

#endif


/********************************************************************
 * Log-History
 *
 * $Log: dgemm.h,v $
 * Revision 1.1  2006/07/19 12:10:58  hackenb
 * + initial commit
 *
 * 
 *******************************************************************/
