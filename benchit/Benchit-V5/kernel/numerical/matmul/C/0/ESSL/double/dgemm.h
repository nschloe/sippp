/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, BLAS, MKL (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.1 $
 * $Date: 2006/10/11 11:41:38 $
 *******************************************************************/

#ifndef DGEMM__

#define DGEMM__
	typedef struct floating_data_struct {
	  double *feld1, *feld2, *feld3;
	} fds;

	long bi_dgemm_start, bi_dgemm_stop, bi_dgemm_increment; 

#else
	extern long bi_dgemm_start, bi_dgemm_stop, bi_dgemm_increment; 

#endif


/********************************************************************
 * Log-History
 *
 * $Log: dgemm.h,v $
 * Revision 1.1  2006/10/11 11:41:38  hackenb
 * initial commit
 *
 *
 * 
 *******************************************************************/
