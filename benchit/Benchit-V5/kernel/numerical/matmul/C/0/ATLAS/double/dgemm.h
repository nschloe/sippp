/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, BLAS, ATLAS (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.3 $
 * $Date: 2005/12/15 09:28:27 $
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
 * Revision 1.3  2005/12/15 09:28:27  hackenb
 * new variable names
 * modified/unified header and footer
 * MIN/MAX/INCREMENT iterating strategy
 *
 * 
 *******************************************************************/
