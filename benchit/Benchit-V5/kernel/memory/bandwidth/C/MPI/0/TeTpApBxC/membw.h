/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Memory Bandwidth (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.2 $
 * $Date: 2006/01/03 18:10:34 $
 *******************************************************************/

#ifndef BENCHIT_MEMORY_TEST_H
#define BENCHIT_MEMORY_TEST_H

typedef struct vs{
  double *a;
  double *b;
  double *c;
} vec_struct;

double mem_read( double *a, double *b, double *c,int problemsize);

#endif /* BENCHIT_MEMORY_TEST_H */


/********************************************************************
 * Log-History
 * 
 * $Log: membw.h,v $
 * Revision 1.2  2006/01/03 18:10:34  hackenb
 * modified/unified header and footer
 * new interface
 * now using bi_gettime()
 *
 * Revision 1.1  2005/08/18 14:00:35  juckel
 * rename
 * 
 * Revision 1.2  2005/08/18 11:42:46  juckel
 * included barrier after measurement and accumulated bandwidth over processors
 * 
 * Revision 1.1  2005/08/17 10:23:52  juckel
 * added kernel to cvs
 * 
 * Revision 1.1  2005/08/16 14:20:53  juckel
 * - included kernel from old benchit into new structure
 * - changed MPI-functionality
 *   old: only root process runs memory test -> generates 1 data set
 *   new: for i=1 to numproc do ruu mesurement -> generates numproc data sets
 * 
 *******************************************************************/
