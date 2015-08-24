/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Memory Access Time (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.2 $
 * $Date: 2006/01/03 16:42:01 $
 *******************************************************************/

#ifndef BENCHIT_MEMORY_TEST_H
#define BENCHIT_MEMORY_TEST_H

void* jump_around( void *mcb, int problemsize, long numjumps);

#endif /* BENCHIT_MEMORY_TEST_H */


/********************************************************************
 * Log-History
 * 
 * $Log: memacc.h,v $
 * Revision 1.2  2006/01/03 16:42:01  hackenb
 * modified/unified header and footer
 * new interface
 * now using bi_gettime()
 *
 * Revision 1.1  2005/08/24 12:22:51  juckel
 * first checkin
 * 
 * Revision 1.1  2005/08/16 14:20:53  juckel
 * - included kernel from old benchit into new structure
 * - changed MPI-functionality
 *   old: only root process runs memory test -> generates 1 data set
 *   new: for i=1 to numproc do ruu mesurement -> generates numproc data sets
 * 
 * Revision 2.0  2003/12/09 11:19:01  juckel
 * build of version 2.0
 * 
 * Revision 1.2  2003/12/04 09:56:09  kluge
 * memacces has now a README and is ready to be used
 * 
 * Revision 1.1  2003/12/01 20:52:03  kluge
 * memaccess_c initial checkin
 * 
 *******************************************************************/
