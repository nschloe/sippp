/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Memory Access Time (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.2 $
 * $Date: 2006/01/03 15:34:34 $
 *******************************************************************/

#ifndef BENCHIT_MEMORY_TEST_H
#define BENCHIT_MEMORY_TEST_H

void* jump_around( void *mcb, long numjumps);

#endif /* BENCHIT_MEMORY_TEST_H */


/********************************************************************
 * Log-History
 * 
 * $Log: pointerchasing.h,v $
 * Revision 1.2  2006/01/03 15:34:34  hackenb
 * modified/unified header and footer
 * new interface
 *
 * Revision 1.1  2005/09/02 11:44:11  mark
 * *** empty log message ***
 * 
 * Revision 1.2  2005/08/24 12:23:20  juckel
 * added NUMBER_OF_JUMPS to PARAMETERS
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
