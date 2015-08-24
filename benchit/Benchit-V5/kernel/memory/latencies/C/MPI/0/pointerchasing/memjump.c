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

#include "interface.h"
#include "memacc.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#define ONE {ptr=(void **) *ptr;}
#define TEN ONE ONE ONE ONE ONE ONE ONE ONE ONE ONE
#define HUN TEN TEN TEN TEN TEN TEN TEN TEN TEN TEN
#define THO HUN HUN HUN HUN HUN HUN HUN HUN HUN HUN

void *jump_around( void *mem, int problemsize, long numjumps) {
  void **ptr;
  int a;

  if(problemsize==0)
    return (void *) 0;	

  ptr=(void **) mem;

  /* numjump Spruenge im Kreis :-) */
  for( a=0; a<numjumps/100; a++) {
    HUN
      }
  return (void *) ptr;
}


/********************************************************************
 * Log-History
 * 
 * $Log: memjump.c,v $
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
 * Revision 1.1  2003/12/01 20:52:03  kluge
 * memaccess_c initial checkin
 * 
 * Revision 1.5  2003/05/06 10:53:49  kluge
 * latest changes for new interface.h done
 * 
 * Revision 1.4  2003/04/13 15:11:26  juckel
 * Bugfix in function jump_around: \n- incompatible declarations in cache_test.h and cache_jump.c\n- return w./o. a value in function jump_around
 * 
 * Revision 1.3  2003/03/14 20:59:51  kluge
 * updated romulus LOCALDEFS and parallel matrix multiply
 * 
 * Revision 1.2  2003/01/28 11:29:19  kluge
 * new header
 * 
 *******************************************************************/
