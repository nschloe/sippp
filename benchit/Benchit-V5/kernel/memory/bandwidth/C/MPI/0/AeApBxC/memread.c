/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Memory Bandwidth (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.2 $
 * $Date: 2006/01/03 18:10:33 $
 *******************************************************************/

#include "interface.h"
#include "membw.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

void mem_read( double *a, double *b, double *c, int problemsize) {
  long i;

  if(problemsize==0)
    return;	


  for( i=0; i<problemsize; i++) {
    a[i]+=b[i]*c[i];
      }
  return;
}


/********************************************************************
 * Log-History
 * 
 * $Log: memread.c,v $
 * Revision 1.2  2006/01/03 18:10:33  hackenb
 * modified/unified header and footer
 * new interface
 * now using bi_gettime()
 *
 * Revision 1.1  2005/08/18 14:26:21  juckel
 * *** empty log message ***
 * 
 * Revision 1.2  2005/08/18 11:42:47  juckel
 * included barrier after measurement and accumulated bandwidth over processors
 * 
 * Revision 1.1  2005/08/17 10:23:52  juckel
 * added kernel to cvs
 * 
 *******************************************************************/
