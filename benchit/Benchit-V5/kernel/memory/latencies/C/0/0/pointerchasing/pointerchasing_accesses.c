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

#define ONE {ptr=(void **) *ptr;}
#define TEN ONE ONE ONE ONE ONE ONE ONE ONE ONE ONE
#define HUN TEN TEN TEN TEN TEN TEN TEN TEN TEN TEN
#define THO HUN HUN HUN HUN HUN HUN HUN HUN HUN HUN

void *jump_around( void *mem, long n) {
  void **ptr;
  long a;


  ptr=(void **) mem;

  /* numjump Sprnge im Kreis :-) */
  for( a=0; a<n/100; a++) {
    HUN
      }
  return (void *) ptr;
}


/********************************************************************
 * Log-History
 * 
 * $Log: pointerchasing_accesses.c,v $
 * Revision 1.2  2006/01/03 15:34:34  hackenb
 * modified/unified header and footer
 * new interface
 *
 * 
 *******************************************************************/
