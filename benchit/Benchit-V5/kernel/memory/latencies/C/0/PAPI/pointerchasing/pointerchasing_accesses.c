/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
* Core for Memory Access Time (C)
*
* Author: Michael Kluge (kluge@zhr.tu-dresden.de)
*
* Version: 1.0
*
* Date:  2002/12/27
*  Start: 2002/09/27
*
***********************************************************************/


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
