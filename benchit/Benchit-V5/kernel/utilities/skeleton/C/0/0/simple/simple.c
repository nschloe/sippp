/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
  * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.5 $
 * $Date: 2006/01/09 16:24:20 $
 *******************************************************************/
 
#include "simple.h"
#include "interface.h"

double simple( myinttype * pi_prob_size )
{
  double dresult = 1.0;
  myinttype ii = 0, pre = 0, prepre = 0;
  

  switch ( * pi_prob_size )
  {
    case 0:
            break;
    case 1:
            break;
    default:
            pre = *pi_prob_size - 1;
            prepre = pre - 1;
            dresult = (double) (simple(&pre) + simple(&prepre));
  }

              
/*  for (ii=*pi_prob_size; ii>0; ii--)
  {
    dresult = dresult * ii;
    dresult = sqrt(dresult);
  }
*/
  return dresult;
}


/********************************************************************
 * Log-History
 * 
 * $Log: simple.c,v $
 * Revision 1.5  2006/01/09 16:24:20  william
 * updated the cvs-header
 *
 * Revision 1.4  2006/01/09 15:57:01  william
 * cvs-keyword-problems
 *
 * Revision 1.3  2005/12/15 15:44:17  hackenb
 * modified/unified header and footer
 *
 * Revision 1.2  2005/12/14 23:33:32  william
 * changed the algorythm of the skeleton -> fibonacci numbers
 *
 * Revision 1.1  2005/12/14 22:37:12  william
 * A simple Version of the skeleton for easy first time development
 * 
 *******************************************************************/
