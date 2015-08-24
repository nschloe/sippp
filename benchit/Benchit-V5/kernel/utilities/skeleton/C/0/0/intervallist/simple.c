/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton with list PARAMETER
  * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2007/06/19 12:57:44 $
 *******************************************************************/
 
#include "simple.h"
#include "interface.h"

double simple( myinttype * pi_prob_size )
{
  double dresult = 1.0;
  myinttype ii = 0, pre = 0, prepre = 0, ij = 0;
  
/*
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
*/
              
	ii=*pi_prob_size;
	ii = ii * ii;
	ij = ii * ii;
  for (ii=*pi_prob_size; ii>0; ii--)
  {
    dresult = dresult * ii;
    dresult = sqrt(dresult);
 		dresult = dresult * dresult;
 		dresult = dresult + dresult;
 		dresult = dresult - dresult;
		for (ij=*pi_prob_size; ij>0; ij--)
		  {
		    dresult = dresult * ii;		    
 		   dresult = sqrt(dresult);
 		   dresult = dresult * dresult;
 		   dresult = dresult + dresult;
 		   dresult = dresult - dresult;
  		}
  }


  return dresult;
}


/********************************************************************
 * Log-History
 * 
 *******************************************************************/
