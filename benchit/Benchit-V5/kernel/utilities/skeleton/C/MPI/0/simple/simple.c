/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton with MPI
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.3 $
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
 * Revision 1.3  2006/01/09 16:24:20  william
 * updated the cvs-header
 *
 * Revision 1.2  2006/01/09 16:01:43  william
 * cvs-keyword-problems
 *
 * Revision 1.1  2006/01/05 19:37:36  william
 * A simple Version of the skeleton with mpi for easy first time development
 *
 * 
 *******************************************************************/
