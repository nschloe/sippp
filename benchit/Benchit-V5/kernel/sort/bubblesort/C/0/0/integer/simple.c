/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
  * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2006/07/03 22:32:24 $
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


void bubblesorti( int *pisort, long lnumber )
   {
   int  ih, ii, ij;

   /*initialize variables*/
   ih = 0;
   ii = 0;
   ij = 0;

   /*moves "biggest" element in the rest array
     to the correct position by... */
   for ( ii = ( lnumber - 1 ); ii >= 0; ii-- )
      {
      /*changing neighbours until the position is set */
      for ( ij = 1; ij <= ii; ij++ )
         {
         /*if neighbouring objects are wrong way around ... */
         if ( pisort[ij - 1] > pisort[ij] )
            {
            /*change them */
            ih = pisort[ij - 1];
            pisort[ij - 1] = pisort[ij];
            pisort[ij] = ih;
            }
         }
      }
   }


int verifyi( int *piprobe, long lelements )
   {
   int  ii;

   /*initialize variables*/
   ii = 0;

/*any element on position n+1 has to be larger 
  or equal to element on position n...*/
   for ( ii = 1; ii < lelements; ii++ )
      {
      /*if not -> "0" means failure */
      if ( piprobe[ii - 1] > piprobe[ii] )
         {
         return 0;
         }
      }

   /*"1" means success */
   return 1;
   }


/********************************************************************
 * Log-History
 * 
 *******************************************************************/
