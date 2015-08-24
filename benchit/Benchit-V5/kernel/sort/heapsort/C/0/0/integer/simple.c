/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
  * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2006/07/24 16:19:11 $
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


void heapsorti( int *pisort, long lnumber )
   {
   /*variables for looping and buffering*/
   int  ih, ii, ij, ik;

   /*initialize variables*/
   ih = 0;
   ii = 0;
   ij = 0;
   ik = 0;

   /*necessary for initialized use*/
   ih = 0;
   ij = 0;
   /*creating heap */
   for ( ii = lnumber >> 1; ii > 0; ii-- )
      {
      /*all nodes beginning in the 2nd level from below
        are pushed down to*/
      /*a lower level if necessary*/
      ih = pisort[ii];
      ik = ii;
      /*as long as there is a lower level*/
      while ( ik <= lnumber >> 1 )
         {
         /*find out which the bigger one of the two children*/
         ij = ik + ik;
         if ( ij < lnumber )
            {
            if ( pisort[ij] < pisort[ij + 1] )
               {
               ij = ij + 1;
               }
            }
         /*if the childs are smaller -> ok break up*/
         if ( ih >= pisort[ij] )
            {
            break;
            }
         /*if not -> write the bigger child
           to the place of the parent*/
         pisort[ik] = pisort[ij];
         /*and continue with the level below*/
         ik = ij;
         }
      /*write the examined number to the correct place*/
      pisort[ik] = ih;
      }
   /*dismantle heap*/
   while ( lnumber > 0 )
      {
      /*the biggest element is always the root element
        -> remove root element -> change it with the last element*/
      ii = pisort[1];
      pisort[1] = pisort[lnumber];
      pisort[lnumber] = ii;
      /*heap is smaller now (root element was removed)*/
      lnumber--;
      /*now recreate the heap by moving the element that was
        changed with the root element (->element on place 1)
        to the correct place*/
      ik = 1;
      ih = pisort[ik];
      /*as long as there is a lower level*/
      while ( ik <= lnumber >> 1 )
         {
         /*find out which the bigger one of the two children*/
         ij = ik + ik;
         if ( ij < lnumber )
            {
            if ( pisort[ij] < pisort[ij + 1] )
               {
               ij = ij + 1;
               }
            }
         /*if the childs are smaller -> ok break up*/
         if ( ih >= pisort[ij] )
            {
            break;
            }
         /*if not -> write the bigger child to the place of the parent*/
         pisort[ik] = pisort[ij];
         /*and continue with the level below*/
         ik = ij;
         }
      /*write the examined number to the correct place*/
      pisort[ik] = ih;
      }
   }


int verifyi( int *piprobe, long lelements )
   {
   int ii;

   /*initialize variables*/
   ii = 0;

   /*any element on position n+1 has to be larger or equal to element on
     position n...*/
   for ( ii = 2; ii < lelements + 1; ii++ )
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
