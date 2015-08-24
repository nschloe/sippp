/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
  * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2006/07/03 22:34:28 $
 *******************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "simple.h"
#include "interface.h"


/**
 * compare-function needed by the clib-qsort
 * integer-elements
 */
int quicksort_clib_myinttype(  const void *pvelement1, const void *pvelement2 )
{
   myinttype  ii, ij;

   /*initialize variables*/
   ii=0;
   ij=0;

   /*debugging level 1: mark begin and end of function*/
   if ( DEBUGLEVEL > 0 )
      {
      prmyinttypef( "reached function quicksort_clib_myinttype\n" );
      fflush(stdout);
      }

   /*void pomyinttypeers must be casted*/
   ii = ( myinttype ) ( *( myinttype * ) pvelement1 );
   ij = ( myinttype ) ( *( myinttype * ) pvelement2 );

   if ( ii > ij )
      {
      /*debugging level 1: mark begin and end of function*/
      if ( DEBUGLEVEL > 0 )
         {
         prmyinttypef( "completed function quicksort_clib_myinttype\n" );
         fflush(stdout);
         }
      return 1;
      }
   if ( ii < ij )
      {
      /*debugging level 1: mark begin and end of function*/
      if ( DEBUGLEVEL > 0 )
         {
         prmyinttypef( "completed function quicksort_clib_myinttype\n" );
         fflush(stdout);
         }
      return -1;
      }
   /*debugging level 1: mark begin and end of function*/
   if ( DEBUGLEVEL > 0 )
      {
      prmyinttypef( "completed function quicksort_clib_myinttype\n" );
      fflush(stdout);
      }
   return 0;
}

/**
 * compare-function needed by the clib-qsort
 * float-elements
 */
int quicksort_clib_flt(  const void *pvelement1, const void *pvelement2 )
{
   float fi, fj;

   /*initialize variables*/
   fi=0;
   fj=0;

   /*debugging level 1: mark begin and end of function*/
   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function quicksort_clib_flt\n" );
      fflush(stdout);
      }

   /*void pointers must be casted*/
   fi = ( float ) ( *( float * ) pvelement1 );
   fj = ( float ) ( *( float * ) pvelement2 );

   if ( fi > fj )
      {
      /*debugging level 1: mark begin and end of function*/
      if ( DEBUGLEVEL > 0 )
         {
         printf( "completed function quicksort_clib_flt\n" );
         fflush(stdout);
         }
      return 1;
      }
   if ( fi < fj )
      {
         /*debugging level 1: mark begin and end of function*/
      if ( DEBUGLEVEL > 0 )
         {
         printf( "completed function quicksort_clib_flt\n" );
         fflush(stdout);
         }
         return -1;
      }
   /*debugging level 1: mark begin and end of function*/
   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function quicksort_clib_flt\n" );
      fflush(stdout);
      }
   return 0;
}


/**
 * compare-function needed by the clib-qsort
 * double-elements
 */
int quicksort_clib_dbl(  const void *pvelement1, const void *pvelement2 )
{
   double fi, fj;

   /*initialize variables*/
   fi=0;
   fj=0;

   /*debugging level 1: mark begin and end of function*/
   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function quicksort_clib_dbl\n" );
      fflush(stdout);
      }

   /*void pointers must be casted*/
   fi = ( double ) ( *( double * ) pvelement1 );
   fj = ( double ) ( *( double * ) pvelement2 );

   if ( fi > fj )
      {
      /*debugging level 1: mark begin and end of function*/
      if ( DEBUGLEVEL > 0 )
         {
         printf( "completed function quicksort_clib_dbl\n" );
         fflush(stdout);
         }
      return 1;
      }
   if ( fi < fj )
      {
         /*debugging level 1: mark begin and end of function*/
      if ( DEBUGLEVEL > 0 )
         {
         printf( "completed function quicksort_clib_dbl\n" );
         fflush(stdout);
         }
         return -1;
      }
   /*debugging level 1: mark begin and end of function*/
   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function quicksort_clib_dbl\n" );
      fflush(stdout);
      }
   return 0;
}



int verify_int( myinttype *pfprobe, long lelements )
   {
   int ii;

   /*initialize variables*/
   ii = 0;

/*any element on position n+1 has to be larger
  or equal to element on position n...*/
   for ( ii = 1; ii < lelements; ii++ )
      {
      if ( pfprobe[ii - 1] > pfprobe[ii] )
         {
         return 0;
         }
      }

   /*"1" means success */
   return 1;
   }



int verify_float( float *pfprobe, long lelements )
   {
   int ii;

   /*initialize variables*/
   ii = 0;

/*any element on position n+1 has to be larger
  or equal to element on position n...*/
   for ( ii = 1; ii < lelements; ii++ )
      {
      if ( pfprobe[ii - 1] > pfprobe[ii] )
         {
         return 0;
         }
      }

   /*"1" means success */
   return 1;
   }



int verify_double( double *pfprobe, long lelements )
   {
   int ii;

   /*initialize variables*/
   ii = 0;

/*any element on position n+1 has to be larger
  or equal to element on position n...*/
   for ( ii = 1; ii < lelements; ii++ )
      {
      if ( pfprobe[ii - 1] > pfprobe[ii] )
         {
         return 0;
         }
      }

   /*"1" means success */
   return 1;
   }



void quicksort_wikipedia_int(int * a, int al, int ar) {
        int links=al, rechts=ar, pivo=a[(al+ar)/2], tmp;
        do {
                while(a[links]<pivo) links++;
                while(a[rechts]>pivo) rechts--;
                if (links <= rechts) {
                        tmp=a[links];
                        a[links]=a[rechts];
                        a[rechts]=tmp;
                        links++;
                        rechts--;
                }       
        } while(links<rechts);
        if (al < rechts) quicksort_wikipedia_int(a, al, rechts);
        if (links < ar) quicksort_wikipedia_int(a, links, ar);
}



void quicksort_wikipedia_flt(float * a, int al, int ar) {
        int links=al, rechts=ar; 
		float pivo=a[(al+ar)/2], tmp;
        do {
                while(a[links]<pivo) links++;
                while(a[rechts]>pivo) rechts--;
                if (links <= rechts) {
                        tmp=a[links];
                        a[links]=a[rechts];
                        a[rechts]=tmp;
                        links++;
                        rechts--;
                }       
        } while(links<rechts);
        if (al < rechts) quicksort_wikipedia_flt(a, al, rechts);
        if (links < ar) quicksort_wikipedia_flt(a, links, ar);
}



void quicksort_wikipedia_dbl(double * a, int al, int ar) {
        int links=al, rechts=ar; 
		double pivo=a[(al+ar)/2], tmp;
        do {
                while(a[links]<pivo) links++;
                while(a[rechts]>pivo) rechts--;
                if (links <= rechts) {
                        tmp=a[links];
                        a[links]=a[rechts];
                        a[rechts]=tmp;
                        links++;
                        rechts--;
                }       
        } while(links<rechts);
        if (al < rechts) quicksort_wikipedia_dbl(a, al, rechts);
        if (links < ar) quicksort_wikipedia_dbl(a, links, ar);
}

/********************************************************************
 * Log-History
 * 
 * $Log: simple.c,v $
 * Revision 1.1  2006/07/03 22:34:28  william
 * quicksort up and running
 *
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
