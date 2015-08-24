/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.3 $
 *  $Date: 2006/01/03 14:00:06 $
 *******************************************************************/
#include "./work.h"
#include "./helper.h"



double gettimeroverhead() 
{
   double start, stop,t;
   int s;

   start= bi_timer();
   for( s=0; s<10000; s++ )bi_timer();
   stop= bi_timer();
   t= stop-start;
   return t/10000.0;
}



/* This function frees the structure that is allocated for saving
 * the vectors.
 ***/
void deallocate( mydata_t *pmem )
{
   if( pmem != NULL )
   {
      if( pmem->pda != NULL )
      {
         free( pmem->pda );
         pmem->pda = NULL;
      }
      if( pmem->pdb != NULL )
      {
         free( pmem->pdb );
         pmem->pdb = NULL;
      }
      if( pmem->pdc != NULL )
      {
         free( pmem->pdc );
         pmem->pdc = NULL;
      }
   }
}



/* This function allocates the memory for the vectors and 
 * and assings the numbers.
 ***/
void allocateANDtouch( mydata_t *pmem, int *pisize )
{
   double *pda, *pdb, *pdc;
   int ii;

   pmem->pda = malloc( ( *pisize + 1 ) * sizeof( double ) );
   pmem->pdb = malloc( ( *pisize + 1 ) * sizeof( double ) );
   pmem->pdc = malloc( ( *pisize + 1 ) * sizeof( double ) );

   if( ( pmem->pda == NULL ) || ( pmem->pdb == NULL )|| ( pmem->pdc == NULL ) )
   {
      printf( "malloc (%.2f MB) failed in bi_init()\n",
             ( double )( 2 * ( * pisize ) * sizeof( double ) )
             / ( double )( 1024*1024 ) );
      deallocate( pmem );
      bi_cleanup( pmem );
      exit( 127 );
   }

   pda = pmem->pda;
   pdb = pmem->pdb;
   pdc = pmem->pdc;

   for( ii = * pisize - 1; ii >= 0; ii -- )
   {
      pda[ii]=1;
      pdb[ii]=1;
      pdc[ii]=1;
   }

}



/* This function is used for calling the FORTRAN-function that
 * is the actual benchmarking function.
 ***/
void entry_( void *ptr, int *pisize, int iunrolled )
   {
   mydata_t *pmem = ( mydata_t* )ptr;
   double *pda = pmem->pda, *pdb = pmem->pdb, *pdc = pmem->pdc;
   int imysize = *pisize, iprec = pmem->precision;

   if( *pisize == 0 ) return;
   else vecadd_( &imysize, &iprec, &iunrolled, pda, pdb, pdc );
}




/* This function is used for determining the timeoverhead for the 
 * calling of the FORTRAN-function that is the actual 
 * benchmarking function.
 ***/
double getseqentryoverhead( void *pmem )
{
   double dstart, dstop;
   int ii, in = 0;

   dstart = bi_timer();

   for( ii = 0; ii < 1000; ii++)
   {
      entry_( pmem, &in, 1 );
   }

   dstop = bi_timer();
   return ( dstop - dstart ) / 1000.0;
}
