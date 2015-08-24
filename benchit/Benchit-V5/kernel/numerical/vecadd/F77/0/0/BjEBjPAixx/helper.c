/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.4 $
 *  $Date: 2006/01/30 13:54:58 $
 *******************************************************************/
#include "./work.h"
#include "./helper.h"



/* This function frees the structure that is allocated for saving
 * the vectors.
 ***/
void deallocate( fds *pmem )
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
   }
}



/* This function allocates the memory for the vectors and 
 * and assings the numbers.
 ***/
void allocateANDtouch( fds *pmem, int *pisize )
{
   double *pda, *pdb;
   int ii, maxproblemsize;
   
   pmem->pda = malloc( ( *pisize + 1 ) * sizeof( double ) );
   pmem->pdb = malloc( ( *pisize + 1 ) * sizeof( double ) );

   if( ( pmem->pda == NULL ) || ( pmem->pdb == NULL ) )
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
   maxproblemsize=pmem->VECADD_START + pmem->VECADD_INCREMENT * pmem->VECADD_STEPS;
   
/*   printf("maxproblemsize=%d Iterations=%d\n",maxproblemsize, pmem->VECADD_PRECISION);
   fflush(stdout);
*/
   for( ii = * pisize - 1; ii >= 0; ii -- )
   {
      pda[ii]= (double) random();
      pdb[ii]= (double) random();
/*
      pda[ii]= 1;
      pdb[ii]= 1;
*/
   }

}



/* This function is used for calling the FORTRAN-function that
 * is the actual benchmarking function.
 ***/
void entry_( void * ptr, int * pisize, int iunrolled )
   {
   fds *pmem = ( fds* )ptr;
   double *pda = pmem->pda, *pdb = pmem->pdb;
   int imysize=3, iprec = pmem->VECADD_PRECISION;

   imysize = * pisize;
   
   if( imysize == 3 || imysize == 0 )
   {
     printf("entry_problemsize=%d\n",imysize);
     fflush(stdout);
   }
   else 
   {
/*
      printf("helper.c: entry_( %d, %d, %d );\n", ptr, imysize, iunrolled);
      printf("helper.c: vecadd_( %d, %d, %d, %d, %d \n",imysize, iprec, iunrolled, pmem->pda, pmem->pdb );
      printf("pda=%.2f pdb=%.2f", pmem->pda[1], pmem->pdb[1]);
*/
      vecadd_( &imysize, &iprec, &iunrolled, pmem->pda, pmem->pdb, &pmem->sum );
   }
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
