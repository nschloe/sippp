#include "matmul1.h"
#include "matmul1_funcs.h"

/* matmul1_funcs.c/measurement
 * SYNOPSIS
 * The following are used to call the measuring (FORTRAN-)function
 * or they are measuring some kind of overhead.
 * They can be found in the file named
 * kernel/matmul1_double/matmul1_funcs.c
 ***/

/* matmul1_funcs.c/measurement::gettimeoverhead
 * SYNOPSIS
 * double gettimeroverhead()
 * DESCRIPTION
 * This function determines the time that is needed for the
 * execution of the function that measures the time.
 ***/
double gettimeroverhead()
   {
   double dstart, dstop;
   int ii;

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function gettimeroverhead\n" );
      fflush(stdout);
      }

   dstart = bi_gettime();
   for( ii = 0; ii < 1000; ii++ )
      {
      ( void ) bi_gettime();
      ( void ) bi_gettime();
      }
   dstop = bi_gettime();

   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function gettimeroverhead\n" );
      fflush(stdout);
      }

   return ( dstart - dstop ) / 1000;
   }

/* matmul1_funcs.c/measurement::gettimeoverhead
 * SYNOPSIS
 * void deallocate( fds *pmem )
 * DESCRIPTION
 * This function frees the structure that is allocated for saving
 * the vectors.
 ***/
void deallocate( fds *pmem )
   {

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function deallocate\n" );
      fflush(stdout);
      }

   if ( pmem != NULL )
      {
      if ( pmem->pda != NULL )
         {
         free( pmem->pda );
         }
      if ( pmem->pdb != NULL )
         {
         free( pmem->pdb );
         }
      if ( pmem->pdc != NULL )
         {
         free( pmem->pdc );
         } 
      } 

   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function deallocate\n" );
      fflush(stdout);
      }

   }

/* matmul1_funcs.c/measurement::gettimeoverhead
 * SYNOPSIS
 * void allocateANDtouch( fds *pmem, long *plsize )
 * DESCRIPTION
 * This function allocates the memory for the vectors and 
 * and assings the numbers.
 ***/
void allocateANDtouch( fds *pmem, int *pisize )
   {
   double *pda, *pdb, *pdc;
   int ii;

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function allocateANDtouch\n" );
      fflush(stdout);
      }

   pmem->pda = malloc( ( *pisize + 1 ) * ( *pisize + 1 )
                       * sizeof( double ) );
   pmem->pdb = malloc( ( *pisize + 1 ) * ( *pisize + 1 )
                       * sizeof( double ) );
   pmem->pdc = malloc( ( *pisize + 1 ) * ( *pisize + 1 )
                       * sizeof( double ) );
   if ( pmem->pda == NULL || pmem->pda == NULL || pmem->pdc == NULL )
      {
      printf( "malloc (%.2f MB) failed in bi_init()\n",
             ( double )( 3 * ( *pisize * *pisize )
             * sizeof( double ) ) / ( double )( 1024*1024 ) );
      deallocate( pmem );
      bi_cleanup( pmem );
      exit( 127 );
      }        

   pda = pmem->pda;
   pdb = pmem->pdb;
   pdc = pmem->pdc;

   for( ii = ( ( *pisize + 1 ) * ( *pisize + 1 ) - 1);
               ii >= 0; ii -- )
      {
      pda[ii] = ( double ) ii;
      pdb[ii] = ( double ) 1.1 * ii;
      pdc[ii] = ( double ) 0;
      }

   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function allocateANDtouch\n" );
      fflush(stdout);
      }

   }

/* matmul1_funcs.c/measurement::gettimeoverhead
 * SYNOPSIS
 * void entry_( void *ptr, long *size, int iunrolled )
 * DESCRIPTION
 * This function is used for calling the FORTRAN-function that
 * is the actual benchmarking function.
 ***/
void entry_( void *ptr, int *pisize, int iunrolled )
   {
   fds *pmem = ( fds* )ptr;
   double *pda = pmem->pda, *pdb = pmem->pdb, *pdc = pmem->pdc;
   int imysize = *pisize, iprec = pmem->MATMUL_PRECISION;

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function entry_\n" );
      fflush(stdout);
      }

   if ( *pisize == 0 )
      {
      return;
      }
   else
      {
      tstcas_( &imysize, &iprec, &iunrolled, pda, pdb, pdc );
      }

   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function entry_\n" );
      fflush(stdout);
      }

   }

/* matmul1_funcs.c/measurement::gettimeoverhead
 * SYNOPSIS
 * double getseqentryoverhead( void *pmem )
 * DESCRIPTION
 * This function is used for determining the timeoverhead for the 
 * calling of the FORTRAN-function that is the actual 
 * benchmarking function.
 ***/
double getseqentryoverhead( void *pmem )
   {
   double dstart, dstop;
   int ii, in = 0;

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function getseqentryoverhead\n" );
      fflush(stdout);
      }

   dstart = bi_gettime();

   for( ii = 0; ii < 1000; ii++)
      {
      entry_( pmem, &in, 1 );
      MPI_Barrier(MPI_COMM_WORLD);
      }

   dstop = bi_gettime();

   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function getseqentryoverhead\n" );
      fflush(stdout);
      }

   return ( dstop - dstart ) / 1000;
   }
