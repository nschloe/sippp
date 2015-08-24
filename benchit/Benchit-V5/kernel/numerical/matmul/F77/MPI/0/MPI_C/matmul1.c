#include "matmul1.h"
#include "matmul1_funcs.h"

int MATMUL_START;
int MATMUL_INCREMENT;
int MATMUL_STEPS;
int MATMUL_PRECISION;

int isize, irank;



/* Reads the environment variables used by this kernel. */
void evaluate_environment()
{
   int errors = 0;
   char * p = 0;
   MATMUL_START = 1;
   MATMUL_INCREMENT = 1;
   MATMUL_STEPS = 10;   
   MATMUL_PRECISION = 1;
   
   p = bi_getenv( "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIC_START", 0 );
   if ( p == 0 ) errors++;
   else MATMUL_START = atoi( p );
   
   p = bi_getenv( "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIC_INCREMENT", 0 );
   if ( p == 0 ) errors++;
   else MATMUL_INCREMENT = atoi( p );
   
   p = bi_getenv( "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIC_STEPS", 0 );
   if ( p == 0 ) errors++;
   else MATMUL_STEPS = atoi( p );
   
   p = bi_getenv( "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIC_PRECISION", 0 );
   if ( p == 0 ) errors++;
   else MATMUL_PRECISION = atoi( p );

   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      fprintf( stderr, "This kernel needs the following environment variables:\n" );
      fprintf( stderr, "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIC_START\n" );
      fprintf( stderr, "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIC_INCREMENT\n" );
      fprintf( stderr, "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIC_STEPS\n" );
      fprintf( stderr, "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIC_PRECISION\n" );
      fprintf( stderr, "\tThis kernel will vary the vectorlength from START to START+INCREMENT*STEPS in INCREMENT steps.\n" );
      exit( 1 );
   }
}

/* matmul1.c/interface
 * SYNOPSIS
 * The following functions are all used in the matmul1-kernel
 * for communication with the benchit interface.
 * They can be found in the file named kernel/matmul1_double/matmul1.c
 ***/

/* matmul1.c/interface::bi_getinfo 
 * DESCRIPTION
 * This function does the same in every kernel.
 * It is used for passing all relevant informations that are needed
 * for generating the diagramms like legendtexts, marking of the axis
 * and so on.
 ***/






void bi_getinfo( bi_info * infostruct )
   {
   int  ii;
   char buffer[200];

   (void) memset ( infostruct, 0, sizeof( bi_info ) );

   /* get environment variables for the kernel */
   evaluate_environment();
   infostruct->codesequence = bi_strdup ( "jik, jik(temp), jki, kji, kij, ikj, ijk" );
   infostruct->maxproblemsize = MATMUL_STEPS;
   infostruct->xaxistext = bi_strdup( "matrix size" );
   infostruct->kernel_execs_mpi1 = 1;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 0;
   infostruct->kernel_execs_pthreads = 0;
   infostruct->numfunctions = 7;

   /* allocating memory for y axis texts and properties */
   infostruct->yaxistexts = malloc( infostruct->numfunctions * sizeof ( char * ) );
   if ( infostruct->yaxistexts == 0 )
   {
     fprintf( stderr, "Allocation of yaxistexts failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   infostruct->outlier_direction_upwards = malloc( infostruct->numfunctions * sizeof( int ) );
   if ( infostruct->outlier_direction_upwards == 0 )
   {
     fprintf( stderr, "Allocation of outlier direction failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   infostruct->legendtexts = malloc( infostruct->numfunctions * sizeof( char* ) );
   if ( infostruct->legendtexts == 0 )
   {
     fprintf( stderr, "Allocation of legendtexts failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   infostruct->log_yaxis = malloc( infostruct->numfunctions * sizeof( int ) );
   if ( infostruct->log_yaxis == 0 )
   {
     fprintf( stderr, "Allocation of log yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   infostruct->base_yaxis = malloc( infostruct->numfunctions * sizeof( double ) );
   if ( infostruct->base_yaxis == 0 )
   {
     fprintf( stderr, "Allocation of base yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }

   /* setting up y axis texts and properties */
   for ( ii = 0; ii < infostruct->numfunctions; ii++ )
   {
      infostruct->yaxistexts[ii] = bi_strdup( "FLOPS" );
      infostruct->outlier_direction_upwards[ii] = 1;
      infostruct->log_yaxis[ii] = 0;
      infostruct->base_yaxis[ii] = 0;
   }

   infostruct->legendtexts[0] = bi_strdup( "jik" );
   infostruct->legendtexts[1] = bi_strdup( "jik(temp)" );
   infostruct->legendtexts[2] = bi_strdup( "jki" );
   infostruct->legendtexts[3] = bi_strdup( "kji" );
   infostruct->legendtexts[4] = bi_strdup( "kij" );
   infostruct->legendtexts[5] = bi_strdup( "ikj" );
   infostruct->legendtexts[6] = bi_strdup( "ijk" );


   if ( DEBUGLEVEL > 3 )
   {
      for ( ii = 0; ii < infostruct->numfunctions; ii++ )
      {
         printf( "yaxis[%2d]=%s\t\t\tlegend[%2d]=%s\n",
            ii, infostruct->yaxistexts[ii], ii, infostruct->legendtexts[ii] );
      }
   }
}



/* matmul1.c/interface::bi_init
 * DESCRIPTION
 * This function is globally used for the preparation of the
 * measurement(s).
 * The argument problemsizemax equals infostruct->maxproblemsize
 * but it is not used in this kernel.
 * In this function the arrays are created that will be sorted later.
 ***/
void *bi_init( int iproblemsizemax )
   {
   fds *pmyfds;

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function bi_init\n" );
      fflush(stdout);
      }

    MPI_Comm_rank ( MPI_COMM_WORLD, &irank );
    MPI_Comm_size ( MPI_COMM_WORLD, &isize );

    pmyfds=malloc( sizeof( fds ) );

    if ( pmyfds==NULL )
       {
       printf("Allocation of myfds failed (problemsize max %d)\n",
             iproblemsizemax);
       exit(127);
       }

   pmyfds->MATMUL_START =  MATMUL_START;
   pmyfds->MATMUL_INCREMENT = MATMUL_INCREMENT;
   pmyfds->MATMUL_STEPS = MATMUL_STEPS;
   pmyfds->MATMUL_PRECISION = MATMUL_PRECISION;

   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function bi_init\n" ); 
      fflush(stdout);
      }

   return pmyfds;
   }

/* matmul1.c/interface::bi_entry
 * DESCRIPTION
 * This function is globally used for starting the measurment.
 * In this case the time is measured that is needed for the
 * execution of "tstcas_".
 * Afterwards the validation is made by just checking if
 * the numbers are in the correct order.
 * SEE ALSO
 * matmul1.c/measurement::tstcas_
 ***/
int bi_entry( void *pmyfds, int icallsize, double *pdresults )
   {
   static double dtimeroverhead = 0, dcalloverhead = 0;
   double dtime;
   int itype, isendbuf, *ircvbuf, inumproz, iproblemsize;

   dtime = 0;
   inumproz=0;
   ircvbuf = &inumproz;
   isendbuf = 1;

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function bi_entry\n" );
      fflush(stdout);
      }

   /* calculate overhead if it is not done yet */
   if ( dtimeroverhead == 0.0 )
      {
      dtimeroverhead=gettimeroverhead();
      /* maybe we have a very fast timer*/
      if ( dtimeroverhead == 0.0 )
         {  
         dtimeroverhead=MINTIME;
         }
      }

   for( itype = 1; itype < 8; itype++ )
      {
      MPI_Barrier( MPI_COMM_WORLD );
      MPI_Allreduce( &isendbuf, ircvbuf, 1, MPI_INT, MPI_SUM,
                     MPI_COMM_WORLD );
      iproblemsize=( long )( MATMUL_START + ( icallsize - 1 )
                             * MATMUL_INCREMENT );
      allocateANDtouch( ( fds* ) pmyfds, &iproblemsize );
      dcalloverhead = getseqentryoverhead( pmyfds );

        MPI_Barrier( MPI_COMM_WORLD );
      dtime = bi_gettime();
      entry_( pmyfds, &iproblemsize, itype );
      MPI_Barrier( MPI_COMM_WORLD );
      dtime = bi_gettime() - dtime;
      dtime = dtime - dcalloverhead - dtimeroverhead;

      if ( dtime < MINTIME )
         {
         dtime=MINTIME;
         }

      if ( irank == 0 )
         {
         if ( pdresults != NULL )
            {
            pdresults[0] = ( double ) ( iproblemsize );
            pdresults[itype] = ( 2 * ( double ) iproblemsize)
               * ( ( double ) iproblemsize )
               * ( ( double ) iproblemsize )
               * ( ( double ) MATMUL_PRECISION)
               * ( ( double ) inumproz ) / ( ( double ) dtime );
            }
          }
      deallocate( (fds*) pmyfds );
      }

   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function bi_entry\n" );
      fflush(stdout);
      }
   return 0;
   }

/* matmul1.c/measurement::bi_cleanup
 * DESCRIPTION
 * Cleanup everything the kernel did not cleanup yet.
 * The arrays that were created in bi_init are freed here.
 ***/
void bi_cleanup( void *pointer )
   {
   fds *data = pointer;
   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function bi_cleanup\n" );
      fflush(stdout);
      }
   if (data!=NULL)
      {
      free( data );
      }
   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function bi_cleanup\n" );
      fflush(stdout);
      }
   }
