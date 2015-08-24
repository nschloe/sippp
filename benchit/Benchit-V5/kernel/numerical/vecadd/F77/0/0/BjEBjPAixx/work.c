/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.7 $
 *  $Date: 2006/09/29 16:34:03 $
 *******************************************************************/
#include "work.h"

int VECADD_START;
int VECADD_INCREMENT;
int VECADD_STEPS;
int VECADD_PRECISION;

/* Reads the environment variables used by this kernel. */
void evaluate_environment()
{
   int errors = 0;
   char * p = 0;
   VECADD_START = 1;
   VECADD_INCREMENT = 1;
   VECADD_STEPS = 10;   
   VECADD_PRECISION = 1;
   
   p = bi_getenv( "START", 0 );
   if ( p == 0 ) errors++;
   else VECADD_START = atoi( p );
   
   p = bi_getenv( "INCREMENT", 0 );
   if ( p == 0 ) errors++;
   else VECADD_INCREMENT = atoi( p );
   
   p = bi_getenv( "STEPS", 0 );
   if ( p == 0 ) errors++;
   else VECADD_STEPS = atoi( p );
   
   p = bi_getenv( "PRECISION", 0 );
   if ( p == 0 ) errors++;
   else VECADD_PRECISION = atoi( p );

   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      fprintf( stderr, "This kernel needs the following environment variables:\n" );
      fprintf( stderr, "START\n" );
      fprintf( stderr, "INCREMENT\n" );
      fprintf( stderr, "STEPS\n" );
      fprintf( stderr, "PRECISION\n" );
      fprintf( stderr, "\tThis kernel will vary the vectorlength from START to START+INCREMENT*STEPS in INCREMENT steps.\n" );
      exit( 1 );
   }
}



void bi_getinfo( bi_info * infostruct )
   {
   int  ii;
   char buffer[200];

   (void) memset ( infostruct, 0, sizeof( bi_info ) );

   /* get environment variables for the kernel */
   evaluate_environment();
   infostruct->codesequence = bi_strdup ( "vecadd ixxj: b(j)=b(j)+a(ixx)" );
   infostruct->maxproblemsize = VECADD_STEPS;
   infostruct->xaxistext = bi_strdup( "vector size" );
   infostruct->num_processes = 1;
   infostruct->num_threads_per_process = 0;
   infostruct->kernel_execs_mpi1 = 0;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 0;
   infostruct->kernel_execs_pthreads = 0;
   infostruct->numfunctions = 8;

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
      sprintf(buffer, "unrolled %d", ii+1 );
      infostruct->legendtexts[ii] = bi_strdup( buffer );
   }

   if ( DEBUGLEVEL > 3 )
   {
      for ( ii = 0; ii < infostruct->numfunctions; ii++ )
      {
         printf( "yaxis[%2d]=%s\t\t\tlegend[%2d]=%s\n",
            ii, infostruct->yaxistexts[ii], ii, infostruct->legendtexts[ii] );
      }
   }
}




void *bi_init( int iproblemsizemax )
{
   fds *pmyfds;

    pmyfds=malloc( sizeof( fds ) );

    if ( pmyfds==NULL )
       {
       printf("Allocation of myfds failed (problemsize max %d)\n",
             iproblemsizemax);
       exit(127);
       }

    pmyfds->VECADD_START = VECADD_START;
    pmyfds->VECADD_INCREMENT = VECADD_INCREMENT;
    pmyfds->VECADD_STEPS = VECADD_STEPS;
    pmyfds->VECADD_PRECISION = VECADD_PRECISION;

   return pmyfds;
}

/* vecadd1.c/interface::bi_entry
 * DESCRIPTION
 * This function is globally used for starting the measurment.
 * In this case the time is measured that is needed for the
 * execution of "tstcas_".
 * Afterwards the validation is made by just checking if
 * the numbers are in the correct order.
 * SEE ALSO
 * vecadd1.c/measurement::tstcas_
 ***/
int bi_entry( void *pmyfds, int icallsize, double *pdresults )
{
   fds * pfds = ( fds * ) pmyfds;
   static double dtimeroverhead = 0, dcalloverhead = 0;
   double starttime, endtime, dtime;
   int iunrolled, isendbuf, *ircvbuf, inumproz, iproblemsize, iflo_ops, ii;

   iflo_ops = 0;
   starttime = 0;
   endtime = 0;
   dtime = 0;
   inumproz=0;
   ircvbuf = &inumproz;
   isendbuf = 1;

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function bi_entry\n" );
      fflush(stdout);
      }

   for( iunrolled = 1; iunrolled < 9; iunrolled++ )
      {
      inumproz = 1; /* just a featurefor later mods */
      iproblemsize=( long )( pfds->VECADD_START + ( icallsize - 1 )
                             * pfds->VECADD_INCREMENT );
/*
      if (iunrolled == 1) {printf( "iproblemsize=%d ", iproblemsize); fflush(stdout);}
*/
      allocateANDtouch( pfds, &iproblemsize );
/*
      dcalloverhead = getseqentryoverhead( pfds );
*/

      starttime = bi_gettime();
      entry_( pfds, &iproblemsize, iunrolled );
/*
printf("work.c:   entry_( %d, %d, %d );\n", pfds, iproblemsize, iunrolled);
*/
      endtime = bi_gettime();
      dtime = endtime - starttime;
      /* - dtimeroverhead; */
      
      /* test for sanity */
/*
      printf("a[i]\t\tb[i]\n");
      for (ii=0; ii<iproblemsize; ii++){
        printf("%.2f\t\t%.2f\n",pfds->pda[ii], pfds->pdb[ii]);
      }
      printf("\n\nsum = %.2f\n\n", pfds->sum);
*/

      if ( DEBUGLEVEL > 0 )
      {
         printf( "VECADD_START=%d, VECADD_INCREMENT=%d, VECADD_STEPS=%d, VECADD_PRECISION=%d\n", pfds->VECADD_START = VECADD_START, pfds->VECADD_INCREMENT = VECADD_INCREMENT, pfds->VECADD_STEPS = VECADD_STEPS, pfds->VECADD_PRECISION = VECADD_PRECISION ) ;
	 printf( "iproblemsize=%d, iunrolled=%d \n", iproblemsize, iunrolled);
	 fflush(stdout);
      }

      if ( dtime < MINTIME )
         {
         dtime=MINTIME;
         }

      if ( pdresults != NULL )
         {
         pdresults[0] = ( double ) ( iproblemsize );
         iflo_ops = ((double)iproblemsize) * ((double)iproblemsize) * ((double)pfds->VECADD_PRECISION) + ((double)iproblemsize);
         
         pdresults[iunrolled] = (double)( (double) iflo_ops / (double) dtime );
/*
         pdresults[iunrolled] = endtime - starttime;
*/
         }
      }

   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function bi_entry\n" ) ;
      fflush(stdout);
      }
   return 0;
   }

/* vecadd1.c/measurement::bi_cleanup
 * DESCRIPTION
 * Cleanup everything the kernel did not cleanup yet.
 * The arrays that were created in bi_init are freed here.
 ***/
void bi_cleanup( void *pointer )
{
   fds * data = pointer;
   deallocate( (fds * ) data );

   if ( DEBUGLEVEL > 0 )
      {
      printf( "reached function bi_cleanup\n" ) ;
      fflush(stdout);
      }
   if (data!=NULL)
      {
      free( data );
      }
   if ( DEBUGLEVEL > 0 )
      {
      printf( "completed function bi_cleanup\n" ) ;
      fflush(stdout);
      }
   }
