/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.6 $
 *  $Date: 2006/09/29 16:34:03 $
 *******************************************************************/
#include "work.h"


/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t * pmydata)
{
   int errors = 0;
   char * p = 0;
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MIN", 0 );
   if ( p == NULL ) errors++;
   else pmydata->min = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MAX", 0 );
   if ( p == NULL ) errors++;
   else pmydata->max = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT", 0 );
   if ( p == NULL ) errors++;
   else pmydata->increment = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_PRECISION", 0 );
   if ( p == NULL ) errors++;
   else pmydata->precision = atoi( p );
   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      exit( 1 );
   }
   pmydata->steps = (myinttype) ( pmydata->max - pmydata->min + 1 ) / pmydata->increment;
   if (( pmydata->max - pmydata->min + 1 ) % pmydata->increment != 0) pmydata->steps++;
}




void bi_getinfo( bi_info * pinfo )
   {
   int  ii;
   char buffer[200];
   mydata_t * penv;
   
   (void) memset ( pinfo, 0, sizeof( bi_info ) );
   penv = (mydata_t *) malloc( sizeof( mydata_t ) );

   /* get environment variables for the kernel */
   evaluate_environment(penv);
   pinfo->codesequence = bi_strdup( "vecadd: c(jxx)=b(jxx)+a(jxx)" );
   pinfo->kerneldescription = bi_strdup( "vector addition with 3 full vectors" );
   pinfo->xaxistext = bi_strdup( "vector size" );
   pinfo->maxproblemsize = penv->steps;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 0;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 0;
   pinfo->numfunctions = 8;

   /* allocating memory for y axis texts and properties */
   pinfo->yaxistexts = malloc( pinfo->numfunctions * sizeof ( char * ) );
   if ( pinfo->yaxistexts == 0 )
   {
     fprintf( stderr, "Allocation of yaxistexts failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   pinfo->outlier_direction_upwards = malloc( pinfo->numfunctions * sizeof( int ) );
   if ( pinfo->outlier_direction_upwards == 0 )
   {
     fprintf( stderr, "Allocation of outlier direction failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   pinfo->legendtexts = malloc( pinfo->numfunctions * sizeof( char* ) );
   if ( pinfo->legendtexts == 0 )
   {
     fprintf( stderr, "Allocation of legendtexts failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   pinfo->log_yaxis = malloc( pinfo->numfunctions * sizeof( int ) );
   if ( pinfo->log_yaxis == 0 )
   {
     fprintf( stderr, "Allocation of log yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   pinfo->base_yaxis = malloc( pinfo->numfunctions * sizeof( double ) );
   if ( pinfo->base_yaxis == 0 )
   {
     fprintf( stderr, "Allocation of base yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }

   /* setting up y axis texts and properties */
   for ( ii = 0; ii < pinfo->numfunctions; ii++ )
   {
      pinfo->yaxistexts[ii] = bi_strdup( "FLOPS" );
      pinfo->outlier_direction_upwards[ii] = 1;
      pinfo->log_yaxis[ii] = 0;
      pinfo->base_yaxis[ii] = 0;
      sprintf(buffer, "unrolled %d", ii+1 );
      pinfo->legendtexts[ii] = bi_strdup( buffer );
   }

   if ( DEBUGLEVEL > 3 )
   {
      for ( ii = 0; ii < pinfo->numfunctions; ii++ )
      {
         printf( "yaxis[%2d]=%s\t\t\tlegend[%2d]=%s\n",
            ii, pinfo->yaxistexts[ii], ii, pinfo->legendtexts[ii] );
      }
   }
}




void* bi_init( int problemsizemax )
{
   mydata_t * pmydata;

   pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( pmydata == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   evaluate_environment(pmydata);
//  fprintf( stderr, "max=%d, min=%d, increment=%d, steps=%d\n",pmydata->max, pmydata->min, pmydata->increment, pmydata->steps);
   return (void *)pmydata;
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
int bi_entry( void * mdpv, int icallsize, double *pdresults )
{
   mydata_t * pmydata = (mydata_t *) mdpv;
   static double dtimeroverhead = 0, dcalloverhead = 0;
   double dtime;
   int iunrolled, isendbuf, *ircvbuf, inumproz, iproblemsize;

   dtime = 0;
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

   for( iunrolled = 1; iunrolled < 9; iunrolled++ )
      {
      iproblemsize=( long )( pmydata->min + (( icallsize - 1 )
                             * pmydata->increment ));
      allocateANDtouch( ( mydata_t* ) pmydata, &iproblemsize );
      dcalloverhead = getseqentryoverhead( pmydata );

      dtime = bi_gettime();
      entry_( pmydata, &iproblemsize, iunrolled );
      dtime = bi_gettime() - dtime;
//      dtime = dtime - dcalloverhead - dtimeroverhead;

      if ( DEBUGLEVEL > 0 )
      {
         printf( "VECADD_START=%d, VECADD_INCREMENT=%d, VECADD_STEPS=%d, VECADD_PRECISION=%d\n", pmydata->min, pmydata->increment, pmydata->steps, pmydata->precision ) ;
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
         pdresults[iunrolled] = (( ( double ) iproblemsize)
            + ( ( double ) iproblemsize )
            * ( ( double ) pmydata->precision ))
            / ( ( double ) dtime );
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
/** Clean up the memory
 */
void bi_cleanup( void* mdpv )
{
   mydata_t * pmydata = (mydata_t*)mdpv;
   if ( pmydata ) free( pmydata );
   return;
}
