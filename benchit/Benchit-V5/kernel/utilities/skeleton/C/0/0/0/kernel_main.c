/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: c kernel skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.10 $
 * $Date: 2007/01/10 12:09:14 $
 *******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"
/*  Header for local functions
 */
#include "work.h"

/** These variables will help us to keep the overview over the arrays
  * we access for our functions/data.
  */
/* Number of different ways an algorithm will be measured.
   Example: loop orders: ijk, ikj, jki, jik, kij, kji -> n_of_works=6 with
   each different loop order in an own function. */
int n_of_works;
/* Number of fixed functions we have per measurement.
   Example: execution time and MFLOPS are measured for each loop order
   -> n_of_sure_funcs_per_work=2 */
int n_of_sure_funcs_per_work;

int MIN, MAX, INCREMENT;

/*  Header for local functions
 */
void evaluate_environment( void );

/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * infostruct )
{
   int i = 0, j = 0; /* loop var for n_of_works */
   (void) memset ( infostruct, 0, sizeof( bi_info ) );
   /* get environment variables for the kernel */
   evaluate_environment();
   infostruct->codesequence = bi_strdup( "work_[1|2]()" );
   infostruct->xaxistext = bi_strdup( "Problem Size" );
   infostruct->maxproblemsize = (MAX-MIN+1)/INCREMENT;
   if( (MAX-MIN+1) % INCREMENT != 0 )
     infostruct->maxproblemsize++;
   infostruct->num_processes = 1;
   infostruct->num_threads_per_process = 0;
//   infostruct->base_xaxis = 0;

   infostruct->kernel_execs_mpi1 = 0;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 0;
   infostruct->kernel_execs_pthreads = 0;
   /* B ########################################################*/
   n_of_works = 2; /* number versions of this algorithm (ijk, ikj, kij, ... = 6 */
   n_of_sure_funcs_per_work = 2; /* time measurement and FLOPS (calculated) */
   /*########################################################*/
   infostruct->numfunctions = n_of_works * n_of_sure_funcs_per_work;

   /* allocating memory for y axis texts and properties */
   infostruct->yaxistexts = malloc( infostruct->numfunctions * sizeof( char* ) );
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
   infostruct->base_yaxis = malloc( infostruct->numfunctions * sizeof( double ) );
   if ( infostruct->base_yaxis == 0 )
   {
     fprintf( stderr, "Allocation of base yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   /* setting up y axis texts and properties */
   for ( j = 0; j < n_of_works; j++ )
   {
      /* B ########################################################*/
      int index1 = 0 * n_of_works + j;
      int index2 = 1 * n_of_works + j;
      //int index3 = 2 * n_of_works + j;
      // 1st function
      infostruct->yaxistexts[index1] = bi_strdup( "Calculation Time in s" );
      infostruct->outlier_direction_upwards[index1] = 1;
      infostruct->base_yaxis[index1] = 0;
      // 2nd function
      infostruct->yaxistexts[index2] = bi_strdup( "FLOPS" );
      infostruct->outlier_direction_upwards[index2] = 0;
      infostruct->base_yaxis[index2] = 0;
      /*########################################################*/
      // 3rd function
      //infostruct->yaxistexts[index3] = bi_strdup( "" );
      //infostruct->outlier_direction_upwards[index3] = 0;
      //infostruct->base_yaxis[index3] = 0;
      switch ( j )
      {
         /* B ########################################################*/
         case 1: // 2nd version legend text; maybe (ikj)
            infostruct->legendtexts[index1] =
               bi_strdup( "Calculation Time in s (2)" ); // "... (ikj)"
            infostruct->legendtexts[index2] =
               bi_strdup( "FLOPS (2)" ); // "... (ikj)"
            break;
         case 0: // 1st version legend text; maybe (ijk)
         default:
            infostruct->legendtexts[index1] =
               bi_strdup( "Calculation Time in s (1)" ); // "... (ijk)"
            infostruct->legendtexts[index2] =
               bi_strdup( "FLOPS (1)" ); // "... (ijk)"
         /*########################################################*/
      }
   }
   if ( DEBUGLEVEL > 3 )
   {
      /* the next for loop: */
      /* this is for your information only and can be ereased if the kernel works fine */
      for ( i = 0; i < infostruct->numfunctions; i++ )
      {
         printf( "yaxis[%2d]=%s\t\t\tlegend[%2d]=%s\n",
            i, infostruct->yaxistexts[i], i, infostruct->legendtexts[i] );
      }
   }
}

/** Implementation of the bi_init of the BenchIT interface.
 *  Here you have the chance to allocate the memory you need.
 *  It is also possible to allocate the memory at the beginning
 *  of every single measurment and to free the memory thereafter.
 *  But making usage always of the same memory is faster.
 *  HAVE A LOOK INTO THE HOWTO !
 */
void* bi_init( int problemsizemax )
{
   mydata_t* mdp;
   mdp = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( mdp == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
/*   if ( problemsizemax > STEPS )
   {
      fprintf( stderr, "Illegal maximum problem size\n" ); fflush( stderr );
      exit( 127 );
   }/*
   /* B ########################################################*/
   /* malloc our own arrays in here */
   /*########################################################*/
   return (void*)mdp;
}

/** The central function within each kernel. This function
 *  is called for each measurment step seperately.
 *  @param  mdpv         a pointer to the structure created in bi_init,
 *                       it is the pointer the bi_init returns
 *  @param  problemsize  the actual problemsize
 *  @param  results      a pointer to a field of doubles, the
 *                       size of the field depends on the number
 *                       of functions, there are #functions+1
 *                       doubles
 *  @return 0 if the measurment was sucessfull, something
 *          else in the case of an error
 */
int bi_entry( void* mdpv, int problemsize, double* results )
{
  /* ts, te: the start and end time of the measurement */
  /* timeinsecs: the time for a single measurement in seconds */
  double ts = 0.0, te = 0.0, timeinsecs = 0.0;
  /* flops stores the calculated FLOPS */
  double flops = 0.0;
  /* j is used for loop iterations */
  int j = 0;
  /* cast void* pointer */
  mydata_t* mdp = (mydata_t*)mdpv;

  /* calculate real problemsize */
  problemsize = MIN + ( problemsize - 1 ) * INCREMENT;

  /* check wether the pointer to store the results in is valid or not */
  if ( results == NULL ) return 1;

  /* B ########################################################*/
  /* maybe some init stuff in here */
  mdp->dummy = 0;
  /*########################################################*/

  for ( j = 0; j < n_of_works; j++ )
  {
    /* B ########################################################*/
    int index1 = 0 * n_of_works + j;
    int index2 = 1 * n_of_works + j;
    /* reset of reused values */
    ts = 0.0;
    te = 0.0;
    /* choose version of algorithm */
    switch ( j ) {
      case 1: // 2nd version legend text; maybe (ikj)
        /* take start time, do measurment, and take end time */
        ts = bi_gettime(); work_2(); te = bi_gettime();
        break;
      case 0: // 1st version legend text; maybe (ijk)
      default:
        /* take start time, do measurment, and take end time */
        ts = bi_gettime(); work_1(); te = bi_gettime();
    }
    /* calculate the used time and FLOPS */
    timeinsecs = te - ts;
    timeinsecs -= dTimerOverhead;
    // this flops value is a made up! this calulations should be replaced
    // by something right for the choosen algorithm
    flops = (double)problemsize;
    /* If the operation was too fast to be measured by the timer function,
     * mark the result as invalid */
    if( timeinsecs < dTimerGranularity ) timeinsecs = INVALID_MEASUREMENT;
    /* store the results in results[1], results[2], ...
    * [1] for the first function, [2] for the second function
    * and so on ...
    * the index 0 always keeps the value for the x axis
    */
    /* B ########################################################*/
    // the xaxis value needs to be stored only once!
    if ( j == 0 ) results[0] = (double)problemsize;
    results[index1 + 1] = timeinsecs;
    results[index2 + 1] = flops;
    /*########################################################*/
  }

  return 0;
}

/** Clean up the memory
 */
void bi_cleanup( void* mdpv )
{
   mydata_t* mdp = (mydata_t*)mdpv;
   /* B ########################################################*/
   /* may be freeing our arrays here */
   /*########################################################*/
   if ( mdp ) free( mdp );
   return;
}
/********************************************************************/
/*************** End of interface implementations *******************/
/********************************************************************/

/* Reads the environment variables used by this kernel. */
void evaluate_environment()
{
   int errors = 0;
   char * p = 0;
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MIN", 0 );
   if ( p == 0 ) errors++;
   else MIN = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MAX", 0 );
   if ( p == 0 ) errors++;
   else MAX = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT", 0 );
   if ( p == 0 ) errors++;
   else INCREMENT = atoi( p );
   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      fprintf( stderr, "This kernel needs the following environment variables:\n" );
      fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_MIN\n" );
      fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_MAX\n" );
      fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT\n" );
      fprintf( stderr, "\nThis kernel will iterate from BENCHIT_KERNEL_PROBLEMSIZE_MIN\n\
to BENCHIT_KERNEL_PROBLEMSIZE_MAX, incrementing by\n\
BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT with each step.\n" );
      exit( 1 );
   }
}


/********************************************************************
 * Log-History
 *
 * $Log: kernel_main.c,v $
 * Revision 1.10  2007/01/10 12:09:14  william
 * added 'base_xaxis' to the infostruct because of a request from the LARS lecture
 *
 * Revision 1.9  2006/01/09 16:24:20  william
 * updated the cvs-header
 *
 * Revision 1.8  2005/12/15 15:44:18  hackenb
 * modified/unified header and footer
 *
 * Revision 1.7  2005/12/13 00:40:33  mickler
 * + Removed references to bi_info::log_[xy]axis
 * + Changed for new unified min, max, increment format
 * 
 * Revision 1.6  2005/12/12 18:15:42  hackenb
 * now define the iterating strategy in PARAMETERS using BENCHIT_MIN, BENCHIT_MAX and BENCHIT_INCREMENT
 * 
 * Revision 1.5  2005/11/22 01:26:53  mickler
 * + Using BI_GET_CALL_OVERHEAD_FUNC macro now
 * + Using bi_timer() function for time measuring
 * 
 * Revision 1.4  2005/11/04 16:24:22  mickler
 * # Changed kernels to set base_[xy]axis=0 if not using logarithmic scale
 * - Fixed some kernels with log_[xy]axis=0 and base_[xy]axis=10 to base=0
 * 
 * Revision 1.3  2005/07/29 11:53:51  wloch
 * removed setting of kernelname in bi getinfo and equalized parameter variables
 * 
 * Revision 1.2  2005/07/20 12:48:52  wloch
 * kernellanguage and libraries are now taken from kernel name
 * 
 * Revision 1.1.1.1  2005/07/18 13:03:20  wloch
 * the final restructured benchit source tree
 * 
 * Revision 1.4  2005/06/21 12:27:24  wloch
 * adapted to new interface in JBI and benchit c
 * 
 * Revision 1.3  2005/06/16 15:16:40  wloch
 * adapted environment hashing changes to bi getenv
 * 
 * Revision 1.2  2005/02/15 11:01:22  wloch
 * spelling corrections
 * 
 * Revision 1.1  2005/02/11 10:29:40  wloch
 * code skeleton for c kernels
 * 
 *******************************************************************/
